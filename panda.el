;;; panda.el --- Client for Bamboo's REST API.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Sebastián Monía
;;
;; Author: Sebastián Monía <smonia@outlook.com>
;; URL: https://github.com/sebasmonia/panda
;; Package-Requires: ((emacs "25"))
;; Version: 1.1
;; Keywords: maint tool

;; This file is not part of GNU Emacs.

;;; License: MIT

;;; Commentary:

;; Consume Bamboo's terrible REST API to do useful things
;;
;; Steps to setup:
;;   1. Place the Panda files in your load-path.  Or install from MELPA.
;;   2. Customize 'panda' to add the Bamboo URL or manually:
;;      (setq 'panda-api-url "https://bamboo.yourorg.com/rest/api/latest"))
;;      - No trailing / -
;;   3. There's a keymay provided for convenience
;;       (require 'panda)
;;        (global-set-key (kbd "C-c b") 'panda-map) ;; b for "Bamboo"
;;
;; For a detailed user manual see:
;; https://github.com/sebasmonia/panda/blob/master/README.md

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'panda-api-utils)
(require 'panda-structs)

(defgroup panda nil
  "Client for Bamboo's REST API."
  :group 'extensions)

(defcustom panda-api-url ""
  "Base URL of the Bamboo API, for example https://bamboo.my-company.com/rest/api/latest, no trailing slash!!!."
  :type 'string)

(defcustom panda-browser-url ""
  "URL to the Bamboo website, to launch a browser to view items.  For example https://bamboo.my-company.com, no trailing slash!!!."
  :type 'string)

(defcustom panda-username ""
  "Username, if empty it will be prompted."
  :type 'string)

(defcustom panda-less-messages nil
  "Display less messages in the echo area.
You can always read Panda's messages in the \"*panda-log*\" buffer."
  :type 'boolean)

(defcustom panda-log-responses nil
  "Display API responses in the log.
Extremely useful for debugging but way too verbose for every day use."
  :type 'boolean)

;; consider making this an independent parameter
;; for builds and deployments
(defcustom panda-latest-max-results 7
  "How many items to retrieve when pulling lists of \"latest items\"."
  :type 'integer)

(defcustom panda-silence-url t
   "Ask url.el not to show messages."
   :type 'boolean)

(defcustom panda-api-timeout 30
   "Timeout for Bamboo API calls, in seconds."
   :type 'integer)

(defcustom panda-deploy-confirmation-regex ""
   "If an environment name matches the regex, Panda will request confirmation before submitting the deploy."
   :type 'string)

(defcustom panda-prefix-buffers nil
   "Don't prefix Panda's buffers with \"Panda\".
The names are long enough on their own! On the other hand, the common prefix makes buffer switching easier."
  :type 'boolean)

(defcustom panda-open-status-after-build 'ask
  "Open the build status for the corresponding branch after requesting a build.
If yes, automatically open it.  No to never ask.  Set to 'ask (default) to be prompted each time."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Ask" ask)))

(defcustom panda-open-status-after-deploy 'ask
  "Open the status for the corresponding project after requesting a deploy.
If yes, automatically open it.  No to never ask.  Set to 'ask (default) to be prompted each time."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Ask" ask)))

(defvar panda--auth-string nil "Caches the credentials for API calls.")
(defvar panda--builds-cache nil "Caches all the projects and plans (and eventually branches) the user has access to.")
(defvar panda--deploys-cache nil "Caches the deployment projects (not build projects) using a single call to /deploy/project/all.")

(defvar panda--base-plan "[Master plan]")
(defvar panda--build-status-for-release "Successful" "This is the only build status allowed to create releases.")

(defvar panda--browse-build "/browse/%s" "What to add to 'panda-browser-url to open builds in the browser.")
(defvar panda--browse-deploy-project "/deploy/viewDeploymentProjectEnvironments.action?id=%s" "What to add to 'panda-browser-url to open deploy projects in the browser.")

(defvar panda--buffer-name-alist
  '((details . "Build Details %s")
    (builds . "Builds %s")
    (deploys . "Environments %s")
    (env-history . "Env. History %s")
    (deploy-log . "Deploy log %s")
    (build-log . "Build log %s"))
  "Templates for the different Panda buffer names.")

(defvar panda--build-buffer-template "
Build key: %s

Project: %s
Master plan: %s
Plan name: %s

State: %s
Started: %s
Finished: %s
Duration: %s

Reason: %s
Build test summary: %s

Jira Issues:
%s

Changes:
%s

Artifacts:
%s" "Template to call 'format' for the build details buffer.")

(defvar-local panda--branch-key nil "Used in `panda--build-results-mode' to store the current branch key.")
(defvar-local panda--current-deploy-project nil "Used in `panda--deploy-results-mode' to store the current deployment project.")

(defvar panda-map
  (let ((main-map (make-sparse-keymap "Bamboo operations"))
        (queue-map (define-prefix-command 'queue nil "Queue new"))
        (status-map (define-prefix-command 'status nil "Status of")))
    (define-key queue-map (kbd "d") `("deploy" . panda-queue-deploy))
    (define-key queue-map (kbd "b") `("build" . panda-queue-build))

    (define-key status-map (kbd "e") `("environment" . panda-environment-history))
    (define-key status-map (kbd "d") `("deployments" . panda-deploy-status))
    (define-key status-map (kbd "b") `("builds" . panda-build-results))

    (define-key main-map (kbd "r") '("refresh cache" . panda-refresh-cache))
    (define-key main-map (kbd "c") '("create release" . panda-create-release))
    (define-key main-map (kbd "q") `("queue..." . ,queue-map))
    (define-key main-map (kbd "s") `("status..." . ,status-map))
    main-map))

; Interactive commands not mapped:
;; panda-clear-credentials

;;------------------Cache for credentials, projects, plans, and branches----------

(defun panda-clear-credentials ()
  "Clear current credentials, next API call will request them again."
  (interactive)
  (setq panda--auth-string nil)
  (panda--message "Done. Next API call will request credentials."))

(defun panda-refresh-cache ()
  "Refresh the cache of projects, plans, and deploys."
  (interactive)
  (panda--refresh-cache-builds)
  (panda--refresh-cache-deploys))

(defun panda--refresh-cache-builds ()
  "Refresh the cache of projects and plans."
  (panda--message "Refreshing Bamboo build projects cache...")
  (setf panda--builds-cache
        (panda--convert-project-response
         ;; If you have more than 10000 projects I doubt you are using this package
         (panda--api-call "/project" "expand=projects.project.plans&max-results=10000"))))

(defun panda--cache-branches (a-plan)
  "Fetch the branches for A-PLAN for future use. Also return the cached branches."
  (let ((branches-from-api (panda--convert-branches-response
                            (panda--api-call (concat "/plan/" (panda--plan-key a-plan) "/branch")))))
    (setf (panda--plan-branches a-plan)
          ;; add master plan to the list of branches returned
          (push (panda--make-branch :name panda--base-plan
                                    :key (panda--plan-key a-plan))
                branches-from-api))))

(defun panda--refresh-cache-deploys ()
  "Refresh the cache of deploys."
  (panda--message "Refreshing Bamboo deployment cache...")
  (setf panda--deploys-cache (panda--convert-deploy-response
                              (panda--api-call "/deploy/project/all"))))

(defun panda--format-branch-cache (br-data)
  "Format BR-DATA for the project cache."
  (let-alist br-data
    (cons .shortName .key)))

(defun panda--deploys ()
  "Get cached list of deploy projects, fetch them if needed."
  (unless panda--deploys-cache
    (panda-refresh-cache))
  panda--deploys-cache)

(defun panda--all-environments ()
  "Return all environments from the cache, in a single list."
  (apply #'append (mapcar (lambda (deploy-project) (nthcdr 2 deploy-project))
                         (panda--deploys))))

;;------------------Build querying and information--------------------------------

(defun panda-display-build-info (build-key)
  "Show a buffer with the details of BUILD-KEY.  Invoked from build status list."
  (let* ((data (panda--api-call (concat "/result/" build-key)
                                "expand=changes,metadata,artifacts,comments,jiraIssues,variable,stages"))
         (buffer-name (format (panda--get-buffer-name 'details) build-key))
         (buffer (get-buffer-create buffer-name))
         (data-to-display nil))
      (setq data-to-display
            (list build-key
                  (gethash "projectName" data)
                  (or (panda--gethash '(master shortName) data) "--")
                  (gethash "planName" data)
                  (gethash "state" data)
                  (gethash "prettyBuildStartedTime" data)
                  (gethash "prettyBuildCompletedTime" data "--")
                  (gethash "buildDurationDescription" data "--")
                  (gethash "buildReason" data "--")
                  (gethash "buildTestSummary" data "--")
                  (panda--build-info-format-jira-issues (panda--gethash
                                                         '(jiraIssues issue)
                                                         data))
                  (panda--build-info-format-changes (panda--gethash
                                                     '(changes change)
                                                     data))
                  (panda--build-info-format-artifacts (panda--gethash
                                                       '(artifacts artifact)
                                                       data))))
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (kill-region (point-min) (point-max)) ;; in case of an update
        (insert (apply #'format panda--build-buffer-template data-to-display))
        (setq buffer-read-only t)
        (local-set-key "g" (lambda ()
                             (interactive)
                             (panda-display-build-info build-key)))
        (local-set-key "q" (lambda ()
                             (interactive)
                             (kill-buffer)))
        (goto-char (point-min))
        (pop-to-buffer buffer)
        (panda--message (concat "Showing details for build " build-key)))))

(defun panda--build-info-format-jira-issues (issues)
  "Create a printable string out of ISSUES."
  (if (= (length issues) 0)
      "--" ;; return "--" if no issues
    ;; why this doesn't work with concat and does with mapconcat?
    ;; it is a mystery...
    (mapconcat #'identity
               (cl-loop for an-issue across issues
                        collect (apply #'format "%s\t%s\t%s\t%s\t\"%s\""
                                       (list
                                        (gethash "key" an-issue "--")
                                        (gethash "issueType" an-issue "--")
                                        (gethash "status" an-issue "--")
                                        (gethash "asignee" an-issue "--")
                                        (gethash "summary" an-issue "--"))))
               "\n")))

(defun panda--build-info-format-changes (changes-vector)
  "Create a printable string out of CHANGES-VECTOR."
  (if (= (length changes-vector) 0)
      "--" ;; defaults to "" if there's no changeset info
    (mapconcat #'identity
               (cl-loop for change across changes-vector
                        collect (apply #'format "%s\t%s"
                                       (list (gethash "changesetId" change "--")
                                             (gethash "fullName" change "--"))))
               "\n")))

(defun panda--build-info-format-artifacts (artifacts-vector)
  "Create a printable string out of ARTIFACTS-VECTOR."
  (if (= (length artifacts-vector) 0)
      "--" ;; defaults to "--" if there are no artifacts
    (mapconcat #'identity
               (cl-loop for artifact across artifacts-vector
                        collect (apply #'format "%s\t%s\tShared: %s"
                                       (list (gethash "name" artifact "--")
                                             (gethash "prettySizeDescription" artifact "--")
                                             (if (gethash "shared" artifact)
                                                 "Yes"
                                               "No"))))
               "\n")))

(defun panda-display-build-logs (build-key)
  "Show a buffer with the logs for the jobs of BUILD-KEY.  Invoked from build status list."
  ;; there is a better way to replace the last part of the build key using a regex
  ;; but I couldn't crack it :(
  (let* ((key-components (split-string build-key "-"))
         (plan-key (apply #'format "%s-%s" (butlast key-components)))
         (build-number (car (last key-components)))
         (plan-jobs (panda--job-keys-names-for-plan plan-key))
         (buffer-name (format (panda--get-buffer-name 'build-log) build-key))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (j plan-jobs)
        (push-mark (point) t nil) ;; This is so that each header has a local mark
        (insert (format "Logs for job \"%s\" (%s):\n\n" (cdr j) (car j)))
        (insert (panda--build-log-from-build-job-data
                 (panda--api-call (format "/result/%s-%s" (car j) build-number)
                                  "expand=logEntries&max-results=1000000")))
        (insert "\n\n"))
      (setq buffer-read-only t)
      (local-set-key "g" (lambda ()
                           (interactive)
                           (panda-display-build-logs build-key)))
      (local-set-key "q" (lambda ()
                           (interactive)
                           (kill-buffer)))
      (panda--message (format "Showing log for the jobs of build %s. Press g to refresh, q to close the buffer." build-key))
      (pop-to-buffer buffer))))

(defun panda--job-keys-names-for-plan (plan-key)
  "Get the job keys for PLAN-KEY."
  (cl-loop for job across (gethash "searchResults" (panda--api-call (format "/search/jobs/%s" plan-key)))
           collect (cons (panda--gethash '(searchEntity key) job)
                         (panda--gethash '(searchEntity jobName) job))))

(defun panda--build-log-from-build-job-data (build-job-data)
  "Extract the log entries from BUILD-JOB-DATA."
  ;; TODO merge this and the function that does the same for deploy logs. They are identical!
  (mapconcat 'identity
             (cl-loop for log-entry across (panda--gethash '(logEntries logEntry) build-job-data)
                      collect (apply #'format "[%s] - %s"
                                     (list (gethash "formattedDate" log-entry)
                                           (gethash "unstyledLog" log-entry))))
             "\n"))

(defun panda-queue-build (&optional plan-key)
  "Queue a build.  If PLAN-KEY is not provided, select it interactively."
  (interactive)
  (cl-destructuring-bind (_project-key plan-key branch-key) (panda--select-build-ppb nil plan-key)
    (when (equal branch-key (concat plan-key "0"))
      ;; the base plan has a branch number of 0 but
      ;; won't build if using the prefix num
      (setq branch-key plan-key))
    (let ((show-status panda-open-status-after-build)) ;; later we'll check for 'ask or t
      (panda--api-call (concat "/queue/" branch-key) nil "POST")
      (when (eq panda-open-status-after-build 'ask)
        (setq show-status (y-or-n-p "Show build status for the branch? ")))
      (if show-status
          (panda-build-results-branch branch-key)
        (panda--message "Build queued")))))

(defun panda-build-results (&optional plan)
  "Fetch the build results for a branch under PLAN.
If PLAN is not provided, select it interactively.
The amount of builds to retrieve is controlled by 'panda-latest-max'."
  (interactive)
  (cl-destructuring-bind (_project-key _plan-key branch-key) (panda--select-build-ppb nil plan)
    (panda-build-results-branch branch-key)))

(defun panda-build-results-branch (branch-key)
  "Fetch the build results for a BRANCH-KEY.  For interactive branch selection, use `panda-build-results'."
  ;; only master plans return the build date on the top level call
  ;; the only option is to fetch the list of build keys and retrieve
  ;; the build date individually for each build
  (let* ((build-data (panda--build-results-data branch-key))
         (buffer-name (format (panda--get-buffer-name 'builds) branch-key))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      ;; setup the tablist
      (panda--build-results-mode)
      ;; buffer local variables
      (setq panda--branch-key branch-key)
      (setq tabulated-list-entries build-data)
      (tabulated-list-print)
      (pop-to-buffer buffer)
      (panda--message (concat "Listing builds for " branch-key ". Press ? for help and bindings available.")))))

(defun panda--build-results-data (branch-key)
  "Get BRANCH-KEY build data for 'tabulated-list-entries'."
  (cl-loop for build-key in (panda--latest-build-keys branch-key)
           collect (panda--fetch-build-bykey build-key)))

(defun panda--latest-build-keys (branch-key)
  "Get the list of links to retreive the latest builds for BRANCH-KEY."
    (let* ((target-url (concat "/result/" branch-key))
           (parameters (concat "max-results=" (number-to-string panda-latest-max-results)
                               "&includeAllStates=true"))
           (data (panda--api-call target-url parameters)))
      (let-alist data
        (cl-loop for build across (panda--gethash '(results result) data)
                 collect (gethash "key" build)))))

(defun panda--fetch-build-bykey (build-key)
  "Return the data for BUILD-KEY formatted for tabulated mode."
  (let* ((build-data (panda--api-call (concat "/result/" build-key)))
         (is-master-plan (gethash "master" build-data)))
    (list (gethash "key" build-data)
          (vector (gethash "key" build-data "")
                  (gethash "state" build-data "")
                  (gethash "prettyBuildStartedTime" build-data "")
                  (gethash "prettyBuildCompletedTime" build-data "")
                  (gethash "buildDurationDescription" build-data "")
                  (if is-master-plan
                      ;; for branches this will be non-empty
                      (panda--gethash '(master key) build-data)
                    ;; else, this is a base plan
                    (panda--gethash '(plan key) build-data))))))

(define-derived-mode panda--build-results-mode tabulated-list-mode "Panda build results view" "Major mode to display Bamboo's build results."
  (setq tabulated-list-format [("Build key" 20 nil)
                               ("State" 11 nil)
                               ("Started" 22)
                               ("Finished" 22 nil)
                               ("Duration" 0 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(define-key panda--build-results-mode-map (kbd "g") 'panda--build-results-refresh)
(define-key panda--build-results-mode-map (kbd "d") 'panda--build-results-info)
(define-key panda--build-results-mode-map (kbd "l") 'panda--build-results-log)
(define-key panda--build-results-mode-map (kbd "b") 'panda--build-results-browse)
(define-key panda--build-results-mode-map (kbd "c") 'panda--build-results-create)
(define-key panda--build-results-mode-map (kbd "?") 'panda--build-results-help)

(defun panda--build-results-refresh ()
  "Refresh data in a `panda--build-results-mode' buffer."
  (interactive)
  (setq tabulated-list-entries (panda--build-results-data panda--branch-key))
  (tabulated-list-print)
  (panda--message (concat "Updated list of builds for " panda--branch-key)))

(defun panda--build-results-info ()
  "Show build info in a `panda--build-results-mode' buffer."
  (interactive)
  (panda-display-build-info (tabulated-list-get-id)))

(defun panda--build-results-log ()
  "Show build logs in a `panda--build-results-mode' buffer."
  (interactive)
  (panda-display-build-logs (tabulated-list-get-id)))

(defun panda--build-results-browse ()
  "Brose a build from a `panda--build-results-mode' buffer."
  (interactive)
  (panda--browse (format panda--browse-build (tabulated-list-get-id))))

(defun panda--build-results-create ()
  "Create a release from the build under point in a `panda--build-results-mode' buffer."
  (interactive)
  (panda--create-release-from-build-status (tabulated-list-get-entry)))

(defun panda--build-results-help ()
  "Show help for the directories buffer."
  (interactive)
  (let ((help-message
        (concat
         "--Panda: Build status mode help--\n\n"
         "In this list you can see the latest builds for a given branch. You customize the group `panda' to "
         "modify the number of \"latest items\" retrieved.\n\n"
         "Bindings:\n\n"
         "* g will refresh the data, as usual in Emacs\n\n"
         "* d shows the (d)etails for the build under point in a new buffer\n\n"
         "* l shows the (l)ogs for the jobs of the build under point in a new buffer\n\n"
         "* b uses `panda-browser-url' to open Bamboo in your default (b)rowser to see build details\n\n"
         "* c to (c)reate a new release out of the build at point\n\n ")))
    (panda--show-help help-message)))

;;------------------Creating deployments and pushing them-------------------------

(defun panda-create-release ()
  "Create a new release from a succesful build."
  (interactive)
  (cl-destructuring-bind (_project-key plan-key branch-key) (panda--select-build-ppb nil nil)
    ;; I could re-work the cache to skip this call if I stored the plan key. But some
    ;; deploys dont have them, so I have to code for that too...let's have one extra
    ;; call and be done with it
    (let* ((did (panda--get-deployid-for-plan-key plan-key))
           (formatted (panda--successful-builds-for-release branch-key))
           (selected-build (completing-read "Select a build: " formatted))
           (release-name nil))
      (setq selected-build (car (split-string selected-build))) ;; really shady
      (setq release-name (read-string "Release name: " (panda--proposed-release-name did selected-build)))
      (panda--create-release-execute selected-build did release-name))))

(defun panda--get-deployid-for-plan-key (plan-key)
  "Obtain the deployment id for PLAN-KEY."
  (gethash "id" (elt (panda--api-call "/deploy/project/forPlan"
                                      (concat "planKey=" plan-key))
                     0)))

(defun panda--create-release-execute (build-key did release-name)
  "Make an API call to create a release in DID with RELEASE-NAME out of BUILD-KEY."
  (let ((payload (json-encode (list (cons 'planResultKey build-key) (cons 'name  release-name)))))
    (panda--api-call (format "/deploy/project/%s/version" did)
                     nil
                     "POST"
                     payload)))

(defun panda--create-release-from-build-status (selected-entry)
  "Create a new release out of SELECTED-ENTRY from the build status screen."
  (interactive)
  (let ((build-key (elt selected-entry 0))
        (plan-key (elt selected-entry 5))
        (build-status (elt selected-entry 1))
        (did nil)
        (release-name nil))
    (if (string= panda--build-status-for-release build-status)
        (progn
          (setq did (panda--get-deployid-for-plan-key plan-key))
          (setq release-name (read-string "Release name: " (panda--proposed-release-name did build-key)))
          (panda--create-release-execute build-key did release-name))
      (panda--message "Can't create a release from a non-successful build."))))

(defun panda--successful-builds-for-release (branch-key)
  "Return the last few successful builds for BRANCH-KEY."
  (let* ((last-builds (mapcar 'cadr (panda--build-results-data branch-key)))
         (successful (cl-remove-if-not (lambda (build) (equal (elt build 1) panda--build-status-for-release)) last-builds)))
    (mapcar (lambda (build) (concat (elt build 0) " - Completed: " (elt build 2))) successful)))

(defun panda--proposed-release-name (did build-key)
  "Use DID (deploy project id) and BUILD-KEY to generate the release name."
  (gethash "nextVersionName" (panda--api-call (format "/deploy/projectVersioning/%s/nextVersion" did)
                                              (concat "resultKey=" build-key))))

(defun panda-queue-deploy (&optional project-name environment-name)
  "Queue a deploy.  If PROJECT-NAME and ENVIRONMENT-ID are not provided, select them interactively."
  (interactive)
  (let* ((deploy-project (panda--select-deploy-project project-name))
         (release-data (panda--releases-for-did (panda--deploy-project-id deploy-project)))
         (release-name (completing-read "Select release: " release-data))
         (environment (panda--select-environment (panda--deploy-project-name deploy-project) environment-name))
         (confirmed t)) ;; we'll check if there's a regex match later
    (when (not (string-empty-p panda-deploy-confirmation-regex))
      (if (string-match-p panda-deploy-confirmation-regex (panda--environment-name environment))
          (setq confirmed (y-or-n-p (format "OK to deploy version %s to environment %s? " release-name environment-name)))
        (setq confirmed t))) ;; if it doesn't match the regex we don't need to ask
    (if confirmed
        (progn
          (panda--api-call "/queue/deployment"
                           (format "environmentId=%s&versionId=%s"
                                   (panda--environment-id environment)
                                   (alist-get release-name release-data nil nil 'equal))
                           "POST")
          (panda--message "Deployment requested")
          (if (and project-name environment-name) ;; not 100% correct way of identifying calls from the deploy status buffer
              (panda-deploy-status (panda--deploy-project-name deploy-project)) ;; just show it/update it
            (panda--show-deploy-status (panda--deploy-project-name deploy-project)))) ;; depends on the config
      (message "Deployment cancelled"))))

(defun panda--show-deploy-status (project-name)
  "Show PROJECT-NAME deploy status, according to the user preferences."
  (let ((show-status panda-open-status-after-deploy)) ;; later we'll check for 'ask or t
    (when (eq panda-open-status-after-deploy 'ask)
      (setq show-status (y-or-n-p "Show deployment status for the project? ")))
    (when show-status
      (panda-deploy-status project-name))))

(defun panda-deploy-status (&optional project-name)
  "Display a project's deploy status.  If PROJECT-NAME is not provided, select it interactively."
  (interactive)
  (let* ((deploy-project (panda--select-deploy-project project-name))
         (data-formatted (panda--deploy-status-fetch deploy-project))
         (buffer-name (format (panda--get-buffer-name 'deploys)
                              (panda--deploy-project-name deploy-project)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      ;; change to tablist mode
      (panda--deploy-results-mode)
      ;;buffer local variables
      (setq panda--current-deploy-project deploy-project)
      (setq tabulated-list-entries data-formatted)
      (tabulated-list-print)
      (pop-to-buffer buffer))
    (panda--message (concat "Listing deploy status for "
                            (panda--deploy-project-name deploy-project)
                            ". Press ? for help and bindings available."))))

(defun panda-environment-history (&optional project-name env-name)
  "Show the history of PROJECT-NAME -> ENV-NAME in a new buffer.
If parameters aren't provided, they will be prompted."
  (interactive)
  (let* ((environment (panda--select-environment project-name env-name))
         (data-formatted (panda--environment-history-fetch (panda--environment-id environment)))
         (buffer-name (format (panda--get-buffer-name 'env-history)
                              (panda--environment-name environment)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (panda--environment-history-mode)
      (setq tabulated-list-entries data-formatted)
      (tabulated-list-print)
      (local-set-key "l" (lambda ()
                           (interactive)
                           (panda--deploy-log (tabulated-list-get-id))))
      (pop-to-buffer buffer)
      (panda--message (concat "Showing deployment history. Press l to see a deploy log for the run under point.")))))

(defun panda--deploy-log (deploy-id)
  "Show the log of DEPLOY-ID in a new buffer."
  (let* ((logs (panda--deploy-log-fetch deploy-id))
         (buffer-name (format (panda--get-buffer-name 'deploy-log) deploy-id))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert logs)
      ;; When reading logs, usually you want to see last first...
      ;; OR: add &optional, and bind "g" using that optional param
      ;; for "don't go up after insert"
      ;; (goto-char (point-min))
      (setq buffer-read-only t)
      (local-set-key "g" (lambda ()
                           (interactive)
                           (panda--deploy-log deploy-id)))
      (local-set-key "q" (lambda ()
                           (interactive)
                           (kill-buffer)))
      (panda--message (format "Showing log for deploy %s. Press g to refresh, q to close the buffer." deploy-id))
      (pop-to-buffer buffer))))

(defun panda--env-id-from-name (env-name)
  "Obtain the env-id for ENV-NAME."
  (cadar (cl-remove-if-not (lambda (env) (string= env-name (car env)))
                           (panda--all-environments))))

(define-derived-mode panda--environment-history-mode tabulated-list-mode "Panda environment history view" "Major mode to display Bamboo's environment history."
  (setq tabulated-list-format [("State" 12 nil)
                               ("Status" 8)
                               ("Started" 20 nil)
                               ("Completed" 20 nil)
                               ("Version name" 0 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(define-derived-mode panda--deploy-results-mode tabulated-list-mode "Panda deploy results view" "Major mode to display Bamboo's deploy results."
  (setq tabulated-list-format [("Environment" 45 nil)
                               ("State" 12 nil)
                               ("Status" 8)
                               ("Started" 20 nil)
                               ("Completed" 20 nil)
                               ("Deploy ID" 12 nil)
                               ("Version name" 0 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(define-key panda--deploy-results-mode-map (kbd "g") 'panda--deploy-results-refresh)
(define-key panda--deploy-results-mode-map (kbd "d") 'panda--deploy-results-queue)
(define-key panda--deploy-results-mode-map (kbd "b") 'panda--deploy-results-browse)
(define-key panda--deploy-results-mode-map (kbd "h") 'panda--deploy-results-history)
(define-key panda--deploy-results-mode-map (kbd "l") 'panda--deploy-results-log)
(define-key panda--deploy-results-mode-map (kbd "?") 'panda--deploy-results-help)

(defun panda--deploy-results-help ()
  "Show help for the directories buffer."
  (interactive)
  (let ((help-message
        (concat
         "--Panda: Deploy status mode help--\n\n"
         "In this list you have one entry per environment for a given deployment project.\n\n"
         "Bindings:\n\n"
         "* g will refresh the data, as usual in Emacs\n\n"
         "* d opens a list of releases to queue a (d)eployment for the environment under point\n\n"
         "* b uses `panda-browser-url' to open Bamboo in your default (b)rowser to see environment details\n\n"
         "* l will open the (l)og for the last deployment of the environment at point\n\n"
         "* h opens the selected environment's (h)istory in a new buffer\n\n ")))
    (panda--show-help help-message)))

(defun panda--deploy-results-refresh ()
  "Reload the current `panda--deploy-results-mode' buffer."
  (interactive)
  (let ((deploy-proj-name (panda--deploy-project-name panda--current-deploy-project)))
    (panda-deploy-status deploy-proj-name)
    (panda--message (concat "Updated deploy status for " deploy-proj-name))))

(defun panda--deploy-results-browse ()
  "Open a browser in the deploy under point in `panda--deploy-results-mode'."
  (interactive)
  (panda--browse (format panda--browse-deploy-project
                         (panda--deploy-project-id panda--current-deploy-project))))

(defun panda--deploy-results-queue ()
  "Queue a deploy for the environment at point in a `panda--deploy-results-mode' list."
  (interactive)
  (panda-queue-deploy (panda--deploy-project-name panda--current-deploy-project)
                      (tabulated-list-get-id)))

(defun panda--deploy-results-history ()
  "Show the selected environment's history in `panda--deploy-results-mode'."
  (interactive)
  (panda-environment-history (panda--deploy-project-name panda--current-deploy-project)
                             (tabulated-list-get-id)))

(defun panda--deploy-results-log ()
  "Show the log for the last (or running) deploy in `panda--deploy-results-mode'."
  (interactive)
  (panda--deploy-log (elt (tabulated-list-get-entry) 5)))

(provide 'panda)
;;; panda.el ends here
