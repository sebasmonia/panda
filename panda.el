;;; panda.el --- Client for Bamboo's REST API.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Sebastian Monia
;;
;; Author: Sebastian Monia <smonia@outlook.com>
;; URL: https://github.com/sebasmonia/panda
;; Package-Requires: ((emacs "25"))
;; Version: 1.0
;; Keywords: maint tool

;; This file is not part of GNU Emacs.

;;; License: MIT

;;; Commentary:

;; Consume Bamboo's terrible REST API to do useful things
;; NOTE: This package hasn't hit version 1 yet, breaking
;;       changes are expected.
;;
;; Steps to setup:
;;   1. Place tfsmacs.el in your load-path.  Or install from MELPA (coming soon)
;;   2. Customize 'panda' to add the Bamboo URL or manually:
;;      (setq 'panda-base-url "https://bamboo.yourorg.com/rest/api/latest"))
;;      - No trailing / -
;;   3. Optionally, customize or manually set panda-username if you don't want
;;      to enter your user name on each session
;;   4. There's a keymay provided for convenience
;;       (require 'panda)
;;        (global-set-key (kbd "C-c b") 'panda-map) ;; b for "Bamboo"
;;
;;   The first request to Bamboo will ask for user/pass and then cache them in
;;   memory as long as Emacs is open.  The information on the projects and plans
;;   is retrieved once and cached for the session.  Branches for each plan are
;;   cached as required.  Information about the deployment projects is cached
;;   at once too.  Call panda-refresh-cache to force a reload of all items.
;;
;;   The commands supported in this version are:
;;
;;   Interactive:
;;
;;   panda-queue-build: starts a new build, interactively requests a project,
;;                      plan and branch
;;   panda-build-results: gets the last 7 (by default) builds for a particular
;;                        branch.
;;   panda-queue-deploy: starts a new deploy, interactively requests a plan,
;;                       environment and deploy.
;;   panda-deploy-status: gets the status of all environments for a deploy
;;                        project.
;;   panda-clear-credentials: force inputting user/pass in the next API call.
;;   panda-refresh-cache: re-fetch list of build plans and deploy projects.
;;
;;   Non interactive:
;;   panda-build-results-branch: if you know you branch key you can call this
;;                               function from elisp to display the build status
;;
;;   In the roadmap: create deploys from builds, improve documentation

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'url)

(defgroup panda nil
  "Client for Bamboo's REST API."
  :group 'extensions)

(defcustom panda-base-url ""
  "Base URL of the Bamboo API."
  :type 'string)

(defcustom panda-username ""
  "Username, if empty it will be prompted."
  :type 'string)

(defcustom panda-less-messages nil
  "Display less messages in the echo area."
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

(defvar panda--auth-string nil "Caches the credentials for API calls.")
(defvar panda--projects-cache nil "Caches all the build projects the user has access to, in one go.")
(defvar panda--plans-cache nil "Caches the plans for each build project the user has access to, in one go.")
(defvar panda--branches-cache nil "Caches the branches for each plan, as they are requested.")
(defvar panda--deploys-cache nil "Caches the deployment projects (not build projects) in one single call to /deploy/project/all.")

(defvar panda--branch-key nil "Buffer local variable for panda--build-status-mode.")
(defvar panda--project-name nil "Buffer local variable for panda--deploy-results-mode.")

(define-prefix-command 'panda-map)
;; Queue commands
(define-key panda-map (kbd "q b") 'panda-queue-build)
(define-key panda-map (kbd "q d") 'panda-queue-deploy)
;; Status commands
(define-key panda-map (kbd "s b") 'panda-build-results)
(define-key panda-map (kbd "s d") 'panda-deploy-status)
;; Create
(define-key panda-map (kbd "c") 'panda-create-release)
;; Refresh
(define-key panda-map (kbd "r") 'panda-refresh-cache)
; Interactive commands not mapped:
;; panda-clear-credentials

;;------------------Package infrastructure----------------------------------------

(defun panda--message (text)
  "Show a TEXT as a message and log it, if 'panda-less-messages' log only."
  (unless panda-less-messages
    (message text))
  (panda--log "Package message:" text "\n"))

(defun panda--log (&rest to-log)
  "Append TO-LOG to the log buffer.  Intended for internal use only."
  (let ((log-buffer (get-buffer-create "*panda-log*"))
        (text (cl-reduce (lambda (accum elem) (concat accum " " (prin1-to-string elem t))) to-log)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert text)
      (insert "\n"))))

;;------------------HTTP Stuff----------------------------------------------------

;; maybe change parameters order? url, method, qs params, data?
(defun panda--api-call (api-url &optional params method data)
  "Retrieve JSON result of calling API-URL with PARAMS and DATA using METHOD (default GET).  Return parsed objects."
  ;; Modified from https://stackoverflow.com/a/15119407/91877
  (unless panda-base-url
    (error "There's no URL for Bamboo configured.  Try customize-group -> panda"))
  (unless data
    (setq data ""))
  (let ((url-request-extra-headers
         `(("Accept" . "application/json")
           ("Content-Type" . "application/json")
           ("Authorization" . ,(panda--auth-header))))
        (url-to-get (concat panda-base-url api-url "?os_authType=basic"))
        (url-request-method (or method "GET"))
        (url-request-data (encode-coding-string data 'utf-8))
        (json-false :false))
    (when params
      (setq url-to-get (concat url-to-get "&" params)))
    (panda--log "API call URL:" url-request-method "to"  url-to-get "with data" url-request-data ".")
    (with-current-buffer (url-retrieve-synchronously url-to-get panda-silence-url nil panda-api-timeout)
      (when panda-log-responses
        (panda--log "API call response: " (buffer-string) "\n"))
      (goto-char url-http-end-of-headers)
      (let ((data 'error))
        (ignore-errors
          ;; if there's a problem parsing the JSON
          ;; data ==> 'error
          (setq data (json-read)))
        (kill-buffer) ;; don't litter with API buffers
        data))))

(defun panda--auth-header ()
  "Return the auth header.  Caches credentials per-session."
  (unless panda--auth-string
    (unless panda-username
      (setq panda-username (read-string "Bamboo username: ")))
    (let ((pass (read-passwd "Bamboo password: ")))
      (setq panda--auth-string
            (base64-encode-string
             (concat panda-username ":" pass)))
       (panda--log "Stored credentials for this session")))
  (concat "Basic " panda--auth-string))


;;------------------JSON traversal and list conversion----------------------------

(defun panda--agetstr (key alist)
  "Do 'alist-get' for KEY in ALIST with string keys."
  (alist-get key alist nil nil 'equal))

(defun panda--name-key-pair (element)
  "Return an alist name . key of ELEMENT."
  (cons (alist-get 'name element)
        (alist-get 'key element)))

(defun panda--json-nav (keys data)
  "Get subsequent KEYS out of JSON-parsed DATA."
  (let ((output data))
    (dolist (k keys output)
      (setq output (alist-get k output)))))

(defun panda--get-pairs (key1 key2 items)
  "Extract from the elements in ITEMS a cons cell with values (KEY1 . KEY2)."
  (mapcar (lambda (i) (cons (alist-get key1 i) (alist-get key2 i))) items))

(defun panda--printable-alist (keys-names alist)
  "Return a new ALIST if the keys are in KEYS-NAMES, using the later as key."
  (let ((result nil))
    (dolist (k-n keys-names result)
      (destructuring-bind (key . name) k-n
        (push (cons name
                    (alist-get key alist)) result)))))

(defun panda--extract-alist (keys alist)
  "Extract KEYS from ALIST, return a new list."
  (let ((result nil))
    (dolist (k keys (reverse result))
      (let ((value (alist-get k alist)))
        (unless value
          (setq value ""))
        (push value result)))))


;;------------------Cache for projects, plans, and branches-----------------------

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
  (panda--message "Refreshing Bamboo build project and plan cache...")
  ;; If you have more than 10000 projects I doubt you are using this package
  (let* ((response (panda--api-call "/project" "expand=projects.project.plans&max-results=10000"))
         ;; convert vector to list
         (data (append (panda--json-nav '(projects project) response) nil))
         (project nil)
         (plans nil))
    (setq panda--projects-cache nil)
    (setq panda--plans-cache nil)
    (setq panda--branches-cache nil)
    (dolist (proj data)
      (setq project (panda--name-key-pair proj))
      (setq plans (panda--get-pairs 'name 'key
                                    (panda--json-nav '(plans plan) proj)))
      (push project panda--projects-cache)
      (push (cons (cdr project) plans) panda--plans-cache)))
  (panda--message "Build cache updated!"))

(defun panda--refresh-cache-deploys ()
  "Refresh the cache of deploys."
  (panda--message "Refreshing Bamboo deployment cache...")
  (let* ((data (panda--api-call "/deploy/project/all"))
         (formatted (mapcar 'panda--format-deploy-entry data)))
    (setq panda--deploys-cache (cl-remove-if-not
                                ;; keep only the ones I can deploy to
                                ;; and have a valid plan
                                (lambda (deploy) (and (cddr deploy)
                                                      (car deploy)))
                                formatted)))
  (panda--message "Deploy cache updated!"))

(defun panda--format-deploy-entry (deploy-project)
  "Convert a DEPLOY-PROJECT to the cache format."
  (let ((did (alist-get 'id deploy-project))
        ;; (plan-key (panda--json-nav '(planKey key) deploy-project))
        (plan-key (alist-get 'name deploy-project))
        (environments (panda--format-environments-entry
                       (alist-get 'environments deploy-project))))
    (unless plan-key
      (message (prin1-to-string deploy-project)))
    (cons plan-key (cons did environments))))

(defun panda--format-environments-entry (deploy-envs)
  "Convert DEPLOY-ENVS to the cache format, only for allowedToExecute environments."
  (let ((as-list (append deploy-envs nil))
        (valid-envs nil))
    (dolist (environment as-list valid-envs)
      (when (eq (panda--json-nav '(operations allowedToExecute) environment) t)
        (push (panda--extract-alist '(name id) environment) valid-envs)))))

(defun panda--projects ()
  "Get cached list of projects, fetch them if needed."
  (unless panda--projects-cache
    (panda-refresh-cache))
  panda--projects-cache)

(defun panda--plans (project-key)
  "Get cached list of plans for a PROJECT-KEY, fetch plans if needed."
  (unless panda--plans-cache
    (panda-refresh-cache))
  (panda--agetstr project-key panda--plans-cache))

(defun panda--branches (plan-key)
  "Get cached list of branches for a PLAN-KEY, fetch and cache if needed."
  (let ((in-cache (panda--agetstr plan-key panda--branches-cache)))
    (unless in-cache
      (panda--message "Caching branches for plan...")
      (let* ((data (panda--api-call (concat "/plan/" plan-key "/branch")))
             (branches-data (panda--json-nav '(branches branch) data))
             (branches (panda--get-pairs 'shortName 'key branches-data)))
        (panda--log (prin1-to-string branches))
        (push (cons "[Base branch]" plan-key) branches) ;; adding master plan
        (push (cons plan-key branches) panda--branches-cache)
        (setq in-cache branches)
        (panda--message "Caching branches for plan...")))
    in-cache))

(defun panda--deploys ()
  "Get cached list of deploy projects, fetch them if needed."
  (unless panda--deploys-cache
    (panda-refresh-cache))
  panda--deploys-cache)


;;------------------Common UI utilities-------------------------------------------

(defun panda--select-build-project ()
  "Run 'ido-completing-read' to select a project.  Return the project key."
  (let* ((projects (panda--projects))
         (selected (ido-completing-read "Select project: "
                                        (mapcar 'first projects))))
    (panda--agetstr selected projects)))

(defun panda--select-build-plan (project-key)
  "Run 'ido-completing-read' to select a plan under PROJECT-KEY.  Return the plan key."
  (let* ((plans (panda--plans project-key))
         (selected (ido-completing-read "Select plan: "
                                        (mapcar 'first plans))))
    (panda--agetstr selected plans)))

(defun panda--select-build-branch (plan-key)
  "Run 'ido-completing-read' to select a plan under PLAN-KEY  Return the branch key."
  (let* ((branches (panda--branches plan-key))
         (selected (ido-completing-read "Select branch: "
                                        (mapcar 'first branches))))
    (panda--agetstr selected branches)))

(defun panda--select-build-ppb (&optional project plan)
  "Select the project, plan and branch for a build and return the keys.
If provided PROJECT and PLAN won't be prompted."
  ;; if the plan is provided skip the project when not set
  (when (and (not project) plan)
    (setq project "--"))
  (let* ((project-key (or project (panda--select-build-project)))
         (plan-key (or plan (panda--select-build-plan project-key)))
         (branch-key (panda--select-build-branch plan-key)))
    (list project-key plan-key branch-key)))

(defun panda--select-deploy-project ()
  "Run 'ido-completing-read' to select a deploy project.  Return the project data."
  (let* ((deploy-names (mapcar 'car (panda--deploys)))
         (selected (ido-completing-read "Select deploy project: " deploy-names)))
    selected))

(defun panda--unixms-to-string (unix-milliseconds)
  "Convert UNIX-MILLISECONDS to date string.  I'm surprised this isn't a built in."
  (let ((format-str "%Y-%m-%d %T")
        (unix-epoch "1970-01-01T00:00:00+00:00")
        (converted "")
        (seconds nil)) ;; default to empty string
    (ignore-errors
      (setq seconds (/ unix-milliseconds 1000))
      (setq converted
            (format-time-string format-str
                                (time-add (date-to-time unix-epoch)
                                          seconds))))
    converted))


;;------------------Build querying and information--------------------------------

;; TODO Make interactive version and write to buffer
(defun panda-get-build-info (build-name plan-key)
  "Retrieve the information of BUILD-NAME for PLAN-KEY."
  (let* ((split-data (split-string build-name "_"))
         (build-number (car (last split-data)))
         (branch-name (mapconcat 'identity (butlast split-data) "_")))
    (unless (string-equal branch-name "develop")
      (setq plan-key (panda--agetstr branch-name (panda--branches plan-key))))
    (panda--api-call (concat "/result/" plan-key "-" build-number))))

(defun panda-queue-build (&optional plan)
  "Queue a build.  If PLAN is not provided, select it interactively."
  (interactive)
  (destructuring-bind (_project-key plan-key branch-key) (panda--select-build-ppb nil plan)
    (when (equal branch-key (concat plan-key "0"))
      ;; the base plan has a branch number of 0 but
      ;; won't build if using the prefix num
      (setq branch-key plan-key))
    (panda--api-call (concat "/queue/" branch-key) nil "POST")
    (panda--message "Build queued")))

(defun panda-build-results (&optional plan)
  "Fetch the build results for a branch under PLAN.
If PLAN is not provided, select it interactively.
The amount of builds to retrieve is controlled by 'panda-latest-max'."
  (interactive)
  (destructuring-bind (_project-key _plan-key branch-key) (panda--select-build-ppb nil plan)
    (panda-build-results-branch branch-key)))

(defun panda-build-results-branch (branch)
  "Fetch the build results for a BRANCH.  For interactive branch selection, use 'panda-build-results'."
  ;; only master plans return the build date on the top level call
  ;; the only option is to fetch the list of build keys and retrieve
  ;; the build date individually for each build
  (let* ((build-data (panda--build-results-data branch))
         (buffer-name (concat "*Panda - Latest builds " branch "*"))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      ;; setup the tablist
      (panda--build-results-mode)
      ;; buffer local variables
      (setq panda--branch-key branch)
      (setq tabulated-list-entries build-data)
      (tabulated-list-print)
      (local-set-key "g" (lambda ()
                           (interactive)
                           (setq tabulated-list-entries (panda--build-results-data panda--branch-key))
                           (tabulated-list-print)
                           (panda--message (concat "Updated list of builds for " panda--branch-key))))
      (switch-to-buffer buffer)
      (panda--message (concat "Listing builds for " branch)))))

(defun panda--build-results-data (branch-key)
  "Get BRANCH-KEY build data for 'tabulated-list-entries'."
    (let ((build-keys (panda--latest-build-keys branch-key)))
      (mapcar 'panda--fetch-build-bykey build-keys)))

(defun panda--latest-build-keys (branch-key)
  "Get the list of links to retreive the latest builds for BRANCH-KEY."
    (let* ((target-url (concat "/result/" branch-key))
           (parameters (concat "max-results=" (number-to-string panda-latest-max-results)
                               "&includeAllStates=true"))
           (data (panda--api-call target-url parameters))
           (build-results (panda--json-nav '(results result) data)))
      (mapcar (lambda (build) (alist-get 'key build))
              build-results)))

(defun panda--fetch-build-bykey (build-key)
  "Return the data for BUILD-KEY formatted for tabulated mode."
  (let* ((build-data (panda--api-call (concat "/result/" build-key)))
         (filtered-data (panda--extract-alist '(key state prettyBuildStartedTime prettyBuildCompletedTime
                                                    buildDurationDescription)
                                              build-data)))
    ;; tabulated list requires a list with an ID and a vector
    (list (car filtered-data) (vconcat filtered-data))))

(define-derived-mode panda--build-results-mode tabulated-list-mode "Panda build results view" "Major mode to display Bamboo's build results."
  (setq tabulated-list-format [("Build key" 20 nil)
                               ("State" 11 nil)
                               ("Started" 22)
                               ("Finished" 22 nil)
                               ("Duration" 0 nil)])
  (set (make-local-variable 'panda--branch-key) nil)
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

;;------------------Creating deployments and pushing them-------------------------

(defun panda-create-release ()
  "Create a new release from a succesful build."
  (interactive)
  (destructuring-bind (_project-key plan-key branch-key) (panda--select-build-ppb nil nil)
    ;; I could re-work the cache to skip this call if I stored the plan key. But some
    ;; deploys dont have them, so I have to code for that too...let's have one extra
    ;; call and be done with it
    (let* ((forplan-response (panda--api-call "/deploy/project/forPlan"
                                              (concat "planKey=" plan-key)))
           (did (alist-get 'id (elt forplan-response 0)))
           (last-builds (mapcar 'cadr (panda--build-results-data branch-key)))
           (successful (cl-remove-if-not (lambda (build) (equal (elt build 1) "Successful")) last-builds))
           (formatted (mapcar (lambda (build) (concat (elt build 0) " - Completed: " (elt build 2))) successful))
           (selected-build (ido-completing-read "Select a build: " formatted))
           (release-name nil)
           (payload nil))
      (setq selected-build (car (split-string selected-build))) ;; really shady
      (setq release-name (read-string "Release name: " selected-build))
      (setq payload (json-encode (list (cons 'planResultKey selected-build) (cons 'name  release-name))))
      (panda--api-call (format "/deploy/project/%s/version" did)
                       nil
                       "POST"
                       payload))))

(defun panda-queue-deploy (&optional project)
  "Queue a deploy.  If PROJECT is not provided, select it interactively."
  (interactive)
  (let* ((project-name (or project (panda--select-deploy-project)))
         (metadata (panda--agetstr project-name (panda--deploys)))
         (did (car metadata))
         (environments (cdr metadata))
         (deploy-data (panda--deploys-for-id did))
         (selected-release (ido-completing-read "Select release: "
                                                (mapcar 'first deploy-data)))
         (selected-environment (ido-completing-read "Select an environment: "
                                                    (mapcar 'first environments))))
    (panda--api-call "/queue/deployment"
                     (format "environmentId=%s&versionId=%s"
                             (car (panda--agetstr selected-environment environments))
                             (car (panda--agetstr selected-release deploy-data)))
                     "POST"))
  (panda--message "Deployment requested"))

(defun panda--deploys-for-id (did)
  "Get the deployments of a DID (deployment id)."
  (let* ((url (format "/deploy/project/%s/versions" did))
         (parameters (format "max-results=%s" panda-latest-max-results))
         (data (panda--api-call url parameters))
         (deploys (alist-get 'versions data)))
    (mapcar
     (lambda (dep) (panda--extract-alist '(name id) dep))
     deploys)))

(defun panda-deploy-status (&optional project)
  "Display a project's deploy status.  If PROJECT is not provided, select it interactively."
  (interactive)
  (let* ((project-name (or project (panda--select-deploy-project)))
         (metadata (panda--agetstr project-name (panda--deploys)))
         (did (car metadata))
         (data (elt (panda--api-call (format "/deploy/dashboard/%s" did)) 0))
         (envs (alist-get 'environmentStatuses data))
         (data-formatted (mapcar 'panda--format-deploy-status envs))
         (buffer-name (concat "*Panda - Deploy status " project-name "*"))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      ;; change to tablist mode
      (panda--deploy-results-mode)
      ;;buffer local variables
      (setq panda--project-name project-name)
      (setq tabulated-list-entries data-formatted)
      (tabulated-list-print)
      (local-set-key "g" (lambda ()
                           (interactive)
                           (panda-deploy-status panda--project-name)
                           (panda--message (concat "Updated deploy status for " panda--project-name))))
      (switch-to-buffer buffer)
      (panda--message (concat "Listing deploy status for " project-name)))))

(defun panda--format-deploy-status (deploy-status)
  "Format DEPLOY-STATUS for tabulated output."
  (let* ((env-name (panda--json-nav '(environment name) deploy-status))
         (result (alist-get 'deploymentResult deploy-status))
         (deploy-name (panda--json-nav '(deploymentVersion name) result))
         (state (alist-get 'lifeCycleState result))
         (status (alist-get 'deploymentState result))
         (started (panda--unixms-to-string (alist-get 'startedDate result)))
         (completed (panda--unixms-to-string (alist-get 'finishedDate result))))
    ;; tabulated list requires a list with an ID and a vector
    (list env-name (vector env-name state status started completed deploy-name))))

(define-derived-mode panda--deploy-results-mode tabulated-list-mode "Panda deploy results view" "Major mode to display Bamboo's deploy results."
  (setq tabulated-list-format [("Environment" 35 nil)
                               ("State" 10 nil)
                               ("Status" 10)
                               ("Started" 20 nil)
                               ("Completed" 20 nil)
                               ("Version name" 0 nil)])
  (set (make-local-variable 'panda--project-name) nil)
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

;; panda-deploy-from-build
;; panda-create-deploy

(provide 'panda)
;;; panda.el ends here
