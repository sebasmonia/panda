;;; panda.el --- Client for Bamboo's REST API.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Sebastian Monia
;;
;; Author: Sebastian Monia <smonia@outlook.com>
;; URL: https://github.com/sebasmonia/panda
;; Package-Requires: ((emacs "25"))
;; Version: 0.5
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
;;
;;   The first request to Bamboo will ask for user/pass and then cache them in
;;   memory as long as Emacs is open.  The information on the projects and plans
;;   is retrieved once and cached for the session.  Call panda-refresh-cache to
;;   for a refresh.  Branches for each plan are cached as required.
;;   There are no default key bindings yet (a keymap is in the roadmap)
;;   The commands supported in this version are:
;;
;;   Interactive:
;;
;;   panda-queue-build: starts a new build, interactively requests a project,
;;                      plan and branch
;;   panda-build-results: gets the last 7 (by default) builds for a particular
;;                        branch.
;;
;;   Non interactive:
;;   panda-build-results-branch: if you know you branch key you can call this
;;                               function from elisp to display the build status
;;
;;   In the roadmap: create deploys from builds, check deploy status, queue deploy,
;;                   provide a keymap, improve documentation

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

;; consider making this an independent parameter
;; for builds and deployments
(defcustom panda-latest-max-results 7
  "How many items to retrieve when pulling lists of \"latest items\"."
  :type 'integer)

;; TODO Maybe v2 as needs replacement
;; for url-insert-file-contents (doesnt support
;; the "silent" param)
;; (defcustom panda-silence-url t
;;   "Ask url.el not to show messages."
;;   :type 'boolean)

(defvar panda--auth-string nil)
(defvar panda--projects-cache nil)
(defvar panda--plans-cache nil)
;; Unlike the list of projects and plans, the branch cache
;; is built as needed. It gets cleared on refresh.
(defvar panda--branches-cache nil)

;; The information on all deployments is read  as needed,
;; we need one call to get the project id, and another to get
;; the environments, the alternative is "/deploy/project/all"
;; but that fetches A TON of data at onces.
;;  It's worth considering using that approach but for now
;; this seems leaner, although more complex.
(defvar panda--deploys-cache nil)

;; buffer local for build-status-mode buffers
(defvar panda--branch-key nil)


;;------------------Package infrastructure----------------------------------------

(defun panda--message (text)
  "Show a TEXT as a message and log it, if 'panda-less-messages' log only."
  (unless panda-less-messages
    (message text))
  (panda--log text))

(defun panda--log (&rest to-log)
  "Append TO-LOG to the log buffer.  Intended for internal use only."
  (let ((log-buffer (get-buffer-create "*panda log*"))
        (text (cl-reduce (lambda (accum elem) (concat accum " " (prin1-to-string elem t))) to-log)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert text)
      (insert "\n"))))

;;------------------HTTP Stuff----------------------------------------------------

(defun panda--fetch-json (method &optional params)
  "Retrieve JSON result of calling METHOD with PARAMS.  Return parsed objects."
  ;; Modified from https://stackoverflow.com/a/15119407/91877
  (let ((url-request-extra-headers
         `(("Accept" . "application/json")
           ("Authorization" . ,(panda--auth-header))))
        (url-to-get (concat panda-base-url method "?os_authType=basic"))
        (json-false :false))
    (when params
      (setq url-to-get (concat url-to-get "&" params)))
    (panda--log "Fetch JSON URL:" url-to-get)
    (with-temp-buffer
      (url-insert-file-contents url-to-get)
      (goto-char (point-min))
      (json-read))))

(defun panda--run-action (method &optional params)
  "Make a POST request to METHOD with PARAMS.  Return parsed objects."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Authorization" . ,(panda--auth-header))))
        (url-to-post (concat panda-base-url method "?os_authType=basic"))
        (json-false :false))
    (when params
      (setq url-to-post (concat url-to-post "&" params)))
    (panda--log "Run action URL:" url-to-post)
    (url-retrieve-synchronously url-to-post)))

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

(defun panda-refresh-cache ()
  "Refresh the cache of projects and plans."
  (interactive)
  (panda--message "Refreshing Bamboo project and plan cache...")
  ;; If you have more than 10000 projects I doubt you are using this package
  (let* ((response (panda--fetch-json "/project" "expand=projects.project.plans&max-results=10000"))
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
  (panda--message "Cache updated!"))

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
      (let* ((data (panda--fetch-json (concat "/plan/" plan-key "/branch")))
             (branches-data (panda--json-nav '(branches branch) data))
             (branches (panda--get-pairs 'shortName 'key branches-data)))
        (panda--log (prin1-to-string branches))
        (push (cons "[Base branch]" plan-key) branches) ;; adding master plan
        (push (cons plan-key branches) panda--branches-cache)
        (setq in-cache branches)
        (panda--message "Caching branches for plan...")))
    in-cache))

(defun panda--deploys (plan-key)
  "Get cached list of deployments for a PLAN-KEY, fetch and cache if needed."
  (let ((in-cache (panda--agetstr plan-key panda--deploys-cache)))
    (unless in-cache
      (panda--message "Caching deploys for plan...")
      ;; gross misuse of in-cache to temp hold the object
      ;; retrieve from the API call
      (setq in-cache (panda--get-plan-deploys plan-key))
      (push in-cache panda--deploys-cache)
      ;; format to return
      (setq in-cache (cdr in-cache)))
    in-cache))

(defun panda--get-plan-deploys (plan-key)
  "Get the deploy data for a PLAN-KEY.
This requires two API calls, one to get the project id and another for the
actual deployments."
  (let* ((raw (panda--fetch-json "/deploy/project/forPlan"
                                 (concat "planKey=" plan-key)))
         (did (alist-get 'id (elt raw 0)))
         (proj-url (format "/deploy/project/%s" did))
         (data (panda--fetch-json proj-url))
         (envs (panda--format-deploy-data (alist-get 'environments data))))
    (cons plan-key (cons did envs))))

(defun panda--format-deploy-data (deploy-data)
  "Format the information DEPLOY-DATA for caching."
  (mapcar (lambda (elem) (panda--extract-alist '(name id) elem))
          deploy-data))

;;------------------Common UI utilities-------------------------------------------

(defun panda--select-project ()
  "Run 'ido-completing-read' to select a project.  Return the project key."
  (let* ((projects (panda--projects))
         (selected (ido-completing-read "Select project: "
                                        (mapcar 'first projects))))
    (panda--agetstr selected projects)))

(defun panda--select-plan (project-key)
  "Run 'ido-completing-read' to select a plan under PROJECT-KEY.  Return the plan key."
  (let* ((plans (panda--plans project-key))
         (selected (ido-completing-read "Select plan: "
                                        (mapcar 'first plans))))
    (panda--agetstr selected plans)))

(defun panda--select-branch (plan-key)
  "Run 'ido-completing-read' to select a plan under PLAN-KEY  Return the branch key."
  (let* ((branches (panda--branches plan-key))
         (selected (ido-completing-read "Select branch: "
                                        (mapcar 'first branches))))
    (panda--agetstr selected branches)))

(defun panda--select-ppb (&optional project plan)
  "Run the functions to select the project, plan and branch and return the keys in a list.  If provided PROJECT and PLAN won't be prompted."
  ;; if the plan is provided skip the project when not set
  (when (and (not project) plan)
    (setq project "--"))
  (let* ((project-key (or project (panda--select-project)))
         (plan-key (or plan (panda--select-plan project-key)))
         (branch-key (panda--select-branch plan-key)))
    (list project-key plan-key branch-key)))

(defun panda--select-pp (&optional project plan)
  "Run the functions to select the PROJECT and PLAN and return the keys in a list.  If provided PROJECT won't be prompted."
  ;; if the plan is provided skip the project when not set
  (when (and (not project) plan)
    (setq project "--"))
  (let* ((project-key (or project (panda--select-project)))
         (plan-key (or plan (panda--select-plan project-key))))
    (list project-key plan-key)))


;;------------------Build querying and information--------------------------------

;; TODO Make interactive version and write to buffer
(defun panda-get-build-info (build-name plan-key)
  "Retrieve the information of BUILD-NAME for PLAN-KEY."
  (let* ((split-data (split-string build-name "_"))
         (build-number (car (last split-data)))
         (branch-name (mapconcat 'identity (butlast split-data) "_")))
    (unless (string-equal branch-name "develop")
      (setq plan-key (panda--agetstr branch-name (panda--branches plan-key))))
    (panda--fetch-json (concat "/result/" plan-key "-" build-number))))

;; TODO move to deploy-status
(defun panda-get-build-date (build-name plan-key)
  "Retrieve the build date of BUILD-NAME for PLAN-KEY."
  (let ((build-date
         (alist-get 'buildCompletedTime (panda-get-build-info build-name plan-key))))
    ;; formatted to "month/day hour:minute"
    ;; technically is not UTC because the original string says UTC-7 but whatever!
    (format-time-string "%m/%d %H:%M"  (date-to-time build-date) "UTC")))

(defun panda-queue-build (&optional plan)
  "Queue a build.  If PLAN is not provided, select it interactively."
  (interactive)
  (destructuring-bind (_project-key plan-key branch-key) (panda--select-ppb nil plan)
    (when (equal branch-key (concat plan-key "0"))
      ;; the base plan has a branch number of 0 but
      ;; won't build if using the prefix num
      (setq branch-key plan-key))
    (panda--run-action (concat "/queue/" branch-key))))

(defun panda-build-results (&optional plan)
  "Fetch the build results for a branch under PLAN.
If PLAN is not provided, select it interactively.
The amount of builds to retrieve is controlled by 'panda-latest-max'."
  (interactive)
  (destructuring-bind (_project-key _plan-key branch-key) (panda--select-ppb nil plan)
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
           (data (panda--fetch-json target-url parameters))
           (build-results (panda--json-nav '(results result) data)))
      (mapcar (lambda (build) (alist-get 'key build))
              build-results)))

(defun panda--fetch-build-bykey (build-key)
  "Return the data for BUILD-KEY formatted for tabulated mode."
  (let* ((build-data (panda--fetch-json (concat "/result/" build-key)))
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
(defun panda-queue-deploy (&optional plan)
  "Queue a deploy.  If PLAN is not provided, select it interactively."
  (interactive)
  (destructuring-bind (_project-key plan-key) (panda--select-pp nil plan)
    (let* ((metadata (panda--deploys plan-key))
           (did (car metadata))
           (environments (cdr metadata))
           (deploy-data (panda--deploys-for-id did))
           (selected-release (ido-completing-read "Select release: "
                                                  (mapcar 'first deploy-data)))
           (selected-environment (ido-completing-read "Select an environment: "
                                                      (mapcar 'first environments))))
      (panda--run-action "/queue/deployment"
                         (format "environmentId=%s&versionId=%s"
                                 (car (panda--agetstr selected-environment environments))
                                 (car (panda--agetstr selected-release deploy-data)))))))


(defun panda--deploys-for-id (did)
  "Get the deployments of a DID (deployment id)."
  (let* ((url (format "/deploy/project/%s/versions" did))
         (parameters (format "max-results=%s" panda-latest-max-results))
         (data (panda--fetch-json url parameters))
         (deploys (alist-get 'versions data)))
    (mapcar
     (lambda (dep) (panda--extract-alist '(name id) dep))
     deploys)))
;(build) or plan?
;; panda-deploy-status

(provide 'panda)
;;; panda.el ends here
