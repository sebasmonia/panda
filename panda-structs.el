;;; panda-structs.el --- Structures for panda.el.  -*- lexical-binding: t; -*-

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

;; This file contains the structures used by Panda.
;; See panda.el for the main file of the package.
;;
;; For a detailed user manual see:
;; https://github.com/sebasmonia/panda/blob/master/README.md

;;; Code:

(require 'cl-lib)

;;------------------Struct definitions--------------------------------------------

(cl-defstruct (panda--project (:constructor panda--make-project))
  key name plans)


(cl-defstruct (panda--plan (:constructor panda--make-plan))
  key name branches)

(cl-defstruct (panda--branch (:constructor panda--make-branch))
  key name)

(cl-defstruct (panda--deploy-project (:constructor panda--make-deploy-project))
  id name build-plan-key environments)

(cl-defstruct (panda--environment (:constructor panda--make-environment))
  id name)

;;------------------Conversion of parsed JSON to Panda structs--------------------

(defun panda--gethash (keys ht)
  "Convenience function to call `gethash' starting on HT for each element on KEYS.
KEYS can be a list of symbols, or strings. The former are converted to the latter.
There's a tendency in the Bamboo responses to nest data in JSON objects..."
  (when (symbolp (car keys))
    ;; convert symbols to strings
    (setf keys (mapcar #'symbol-name keys)))
  (dolist (a-key keys ht)
    (when ht ;; silently return nil. LIVIN' ON THE EDGE
      (setf ht (gethash a-key ht)))))

(defun panda--convert-project-response (api-response)
  "Convert API-RESPONSE, a vector of hashtables from parsed JSON, to Panda build structures."
  (cl-loop for project across (panda--gethash '(projects project) api-response)
           collect (panda--process-project project)))

(defun panda--process-project (project)
  "Convert a PROJECT (a hashtable from parsed JSON) to a project->plans structure."
  (let ((plans (cl-loop for plan across (panda--gethash '(plans plan) project)
                        collect (panda--make-plan :key (gethash "key" plan)
                                                  :name (gethash "name" plan)))))
    (panda--make-project :key (gethash "key" project)
                         :name (gethash "name" project)
                         :plans plans)))

(defun panda--convert-branches-response (api-response)
  "Convert API-RESPONSE, a vector of hashtables from parsed JSON, to Panda build structures."
  (cl-loop for branch across (panda--gethash '(branches branch) api-response)
           collect (panda--make-branch :key (gethash "key" branch)
                                       :name (gethash "shortName" branch))))

(defun panda--convert-deploy-response (api-response)
  "Convert API-RESPONSE, a vector of hashtables from parsed JSON, to Panda deploy structures."
  (cl-loop for deploy-project across api-response
           collect (panda--make-deploy-project :id (gethash "id" deploy-project)
                                               :name (gethash "name" deploy-project)
                                               :build-plan-key (panda--gethash '(planKey key) deploy-project)
                                               :environments (panda--process-environments
                                                              (gethash "environments" deploy-project)))))

(defun panda--process-environments (environments)
  "Convert ENVIRONMENTS (a vector from parsed JSON) to Panda environments structures.
Environments without \"allowedToExecute\" permissions are removed."
  (cl-loop for env across environments
           if (not (eq :false (panda--gethash '(operations allowedToExecute) env)))
           collect (panda--make-environment :id (gethash "id" env)
                                            :name (gethash "name" env))))

;;-------------------Access to the cached data for builds, plans and branches-----

(defun panda--select-build-project ()
  "Call `completing-read' for a project, return the `panda--project' selected."
  (unless panda--builds-cache
    (panda--refresh-cache-builds))
  (let ((selected-project-name (completing-read "Select project: "
                                                (cl-loop for project in panda--builds-cache
                                                         collect (panda--project-name project)))))
    (seq-find (lambda (a-proj) (string=
                                selected-project-name
                                (panda--project-name a-proj)))
              panda--builds-cache)))

(defun panda--select-build-plan (the-project)
  "Run `completing-read' to select a plan under THE-PROJECT.  Return the plan struct."
  (let* ((plans-in-proj (panda--project-plans the-project))
         (selected-plan-name (completing-read "Select plan: "
                                             (cl-loop for plan in plans-in-proj
                                                      collect (panda--plan-name plan)))))
    (seq-find (lambda (a-plan) (string=
                                selected-plan-name
                                (panda--plan-name a-plan)))
              plans-in-proj)))

(defun panda--select-build-branch (the-plan)
  "Run `completing-read' to select a branch under THE-PLAN.  Return the branch struct."
  ;; TODO: return the branch or the key(s)?
  (unless (panda--plan-branches the-plan)
    ;; we still have cached the branches for this one
    (panda--cache-branches the-plan))
  (let* ((branches-in-proj (panda--plan-branches the-plan))
         (selected-branch-name (completing-read "Select branch: "
                                              (cl-loop for branch in branches-in-proj
                                                      collect (panda--branch-name branch)))))
    (seq-find (lambda (a-branch) (string=
                                  selected-branch-name
                                  (panda--branch-name a-branch)))
              branches-in-proj)))

(defun panda--select-build-ppb (&optional project-key plan-key)
  "Select the project, plan and branch for a build and return the keys.
If provided PROJECT-KEY and PLAN-KEY won't be prompted."
  ;; if the plan-key is provided, but not the project, derive the project key from it
  (unless panda--builds-cache
    (panda--refresh-cache-builds))
  (when (and (not project-key) plan-key)
    (setq project-key (cl-first (split-string plan "-"))))
  (let* ((project (if project-key
                      (seq-find (lambda (a-proj) (string=
                                              project-key
                                              (panda--project-key a-proj)))
                                panda--builds-cache)
                    (panda--select-build-project)))
         (plan (if plan-key
                   (seq-find (lambda (a-plan) (string=
                                               plan-key
                                               (panda--plan-key a-plan)))
                             (panda--project-plans project))
                 (panda--select-build-plan project)))
         (branch (panda--select-build-branch plan)))
    (list (panda--project-key project)
          (panda--plan-key plan)
          (panda--branch-key branch))))

;;-------------------Access to the cached data for deploys and environments-------

(defun panda--select-deploy-project ()
  "Run `completing-read' to select a deploy project.  Return a `panda--deploy-project'."
  (unless panda--deploys-cache
    (panda--refresh-cache-deploys))
  (let ((selected-deploy-name (completing-read "Select deploy project: "
                                               (cl-loop for dep-proj in panda--deploys-cache
                                                        collect (panda--deploy-project-name dep-proj)))))
    (seq-find (lambda (a-proj) (string=
                                selected-deploy-name
                                (panda--deploy-project-name a-proj)))
              panda--deploys-cache)))


(provide 'panda-structs)

;;; panda-structs.el ends here
