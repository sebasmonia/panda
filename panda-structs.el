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

(cl-defstruct (panda--plan-branch (:constructor panda--make-plan-branch))
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

;;------------------"Accessors" of sorts to the cached data-----------------------

(defun panda--select-build-project ()
  "Call `completing-read' for a project, return the key of the one selected."
  (unless panda--builds-cache
    (panda--refresh-cache-builds))
  (let* ((project-names-keys (cl-loop for project in panda--builds-cache
                                      collect (cons (panda--project-name project)
                                                    (panda--project-key project))))
         (name-selected (completing-read "Select project: " project-names-keys)))
    (alist-get name-selected project-names-keys nil nil 'equal)))



(defun panda--plans (project-key)
  "Get cached list of plans for a PROJECT-KEY, fetch plans if needed."
  (unless panda--builds-cache
    (panda--refresh-cache-builds))
  ;; this is probably the fanciest Lisp I wrote so far. See this entry in the CL cookbook:
  ;; https://lispcookbook.github.io/cl-cookbook/iteration.html#named-loops-and-early-exit
  ;; I like this better than manually going over the list on my own
  (cl-loop named find-plans
           for project in panda--builds-cache
           when (string= project-key (panda--project-key project))
           do (cl-return-from find-plans (panda--project-plans project))))

(defun panda--plan (plan-key)
  "Get cached list of plans for a PROJECT-KEY, fetch plans if needed."
  (unless panda--builds-cache
    (panda--refresh-cache-builds))
  ;; this is probably the fanciest Lisp I wrote so far. See this entry in the CL cookbook:
  ;; https://lispcookbook.github.io/cl-cookbook/iteration.html#named-loops-and-early-exit
  ;; I like this better than manually going over the list on my own
  (cl-loop named find-plans
           for project in panda--builds-cache
           when (string= project-key (panda--project-key project))
           do (cl-return-from find-plans (panda--project-plans project))))

(defun panda--branches (plan-key)
  (cl-destructuring-bind (project-key &rest plan) (split-string plan-key "-")
    (let* ((plan (panda--plans project-key))
           (plan (panda--project-plans project)
    (message "Project: %s Plan: %s" project-key (string-join plan "-")))

  )

(panda--project-plans project)

(provide 'panda-structs)

;;; panda-structs.el ends here
