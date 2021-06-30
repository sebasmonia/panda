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


(cl-defstruct (panda--project (:constructor panda--make-project))
  project-key project-name plan-list)


(cl-defstruct (panda--plan (:constructor panda--make-plan))
  project-key project-name branches)

(cl-defstruct (panda--plan-branch (:constructor panda--make-plan-branch))
  branch-key branch-name)


(panda--api-call "/project" "expand=projects.project.plans&max-results=10000")


(provide 'panda-structs)
;;; panda-structs.el ends here
