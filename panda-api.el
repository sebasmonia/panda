;;; panda-api.el --- API interactions for panda.el.  -*- lexical-binding: t; -*-

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

;; This file holds the http interactions and JSON parsing for Panda.
;; See panda.el for the main file of the package.
;;
;; For a detailed user manual see:
;; https://github.com/sebasmonia/panda/blob/master/README.md

;;; Code:

;;------------------HTTP Stuff----------------------------------------------------

;; maybe change parameters order? url, method, qs params, data?
(defun panda--api-call (api-url &optional params method data)
  "Retrieve JSON result of calling API-URL with PARAMS and DATA using METHOD (default GET).  Return parsed objects."
  ;; Modified from https://stackoverflow.com/a/15119407/91877
  (unless panda-api-url
    (error "There's no API URL for Bamboo configured.  Try customize-group -> panda"))
  (unless data
    (setq data ""))
  (let ((url-request-extra-headers
         `(("Accept" . "application/json")
           ("Content-Type" . "application/json")
           ("Authorization" . ,(panda--auth-header))))
        (url-to-get (concat panda-api-url api-url "?os_authType=basic"))
        (url-request-method (or method "GET"))
        (url-request-data (encode-coding-string data 'utf-8))
        (json-false :false))
    (when params
      (setq url-to-get (concat url-to-get "&" params)))
    (panda--log "----- API call: " url-request-method "to "  url-to-get "with data" url-request-data " -----")
    (with-current-buffer (url-retrieve-synchronously url-to-get panda-silence-url nil panda-api-timeout)
      (set-buffer-multibyte t)
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


(provide 'panda-api)
;;; panda-api.el ends here
