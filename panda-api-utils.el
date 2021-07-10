;;; panda-api-utils.el --- API calls and utilities for panda.el.  -*- lexical-binding: t; -*-

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

;; This file holds the http interactions and JSON parsing for Panda,
;; and some common utils functions.
;; See panda.el for the main file of the package.
;;
;; For a detailed user manual see:
;; https://github.com/sebasmonia/panda/blob/master/README.md

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'browse-url)

(defvar panda--auth-string nil "Caches the credentials for API calls.")

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
        (json-object-type 'hash-table)
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
          (if (fboundp 'json-parse-buffer)
              (setq data (json-parse-buffer))
            (setq data (json-read))))
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

;;------------------Package utilities---------------------------------------------

(defun panda--message (text)
  "Show a TEXT as a message and log it, if 'panda-less-messages' log only."
  (unless panda-less-messages
    (message text))
  (panda--log "Package message:" text))

(defun panda--log (&rest to-log)
  "Append TO-LOG to the log buffer.  Intended for internal use only."
  (let ((log-buffer (get-buffer-create "*panda-log*"))
        (text (cl-reduce (lambda (accum elem) (concat accum " " (prin1-to-string elem t))) to-log)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert text)
      (insert "\n"))))

(defun panda--show-help (help-message)
  "Display the *Panda Help* buffer with the text in HELP-MESSAGE."
  (with-output-to-temp-buffer "*Panda Help*"
    (princ help-message)))

(defun panda--get-buffer-name (key)
  "Return the buffer name to a KEY, considering the user's customizations."
  (let ((prefix (if panda-prefix-buffers "Panda - " ""))
        (name (alist-get key panda--buffer-name-alist)))
    (format "*%s%s*" prefix name)))

(defun panda--unixms-to-string (unix-milliseconds)
  "Convert UNIX-MILLISECONDS to date string.  I'm surprised this isn't a built in."
  (let ((format-str "%Y-%m-%d %T")
        (unix-epoch "1970-01-01T00:00:00+00:00")
        (converted "")
        (seconds nil))
    (condition-case nil
        (progn
          (setq seconds (/ unix-milliseconds 1000))
          (setq converted
                (format-time-string format-str
                                    (time-add (date-to-time unix-epoch)
                                              seconds))))
      (error (setq converted "")))
    converted))

(defun panda--browse (path)
  "Open the default browser using PATH."
  (unless panda-browser-url
    (error "There's no broser URL for Bamboo configured.  Try customize-group -> panda"))
  (browse-url (concat panda-browser-url path)))

(provide 'panda-api-utils)
;;; panda-api-utils.el ends here
