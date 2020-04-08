;;; thinkorswim.el --- TD Ameritrade Developer API  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 27 Mar 2020
;; Modified: 7 Apr 2020
;; Version: 0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: finance
;; URL: https://github.com/jwiegley/thinkorswim

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package assists with logging in to the TD Ameritrade Developer API.

;;; Code:

(require 'parse-csv)                    ; mrc/el-csv on GitHub
(require 'rx)
(require 'eieio)
(require 'anaphora)
(require 'request)
(require 'url-util)
;; (require 'web-server)

(defconst tos-base-url "https://api.tdameritrade.com/v1")

(defconst tos-client-id
  (lookup-password "developer.tdameritrade.com.client-id" "jwiegley" 80))

(defvar tos-refresh-token nil)
(defvar tos-access-token nil
  "The access token, a cons cell of the form (STRING . TIME).
Access tokens are only valid for 30 minutes.")

(defconst tos-access-token-expires (* 5 60))
(defconst tos-refresh-token-expires (* 30 60))

;; (defvar tos-web-server nil)

(defun tos-authorize ()
  (interactive)
  ;; (unless tos-web-server
  ;;   (setq tos-web-server
  ;;         (ws-start
  ;;          '(((lambda (_) t) .
  ;;             (lambda (request)
  ;;               (with-slots (process) request
  ;;                 (ws-response-header process 200 '("Content-type" . "text/plain"))
  ;;                 (process-send-string process "<html><p>Hello, world!</p></html>")))))
  ;;          9595)))
  (browse-url (concat "https://auth.tdameritrade.com/auth?"
                      "response_type=code&"
                      "redirect_uri=http%3A%2F%2F127.0.0.1%3A9595&"
                      "client_id=" (url-hexify-string tos-client-id)))
  (setq tos-refresh-token
        (cdr
         (assq
          'refresh_token
          (request-response-data
           (request
             (format "%s/oauth2/token" tos-base-url)
             :sync t
             :type "POST"
             :headers
             '(("Content-Type" . "application/x-www-form-urlencoded"))
             :data
             (format
              (concat "grant_type=authorization_code&"
                      "refresh_token=&"
                      "access_type=offline&"
                      "code=%s&"
                      "client_id=%s&"
                      "redirect_uri=http%%3A%%2F%%2F127.0.0.1:9595")
              (with-temp-buffer
                (insert (read-string "URL response from tdameritrade.com: "))
                (goto-char (point-min))
                (search-forward "code=")
                (delete-region (point-min) (match-end 0))
                (buffer-string))
              (url-hexify-string tos-client-id))
             :parser 'json-read))))
        tos-access-token nil)
  (message "thinkorswim has been authorized"))

(defun tos-get-access-token ()
  (if (and tos-access-token
           (< (float-time (time-subtract (current-time) (cdr tos-access-token)))
              tos-access-token-expires))
      (car tos-access-token)
    (setq tos-access-token
          (cons
           (cdr
            (assq
             'access_token
             (request-response-data
              (request
               (format "%s/oauth2/token" tos-base-url)
               :sync t
               :type "POST"
               :headers
               '(("Content-Type" . "application/x-www-form-urlencoded"))
               :data
               (format
                (concat "grant_type=refresh_token&"
                        "refresh_token=%s&"
                        "access_type=offline&"
                        "code=&"
                        "client_id=%s&"
                        "redirect_uri=http%%3A%%2F%%2F127.0.0.1:9595")
                (url-hexify-string tos-refresh-token)
                (url-hexify-string tos-client-id))
               :parser 'json-read))))
           (current-time)))
    (car tos-access-token)))

(provide 'thinkorswim)
