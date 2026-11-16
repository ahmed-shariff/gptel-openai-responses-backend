;;; gptel-openai-responses-backend.el --- Hosted Responses tools for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2025 Shariff AM Faleel
;; Author: Shariff AM Faleel
;; Package-Requires: ((emacs "28") (gptel "0.9.9"))
;; Version: 0.1-pre
;; Homepage: https://github.com/ahmed-shariff/gptel-openai-assistant
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;; Extend gptel Responses backends, including OpenAI OAuth, with hosted web
;; search.  File search is available only for non-OAuth Responses backends.
;; Load this library and call
;; `gptel-openai-responses-extended-setup-builtin-transient' to add the menu.
;; Existing Responses and OAuth backend instances need not be recreated.

;;; Code:
(require 'gptel)
(require 'gptel-openai-responses)
(require 'transient)

(defcustom gptel-openai-responses-extended-include-file-search-results t
  "Include results when file search is enabled."
  :type 'boolean)

(defcustom gptel-openai-responses-extended-include-web-search-results t
  "Include sources when web search is enabled."
  :type 'boolean)

(defcustom gptel-openai-responses-extended-vector-store-ids nil
  "Vector store IDs for the public API file-search tool."
  :type '(repeat string))

(defvar gptel-openai-responses-extended--tools nil
  "Selected hosted tool plists, or functions returning tool plists.
File-search selections are ignored for OAuth requests.")

(defun gptel-openai-responses-extended--oauth-backend-p (backend)
  "Return non-nil when BACKEND uses OpenAI OAuth."
  (and (fboundp 'gptel-openai-oauth-p) (gptel-openai-oauth-p backend)))

(cl-defmethod gptel--request-data :around
  ((backend gptel-openai-responses) _prompts)
  "Append hosted tools without replacing function tools or OAuth filtering."
  (let* ((data (cl-call-next-method))
         (oauth (gptel-openai-responses-extended--oauth-backend-p backend))
         (tools nil))
    ;; Explicit request tools take precedence over menu selections of that type.
    (dolist (tool (append (append (plist-get data :tools) nil)
                          (mapcar (lambda (tool)
                                    (if (functionp tool) (funcall tool) tool))
                                  gptel-openai-responses-extended--tools)))
      (let ((type (plist-get tool :type)))
        (unless (or (and oauth (equal type "file_search"))
                    (and (member type '("web_search" "file_search"))
                         (cl-find type tools :key (lambda (item) (plist-get item :type))
                                  :test #'equal)))
          (push tool tools))))
    (when (or tools (plist-member data :tools))
      (plist-put data :tools (vconcat (nreverse tools))))
    (let ((includes (append (plist-get data :include) nil)))
      (when oauth (setq includes (delete "file_search_call.results" includes)))
      (dolist (entry `(("web_search" "web_search_call.action.sources"
                        ,gptel-openai-responses-extended-include-web-search-results)
                       ("file_search" "file_search_call.results"
                        ,gptel-openai-responses-extended-include-file-search-results)))
        (when (and (nth 2 entry)
                   (cl-find (car entry) (append (plist-get data :tools) nil)
                            :key (lambda (tool) (plist-get tool :type)) :test #'equal))
          (setq includes (append includes (list (cadr entry))))))
      (if includes
          (plist-put data :include (vconcat (delete-dups includes)))
        (cl-remf data :include)))
    data))

(defun gptel-openai-responses-extended--process-annotation (annotation _info)
  "Return a citation link or marker for ANNOTATION."
  (pcase (plist-get annotation :type)
    ("file_citation" (format "[file_citation:%s]" (plist-get annotation :file_id)))
    ("url_citation"
     (when-let* ((url (plist-get annotation :url)))
       (format "[%s](%s)" (or (plist-get annotation :title) url) url)))))

(defun gptel-openai-responses-extended--collect-output (items info)
  "Retain hosted search metadata from ITEMS in INFO and return citations.
Keep the legacy singular metadata keys and all calls in plural keys."
  (let (citations)
    (mapc
     (lambda (item)
       (pcase (plist-get item :type)
         ("web_search_call"
          (plist-put info :web-search-call-action (plist-get item :action))
          (plist-put info :web-search-calls
                     (append (plist-get info :web-search-calls) (list item))))
         ("file_search_call"
          (plist-put info :file-search-call-queries (plist-get item :queries))
          (plist-put info :file-search-call-results (plist-get item :results))
          (plist-put info :file-search-calls
                     (append (plist-get info :file-search-calls) (list item))))
         ("message"
          (mapc (lambda (part)
                  (mapc (lambda (annotation)
                          (when-let* ((text (gptel-openai-responses-extended--process-annotation
                                            annotation info)))
                            (push text citations)))
                        (plist-get part :annotations)))
                (plist-get item :content)))))
     items)
    (nreverse citations)))

(cl-defmethod gptel--parse-response :around
  ((_backend gptel-openai-responses) response info)
  "Keep gptel parsing and append hosted citations as reference links."
  (let ((text (cl-call-next-method))
        (citations (gptel-openai-responses-extended--collect-output
                    (plist-get response :output) info)))
    (if citations (concat text "\n" (string-join (delete-dups citations) "\n")) text)))

(cl-defmethod gptel-curl--parse-stream :around
  ((_backend gptel-openai-responses) info)
  "Keep gptel streaming and inspect only events it successfully consumed."
  (let* ((start (point))
         (text (cl-call-next-method))
         (end (point))
         citations)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^event: *\\(.+\\)" end t)
        (let ((event (match-string 1)))
          (forward-line 1)
          (when (and (looking-at "data:") (< (+ (point) 5) end))
            (forward-char 5)
            (let ((data (gptel--json-read)))
              (when (equal event "response.completed")
                ;; Completed output has the full source lists and annotations;
                ;; use it once rather than duplicating output_item.done events.
                (setq citations
                      (gptel-openai-responses-extended--collect-output
                       (map-nested-elt data '(:response :output)) info))))))))
    (if citations (concat text "\n" (string-join (delete-dups citations) "\n")) text)))

(defclass gptel-openai-responses-extended--add-to-list-switch (transient-variable)
  ((target-value :initarg :target-value)
   (target-list :initarg :target-list)
   (format :initarg :format :initform " %k %d")))

(cl-defmethod transient-infix-read
  ((_obj gptel-openai-responses-extended--add-to-list-switch)))

(cl-defmethod transient-infix-set
  ((obj gptel-openai-responses-extended--add-to-list-switch) _)
  (let ((value (oref obj target-value))
        (values (symbol-value (oref obj target-list))))
    (set (oref obj target-list)
         (if (member value values) (delete value values)
           ;; Web-search context selections are mutually exclusive.
           (append (if (equal (plist-get value :type) "web_search")
                       (cl-remove-if (lambda (item)
                                       (and (listp item)
                                            (equal (plist-get item :type) "web_search")))
                                     values)
                     values)
                   (list value)))))
  (transient-setup))

(cl-defmethod transient-format-description
  ((obj gptel-openai-responses-extended--add-to-list-switch))
  (propertize (transient--get-description obj) 'face
              (if (member (oref obj target-value) (symbol-value (oref obj target-list)))
                  'transient-value 'transient-inactive-value)))

;;;###autoload
(transient-define-prefix gptel-openai-responses-extended-built-in-tools ()
  [["Built-in tools"
    ("wl" "Web search (low context)" ""
     :class gptel-openai-responses-extended--add-to-list-switch
     :target-value (:type "web_search" :search_context_size "low" :external_web_access t)
     :target-list gptel-openai-responses-extended--tools)
    ("wm" "Web search (medium context)" ""
     :class gptel-openai-responses-extended--add-to-list-switch
     :target-value (:type "web_search" :search_context_size "medium" :external_web_access t)
     :target-list gptel-openai-responses-extended--tools)
    ("wh" "Web search (high context)" ""
     :class gptel-openai-responses-extended--add-to-list-switch
     :target-value (:type "web_search" :search_context_size "high" :external_web_access t)
     :target-list gptel-openai-responses-extended--tools)
    ("fo" "File search" ""
     :if (lambda () (not (gptel-openai-responses-extended--oauth-backend-p gptel-backend)))
     :class gptel-openai-responses-extended--add-to-list-switch
     :target-value (lambda ()
                     (list :type "file_search" :vector_store_ids
                           (vconcat gptel-openai-responses-extended-vector-store-ids)))
     :target-list gptel-openai-responses-extended--tools)
    ("DEL" "Remove all" (lambda () (interactive)
                             (setq gptel-openai-responses-extended--tools nil)
                             (transient-setup)) :transient t)
    ("RET" "Done" transient-quit-one)]])

;;;###autoload
(defun gptel-openai-responses-extended-setup-builtin-transient ()
  "Add hosted tool selection to `gptel-menu' for Responses backends."
  (transient-append-suffix 'gptel-menu '(0 -1)
    ["OpenAI built-in tools"
     :if (lambda () (gptel-openai-responses-p gptel-backend))
     (gptel-openai-responses-extended-built-in-tools :key "T" :description "Select")]))

(provide 'gptel-openai-responses-backend)
;;; gptel-openai-responses-backend.el ends here
