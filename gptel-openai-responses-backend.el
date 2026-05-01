;;; gptel-openai-responses-backend.el --- openai assistant interface for gptel -*- lexical-binding: t -*-

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

;; openai responses backend for gptel

;;; Code:

(require 'gptel)
(require 'gptel-openai)
(require 'gptel-openai-responses)
(require 'url)
(require 'map)
(require 'transient)

(defcustom gptel-openai-responses-extended-include-file-search-results t
  "Include file-search-results?"
  :type 'boolean)

(defcustom gptel-openai-responses-extended-vector-store-ids nil
  "List of vector store ids to use when using file search tool."
  :type '(repeat string))

(cl-defstruct (gptel-openai-responses-extended
               (:constructor gptel-openai--make-responses)
               (:copier nil)
               (:include gptel-openai-responses)))

(cl-defun gptel-make-openai-responses-extended
    (name &key curl-args models stream key request-params
          (header
           (lambda () (when-let* ((key (gptel--get-api-key)))
                   `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.openai.com")
          (protocol "https")
          (endpoint "/v1/responses"))
  "Register an OpenAI Responses compatible backend with gptel."
  (declare (indent 1))
  (let ((backend (gptel-openai--make-responses
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (gptel--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name gptel--known-backends
                       nil nil #'equal)
                  backend))))

;; (defun gptel-openai-responses-extended--process-output (output-item info)
;;   ;; both streams output_item.done.item and the output-item in response are the same.
;;   (pcase (plist-get output-item :type)
;;     ("function_call"
;;      (gptel--inject-prompt        ; First add the tool call to the prompts list
;;       (plist-get info :backend)
;;       (plist-get info :data)
;;       (copy-sequence output-item)) ;; copying to avoid following changes effect output-item
;;      (ignore-errors (plist-put output-item :args
;;                                (gptel--json-read-string
;;                                 (plist-get output-item :arguments))))
;;      (plist-put output-item :arguments nil)
;;      (plist-put info :tool-use
;;                 (append
;;                  (plist-get info :tool-use)
;;                  (list output-item))))
;;     ("reasoning"
;;      (gptel--inject-prompt        ; responses expects the reasoning blocks
;;       (plist-get info :backend) (plist-get info :data) output-item)
;;      (plist-put info :reasoning
;;                 (append
;;                  (plist-get info :reasoning)
;;                  (list (map-nested-elt output-item '(:summary :text))))))
;;     ("file_search_call"
;;      (plist-put info :file-search-call-queries (plist-get output-item :queries))
;;      (plist-put info :file-search-call-results (plist-get output-item :results)))
;;     (_ ;; TODO handle others
;;      )))

(defun gptel-openai-responses-extended--process-annotation (annotation info)
  "Returns string that can be added to content or nil."
  (pcase (plist-get annotation :type)
    ("file_citation"
     (format "[file_citation:%s]" (plist-get annotation :file_id)))
    (_ ;; TODO handle otheres
     )))

(cl-defmethod gptel--request-data ((backend gptel-openai-responses-extended) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let* ((prompts (cl-call-next-method)))
    (when gptel-openai-responses-extended-include-file-search-results
      (plist-put prompts :include (vconcat (plist-get prompts :include)
                                          '("file_search_call.results"))))
    ;; Adding built-in tools
    (when gptel-openai-responses-extended--tools
      (plist-put prompts :tools (vconcat (plist-get prompts :tools)
                                         (mapcar (lambda (built-in-tool)
                                                   (if (functionp built-in-tool)
                                                       (funcall built-in-tool)
                                                     built-in-tool))
                                                 gptel-openai-responses-extended--tools))))
    prompts))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai-responses-extended) info)
  "Parse an OpenAI Responses API data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it."
  (let ((content-strs) wait)
    (condition-case nil
        (while (and (not wait) (re-search-forward "^event: *\\(.+\\)" nil t))
          (let ((event-type (match-string 1)) data)
            (forward-line 1)
            (if (not (looking-at "data:" t))
                (progn (goto-char (match-beginning 0)) ;not enough data, reset
                       (setq wait t))
              (forward-char 5)
              (setq data (gptel--json-read))
              (pcase event-type
                ;; Text content delta
                ("response.output_text.delta"
                 (when-let* ((delta (plist-get data :delta))
                             ((not (string-empty-p delta))))
                   (push delta content-strs)))
                ;; Function call arguments delta
                ("response.function_call_arguments.delta"
                 (when-let* ((delta (plist-get data :delta)))
                   (plist-put info :partial_json
                              (cons delta (plist-get info :partial_json)))))
                ;; Function call completed (user-defined tools)
                ("response.output_text.annotation.added"
                 (let ((annotation (plist-get data :annotation)))
                   (push (gptel-openai-responses-extended--process-annotation annotation info) content-strs)))
                ("response.output_item.done"
                 (when-let* ((item (plist-get data :item))
                             ((equal (plist-get item :type) "function_call"))
                             (tool-call
                              (list :id (plist-get item :call_id)
                                    :name (plist-get item :name)
                                    :args (ignore-errors
                                            (gptel--json-read-string
                                             (plist-get item :arguments))))))
                   (plist-put info :tool-use
                              (cons tool-call (plist-get info :tool-use)))
                   (plist-put info :partial_json nil)))
                ;; Reasoning content
                ((or "response.reasoning_summary_text.delta"
                     "response.reasoning.delta")
                 (when-let* ((delta (plist-get data :delta)))
                   (plist-put info :reasoning
                              (concat (plist-get info :reasoning) delta))))
                ((or "response.reasoning_summary_text.done"
                     "response.reasoning.done")
                 (plist-put info :reasoning-block t))
                ;; NOTE: backend tools are not supported in gptel yet, this
                ;; parsing is for the future
                ;; Web search completed (server-side tool)
                ("response.web_search_call.completed"
                 (push "\n[Web search completed]" content-strs))
                ;; Code interpreter output (server-side tool)
                ("response.code_interpreter_call.completed"
                 (when-let* ((item (plist-get data :item))
                             (results (plist-get item :results)))
                   (cl-loop
                    for result across results
                    if (equal (plist-get result :type) "logs")
                    do (push (format "\n```\n%s\n```" (plist-get result :logs))
                             content-strs))))
                ;; Response completed
                ("response.completed"
                 (when-let* ((tool-use (plist-get info :tool-use)))
                   ;; Inject tool calls into prompt data for continuation
                   ;; TODO(responses-api) Avoid re-encoding these tool calls,
                   ;; especially :arguments
                   (gptel--inject-prompt
                    (plist-get info :backend) (plist-get info :data)
                    (mapcar (lambda (tc)
                              (list :type "function_call"
                                    :call_id (plist-get tc :id)
                                    :name (plist-get tc :name)
                                    :arguments
                                    (gptel--json-encode (plist-get tc :args))))
                            tool-use)))
                 (when-let* ((resp (plist-get data :response)))
                   ;; KLUDGE: `gptel--parse-response' handles this as well.
                   (mapcar (lambda (item)
                             (when (equal (plist-get item :type) "file_search_call")
                               (plist-put info :file-search-call-queries (plist-get item :queries))
                               (plist-put info :file-search-call-results (plist-get item :results))))
                           (plist-get resp :output))
                   (plist-put info :stop-reason (plist-get resp :status))
                   (plist-put info :tokens (gptel--openai-responses-update-tokens
                                            (plist-get resp :usage) info))))))))
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-openai-responses-extended) response info)
  "Parse an OpenAI Responses API RESPONSE and return response text.
Mutate state INFO with response metadata."
  (let ((output-items (plist-get response :output))
        (content-strs) (tool-use) (tool-calls))
    ;; Store usage info
    (plist-put info :stop-reason (plist-get response :status))
    (plist-put info :tokens (gptel--openai-responses-update-tokens
                             (plist-get response :usage) info))
    ;; Process output items
    (cl-loop
     for item across output-items
     for item-type = (plist-get item :type)
     do
     (pcase item-type
       ;; Text message output
       ("message"
        (when-let* ((content (plist-get item :content)))
          (cl-loop
           for part across content
           for part-type = (plist-get part :type)
           if (equal part-type "output_text")
           do (progn
                (push (plist-get part :text) content-strs)
                (push (string-join
                       (mapcar (lambda (annotation)
                                 (gptel-openai-responses-extended--process-annotation annotation info))
                               (map-nested-elt item '(:content 0 :annotations))))
                      content-strs))
           else if (equal part-type "refusal")
           do (push (format "[Refused: %s]" (plist-get part :refusal))
                    content-strs))))
       ;; Function call from model (user-defined tools)
       ("function_call"
        (push item tool-calls)
        (push (list :id (plist-get item :call_id)
                    :name (plist-get item :name)
                    :args (ignore-errors
                            (gptel--json-read-string
                             (plist-get item :arguments))))
              tool-use))
       ;; Reasoning summary
       ("reasoning"
        (cl-loop with summary = (plist-get item :summary)
                 with content = (plist-get item :content)
                 for s across
                 (if (length= content 0) summary content)
                 collect (plist-get s :text) into reasoning
                 finally do
                 (plist-put info :reasoning (apply #'concat reasoning))))
       ;; Web search results (server-side tool)
       ("web_search_call"
        (when-let* ((status (plist-get item :status))
                    ((equal status "completed")))
          ;; Results are inline, just note that search was done
          (push "\n[Web search completed]" content-strs)))
       ;; Code interpreter output (server-side tool)
       ("code_interpreter_call"
        (when-let* ((status (plist-get item :status))
                    ((equal status "completed"))
                    (results (plist-get item :results)))
          (cl-loop
           for result across results
           for result-type = (plist-get result :type)
           if (equal result-type "logs")
           do (push (format "\n```\n%s\n```" (plist-get result :logs))
                    content-strs))))
       ;; File search results (server-side tool)
       ("file_search_call"
        (when-let* ((status (plist-get item :status))
                    ((equal status "completed"))
                    (results (plist-get item :results)))
          (push (format "\n[File search: %d results]" (length results))
                content-strs))
        (plist-put info :file-search-call-queries (plist-get item :queries))
        (plist-put info :file-search-call-results (plist-get item :results)))))
    ;; Store tool calls for user-defined function tools
    (when tool-use
      (plist-put info :tool-use (nreverse tool-use))
      ;; Inject into prompts for conversation continuity
      (gptel--inject-prompt
       (plist-get info :backend) (plist-get info :data)
       (nreverse tool-calls)))
    ;; Return concatenated content
    (when content-strs
      (apply #'concat (nreverse content-strs)))))

;; (cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai-responses-extended) info)
;;   "Parse an OpenAI API data stream.

;; Return the text response accumulated since the last call to this
;; function.  Additionally, mutate state INFO to add tool-use
;; information if the stream contains it."
;;   (let* ((content-strs))
;;     (condition-case err
;;         (while (re-search-forward "^data:" nil t)
;;           (save-match-data
;;             (let ((json-response (save-excursion
;;                                    (gptel--json-read))))
;;               (pcase (plist-get json-response :type)
;;                 ;; ("response.completed"
;;                 ;;  ;; Once stream end processing
;;                 ;;  )
;;                 ("response.output_text.delta"
;;                  (push (plist-get json-response :delta) content-strs))
;;                 ("response.output_text.annotation.added"
;;                  (let ((annotation (plist-get json-response :annotation)))
;;                    (push (gptel-openai-responses-extended--process-annotation annotation info) content-strs)))
;;                 ("response.output_item.done"
;;                  (let ((output-item (plist-get json-response :item)))
;;                    (gptel-openai-responses-extended--process-output output-item info)))))))
;;       (error (goto-char (match-beginning 0))))
;;     (apply #'concat (nreverse content-strs))))

;; (cl-defmethod gptel--parse-response ((_backend gptel-openai-responses-extended) response info)
;;   "Parse an OpenAI (non-streaming) RESPONSE and return response text.

;; Mutate state INFO with response metadata."
;;   (plist-put info :stop-reason
;;              (list (plist-get response :status)
;;                               (plist-get response :incomplete_details)))
;;   (plist-put info :output-tokens
;;              (map-nested-elt response '(:usage :total_tokens)))

;;   (cl-loop for output-item across (plist-get response :output)
;;            if (equal (plist-get output-item :type) "message")
;;              collect
;;              (string-join
;;               (list (map-nested-elt output-item '(:content 0 :text))
;;                     (string-join
;;                      (mapcar (lambda (annotation)
;;                                (gptel-openai-responses-extended--process-annotation annotation info))
;;                              (map-nested-elt output-item '(:content 0 :annotations)))
;;                      "\n"))
;;               "\n")
;;              into return-val
;;            else
;;              do (gptel-openai-responses-extended--process-output output-item info)
;;            finally return (funcall #'string-join return-val)))

;;; Supporting built-in tools for responses ***********************************************
(defclass gptel-openai-responses-extended--add-to-list-switch (transient-variable)
  ((target-value :initarg :target-value)
   (target-list  :initarg :target-list)
   (format       :initarg :format      :initform " %k %d")
   ))

(cl-defmethod transient-infix-read ((obj gptel-openai-responses-extended--add-to-list-switch))
  ;;Do nothing
  )

(cl-defmethod transient-infix-set ((obj gptel-openai-responses-extended--add-to-list-switch) _)
  (if (member (oref obj target-value) (symbol-value (oref obj target-list)))
      (set (oref obj target-list)
           (delete (oref obj target-value) (symbol-value (oref obj target-list))))
    (set (oref obj target-list)
         (append (symbol-value (oref obj target-list))
                 (list (oref obj target-value)))))
  (transient-setup))

(cl-defmethod transient-format-description ((obj gptel-openai-responses-extended--add-to-list-switch))
  (propertize (transient--get-description obj) 'face
              (if (member (oref obj target-value) (symbol-value (oref obj target-list)))
                  'transient-value
                'transient-inactive-value)))

(defvar gptel-openai-responses-extended--known-tools '(("web_search_preview" .
                                                 (lambda ()
                                                   (list :type "web_search_preview" :search-context-size "low")))))

(defvar gptel-openai-responses-extended--tools nil)

;;;###autoload (autoload 'gptel-openai-responses-extended-built-in-tools "gptel-openai-responses-extended-backend" nil t)
(transient-define-prefix gptel-openai-responses-extended-built-in-tools ()
  [["Built in tools"
    ("wl" "web search (low context)" ""
     :class gptel-openai-responses-extended--add-to-list-switch
     :target-value (:type "web_search" :search_context_size "low")
     :target-list gptel-openai-responses-extended--tools)
    ("wm" "web search (medium context)" ""
     :class gptel-openai-responses-extended--add-to-list-switch
     :target-value (:type "web_search" :search_context_size "medium")
     :target-list gptel-openai-responses-extended--tools)
    ("wh" "web search (high context)" ""
     :class gptel-openai-responses-extended--add-to-list-switch
     :target-value (:type "web_search" :search_context_size "high")
     :target-list gptel-openai-responses-extended--tools)
    ("fo" "File search (org)" ""
     :class gptel-openai-responses-extended--add-to-list-switch
     :target-value (lambda ()
                     `(:type "file_search"
                             :vector_store_ids ,gptel-openai-responses-extended-vector-store-ids))
     :target-list gptel-openai-responses-extended--tools)
    ""
    ("DEL" "Remove all" (lambda ()
                          (interactive)
                          (setq gptel-openai-responses-extended--tools nil)
                          (transient-setup))
     :transient t
     :if (lambda () gptel-openai-responses-extended--tools))
    ("RET" "Done" transient-quit-one)
    ]])

;;;###autoload
(defun gptel-openai-responses-extended-setup-builtin-transient ()
  "Configure `gptel-menu' with responses builtin tools."
  (transient-append-suffix 'gptel-menu '(0 -1)
    [;;:if (lambda () (gptel-openai-responses-extended-p gptel-backend))
         ""
         (:info
          (lambda ()
            (concat
             "OpenAI Built-in tools"
             (and gptel-openai-responses-extended--tools
                  (concat " (" (propertize (format "%d"
                                                   (length gptel-openai-responses-extended--tools))
                                           'face 'warning)
                          ")"))))
          :format "%d" :face transient-heading)
         (gptel-openai-responses-extended-built-in-tools :key "T" :description "Select")]))

(provide 'gptel-openai-responses-backend)
;;; gptel-openai-responses-backend.el ends here


