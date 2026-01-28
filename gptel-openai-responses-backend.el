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
(require 'url)
(require 'map)

(defcustom gptel-openai-response-include-file-search-results t
  "Include file-search-results?"
  :type 'boolean)

(defcustom gptel-openai-response-vector-store-ids nil
  "List of vector store ids to use when using file search tool."
  :type '(repeat string))

(cl-defstruct (gptel-openai-responses
               (:constructor gptel-openai--make-responses)
               (:copier nil)
               (:include gptel-openai)))

(cl-defun gptel-make-openai-responses
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

(defun gptel-openai-response--process-output (output-item info)
  ;; both streams output_item.done.item and the output-item in response are the same.
  (pcase (plist-get output-item :type)
    ("function_call"
     (gptel--inject-prompt        ; First add the tool call to the prompts list
      (plist-get info :backend)
      (plist-get info :data)
      (copy-sequence output-item)) ;; copying to avoid following changes effect output-item
     (ignore-errors (plist-put output-item :args
                               (gptel--json-read-string
                                (plist-get output-item :arguments))))
     (plist-put output-item :arguments nil)
     (plist-put info :tool-use
                (append
                 (plist-get info :tool-use)
                 (list output-item))))
    ("reasoning"
     (gptel--inject-prompt        ; responses expects the reasoning blocks
      (plist-get info :backend) (plist-get info :data) output-item)
     (plist-put info :reasoning
                (append
                 (plist-get info :reasoning)
                 (list (map-nested-elt output-item '(:summary :text))))))
    ("file_search_call"
     (plist-put info :file-search-call-queries (plist-get output-item :queries))
     (plist-put info :file-search-call-results (plist-get output-item :results)))
    (_ ;; TODO handle others
     )))

(defun gptel-openai-response--process-annotation (annotation info)
  "Returns string that can be added to content or nil."
  (pcase (plist-get annotation :type)
    ("file_citation"
     (format "[file_citation:%s]" (plist-get annotation :file_id)))
    (_ ;; TODO handle otheres
     )))

(cl-defmethod gptel--request-data ((backend gptel-openai-responses) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (let* ((prompts (cl-call-next-method))
         (p prompts))
    (when gptel-openai-response-include-file-search-results
      (plist-put prompts :include (vconcat (plist-get prompts :include)
                                          '("file_search_call.results"))))
    ;; Adding built-in tools
    (when gptel-openai-responses--tools
      (plist-put prompts :tools (vconcat (plist-get prompts :tools)
                                         (mapcar (lambda (built-in-tool)
                                                   (if (functionp built-in-tool)
                                                       (funcall built-in-tool)
                                                     built-in-tool))
                                                 gptel-openai-responses--tools))))
    (when-let (tool-defs (plist-get prompts :tools))
      (plist-put prompts :tools
                 (cl-loop for tool-def across tool-defs
                          if (string-equal (plist-get tool-def :type) "function")
                          collect (append '(:type "function") (plist-get tool-def :function))
                          into out
                          else collect tool-def into out
                          finally return (apply #'vector out))))
    (while p
      (when (eq (car p) :messages)
        (setcar p :input)
        (setcar (cdr p)
                (apply #'vector
                       (mapcar
                        (lambda (input-item)
                          (let ((tool-calls (plist-get input-item :tool_calls))
                                (content (plist-get input-item :content)))
                            (cond
                             ;; Handle tool-cal data
                             (tool-calls
                              (cl-assert (= (length tool-calls) 1) nil
                                         "Expected exactly one value in :tool_calls")
                              (let ((tool-call (aref tool-calls 0)))
                                `(:type "function_call"
                                        :call_id ,(plist-get tool-call :id)
                                        ,@(plist-get tool-call :function))))
                             ;; Handle multi-part data
                             ((and content (vectorp content))
                              (plist-put
                               input-item :content
                               (apply #'vector
                                      (cl-loop for el across content
                                               for type = (cadr el)
                                               collect
                                               (pcase type
                                                 ("image_url"
                                                  (list :type "input_image"
                                                        :image_url (map-nested-elt el '(:image_url :url))))
                                                 ("text"
                                                  (append (list :type "input_text")
                                                          (cddr el)))
                                                 (t el)))))
                              input-item)
                             (t input-item))))
                        (cadr p)))))
      (when (memq (car p) '(:max_completion_tokens :max_tokens))
        (setcar p :max_output_tokens))
      (setq p (cddr p)))
    prompts))

(cl-defmethod gptel--inject-prompt
    ((backend gptel-openai-responses) data new-prompt &optional _position)
  "JSON encode PROMPTS for sending to ChatGPT."
  (when (keywordp (car-safe new-prompt)) ;Is new-prompt one or many?
    (setq new-prompt (list new-prompt)))
  (let ((prompts (plist-get data :input)))
    (plist-put data :input (vconcat prompts new-prompt))))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-openai-responses) info)
  "Parse an OpenAI API data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it."
  (let* ((content-strs))
    (condition-case err
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (let ((json-response (save-excursion
                                   (gptel--json-read))))
              (pcase (plist-get json-response :type)
                ;; ("response.completed"
                ;;  ;; Once stream end processing
                ;;  )
                ("response.output_text.delta"
                 (push (plist-get json-response :delta) content-strs))
                ("response.output_text.annotation.added"
                 (let ((annotation (plist-get json-response :annotation)))
                   (push (gptel-openai-response--process-annotation annotation info) content-strs)))
                ("response.output_item.done"
                 (let ((output-item (plist-get json-response :item)))
                   (gptel-openai-response--process-output output-item info)))))))
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-openai-responses) response info)
  "Parse an OpenAI (non-streaming) RESPONSE and return response text.

Mutate state INFO with response metadata."
  (plist-put info :stop-reason
             (list (plist-get response :status)
                              (plist-get response :incomplete_details)))
  (plist-put info :output-tokens
             (map-nested-elt response '(:usage :total_tokens)))

  (cl-loop for output-item across (plist-get response :output)
           if (equal (plist-get output-item :type) "message")
             collect
             (string-join
              (list (map-nested-elt output-item '(:content 0 :text))
                    (string-join
                     (mapcar (lambda (annotation)
                               (gptel-openai-response--process-annotation annotation info))
                             (map-nested-elt output-item '(:content 0 :annotations)))
                     "\n"))
              "\n")
             into return-val
           else
             do (gptel-openai-response--process-output output-item info)
           finally return (funcall #'string-join return-val)))

(cl-defmethod gptel--parse-tool-results ((_backend gptel-openai-responses) tool-use)
  "Return a prompt containing tool call results in TOOL-USE."
  (mapcar
   (lambda (tool-call)
     (list
      :type "function_call_output"
      :call_id (plist-get tool-call :id)
      :output (plist-get tool-call :result)))
   tool-use))

;;; Supporting built-in tools for responses ***********************************************
(defclass gptel-openai-response--add-to-list-switch (transient-variable)
  ((target-value :initarg :target-value)
   (target-list  :initarg :target-list)
   (format       :initarg :format      :initform " %k %d")
   ))

(cl-defmethod transient-infix-read ((obj gptel-openai-response--add-to-list-switch))
  ;;Do nothing
  )

(cl-defmethod transient-infix-set ((obj gptel-openai-response--add-to-list-switch) _)
  (if (member (oref obj target-value) (symbol-value (oref obj target-list)))
      (set (oref obj target-list)
           (delete (oref obj target-value) (symbol-value (oref obj target-list))))
    (set (oref obj target-list)
         (append (symbol-value (oref obj target-list))
                 (list (oref obj target-value)))))
  (transient-setup))

(cl-defmethod transient-format-description ((obj gptel-openai-response--add-to-list-switch))
  (propertize (transient--get-description obj) 'face
              (if (member (oref obj target-value) (symbol-value (oref obj target-list)))
                  'transient-value
                'transient-inactive-value)))

(defvar gptel-openai-responses--known-tools '(("web_search_preview" .
                                                 (lambda ()
                                                   (list :type "web_search_preview" :search-context-size "low")))))

(defvar gptel-openai-responses--tools nil)

(transient-define-prefix gptel-openai-response-built-in-tools ()
  [["Built in tools"
    ("wl" "web search (low context)" ""
     :class gptel-openai-response--add-to-list-switch
     :target-value (:type "web_search" :search_context_size "low")
     :target-list gptel-openai-responses--tools)
    ("wm" "web search (medium context)" ""
     :class gptel-openai-response--add-to-list-switch
     :target-value (:type "web_search" :search_context_size "medium")
     :target-list gptel-openai-responses--tools)
    ("wh" "web search (high context)" ""
     :class gptel-openai-response--add-to-list-switch
     :target-value (:type "web_search" :search_context_size "high")
     :target-list gptel-openai-responses--tools)
    ("fo" "File search (org)" ""
     :class gptel-openai-response--add-to-list-switch
     :target-value (lambda ()
                     `(:type "file_search"
                             :vector_store_ids ,gptel-openai-response-vector-store-ids))
     :target-list gptel-openai-responses--tools)
    ""
    ("DEL" "Remove all" (lambda ()
                          (interactive)
                          (setq gptel-openai-responses--tools nil)
                          (transient-setup))
     :transient t
     :if (lambda () gptel-openai-responses--tools))
    ("RET" "Done" transient-quit-one)
    ]])

(transient-append-suffix 'gptel-menu '(0 -1)
  [:if (lambda () (gptel-openai-responses-p gptel-backend))
   ""
   (:info
    (lambda ()
      (concat
       "OpenAI Built-in tools"
       (and gptel-openai-responses--tools
            (concat " (" (propertize (format "%d"
                                             (length gptel-openai-responses--tools))
                                     'face 'warning)
                    ")"))))
    :format "%d" :face transient-heading)
   (gptel-openai-response-built-in-tools :key "T" :description "Select")])

(provide 'gptel-openai-responses-backend)
;;; gptel-openai-responses-backend.el ends here


