;;; gptel-openai-responses-backend-tests.el --- Hosted tool tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel-openai-oauth)
(require 'gptel-openai-responses-backend)

(ert-deftest gptel-hosted-request-tools ()
  (dolist (backend (list (gptel--make-openai-responses)
                         (gptel--make-openai-oauth)))
    (let* ((gptel-backend backend)
           (gptel-model 'gpt-5.5)
           (gptel-temperature nil)
           (gptel-max-tokens nil)
           (gptel-system-prompt nil)
           (gptel-use-tools nil)
           (gptel--request-params
            '(:tools [(:type "function" :name "example")
                       (:type "web_search")]))
           (gptel-openai-responses-extended--tools
            '((:type "web_search" :external_web_access t)
              (:type "web_search")
              (:type "file_search" :vector_store_ids ["vs_test"])))
           (data (gptel--request-data backend nil))
           (types (mapcar (lambda (tool) (plist-get tool :type))
                          (append (plist-get data :tools) nil))))
      (should (= 1 (cl-count "web_search" types :test #'equal)))
      (should (member "function" types))
      (should (member "web_search_call.action.sources"
                      (append (plist-get data :include) nil)))
      (if (gptel-openai-oauth-p backend)
          (progn (should-not (member "file_search" types))
                 (should-not (member "file_search_call.results"
                                     (append (plist-get data :include) nil))))
        (should (member "file_search" types))
        (should (member "file_search_call.results"
                        (append (plist-get data :include) nil)))))))

(ert-deftest gptel-hosted-no-inactive-includes ()
  (let* ((gptel-backend (gptel--make-openai-responses))
         (gptel-model 'gpt-5.5) (gptel-temperature nil)
         (gptel-max-tokens nil) (gptel-system-prompt nil)
         (gptel-use-tools nil) (gptel--request-params nil)
         (gptel-openai-responses-extended--tools nil))
    (should-not (plist-get (gptel--request-data gptel-backend nil) :include))))

(ert-deftest gptel-hosted-parsing ()
  (dolist (backend (list (gptel--make-openai-responses)
                         (gptel--make-openai-oauth)))
    (let ((response '(:status "completed"
                     :output [(:type "web_search_call" :status "completed"
                               :action (:type "search" :sources [(:url "https://example.com")]))
                              (:type "file_search_call" :queries ["query"] :results [])
                              (:type "message" :content
                               [(:type "output_text" :text "Answer"
                                 :annotations [(:type "url_citation" :title "Example"
                                                :url "https://example.com")
                                               (:type "file_citation" :file_id "file_test")])])]))
          (info (list :backend backend)))
      (let ((text (gptel--parse-response backend response info)))
        (should (string-match-p (regexp-quote "[Example](https://example.com)") text))
        (should (string-match-p (regexp-quote "[file_citation:file_test]") text))
        (should (plist-get info :web-search-call-action))
        (should (equal (plist-get info :file-search-call-queries) ["query"])))
      (setq info (list :backend backend))
      (with-temp-buffer
        (insert "event: response.output_text.delta\ndata: {\"delta\":\"Answer\"}\n\n"
                "event: response.completed\ndata: "
                (decode-coding-string
                 (gptel--json-encode (list :response response)) 'utf-8)
                "\n\n")
        (goto-char (point-min))
        (let ((text (gptel-curl--parse-stream backend info)))
          (should (string-match-p "Answer" text))
          (should (string-match-p (regexp-quote "[Example](https://example.com)") text))
          (should (= 1 (length (plist-get info :web-search-calls)))))
        (should (equal "" (gptel-curl--parse-stream backend info)))
        (should (= 1 (length (plist-get info :web-search-calls))))))))

(provide 'gptel-openai-responses-backend-tests)
;;; gptel-openai-responses-backend-tests.el ends here
