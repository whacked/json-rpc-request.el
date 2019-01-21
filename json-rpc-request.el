(require 'json-rpc)

;; the request function in https://github.com/skeeto/elisp-json-rpc
;; does not support arbitrary endpoints. Here is a simple function
;; that does. It is a stopgap before a more solution like
;; http://lists.gnu.org/archive/html/emacs-devel/2018-05/msg00398.html
;; becomes widely available.

(defun json-rpc-request (connection endpoint method &rest mixed-params)
  "Send request of METHOD to CONNECTION at ENDPOINT, return result or error.
   USAGE:
   (json-rpc-request (json-rpc-connect \"localhost\" 5000) \"/rpc\"
                     \"App.index\"
                     ;; send a json dict with an array
                     (list :somekey (list \"elem1\" \"elem1\")))
  The test will get rejected by the example server at
  https://github.com/whacked/hy-sib-json-rpc-example;
  remove the (list ...) and it will succeed.
  "
  (let* ((id (cl-incf (json-rpc-id-counter connection)))
         (params (if (and (= 1 (length mixed-params))
                          (eq 'cons (type-of (car mixed-params))))
                     (car mixed-params)
                   (vconcat mixed-params)))
         (request `(:jsonrpc "2.0" :method ,method :params ,params :id ,id))
         (process (json-rpc-process (json-rpc-ensure connection)))
         (encoded (json-encode request)))
    (with-current-buffer (process-buffer (json-rpc-process connection))
      (erase-buffer))
    (with-temp-buffer
      (insert (concat "POST " endpoint " HTTP/1.1\r\n"))
      (insert (format (concat "Content-Length: %d\r\n"
                              "Content-Type: application/json\r\n"
                              "\r\n")
                      (string-bytes encoded))
              encoded)
      (process-send-region process (point-min) (point-max)))
    (json-rpc-wait connection)))
