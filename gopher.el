(defun gopher-goto-url (url)
  (interactive "MGopher URL: ")
  (setq gopher-current-network-process (make-network-process :name "gopher" :buffer "*gopher*" :host url :service 70 :filter 'gopher-server-filter :sentinel 'gopher-server-sentinel)
        gopher-line-fragment nil)
  (process-send-string gopher-current-network-process "\n")
  (set-window-buffer (selected-window) "*gopher*")
  (with-current-buffer "*gopher*"
    (insert "\n\n")))

(defun gopher-process-line (line)
  (let* ((lineparts (split-string line "\t"))
         (item-type (substring (nth 0 lineparts) 0 1))
         (display-string (substring (nth 0 lineparts) 1))
         (selector (nth 1 lineparts))
         (hostname (nth 2 lineparts))
         (port (nth 3 lineparts)))
    (list :item-type item-type
          :display-string display-string
          :selector selector
          :hostname hostname
          :port port)))

(defun gopher-server-filter (proc string)
  (let* ((lines (split-string string "\n"))
         (first-line (pop lines))
         (last-line (car (last lines))))
    (with-current-buffer "*gopher*"
      (if gopher-line-fragment
          (insert (gopher-display-line (concat gopher-line-fragment first-line)))
        (insert (gopher-display-line first-line)))
      (mapc (lambda (line)
              (if (string-match "$" line)
                  (insert (gopher-display-line line))))
            lines)
      (if (string-match "$" last-line)
          (setq gopher-line-fragment nil)
        (setq gopher-line-fragment last-line)))))

(defun gopher-display-line (line)
  (if (zerop (length line))
      ""
    (concat "     " (gopher-format-line (gopher-process-line line)) "\n")))

(defun gopher-directory-menu-listing-p (line-data)
  (string= (getf line-data :item-type) "1"))

(defun gopher-plain-text-p (line-data)
  (string= (getf line-data :item-type) "0"))

(defun gopher-informational-message-p (line-data)
  (string= (getf line-data :item-type) "i"))

(defun gopher-format-line (line-data)
  (cond 
   ((gopher-directory-menu-listing-p line-data) (gopher-formatted-directory-menu-listing-text line-data))
   ((gopher-plain-text-p line-data) (gopher-formatted-plain-text-text line-data))
   ((gopher-informational-message-p line-data) (gopher-formatted-informational-message-text line-data))
   (t (getf line-data :display-string))))

(defun gopher-formatted-directory-menu-listing-text (line-data)
  (propertize (getf line-data :display-string) 'face 'gopher-directory-menu-listing-face))

(defun gopher-formatted-informational-message-text (line-data)
  (propertize (getf line-data :display-string) 'face 'gopher-informational-message-face))

(defun gopher-formatted-plain-text-text (line-data)
  (propertize (getf line-data :display-string) 'face 'gopher-plain-text-face))

(defface gopher-plain-text-face
  '((t :foreground "black"))
  "Face for Gopher directory plain text."
  :group 'gopher-faces)

(defface gopher-directory-menu-listing-face
  '((t :foreground "blue" :weight bold))
  "Face for Gopher directory menu listings."
  :group 'gopher-faces)

(defface gopher-informational-message-face
  '((t :foreground "gray57"))
  "Face for Gopher informational messages."
  :group 'gopher-faces)
  
(defun gopher-server-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (message (format "client %s has quit" proc))))
    ;; (with-current-buffer "*gopher*"
    ;;   (setq buffer-read-only t))))
