;;; gopher.el --- easily access and navigate Gopher servers

;; Copyright (C) 2011 Matthew Snyder

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Matthew Snyder <matthew.c.snydere@gmail.com>
;; URL: http://github.com/ardekantur/gopher.el
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; gopher.el allows you to navigate Gopher servers.

;; "M-x gopher" prompts you for an address. <TAB> and <M-TAB> navigate
;; between links on a directory listing, while <[> and <]> navigate
;; between text documents. <RET> opens the link at the cursor's
;; position. There's no history yet, but you can navigate upwards the
;; directory tree with <u>.

(require 'cl)

(defconst gopher-available-content-types
  '(("0" . plain-text)
    ("1" . directory-listing)
    ("i" . informational-message)
    ("g" . gif)
    ("I" . generic-image)
    ("7" . search-query)))

(defconst gopher-extra-network-arguments
  '((gif . (:coding binary))
    (generic-image . (:coding binary))))

(defconst gopher-faces
  '((directory-listing . font-lock-builtin-face)
    (informational-message . font-lock-comment-face)
    (gif . font-lock-variable-name-face)
    (generic-image . font-lock-string-face)))

(defvar gopher-buffer-name "*gopher*")

(defun gopher-get-matching (function content-type)
  (let ((name (intern (concat "gopher-" function "-" (symbol-name content-type)))))
    (if (fboundp name)
        name
      (intern (concat "gopher-" function)))))

(defun gopher-refresh-current-address ()
  (interactive)
  (apply 'gopher-goto-url gopher-current-address))

(defun gopher-get-content-type (line-data)
  (let ((content-type (assoc (getf line-data :item-type) gopher-available-content-types)))
    (if content-type
        (cdr content-type)
      nil)))

(defun gopher-get-face (content-type)
  (let ((face (assoc content-type gopher-faces)))
    (if face
        (cdr face)
      nil)))

(defun gopher-get-extra-network-args (content-type)
  (let ((args (assoc content-type gopher-extra-network-arguments)))
    (if args
        (cdr args)
      nil)))

(defun gopher (address)
  (interactive "MGopher URL: ")
  (let* ((split-address (split-string (replace-regexp-in-string "^gopher:\/\/" "" address) "/"))
         (url (car split-address))
         (selector (mapconcat 'identity (cdr split-address) "/")))
    (if (< (length selector) 1)
        (gopher-goto-url url nil)
      (gopher-goto-url url (concat "/" selector)))))

(defun gopher-goto-url (&optional url selector content-type search-argument)
  (interactive)
  (if (get-buffer gopher-buffer-name)
      (kill-buffer gopher-buffer-name))
  (if (not content-type)
      (setq content-type 'directory-listing))
  (setq gopher-network-args (append (list
                                     :name "gopher" 
                                     :buffer gopher-buffer-name 
                                     :host url 
                                     :service 70 
                                     :filter (gopher-get-matching "filter" content-type)
                                     :sentinel (gopher-get-matching "sentinel" content-type))
                                    (gopher-get-extra-network-args content-type)))
  (setq gopher-current-network-process (apply 'make-network-process gopher-network-args)
        gopher-line-fragment nil)
  (process-send-string gopher-current-network-process (gopher-prepare-request selector search-argument))
  (gopher-prepare-buffer url selector))

(defun gopher-prepare-request (selector search-argument)
  (cond
   ((and selector search-argument) (format "%s\t%s\r\n" selector search-argument))
   (selector (format "%s\r\n" selector))
   (t "\r\n")))
  
(defun gopher-prepare-buffer (url selector)
  (set-window-buffer (selected-window) gopher-buffer-name)
  (with-current-buffer gopher-buffer-name
    (gopher-mode)
    (setq gopher-current-address (list url selector)
          gopher-current-data nil
          line-spacing 3)
    (insert "\n\n")))

(defun gopher-format-address (address)
  (let ((url (nth 0 address))
        (selector (nth 1 address)))
    (cond
     ((and selector
           (not (zerop (length selector)))
           (string= "/" (substring selector 0 1))) (format "%s%s" url selector))
     (selector (format "%s/%s" url selector))
     (t url))))

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

(defun gopher-filter (proc string)
  (with-current-buffer gopher-buffer-name
    (insert string)))

(defun gopher-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun gopher-filter-gif (proc string)
  (with-current-buffer gopher-buffer-name
    (setq gopher-current-data (concat gopher-current-data string))))

(defun gopher-filter-generic-image (proc string)
  (with-current-buffer gopher-buffer-name
    (setq gopher-current-data (concat gopher-current-data string))))

(defun gopher-filter-directory-listing (proc string)
  (with-current-buffer gopher-buffer-name
    (setq gopher-current-data (concat gopher-current-data string))))

(defun gopher-display-line (line)
  (if (or 
       (zerop (length line))
       (string-match "^\.$" line))
      ""
    (let* ((line-data (gopher-process-line line))
           (indent (apply 'propertize "     " line-data)))
      (concat indent (gopher-format-line line-data) "\n"))))

(defun gopher-format-line (line-data)
  (let ((content-type (gopher-get-content-type line-data)))
    (if (and content-type (gopher-get-face content-type))
        (propertize (getf line-data :display-string) 
                    'face (gopher-get-face content-type))
      (getf line-data :display-string))))

(defun gopher-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (gopher-finish-buffer))))

(defun gopher-sentinel-directory-listing (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (let* ((lines (split-string gopher-current-data "\r\n")))
        (mapc (lambda (line) (insert (gopher-display-line line))) lines))
      (gopher-finish-buffer))))

(defalias 'gopher-sentinel-search-query 'gopher-sentinel-directory-listing)
(defalias 'gopher-filter-search-query 'gopher-filter-directory-listing)

(defun gopher-sentinel-plain-text (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (gopher-finish-buffer))))

(defun gopher-sentinel-gif (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (insert "     ")
      (insert-image (create-image gopher-current-data 'gif 'data))
      (gopher-finish-buffer))))

(defun gopher-sentinel-generic-image (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (let ((image-type (image-type-from-data gopher-current-data)))
        (if image-type
            (progn
              (insert "     ")
              (insert-image (create-image
                             gopher-current-data image-type 'data))
              (gopher-finish-buffer))
          (error (format "Could not determine image type for %s" gopher-current-address)))))))

(defun gopher-finish-buffer ()
  (setq buffer-read-only t)
  (goto-char (point-min))
  (gopher-remove-dos-eol)
  (message "Loaded %s." (gopher-format-address gopher-current-address)))

(defun gopher-goto-url-at-point (&optional arg)
  (interactive)
  (move-beginning-of-line nil)
  (let* ((properties (text-properties-at (point)))
        (content-type (gopher-get-content-type properties)))
    (if (eq content-type 'search-query)
        (call-interactively 'gopher-goto-search)
      (gopher-goto-url (getf properties :hostname)
                       (getf properties :selector)
                       content-type))))

(defun gopher-goto-parent (&optional arg)
  (interactive)
  (let* ((address gopher-current-address)
         (url (nth 0 address))
         (selector (nth 1 address)))
  (gopher-goto-url url (gopher-selector-parent selector))))

(defun gopher-goto-search (search-argument)
  (interactive "MSearch argument: ")
  (let* ((properties (text-properties-at (point)))
         (content-type (gopher-get-content-type properties)))
    (gopher-goto-url (getf properties :hostname)
                     (getf properties :selector)
                     content-type search-argument)))

(define-derived-mode gopher-mode fundamental-mode "Gopher"
  (set (make-local-variable 'gopher-current-data) nil)
  (set (make-local-variable 'gopher-current-address) nil))

(defvar gopher-current-data nil)
(defvar gopher-current-address nil)

(defalias 'gopher-next-line 'next-line)
(defalias 'gopher-previous-line 'previous-line)

(defun gopher-pop-last (list)
  (remove-if (lambda (x) t) list :count 1 :from-end t))

(defun gopher-selector-parent (selector)
  (mapconcat 'identity (gopher-pop-last (split-string selector "/")) "/"))

(defun w3m-open-this-url-in-gopher ()
  "Open this URL in Gopher."
  (interactive)
  (gopher (w3m-anchor)))

(defmacro gopher-navigate (direction content-type)
  `(defun ,(intern (concat "gopher-" (symbol-name direction) "-" (symbol-name content-type))) ()
     (interactive)
     (,direction)
     (move-beginning-of-line nil)
     (while (not (eq ',content-type (gopher-get-content-type (text-properties-at (point)))))
       (,direction))))

(defun gopher-define-keymaps ()
  (setq gopher-mode-map (make-sparse-keymap))
  (define-key gopher-mode-map "\r" 'gopher-goto-url-at-point)
  (define-key gopher-mode-map "n" 'gopher-next-line)
  (define-key gopher-mode-map "p" 'gopher-previous-line)
  (define-key gopher-mode-map "g" 'gopher)
  (define-key gopher-mode-map "\t" (gopher-navigate next-line directory-listing))
  (define-key gopher-mode-map "\M-\t" (gopher-navigate previous-line directory-listing))
  (define-key gopher-mode-map "]" (gopher-navigate next-line plain-text))
  (define-key gopher-mode-map "[" (gopher-navigate previous-line plain-text))
  (define-key gopher-mode-map "u" 'gopher-goto-parent)
  (define-key gopher-mode-map "r" 'gopher-refresh-current-address)
  (define-key gopher-mode-map "q" 'quit-window))

(gopher-define-keymaps)

(defun gopher-kill-address-at-point ()
  (interactive)
  (move-beginning-of-line nil)
  (let* ((properties (text-properties-at (point)))
         (string (mapconcat 'identity (list
                                       (getf properties :hostname)
                                       (getf properties :selector)) "/")))
    (kill-new string)
    (message string)))

(provide 'gopher)

;;; gopher.el ends here
