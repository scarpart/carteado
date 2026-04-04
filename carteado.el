;;; -*- lexical-binding: t -*-

(defvar *org-home* "/Users/arthur.rodrigues/Org/")
(defvar *scpt/carteado/home* "/Users/arthur.rodrigues/Org/carteado/")
(defvar *scpt/carteado/base-template* "/Users/arthur.rodrigues/.emacs.d/template.org")

;(unintern 'easy-button-face)
;(unintern 'medium-button-face)
;(unintern 'hard-button-face)

(define-button-type 'scpt/carteado/card-button
  'face 'link
  'follow-link t
  'help-echo "This closes the current card and persists the apparent difficulty.")

(defface easy-button-face
  '((t :foreground "#98c379"
       :background "#2d2d2d"
       :box (:line-width 1 :color "#5f9f5f")
       :weight bold
       :underline nil
       :height 1.5))
  "Face for the easy buttons.")
(defface medium-button-face
  '((t :foreground "#61afef"      
       :background "#2d2d2d"
       :box (:line-width 2 :color "#5f87af")
       :weight bold
       :underline nil
       :height 1.5))
  "Face for the medium buttons.")
(defface hard-button-face
  '((t :foreground "#e06c75"      
       :background "#2d2d2d"
       :box (:line-width 2 :color "#af5f5f")
       :weight bold
       :underline nil
       :height 1.5))
  "Face for the difficult buttons")

(define-button-type 'scpt/carteado/easy-button
  :supertype 'scpt/carteado/card-button
  'face 'easy-button-face)
(define-button-type 'scpt/carteado/medium-button
  :supertype 'scpt/carteado/card-button
  'face 'medium-button-face)
(define-button-type 'scpt/carteado/hard-button
  :supertype 'scpt/carteado/card-button
  'face 'hard-button-face)

(defun scpt/carteado/run (category)
  (interactive)
  (let ((buffer (generate-new-buffer (format "carteado-%s" category)))
	(cards (scpt/carteado/get-cards-to-show category)))
    (display-buffer buffer)
    (scpt/carteado/show-next-card buffer category cards 1 (length cards))))

(defun scpt/carteado/show-next-card (buffer category card-paths-and-dates cur-pos total-cards)
  (interactive)
  (if (null card-paths-and-dates)
      (message "Done!")
    (let* ((card (car card-paths-and-dates))
	   (remaining (cdr card-paths-and-dates))
	   (trigger-next-card
	    (lambda () (interactive)
	      (scpt/carteado/show-next-card buffer category remaining
					    (+ 1 cur-pos) total-cards))))
      (scpt/carteado/render-card buffer category card cur-pos total-cards trigger-next-card))))

(defun scpt/carteado/render-card (buffer category card-path-and-date cur-pos total-cards callback)
  (with-current-buffer buffer
    (cl-destructuring-bind (card-path . last-seen) card-path-and-date
      (let* ((w-width (window-body-width))
	     (w-height (window-body-height))
	     (card-sides (scpt/carteado/get-card-sides card-path))
	     (front (car card-sides))
	     (back (cdr card-sides))
	     (left-padding (/ w-width 6)))
	(erase-buffer)
	;; Write title - the category name
	(insert (make-string 2 ?\n))
	(insert (make-string left-padding ?\s))
	(insert (propertize category 'face
			    '(:foreground "#d3869b" :weight ultra-bold :height 1.5)))
	;; Write the front of the card
	(insert (make-string 2 ?\n))
	(insert (make-string left-padding ?\s))
	(insert (propertize "Front" 'face
			    '(:foreground "#b8bb26" :weight ultra-bold :height 1.1)))
	(dolist (txt front)
	  (insert (make-string 2 ?\n))
	  (insert (make-string left-padding ?\s))
	  (insert txt))
	;; TODO: make the actual engine work
	(setq already-pressed nil)
	(let ((show-back-and-buttons
	       (lambda () (interactive)
		 (unless already-pressed 
		   (setq already-pressed t)
		   (%scpt/carteado/show-back-and-buttons buffer card-path back left-padding callback)))))
	  (if (bound-and-true-p evil-mode)
	      (dolist (state '(normal insert))
		(evil-local-set-key state (kbd "RET") show-back-and-buttons))
	    (local-set-key (kbd "RET") show-back-and-buttons)))))))

(defun %scpt/carteado/show-back-and-buttons (buffer card-path back padding callback)
  (interactive)
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert (make-string 3 ?\n))
    (insert (make-string padding ?\s))
    (insert (propertize "Back" 'face
			'(:foreground "#b8bb26" :weight ultra-bold :height 1.1)))
    (dolist (txt back)
      (insert (make-string 2 ?\n))
      (insert (make-string padding ?\s))
      (insert txt))
    (insert (make-string 3 ?\n))
    (insert (make-string padding ?\s))
    (insert-button "Hard"
		   'type 'scpt/carteado/hard-button
		   'action (lambda (x)
			     (message "hard button clicked")
			     (funcall callback)))
    (insert (make-string 1 ?\n))
    (insert (make-string padding ?\s))
    (insert-button "Medium"
		   'type 'scpt/carteado/medium-button
		   'action (lambda (x)
			     (message "medium button clicked")
			     (funcall callback)))
    (setq medium-point (point))
    (insert (make-string 1 ?\n))
    (insert (make-string padding ?\s))
    (insert-button "Easy"
		   'type 'scpt/carteado/easy-button
		   'action (lambda (x)
			     (message "easy button clicked")
			     (funcall callback)))
    (when-let ((win (get-buffer-window buffer)))
      (set-window-point win medium-point))))

(defun scpt/carteado/get-cards-to-show (category)
  (let* ((today (format-time-string "%Y-%m-%d"))
	 (grep-command
	  (format "rg -PoN '#\\+SHOW_NEXT:.*\\K\\d{4}-\\d{2}-\\d{2}' /Users/arthur.rodrigues/Org/carteado/%s" category))
	 (cards (s-split "\n" (shell-command-to-string grep-command) t))
	 (acc nil))
    (dolist (card-grep cards (nreverse acc))
      (let* ((separator-idx (string-match ":" card-grep))
	     (show-next (substring card-grep 0 separator-idx))
	     (last-seen (substring card-grep (+ 1 separator-idx))))
	(when (and (or (string< last-seen today) (string= last-seen today))
		   (not (or (auto-save-file-name-p show-next) (backup-file-name-p show-next))))
	  (push (cons show-next last-seen) acc))))))

(defun scpt/carteado/get-card-sides (filepath)
  ;; Gets the contents of the headers tagged with :card_front: and :card_back:, respectively.
  ;; Returns a cons cell containing the resulting contents trimmed and in order.
  (let* ((file-string (with-temp-buffer
			(insert-file-contents filepath)
			(buffer-string)))
	 (lines (split-string file-string "\n")))
    (cl-labels
	((header-p (line &optional tag)
	   (and (string-search (or tag "") line)
		(string-match-p "^\\*+ " (s-trim line))))
	 (get-contents-of-header-by-tag (line lines tag inside-tag-p acc)
	   (cond
	    ((null line) acc)
	    (inside-tag-p
	     (if (header-p line)
		 acc
	       (get-contents-of-header-by-tag
		(car lines) (cdr lines) tag inside-tag-p
		(push line acc))))
	    ((header-p line tag)
	     (get-contents-of-header-by-tag
	      (car lines) (cdr lines) tag t acc))
	    (t (get-contents-of-header-by-tag
		(car lines) (cdr lines) tag inside-tag-p acc)))))
      (cons
       (->> (get-contents-of-header-by-tag
	     (car lines) (cdr lines) ":card_front:" nil nil)
	    (reverse)
	    (cl-remove-if (lambda (x) (string-empty-p (s-trim x)))))
       (->> (get-contents-of-header-by-tag
	      (car lines) (cdr lines) ":card_back:" nil nil)
	     (reverse)
	     (cl-remove-if (lambda (x) (string-empty-p (s-trim x)))))))))

(defun scpt/carteado/replace-text (start end replacement)
  (delete-region start end)
  (insert replacement))

(defun scpt/carteado/ensure-filepath (filepath)
  (if (file-directory-p filepath)
      t
    (make-directory filepath)))

(defun scpt/carteado/sanitize-directories (files)
  (cl-labels
      ((sanitize-aux (files sanitized) 
	 (if (null files)
	     (nreverse sanitized)
	   (let ((cur-file (car files))
		 (fullpath
		  (expand-file-name (car files) *scpt/carteado/home*)))
	     (if (file-directory-p fullpath)
		 (sanitize-aux (cdr files) (cons (cons cur-file fullpath) sanitized))
	       (sanitize-aux (cdr files) sanitized))))))
    (sanitize-aux
     (cl-remove-if (lambda (x)
		     (or
		      (string= "." x)
		      (string= ".." x)))
		   files)
     nil)))

(defun scpt/carteado/create-category ()
  (let* ((categories
	  (scpt/carteado/sanitize-directories
	   (directory-files *scpt/carteado/home*)))
	 (picked-cat
	  (completing-read "Select a category or create a new one:"
			   (mapcar #'car categories)
			   nil nil))
	 (cat-filepath
	  (or (cdar (cl-remove-if-not
		     (lambda (x) (string= picked-cat (car x)))
		     categories))
	      (expand-file-name picked-cat "/Users/arthur.rodrigues/Org/carteado/"))))
    (scpt/carteado/ensure-filepath cat-filepath)
    (cons picked-cat cat-filepath)))

(defun scpt/carteado/create-template ()
  (interactive)
  (let* ((category (scpt/carteado/create-category))
	 (template-filepath
	  (expand-file-name "template.org" (cdr category))))
    (find-file template-filepath)
    (insert-file-contents *scpt/carteado/base-template*)))

(defun scpt/carteado/create-card ()
  (interactive)
  (let* ((category (scpt/carteado/create-category))
	 (card (ensure-string
		(read-string "Enter the name of this card: ")
		"Card name cannot be empty."))
	 (card-filepath
	  (expand-file-name card (cdr category))))
    (find-file card-filepath)
    (unless (file-exists-p card-filepath) 
      (find-file card-filepath)
      (let ((category-template
	     (expand-file-name "template.org" (cdr category))))
	(if (file-exists-p category-template)
	    (insert-file-contents category-template))))))

(defun ensure-string (string errmsg)
  (unless (and string (not (string-empty-p string)))
    (error errmsg))
  string)









