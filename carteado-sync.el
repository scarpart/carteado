;;; carteado-sync.el --- One-way sync of carteado cards to Anki -*- lexical-binding: t -*-

(require 'url)
(require 'json)
(require 'ox)
(require 'ox-html)
(require 'cl-lib)

;;; Layer 1 — AnkiConnect HTTP transport

(defun scpt/carteado/sync--anki-request (action &optional params)
  "Send ACTION with PARAMS to AnkiConnect. Returns the `result' field.
Signals an error if AnkiConnect returns a non-nil error or HTTP fails."
  (let* ((payload `((action . ,action)
                    (version . 6)
                    ,@(when params `((params . ,params)))
                    ,@(when *scpt/carteado/sync/anki-key*
                        `((key . ,*scpt/carteado/sync/anki-key*)))))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (response-buffer nil))
    (unwind-protect
        (condition-case err
            (progn
              (setq response-buffer (url-retrieve-synchronously
                                     *scpt/carteado/sync/anki-url* t nil 30))
              (unless response-buffer
                (error "No response from AnkiConnect"))
              (with-current-buffer response-buffer
                (goto-char (point-min))
                (re-search-forward "\n\n")
                (let* ((json-object-type 'alist)
                       (json-array-type 'vector)
                       (json-key-type 'symbol)
                       (resp (json-read))
                       (err-field (alist-get 'error resp))
                       (result (alist-get 'result resp)))
                  (when err-field
                    (error "AnkiConnect %s: %s" action err-field))
                  result)))
          (error
           (if (string-match-p "AnkiConnect" (error-message-string err))
               (signal (car err) (cdr err))
             (error "Cannot reach AnkiConnect at %s — is Anki running? (%s)"
                    *scpt/carteado/sync/anki-url* (error-message-string err)))))
      (when response-buffer (kill-buffer response-buffer)))))

;;; Layer 2 — AnkiConnect operation wrappers

(defun scpt/carteado/sync--create-deck (deck-name)
  "Create DECK-NAME in Anki (idempotent). Returns deck ID."
  (scpt/carteado/sync--anki-request "createDeck" `((deck . ,deck-name))))

(defun scpt/carteado/sync--add-note (deck-name front-html back-html)
  "Add a new note to DECK-NAME with FRONT-HTML and BACK-HTML. Returns note ID."
  (scpt/carteado/sync--anki-request
   "addNote"
   `((note . ((deckName . ,deck-name)
              (modelName . ,*scpt/carteado/sync/note-type*)
              (fields . ((Front . ,front-html)
                         (Back . ,back-html)))
              (tags . ,(vector *scpt/carteado/sync/tag*)))))))

(defun scpt/carteado/sync--update-note (note-id front-html back-html)
  "Update fields of existing note NOTE-ID. Returns nil on success."
  (scpt/carteado/sync--anki-request
   "updateNoteFields"
   `((note . ((id . ,note-id)
              (fields . ((Front . ,front-html)
                         (Back . ,back-html))))))))

(defun scpt/carteado/sync--trigger-sync ()
  "Trigger Anki's built-in sync to remote server."
  (scpt/carteado/sync--anki-request "sync"))

;;; Layer 3 — Content conversion

(defun scpt/carteado/sync--org-lines-to-html (lines)
  "Convert a list of org-mode text LINES to body-only HTML.
Handles LaTeX fragments via org-export's MathJax support."
  (let ((org-export-show-temporary-export-buffer nil)
        (org-export-with-toc nil)
        (org-export-with-section-numbers nil)
        (org-html-with-latex 'mathjax))
    (s-trim (org-export-string-as (string-join lines "\n") 'html t))))

(defun scpt/carteado/sync--content-hash (front-html back-html)
  "Return MD5 hash of FRONT-HTML and BACK-HTML for change detection."
  (md5 (concat front-html "---" back-html)))

;;; Layer 4 — State persistence

(defun scpt/carteado/sync--read-state ()
  "Read sync state from file. Returns alist or nil if missing/corrupt.
State format: ((filepath note-id . content-hash) ...)"
  (when (file-exists-p *scpt/carteado/sync/state-file*)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents *scpt/carteado/sync/state-file*)
          (let ((data (read (current-buffer))))
            (when (listp data) data)))
      (error
       (message "carteado: warning — sync state file corrupt, starting fresh")
       nil))))

(defun scpt/carteado/sync--write-state (state)
  "Write STATE alist to the sync state file."
  (with-temp-file *scpt/carteado/sync/state-file*
    (insert ";; carteado sync state — do not edit\n")
    (prin1 state (current-buffer))
    (insert "\n")))

(defun scpt/carteado/sync--state-get (state filepath)
  "Look up FILEPATH in STATE. Returns (note-id . hash) or nil."
  (cdr (assoc filepath state)))

(defun scpt/carteado/sync--state-put (state filepath note-id hash)
  "Return new STATE with FILEPATH mapped to (NOTE-ID . HASH)."
  (cons (cons filepath (cons note-id hash))
        (cl-remove-if (lambda (entry) (string= filepath (car entry))) state)))

;;; Layer 5 — File discovery

(defun scpt/carteado/sync--list-card-files ()
  "Return all .org card files under `*scpt/carteado/home*'.
Excludes templates, autosave files, and backup files."
  (cl-remove-if
   (lambda (f)
     (let ((name (file-name-nondirectory f)))
       (or (string= name "template.org")
           (auto-save-file-name-p name)
           (backup-file-name-p f))))
   (directory-files-recursively *scpt/carteado/home* "\\.org\\'")))

(defun scpt/carteado/sync--filepath-to-deck (filepath)
  "Convert FILEPATH to an Anki deck name.
E.g. ~/Org/carteado/edo/card.org -> \"carteado::edo\""
  (let* ((rel-dir (directory-file-name
                   (file-relative-name (file-name-directory filepath)
                                       *scpt/carteado/home*)))
         (deck-suffix (if (or (string= rel-dir ".") (string= rel-dir ""))
                          nil
                        (replace-regexp-in-string "/" "::" rel-dir))))
    (if deck-suffix
        (concat *scpt/carteado/sync/deck-prefix* "::" deck-suffix)
      *scpt/carteado/sync/deck-prefix*)))

;;; Layer 6 — Single-card sync

(defun scpt/carteado/sync--sync-one (filepath state)
  "Sync one card at FILEPATH against STATE.
Returns (new-state . action) where action is created, updated, or skipped."
  (let* ((sides (scpt/carteado/get-card-sides filepath))
         (front-lines (car sides))
         (back-lines (cdr sides)))
    (if (or (null front-lines) (null back-lines))
        (progn
          (message "carteado: skipping %s (empty front or back)" filepath)
          (cons state 'skipped))
      (let* ((front-html (scpt/carteado/sync--org-lines-to-html front-lines))
             (back-html (scpt/carteado/sync--org-lines-to-html back-lines))
             (hash (scpt/carteado/sync--content-hash front-html back-html))
             (existing (scpt/carteado/sync--state-get state filepath)))
        (cond
         ;; New card — not in state
         ((null existing)
          (let* ((deck (scpt/carteado/sync--filepath-to-deck filepath))
                 (_ (scpt/carteado/sync--create-deck deck))
                 (note-id (scpt/carteado/sync--add-note deck front-html back-html)))
            (cons (scpt/carteado/sync--state-put state filepath note-id hash)
                  'created)))
         ;; Unchanged — hash matches
         ((string= hash (cdr existing))
          (cons state 'skipped))
         ;; Changed — update existing note
         (t
          (let ((note-id (car existing)))
            (condition-case nil
                (progn
                  (scpt/carteado/sync--update-note note-id front-html back-html)
                  (cons (scpt/carteado/sync--state-put state filepath note-id hash)
                        'updated))
              ;; Stale note ID — re-create
              (error
               (let* ((deck (scpt/carteado/sync--filepath-to-deck filepath))
                      (_ (scpt/carteado/sync--create-deck deck))
                      (new-id (scpt/carteado/sync--add-note deck front-html back-html)))
                 (message "carteado: re-created %s (stale note ID)" filepath)
                 (cons (scpt/carteado/sync--state-put state filepath new-id hash)
                       'created)))))))))))

;;; Layer 7 — Interactive commands

(defun scpt/carteado/sync ()
  "Sync all carteado org-mode flashcards to Anki via AnkiConnect."
  (interactive)
  (message "carteado: starting Anki sync...")
  (let* ((files (scpt/carteado/sync--list-card-files))
         (state (scpt/carteado/sync--read-state))
         (total (length files))
         (created 0) (updated 0) (skipped 0) (errors 0)
         (idx 0))
    (dolist (f files)
      (setq idx (1+ idx))
      (message "carteado: syncing %d/%d — %s" idx total (file-name-nondirectory f))
      (condition-case err
          (let ((result (scpt/carteado/sync--sync-one f state)))
            (setq state (car result))
            (cl-case (cdr result)
              (created (setq created (1+ created)))
              (updated (setq updated (1+ updated)))
              (skipped (setq skipped (1+ skipped)))))
        (error
         (setq errors (1+ errors))
         (message "carteado: error syncing %s: %s" f (error-message-string err)))))
    (scpt/carteado/sync--write-state state)
    (message "carteado: sync complete. %d created, %d updated, %d skipped, %d errors."
             created updated skipped errors)
    (when (y-or-n-p "Trigger Anki remote sync? ")
      (condition-case err
          (progn (scpt/carteado/sync--trigger-sync)
                 (message "carteado: remote sync triggered."))
        (error (message "carteado: remote sync failed: %s" (error-message-string err)))))))

(defun scpt/carteado/sync-file ()
  "Sync the current buffer's org card to Anki."
  (interactive)
  (let* ((filepath (buffer-file-name))
         (state (scpt/carteado/sync--read-state)))
    (unless filepath
      (error "Buffer is not visiting a file"))
    (let ((result (scpt/carteado/sync--sync-one filepath state)))
      (scpt/carteado/sync--write-state (car result))
      (message "carteado: %s — %s" (file-name-nondirectory filepath) (cdr result)))))

(defun scpt/carteado/sync-reset ()
  "Delete the sync state file. Next sync will re-create all cards."
  (interactive)
  (when (y-or-n-p "Delete sync state? All cards will be re-created on next sync. ")
    (when (file-exists-p *scpt/carteado/sync/state-file*)
      (delete-file *scpt/carteado/sync/state-file*))
    (message "carteado: sync state deleted.")))
