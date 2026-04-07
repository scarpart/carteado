;;; carteado-sync.el --- Bidirectional sync of carteado cards with Anki -*- lexical-binding: t -*-

(require 'url)
(require 'json)
(require 'ox)
(require 'ox-html)
(require 'cl-lib)

;;; Layer 1 -- AnkiConnect HTTP transport

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
             (error "Cannot reach AnkiConnect at %s -- is Anki running? (%s)"
                    *scpt/carteado/sync/anki-url* (error-message-string err)))))
      (when response-buffer (kill-buffer response-buffer)))))

;;; Layer 2 -- AnkiConnect operation wrappers

(defun scpt/carteado/sync--ensure-model ()
  "Ensure the Carteado note model exists in Anki. Create it if missing."
  (let* ((models (scpt/carteado/sync--anki-request "modelNames"))
         (model-name *scpt/carteado/sync/model-name*))
    (unless (seq-contains-p models model-name #'string=)
      (scpt/carteado/sync--anki-request
       "createModel"
       `((modelName . ,model-name)
         (inOrderFields . ,(vector "Front" "Back" "OrgFront" "OrgBack" "SourceFile"))
         (cardTemplates . ,(vector
                            `((Name . "Card 1")
                              (Front . "{{Front}}")
                              (Back . "{{FrontSide}}<hr id=answer>{{Back}}"))))))
      (message "carteado: created Anki model '%s'" model-name))))

(defun scpt/carteado/sync--create-deck (deck-name)
  "Create DECK-NAME in Anki (idempotent). Returns deck ID."
  (scpt/carteado/sync--anki-request "createDeck" `((deck . ,deck-name))))

(defun scpt/carteado/sync--add-note (deck-name front-html back-html org-front org-back source-file)
  "Add a new note to DECK-NAME. Returns note ID.
Stores both HTML (for review) and org source (for sync) in the Carteado model."
  (scpt/carteado/sync--anki-request
   "addNote"
   `((note . ((deckName . ,deck-name)
              (modelName . ,*scpt/carteado/sync/model-name*)
              (fields . ((Front . ,front-html)
                         (Back . ,back-html)
                         (OrgFront . ,org-front)
                         (OrgBack . ,org-back)
                         (SourceFile . ,source-file)))
              (tags . ,(vector *scpt/carteado/sync/tag*)))))))

(defun scpt/carteado/sync--update-note (note-id front-html back-html org-front org-back)
  "Update fields of existing note NOTE-ID. Returns nil on success."
  (scpt/carteado/sync--anki-request
   "updateNoteFields"
   `((note . ((id . ,note-id)
              (fields . ((Front . ,front-html)
                         (Back . ,back-html)
                         (OrgFront . ,org-front)
                         (OrgBack . ,org-back))))))))

(defun scpt/carteado/sync--get-note-info (note-id)
  "Get full info for NOTE-ID. Returns alist with fields, mod, cards, etc."
  (let ((results (scpt/carteado/sync--anki-request
                  "notesInfo" `((notes . ,(vector note-id))))))
    (when (and results (> (length results) 0))
      (aref results 0))))

(defun scpt/carteado/sync--get-note-mod (note-id)
  "Get the mod timestamp for NOTE-ID from Anki."
  (let ((info (scpt/carteado/sync--get-note-info note-id)))
    (when info (alist-get 'mod info))))

;;; Layer 3 -- Content conversion

(defun scpt/carteado/sync--org-lines-to-html (lines)
  "Convert a list of org-mode text LINES to body-only HTML.
Handles LaTeX fragments via org-export's MathJax support.
After export, uploads any images to Anki and rewrites src paths."
  (let ((org-export-show-temporary-export-buffer nil)
        (org-export-with-toc nil)
        (org-export-with-section-numbers nil)
        (org-html-with-latex 'mathjax))
    (let ((html (s-trim (org-export-string-as (string-join lines "\n") 'html t))))
      (scpt/carteado/sync--process-images html))))

(defun scpt/carteado/sync--process-images (html)
  "Find <img> tags in HTML, upload images to Anki, rewrite src to filenames."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (re-search-forward "<img[^>]+src=\"\\([^\"]+\\)\"" nil t)
      (let* ((src (match-string 1))
             (mbeg (match-beginning 0))
             (mend (match-end 0))
             (path (replace-regexp-in-string "\\`file:/+" "/" src))
             (expanded (expand-file-name path))
             (filename (file-name-nondirectory expanded)))
        (when (file-exists-p expanded)
          (scpt/carteado/sync--upload-media expanded filename)
          (goto-char mbeg)
          (delete-region mbeg mend)
          (insert (concat "<img src=\"" filename "\"")))))
    (buffer-string)))

(defun scpt/carteado/sync--upload-media (filepath filename)
  "Upload FILEPATH to Anki's media collection as FILENAME."
  (let* ((data (with-temp-buffer
                 (insert-file-contents-literally filepath)
                 (base64-encode-region (point-min) (point-max))
                 (buffer-string))))
    (scpt/carteado/sync--anki-request
     "storeMediaFile"
     `((filename . ,filename)
       (data . ,data)))))

(defun scpt/carteado/sync--content-hash (front-html back-html)
  "Return MD5 hash of FRONT-HTML and BACK-HTML for change detection."
  (md5 (concat front-html "---" back-html)))

(defun scpt/carteado/sync--html-to-plain (html)
  "Best-effort conversion of HTML to plain text for org files.
Used as fallback when OrgFront/OrgBack fields are empty (e.g. Basic notes)."
  (with-temp-buffer
    (insert html)
    ;; Convert common HTML to org-ish equivalents
    (goto-char (point-min))
    (while (re-search-forward "<br\\s*/?>" nil t) (replace-match "\n"))
    (goto-char (point-min))
    (while (re-search-forward "<p>\\(\\(.\\|\n\\)*?\\)</p>" nil t)
      (replace-match "\\1\n"))
    (goto-char (point-min))
    (while (re-search-forward "<b>\\(\\(.\\|\n\\)*?\\)</b>" nil t)
      (replace-match "*\\1*"))
    (goto-char (point-min))
    (while (re-search-forward "<i>\\(\\(.\\|\n\\)*?\\)</i>" nil t)
      (replace-match "/\\1/"))
    (goto-char (point-min))
    (while (re-search-forward "<code>\\(\\(.\\|\n\\)*?\\)</code>" nil t)
      (replace-match "~\\1~"))
    (goto-char (point-min))
    (while (re-search-forward "<li>\\(\\(.\\|\n\\)*?\\)</li>" nil t)
      (replace-match "- \\1"))
    ;; Strip remaining tags
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t) (replace-match ""))
    ;; Clean up entities
    (goto-char (point-min))
    (while (re-search-forward "&amp;" nil t) (replace-match "&"))
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t) (replace-match "<"))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t) (replace-match ">"))
    (goto-char (point-min))
    (while (re-search-forward "&nbsp;" nil t) (replace-match " "))
    (s-trim (buffer-string))))

;;; Layer 4 -- State persistence

(defun scpt/carteado/sync--read-state ()
  "Read sync state from file. Returns alist or nil if missing/corrupt.
State format: ((filepath . (note-id local-hash anki-mod)) ...)
Also handles old format: ((filepath . (note-id . content-hash)) ...)"
  (when (file-exists-p *scpt/carteado/sync/state-file*)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents *scpt/carteado/sync/state-file*)
          (let ((data (read (current-buffer))))
            (when (listp data)
              ;; Migrate old format entries
              (mapcar #'scpt/carteado/sync--migrate-state-entry data))))
      (error
       (message "carteado: warning -- sync state file corrupt, starting fresh")
       nil))))

(defun scpt/carteado/sync--migrate-state-entry (entry)
  "Migrate a state ENTRY from old format to new if needed.
Old: (filepath . (note-id . hash-string))
New: (filepath . (note-id local-hash anki-mod))"
  (let* ((filepath (car entry))
         (val (cdr entry)))
    (if (and (consp val) (stringp (cdr val)))
        ;; Old format: (note-id . hash) -- migrate with nil anki-mod
        (cons filepath (list (car val) (cdr val) nil))
      ;; New format already: (note-id local-hash anki-mod)
      entry)))

(defun scpt/carteado/sync--write-state (state)
  "Write STATE alist to the sync state file."
  (with-temp-file *scpt/carteado/sync/state-file*
    (insert ";; carteado sync state -- do not edit\n")
    (prin1 state (current-buffer))
    (insert "\n")))

(defun scpt/carteado/sync--state-get (state filepath)
  "Look up FILEPATH in STATE. Returns (note-id local-hash anki-mod) or nil."
  (cdr (assoc filepath state)))

(defun scpt/carteado/sync--state-put (state filepath note-id local-hash anki-mod)
  "Return new STATE with FILEPATH mapped to (NOTE-ID LOCAL-HASH ANKI-MOD)."
  (cons (cons filepath (list note-id local-hash anki-mod))
        (cl-remove-if (lambda (entry) (string= filepath (car entry))) state)))

(defun scpt/carteado/sync--state-find-by-note-id (state note-id)
  "Find entry in STATE by NOTE-ID. Returns (filepath note-id local-hash anki-mod) or nil."
  (cl-find-if (lambda (entry)
                (equal note-id (nth 0 (cdr entry))))
              state))

;;; Layer 5 -- File discovery

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

(defun scpt/carteado/sync--filepath-to-source (filepath)
  "Convert FILEPATH to a relative source path for the SourceFile field.
E.g. /home/user/Org/carteado/edo/card.org -> \"edo/card.org\""
  (file-relative-name filepath *scpt/carteado/home*))

(defun scpt/carteado/sync--deck-to-dirpath (deck-name)
  "Convert Anki DECK-NAME to a directory path relative to carteado home.
E.g. \"carteado::edo\" -> \"edo/\", \"carteado\" -> \"\""
  (let ((prefix *scpt/carteado/sync/deck-prefix*))
    (if (string= deck-name prefix)
        ""
      (let ((suffix (string-remove-prefix (concat prefix "::") deck-name)))
        (concat (replace-regexp-in-string "::" "/" suffix) "/")))))

;;; Layer 6 -- Push: single-card sync (Org -> Anki)

(defun scpt/carteado/sync--sync-one (filepath state)
  "Sync one card at FILEPATH against STATE (push direction).
Returns (new-state . action) where action is created, updated, skipped, or conflict-local-wins."
  (let* ((sides (scpt/carteado/get-card-sides filepath))
         (front-lines (car sides))
         (back-lines (cdr sides)))
    (if (or (null front-lines) (null back-lines))
        (progn
          (message "carteado: skipping %s (empty front or back)" filepath)
          (cons state 'skipped))
      (let* ((org-front (string-join front-lines "\n"))
             (org-back (string-join back-lines "\n"))
             (front-html (scpt/carteado/sync--org-lines-to-html front-lines))
             (back-html (scpt/carteado/sync--org-lines-to-html back-lines))
             (hash (scpt/carteado/sync--content-hash front-html back-html))
             (source-file (scpt/carteado/sync--filepath-to-source filepath))
             (existing (scpt/carteado/sync--state-get state filepath))
             (existing-id (nth 0 existing))
             (existing-hash (nth 1 existing))
             (existing-mod (nth 2 existing)))
        (cond
         ;; New card -- not in state
         ((null existing)
          (let* ((deck (scpt/carteado/sync--filepath-to-deck filepath))
                 (_ (scpt/carteado/sync--create-deck deck))
                 (note-id (scpt/carteado/sync--add-note
                           deck front-html back-html org-front org-back source-file))
                 (anki-mod (scpt/carteado/sync--get-note-mod note-id)))
            (cons (scpt/carteado/sync--state-put state filepath note-id hash anki-mod)
                  'created)))
         ;; Unchanged locally -- hash matches
         ((string= hash existing-hash)
          (cons state 'skipped))
         ;; Local changed -- check if Anki also changed (conflict)
         (t
          (let* ((anki-mod-now (condition-case nil
                                   (scpt/carteado/sync--get-note-mod existing-id)
                                 (error nil)))
                 (anki-changed (and existing-mod anki-mod-now
                                    (not (equal anki-mod-now existing-mod)))))
            (if anki-changed
                ;; Conflict: both sides changed -- latest writer wins
                (let ((local-mtime (float-time
                                    (file-attribute-modification-time
                                     (file-attributes filepath)))))
                  (if (> local-mtime anki-mod-now)
                      ;; Local is newer -- push
                      (progn
                        (message "carteado: conflict on %s -- local wins (newer)" source-file)
                        (scpt/carteado/sync--do-push
                         filepath state existing-id front-html back-html
                         org-front org-back hash source-file))
                    ;; Anki is newer -- skip push, pull will handle it
                    (progn
                      (message "carteado: conflict on %s -- anki wins (newer), skipping push" source-file)
                      (cons state 'skipped))))
              ;; Only local changed -- push
              (scpt/carteado/sync--do-push
               filepath state existing-id front-html back-html
               org-front org-back hash source-file)))))))))

(defun scpt/carteado/sync--do-push (filepath state note-id front-html back-html
                                              org-front org-back hash source-file)
  "Push local changes to Anki for FILEPATH. Returns (new-state . action)."
  (condition-case nil
      (progn
        (scpt/carteado/sync--update-note note-id front-html back-html org-front org-back)
        (let ((anki-mod (scpt/carteado/sync--get-note-mod note-id)))
          (cons (scpt/carteado/sync--state-put state filepath note-id hash anki-mod)
                'updated)))
    ;; Stale note ID -- re-create
    (error
     (let* ((deck (scpt/carteado/sync--filepath-to-deck filepath))
            (_ (scpt/carteado/sync--create-deck deck))
            (new-id (scpt/carteado/sync--add-note
                     deck front-html back-html org-front org-back source-file))
            (anki-mod (scpt/carteado/sync--get-note-mod new-id)))
       (message "carteado: re-created %s (stale note ID)" filepath)
       (cons (scpt/carteado/sync--state-put state filepath new-id hash anki-mod)
             'created)))))

;;; Layer 7 -- Pull: Anki -> Org

(defun scpt/carteado/sync--fetch-anki-notes ()
  "Fetch all carteado-tagged notes from Anki.
Returns list of alists, each with: note-id, fields, mod, deck-name."
  (let* ((note-ids (scpt/carteado/sync--anki-request
                    "findNotes"
                    `((query . ,(format "tag:%s" *scpt/carteado/sync/tag*)))))
         (results nil))
    (when (and note-ids (> (length note-ids) 0))
      ;; Fetch note info in batches of 100
      (let ((id-list (append note-ids nil))) ; vector -> list
        (while id-list
          (let* ((batch (seq-take id-list 100))
                 (batch-vec (vconcat batch))
                 (infos (scpt/carteado/sync--anki-request
                         "notesInfo" `((notes . ,batch-vec)))))
            (dotimes (i (length infos))
              (let* ((info (aref infos i))
                     (note-id (alist-get 'noteId info))
                     (cards (alist-get 'cards info))
                     ;; Get deck name from first card
                     (deck-name
                      (when (and cards (> (length cards) 0))
                        (let ((card-info (scpt/carteado/sync--anki-request
                                          "cardsInfo"
                                          `((cards . ,(vector (aref cards 0)))))))
                          (when (and card-info (> (length card-info) 0))
                            (alist-get 'deckName (aref card-info 0)))))))
                (push `((note-id . ,note-id)
                        (fields . ,(alist-get 'fields info))
                        (mod . ,(alist-get 'mod info))
                        (deck-name . ,deck-name)
                        (model . ,(alist-get 'modelName info))
                        (tags . ,(alist-get 'tags info)))
                      results)))
            (setq id-list (seq-drop id-list 100))))))
    (nreverse results)))

(defun scpt/carteado/sync--extract-field-value (fields field-name)
  "Extract the value of FIELD-NAME from Anki FIELDS alist."
  (let ((field (alist-get (intern field-name) fields)))
    (when field
      (let ((val (alist-get 'value field)))
        (when (and val (not (string-empty-p val)))
          val)))))

(defun scpt/carteado/sync--note-to-org-content (note)
  "Extract org front/back content from an Anki NOTE.
Prefers OrgFront/OrgBack fields; falls back to HTML stripping."
  (let* ((fields (alist-get 'fields note))
         (org-front (scpt/carteado/sync--extract-field-value fields "OrgFront"))
         (org-back (scpt/carteado/sync--extract-field-value fields "OrgBack")))
    (if (and org-front org-back)
        (cons org-front org-back)
      ;; Fallback: strip HTML from Front/Back
      (let ((front-html (or (scpt/carteado/sync--extract-field-value fields "Front") ""))
            (back-html (or (scpt/carteado/sync--extract-field-value fields "Back") "")))
        (cons (scpt/carteado/sync--html-to-plain front-html)
              (scpt/carteado/sync--html-to-plain back-html))))))

(defun scpt/carteado/sync--note-source-file (note)
  "Get the SourceFile from NOTE, or generate one from content."
  (let ((source (scpt/carteado/sync--extract-field-value
                 (alist-get 'fields note) "SourceFile")))
    (if source
        source
      ;; Generate filename from front content
      (let* ((front-html (or (scpt/carteado/sync--extract-field-value
                              (alist-get 'fields note) "Front") ""))
             (plain (scpt/carteado/sync--html-to-plain front-html))
             (sanitized (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_"
                                                  (substring plain 0 (min 40 (length plain)))))
             (deck (or (alist-get 'deck-name note) *scpt/carteado/sync/deck-prefix*))
             (dir (scpt/carteado/sync--deck-to-dirpath deck)))
        (concat dir sanitized ".org")))))

(defun scpt/carteado/sync--write-org-file (filepath org-front org-back card-name)
  "Write a new org card file at FILEPATH with ORG-FRONT and ORG-BACK content."
  (let ((dir (file-name-directory filepath)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (with-temp-file filepath
      (let ((today (format-time-string "<%Y-%m-%d %a>")))
        (insert (format "#+DATE: %s\n" today))
        (insert (format "#+LAST_SEEN: %s\n" today))
        (insert (format "#+SHOW_NEXT: %s\n" today))
        (insert "\n")
        (insert (format "* %s\n" card-name))
        (insert "** Front                                                       :card_front:\n")
        (dolist (line (split-string org-front "\n"))
          (insert (format "   %s\n" line)))
        (insert "\n")
        (insert "** Back                                                        :card_back:\n")
        (dolist (line (split-string org-back "\n"))
          (insert (format "   %s\n" line)))))))

(defun scpt/carteado/sync--update-org-file (filepath org-front org-back)
  "Update an existing org card at FILEPATH with new ORG-FRONT and ORG-BACK.
Preserves metadata headers, replaces card content."
  (let* ((file-string (with-temp-buffer
                        (insert-file-contents filepath)
                        (buffer-string)))
         (lines (split-string file-string "\n"))
         (metadata-lines nil)
         (found-content nil))
    ;; Collect metadata lines (#+KEY: value) and the first heading
    (dolist (line lines)
      (if (or (string-match-p "^#\\+" line)
              (string-empty-p line))
          (unless found-content
            (push line metadata-lines))
        (setq found-content t)))
    (with-temp-file filepath
      ;; Re-emit metadata
      (dolist (line (nreverse metadata-lines))
        (insert line "\n"))
      ;; Find original card name from file
      (let ((card-name (file-name-sans-extension (file-name-nondirectory filepath))))
        (insert (format "* %s\n" card-name))
        (insert "** Front                                                       :card_front:\n")
        (dolist (line (split-string org-front "\n"))
          (insert (format "   %s\n" line)))
        (insert "\n")
        (insert "** Back                                                        :card_back:\n")
        (dolist (line (split-string org-back "\n"))
          (insert (format "   %s\n" line)))))))

(defun scpt/carteado/sync--pull-notes (state)
  "Pull carteado-tagged notes from Anki, creating/updating local org files.
Returns (new-state pulled-count updated-count skipped-count conflict-count)."
  (let ((anki-notes (scpt/carteado/sync--fetch-anki-notes))
        (pulled 0) (updated 0) (skipped 0) (conflicts 0))
    (dolist (note anki-notes)
      (let* ((note-id (alist-get 'note-id note))
             (anki-mod (alist-get 'mod note))
             (source-file (scpt/carteado/sync--note-source-file note))
             (filepath (expand-file-name source-file *scpt/carteado/home*))
             (existing (scpt/carteado/sync--state-find-by-note-id state note-id))
             (existing-filepath (car existing))
             (existing-data (cdr existing))
             (existing-hash (nth 1 existing-data))
             (existing-mod (nth 2 existing-data)))
        (cond
         ;; Unknown note -- not in state at all -> create org file
         ((null existing)
          (if (file-exists-p filepath)
              ;; File exists but not in state (maybe state was reset) -- skip to avoid overwrite
              (progn
                (message "carteado: pull -- file exists but not in state, skipping %s" source-file)
                (setq skipped (1+ skipped)))
            (let ((content (scpt/carteado/sync--note-to-org-content note))
                  (card-name (file-name-sans-extension
                              (file-name-nondirectory source-file))))
              (scpt/carteado/sync--write-org-file filepath (car content) (cdr content) card-name)
              ;; Compute hash of what we just wrote, to store in state
              (let* ((front-html (scpt/carteado/sync--org-lines-to-html
                                  (split-string (car content) "\n")))
                     (back-html (scpt/carteado/sync--org-lines-to-html
                                 (split-string (cdr content) "\n")))
                     (hash (scpt/carteado/sync--content-hash front-html back-html)))
                (setq state (scpt/carteado/sync--state-put
                             state filepath note-id hash anki-mod)))
              (message "carteado: pulled new card %s" source-file)
              (setq pulled (1+ pulled)))))

         ;; Known note -- check if Anki side changed
         (t
          (if (and existing-mod (equal anki-mod existing-mod))
              ;; Anki hasn't changed since last sync
              (setq skipped (1+ skipped))
            ;; Anki changed -- check local side too
            (let* ((use-filepath (or existing-filepath filepath))
                   (local-changed
                    (when (file-exists-p use-filepath)
                      (let* ((sides (scpt/carteado/get-card-sides use-filepath))
                             (front-lines (car sides))
                             (back-lines (cdr sides)))
                        (when (and front-lines back-lines)
                          (let* ((fh (scpt/carteado/sync--org-lines-to-html front-lines))
                                 (bh (scpt/carteado/sync--org-lines-to-html back-lines))
                                 (cur-hash (scpt/carteado/sync--content-hash fh bh)))
                            (not (string= cur-hash existing-hash))))))))
              (if local-changed
                  ;; Both sides changed -- latest writer wins
                  (let ((local-mtime
                         (when (file-exists-p use-filepath)
                           (float-time
                            (file-attribute-modification-time
                             (file-attributes use-filepath))))))
                    (if (and local-mtime (> local-mtime anki-mod))
                        ;; Local is newer -- skip pull, push will handle it
                        (progn
                          (message "carteado: conflict on %s -- local wins (newer), skipping pull"
                                   source-file)
                          (setq conflicts (1+ conflicts)))
                      ;; Anki is newer -- overwrite local
                      (let ((content (scpt/carteado/sync--note-to-org-content note)))
                        (scpt/carteado/sync--update-org-file
                         use-filepath (car content) (cdr content))
                        (let* ((front-html (scpt/carteado/sync--org-lines-to-html
                                            (split-string (car content) "\n")))
                               (back-html (scpt/carteado/sync--org-lines-to-html
                                           (split-string (cdr content) "\n")))
                               (hash (scpt/carteado/sync--content-hash front-html back-html)))
                          (setq state (scpt/carteado/sync--state-put
                                       state use-filepath note-id hash anki-mod)))
                        (message "carteado: conflict on %s -- anki wins (newer), pulled"
                                 source-file)
                        (setq conflicts (1+ conflicts)))))
                ;; Only Anki changed -- pull
                (let ((content (scpt/carteado/sync--note-to-org-content note)))
                  (if (file-exists-p use-filepath)
                      (scpt/carteado/sync--update-org-file
                       use-filepath (car content) (cdr content))
                    (let ((card-name (file-name-sans-extension
                                      (file-name-nondirectory source-file))))
                      (scpt/carteado/sync--write-org-file
                       use-filepath (car content) (cdr content) card-name)))
                  (let* ((front-html (scpt/carteado/sync--org-lines-to-html
                                      (split-string (car content) "\n")))
                         (back-html (scpt/carteado/sync--org-lines-to-html
                                     (split-string (cdr content) "\n")))
                         (hash (scpt/carteado/sync--content-hash front-html back-html)))
                    (setq state (scpt/carteado/sync--state-put
                                 state use-filepath note-id hash anki-mod)))
                  (message "carteado: pulled update for %s" source-file)
                  (setq updated (1+ updated))))))))))
    (list state pulled updated skipped conflicts)))

;;; Layer 8 -- Interactive commands

(defun scpt/carteado/sync ()
  "Bidirectional sync of carteado org-mode flashcards with Anki.
Pull-before-push: remote changes are pulled first, then local changes pushed."
  (interactive)
  (message "carteado: starting bidirectional sync...")

  ;; Ensure model exists
  (scpt/carteado/sync--ensure-model)

  (let ((state (scpt/carteado/sync--read-state)))

    ;; Phase 1: PULL (Anki -> Org)
    (message "carteado: pulling from Anki...")
    (let ((pull-result (scpt/carteado/sync--pull-notes state)))
      (setq state (nth 0 pull-result))
      (let ((pull-new (nth 1 pull-result))
            (pull-updated (nth 2 pull-result))
            (pull-skipped (nth 3 pull-result))
            (pull-conflicts (nth 4 pull-result)))
        (message "carteado: pull done. %d new, %d updated, %d skipped, %d conflicts."
                 pull-new pull-updated pull-skipped pull-conflicts))

      ;; Phase 2: PUSH (Org -> Anki)
      (message "carteado: pushing to Anki...")
      (let* ((files (scpt/carteado/sync--list-card-files))
             (total (length files))
             (created 0) (updated 0) (skipped 0) (errors 0)
             (idx 0))
        (dolist (f files)
          (setq idx (1+ idx))
          (message "carteado: pushing %d/%d -- %s" idx total (file-name-nondirectory f))
          (condition-case err
              (let ((result (scpt/carteado/sync--sync-one f state)))
                (setq state (car result))
                (cl-case (cdr result)
                  (created (setq created (1+ created)))
                  (updated (setq updated (1+ updated)))
                  (skipped (setq skipped (1+ skipped)))))
            (error
             (setq errors (1+ errors))
             (message "carteado: error pushing %s: %s" f (error-message-string err)))))

        (scpt/carteado/sync--write-state state)
        (message "carteado: push done. %d created, %d updated, %d skipped, %d errors."
                 created updated skipped errors)))

    ))

(defun scpt/carteado/sync-file ()
  "Sync the current buffer's org card to Anki (push only)."
  (interactive)
  (scpt/carteado/sync--ensure-model)
  (let* ((filepath (buffer-file-name))
         (state (scpt/carteado/sync--read-state)))
    (unless filepath
      (error "Buffer is not visiting a file"))
    (let ((result (scpt/carteado/sync--sync-one filepath state)))
      (scpt/carteado/sync--write-state (car result))
      (message "carteado: %s -- %s" (file-name-nondirectory filepath) (cdr result)))))

(defun scpt/carteado/sync-reset ()
  "Delete the sync state file. Next sync will re-create all cards."
  (interactive)
  (when (y-or-n-p "Delete sync state? All cards will be re-created on next sync. ")
    (when (file-exists-p *scpt/carteado/sync/state-file*)
      (delete-file *scpt/carteado/sync/state-file*))
    (message "carteado: sync state deleted.")))

(defun scpt/carteado/sync-pull ()
  "Pull-only sync: fetch carteado notes from Anki, create/update local org files."
  (interactive)
  (message "carteado: pull-only sync starting...")
  (scpt/carteado/sync--ensure-model)
  (let* ((state (scpt/carteado/sync--read-state))
         (result (scpt/carteado/sync--pull-notes state)))
    (scpt/carteado/sync--write-state (nth 0 result))
    (message "carteado: pull done. %d new, %d updated, %d skipped, %d conflicts."
             (nth 1 result) (nth 2 result) (nth 3 result) (nth 4 result))))
