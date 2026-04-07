;;; carteado-sync-config.el --- Configuration for Anki sync -*- lexical-binding: t -*-

(defvar *scpt/carteado/sync/anki-url* "http://localhost:8765"
  "URL of the local AnkiConnect API.")

(defvar *scpt/carteado/sync/anki-key* nil
  "Optional API key for AnkiConnect. nil means no key.")

(defvar *scpt/carteado/sync/note-type* "Basic"
  "Anki note type (model) to use. \"Basic\" has Front and Back fields.")

(defvar *scpt/carteado/sync/deck-prefix* "carteado"
  "Top-level Anki deck name. Categories become sub-decks: carteado::edo.")

(defvar *scpt/carteado/sync/state-file*
  (expand-file-name ".anki-sync-state" *scpt/carteado/home*)
  "Path to the sync state file.")

(defvar *scpt/carteado/sync/tag* "carteado"
  "Tag applied to all notes created by carteado in Anki.")

(defvar *scpt/carteado/sync/model-name* "Carteado"
  "Custom Anki note type for bidirectional sync.
Has fields: Front, Back, OrgFront, OrgBack, SourceFile.")
