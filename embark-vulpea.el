;;; embark-vulpea.el --- Embark actions and export for Vulpea notes -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fabrizio Contigiani

;; Author: Fabrizio Contigiani <fabcontigiani@gmail.com>
;; Maintainer: Fabrizio Contigiani <fabcontigiani@gmail.com>
;; URL: https://github.com/fabcontigiani/embark-vulpea
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2") (vulpea "2.0.0") (embark "0.23"))
;; Keywords: convenience, notes, vulpea

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides Embark integration for Vulpea notes,
;; offering contextual actions and an export buffer.
;;
;; Features:
;;
;; 1. **Embark actions**: A dedicated keymap for `vulpea-note'
;;    candidates with actions like visit, insert link, find
;;    backlinks, copy ID, and delete.
;;
;; 2. **Embark export**: Select multiple vulpea notes with
;;    `embark-select', then use `embark-export' to create an org
;;    buffer containing links to the selected notes.


;;; Code:

;;;; Requirements

(require 'embark)
(require 'vulpea)
(require 'vulpea-note)
(require 'vulpea-db)
(require 'vulpea-db-query)

;;;; Customization

(defgroup embark-vulpea nil
  "Embark integration for Vulpea notes."
  :group 'vulpea
  :group 'embark
  :link '(url-link :tag "GitHub" "https://github.com/fabcontigiani/embark-vulpea"))

(defcustom embark-vulpea-export-readonly nil
  "Whether the export buffer should be read-only.

When nil, the export buffer contains a checklist of links to
vulpea notes.  When non-nil, contains a plain list of links."
  :type 'boolean
  :group 'embark-vulpea)

;;;; Transformer

(defun embark-vulpea--transformer (type target)
  "Transform a `vulpea-note' TARGET into a note ID.

TYPE is passed through unchanged.  TARGET is the completion
candidate string.  The `vulpea-note-id' text property (set by
`vulpea-select-describe') is extracted and returned as the new
target, making the note ID available to all actions."
  (let ((id (get-text-property 0 'vulpea-note-id target)))
    (cons type (or id target))))

;;;; Helper functions

(defun embark-vulpea--get-note (id)
  "Resolve ID to a `vulpea-note' from the database."
  (vulpea-db-get-by-id id))

;;;; Actions

(defun embark-vulpea-visit (id)
  "Visit the vulpea note with ID."
  (interactive "sNote ID: ")
  (if-let* ((note (embark-vulpea--get-note id)))
      (vulpea-visit note)
    (user-error "Cannot find vulpea note: %s" id)))

(defun embark-vulpea-visit-other-window (id)
  "Visit the vulpea note with ID in another window."
  (interactive "sNote ID: ")
  (if-let* ((note (embark-vulpea--get-note id)))
      (vulpea-visit note t)
    (user-error "Cannot find vulpea note: %s" id)))

(defun embark-vulpea-insert-link (id)
  "Insert an `id:' link to the vulpea note with ID."
  (interactive "sNote ID: ")
  (if-let* ((note (embark-vulpea--get-note id)))
      (insert (org-link-make-string
               (concat "id:" (vulpea-note-id note))
               (vulpea-note-title note)))
    (user-error "Cannot find vulpea note: %s" id)))

(defun embark-vulpea-find-backlinks (id)
  "Find notes that link to the vulpea note with ID."
  (interactive "sNote ID: ")
  (if-let* ((note (embark-vulpea--get-note id)))
      (let ((backlinks (vulpea-db-query-by-links-some
                        (list (cons "id" (vulpea-note-id note))))))
        (if backlinks
            (vulpea-find :candidates-fn (lambda (_) backlinks)
                         :require-match t)
          (message "No backlinks found for \"%s\"" (vulpea-note-title note))))
    (user-error "Cannot find vulpea note: %s" id)))

(defun embark-vulpea-copy-id (id)
  "Copy the ID of the vulpea note with ID to the kill ring."
  (interactive "sNote ID: ")
  (if-let* ((note (embark-vulpea--get-note id)))
      (progn
        (kill-new (vulpea-note-id note))
        (message "Copied ID: %s" (vulpea-note-id note)))
    (user-error "Cannot find vulpea note: %s" id)))

(defun embark-vulpea-copy-link (id)
  "Copy an `id:' link to the vulpea note with ID to the kill ring."
  (interactive "sNote ID: ")
  (if-let* ((note (embark-vulpea--get-note id)))
      (let ((link (org-link-make-string
                   (concat "id:" (vulpea-note-id note))
                   (vulpea-note-title note))))
        (kill-new link)
        (message "Copied link: %s" link))
    (user-error "Cannot find vulpea note: %s" id)))

(defun embark-vulpea-delete (id)
  "Delete the vulpea note with ID."
  (interactive "sNote ID: ")
  (if-let* ((note (embark-vulpea--get-note id)))
      (when (yes-or-no-p (format "Delete note \"%s\"? " (vulpea-note-title note)))
        (let ((path (vulpea-note-path note))
              (level (vulpea-note-level note)))
          (if (= level 0)
              ;; File-level note: delete the file
              (progn
                (delete-file path)
                (message "Deleted file: %s" path))
            ;; Heading-level note: remove the subtree
            (with-current-buffer (find-file-noselect path)
              (goto-char (vulpea-note-pos note))
              (org-back-to-heading t)
              (org-cut-subtree)
              (save-buffer))
            (message "Deleted heading: %s" (vulpea-note-title note)))))
    (user-error "Cannot find vulpea note: %s" id)))

(defun embark-vulpea-add-tags (ids)
  "Ask for one or more tags and add them to notes with id in IDS."
  (let* ((all-vulpea-tags (vulpea-db-query-tags))
         (tags-to-add
          (completing-read-multiple "Tag to add: " all-vulpea-tags nil t)))

    (dolist (id ids)
      (let* ((note (vulpea-db-get-by-id id)))

        (vulpea-utils-with-note-sync
            note (apply #'vulpea-tags-add id tags-to-add))))))

(defun embark-vulpea-remove-tags (ids)
  "Ask for one or more tags and remove them from notes with id in IDS."
  (let* ((all-vulpea-tags (vulpea-db-query-tags))
         (tags-to-remove
          (completing-read-multiple "Tag to remove: " all-vulpea-tags nil t)))
    (dolist (id ids)
      (let* ((note (vulpea-db-get-by-id id)))
        (vulpea-utils-with-note-sync
            note (apply #'vulpea-tags-remove id tags-to-remove))))))

;;;; Keymap

(defvar embark-vulpea-note-map
  (let ((map (make-sparse-keymap))
        (tag-operations-map (make-sparse-keymap)))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "RET") #'embark-vulpea-visit)
    (define-key map (kbd "o")   #'embark-vulpea-visit-other-window)
    (define-key map (kbd "i")   #'embark-vulpea-insert-link)
    (define-key map (kbd "b")   #'embark-vulpea-find-backlinks)
    (define-key map (kbd "w")   #'embark-vulpea-copy-id)
    (define-key map (kbd "W")   #'embark-vulpea-copy-link)
    (define-key map (kbd "D")   #'embark-vulpea-delete)
    (define-key map (kbd "t") tag-operations-map)
    (define-key tag-operations-map (kbd "a") #'embark-vulpea-add-tags)
    (define-key tag-operations-map (kbd "r") #'embark-vulpea-remove-tags)
    map)
  "Keymap for Embark actions on vulpea notes.")

;;;; Export

(defun embark-vulpea-export (candidates)
  "Export selected vulpea note CANDIDATES to an org buffer.

Creates an org buffer listing links to the provided notes.
Use `embark-select' to mark candidates, then `embark-export'.

CANDIDATES is a list of note ID strings (after transformation)
or completion candidate strings with `vulpea-note-id' text
properties."
  (let ((buf (generate-new-buffer "*Embark Export Vulpea*")))
    (with-current-buffer buf
      (let ((suffix (if (not embark-vulpea-export-readonly)
                        " [/]:"
                      ":")))
        (insert (propertize
                 (format "Links to selected vulpea notes%s\n" suffix)
                 'face 'list-matching-lines-buffer-name-face)))
      (dolist (cand candidates)
        ;; cand may be a note ID (via transformer) or a raw
        ;; candidate string with text properties.
        (when-let* ((id (or (get-text-property 0 'vulpea-note-id cand)
                            cand))
                    (note (vulpea-db-get-by-id id))
                    (title (vulpea-note-title note)))
          (if (not embark-vulpea-export-readonly)
              (insert "1. [ ] ")
            (insert "1. "))
          (insert (org-link-make-string
                   (concat "id:" (vulpea-note-id note))
                   title))
          (org-list-repair)
          (insert "\n")))
      (goto-char (point-min))
      (unless embark-vulpea-export-readonly
        (org-update-statistics-cookies 4))
      (org-mode)
      (when embark-vulpea-export-readonly
        (read-only-mode)))
    (pop-to-buffer buf)))

;;;; Registration

;; Register embark-vulpea-add-tags and embark-vulpea-remove-tags
;; to embark-multitarget-actions, since they operate on multiple note IDs.
(add-to-list 'embark-multitarget-actions 'embark-vulpea-add-tags)
(add-to-list 'embark-multitarget-actions 'embark-vulpea-remove-tags)

;; Register transformer to convert candidate string → note ID
(setf (alist-get 'vulpea-note embark-transformer-alist)
      #'embark-vulpea--transformer)

;; Register keymap for vulpea-note category
(add-to-list 'embark-keymap-alist '(vulpea-note . embark-vulpea-note-map))

;; Register exporter for vulpea-note category
(setf (alist-get 'vulpea-note embark-exporters-alist)
      #'embark-vulpea-export)

;;;; Footer

(provide 'embark-vulpea)
;;; embark-vulpea.el ends here
