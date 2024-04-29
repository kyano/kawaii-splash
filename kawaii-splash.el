;;; kawaii-splash --- Custom splash buffer with the kawaii image -*- lexical-binding: t -*-

;; Copyright (C) 2024 Anho Ki

;; Author: Anho Ki
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/kawaii-splash
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (all-the-icons "6.0.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Custom splash buffer with the kawaii image

;;; Code:

(require 'all-the-icons)

(defconst kawaii-splash/menu-items
  `(("Open a File"
     "Specify a new file's name, to edit the file"
     ,(lambda (_button) (call-interactively 'find-file))
     ,(all-the-icons-fontawesome-4 "file-text-o"))
    ("Open a Directory"
     "Open a directory, to operate on its files"
     ,(lambda (_button) (call-interactively 'dired))
     ,(all-the-icons-fontawesome-4 "folder-open-o"))
    ("Open Treemacs"
     "Start treemacs mode and show the sidebar"
     ,(lambda (_button) (treemacs))
     ,(all-the-icons-icon-for-mode 'treemacs-mode))
    ("Open VTerm"
     "Start an interactive VTerm buffer"
     ,(lambda (_button) (vterm))
     ,(all-the-icons-icon-for-mode 'vterm-mode))
    ("Customize"
     "Change settings"
     ,(lambda (_button) (customize))
     ,(all-the-icons-fontawesome-4 "gears"))
    (,(concat "Edit `" user-emacs-directory "init.el" "'")
     "Open the default Init file"
     ,(lambda (_button) (find-file (concat user-emacs-directory "init.el")))
     ,(all-the-icons-fontawesome-4 "gears"))
    ("Toggle frame maximized"
     "Toggle the frame to maximized or vice-versa"
     ,(lambda (_button) (toggle-frame-maximized))
     ,(all-the-icons-fontawesome-4 "window-maximize"))
    ("Toggle frame fullscreen"
     "Toggle the frame to fullscreen or vice-versa"
     ,(lambda (_button) (toggle-frame-fullscreen))
     ,(all-the-icons-fluentui-system-icons "full_screen_maximize" :style 'filled))))

(defun kawaii-splash/scale-svg-icon (icon scale)
  "Scale the embedded SVG icon of ICON to SCALE.

ICON must be a returned character from `all-the-icons' that has a
property `display'."

  (let ((img (get-text-property 0 'display icon)))
    (setf (image-property img :scale) scale)
    (set-text-properties 0 0 '('display img) icon)
    icon))

(defun kawaii-splash/splash-screen ()
  "Generate a buffer for splash screen.

If `fancy-splash-image' is not the default value(nil), the bundled
image is used instead."

  (let ((splash-buffer (get-buffer-create "*splash*")))
    (with-current-buffer splash-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let* ((image-file (fancy-splash-image-file))
               (img (create-image image-file)))
	  (when (and (memq 'xpm img)
		     (eq (frame-parameter nil 'background-mode) 'dark))
	    (setq img (append img '(:color-symbols (("#000000" . "gray30"))))))
          (insert-image img)
          (insert "\n\n"))
        (fancy-splash-insert :face '(variable-pitch (:height 1.2) font-lock-comment-face)
                             "Welcome to GNU Emacs, one component of the GNU operating system.\n")
        (insert "\n\n")
        (dolist (menu-item kawaii-splash/menu-items)
          (let ((name (nth 0 menu-item))
                (description (nth 1 menu-item))
                (function (nth 2 menu-item))
                (bullet-icon (nth 3 menu-item)))
            (fancy-splash-insert :face '(variable-pitch (:height 1.1))
                                 "\t"
                                 (if (not bullet-icon)
                                     (format "%s\t" (kawaii-splash/scale-svg-icon (all-the-icons-file-icons "emacs") 1.1))
                                   (format "%s\t" (kawaii-splash/scale-svg-icon bullet-icon 1.1)))
                                 :link `(,name ,function ,description)
                                 "\n")))
        (insert "\n\n")
        (when auto-save-list-file-prefix
          (let ((dir  (file-name-directory auto-save-list-file-prefix))
	        (name (file-name-nondirectory auto-save-list-file-prefix))
	        files)
            (and (file-directory-p dir)
	         (setq files (directory-files dir nil (concat "\\`" name) t))
	         (fancy-splash-insert :face '(variable-pitch font-lock-comment-face)
				      (if (= (length files) 1)
				          "An auto-save file list was found.  "
				        "Auto-save file lists were found.  ")
				      "If an Emacs session crashed recently,\ntype "
				      :link `("M-x recover-session RET"
                                              ,(lambda (_button) (call-interactively 'recover-session)))
				      " to recover the files you were editing.\n"
                                      "\n\n"))))
        (fancy-splash-insert :face '(variable-pitch font-lock-builtin-face)
                             (emacs-version) "\n")
        (let ((wsl-version (condition-case nil
                               (process-lines "/usr/bin/wslinfo" "--wsl-version")
                             (error nil)))
              (os-icon))
          (cond ((eq system-type 'gnu/linux)
                 (setq os-icon (all-the-icons-fontawesome-4 "linux")))
                ((eq system-type 'darwin)
                 (setq os-icon (all-the-icons-fontawesome-4 "apple")))
                (t (setq os-icon (all-the-icons-fontawesome-4 "desktop"))))
          (fancy-splash-insert :face '(variable-pitch (:height 0.8) font-lock-builtin-face)
                               "System Type: "
                               (kawaii-splash/scale-svg-icon os-icon 0.8)
                               (upcase (symbol-name system-type))
                               (if wsl-version
                                   (concat " on "
                                           (kawaii-splash/scale-svg-icon (all-the-icons-fontawesome-4 "windows") 0.8)
                                           "WSL\n")
                                 "\n"))
          (fancy-splash-insert :face '(variable-pitch (:height 0.8) font-lock-builtin-face)
                               "Window System: " os-icon (upcase (symbol-name window-system)) "\n"))
        (fancy-splash-insert :face '(variable-pitch (:height 0.9))
                             emacs-copyright "\n")
	(skip-chars-backward "\n")
	(delete-region (point) (point-max))
	(insert "\n")
        (use-local-map splash-screen-keymap)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (if (and view-read-only (not view-mode))
	    (view-mode-enter nil 'kill-buffer))
        splash-buffer))))

(when (not fancy-splash-image)
  (setq fancy-splash-image
        (concat
         (file-name-directory load-file-name)
         "gnome_mage.png")))
(setq initial-buffer-choice #'kawaii-splash/splash-screen)

(provide 'kawaii-splash)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; kawaii-splash.el ends here
