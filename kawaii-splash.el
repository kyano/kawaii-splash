;;; kawaii-splash --- Custom splash buffer with the kawaii image -*- lexical-binding: t -*-

;; Copyright (C) 2024 Anho Ki

;; Author: Anho Ki
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/kawaii-splash
;; Version: 0.0.2
;; Package-Requires: ((emacs "29.1") (nerd-icons))

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

(require 'nerd-icons)

(defconst kawaii-splash/menu-items
  `(("Emacs Tutorial"
     "Learn basic keystroke commands"
     ,(lambda (_button) (help-with-tutorial)))
    ("View Emacs Manual"
     "View the Emacs manual using Info"
     ,(lambda (_button) (info-emacs-manual)))
    ("Open a File"
     "Specify a new file's name, to edit the file"
     ,(lambda (_button) (call-interactively #'find-file))
     ,(nerd-icons-faicon "nf-fa-file_o" :height 1.1))
    ("Open Home Directory"
     "Open your home directory, to operate on its files"
     ,(lambda (_button) (dired "~"))
     ,(nerd-icons-faicon "nf-fa-folder_o" :height 1.1))
    ,(when (fboundp 'gptel)
       (list "Open gptel menu"
             "Start gptel and show the transient menu"
             (lambda (_button) (call-interactively #'gptel-menu))
             (nerd-icons-mdicon "nf-md-chat_alert" :height 1.1)))
    ("Open Terminal"
     "Start a terminal-emulator in a new buffer"
     ,(lambda (_button)
        (cond ((fboundp 'ghostel) (ghostel t))
              ((fboundp 'vterm) (vterm t))
              ((fboundp 'eat) (eat t))
              (t (call-interactively #'term))))
     ,(nerd-icons-devicon "nf-dev-terminal" :height 1.1))
    ("Customize"
     "Select a customization buffer which you can use to set user options"
     ,(lambda (_button) (customize))
     ,(nerd-icons-faicon "nf-fa-gears" :height 1.1))
    (,(concat "Edit `" user-emacs-directory "init.el" "'")
     "Open `init.el'"
     ,(lambda (_button) (find-file (concat user-emacs-directory "init.el")))
     ,(nerd-icons-faicon "nf-fa-gears" :height 1.1))
    ("Explore Packages"
     "Explore, install and remove Emacs packages (requires Internet connection)"
     ,(lambda (_button) (call-interactively #'list-packages))
     ,(nerd-icons-faicon "nf-fa-archive" :height 1.1))
    ("Toggle frame maximized"
     "Toggle maximization state"
     ,(lambda (_button) (toggle-frame-maximized))
     ,(nerd-icons-faicon "nf-fa-window_maximize" :height 1.1))
    ("Toggle frame fullscreen"
     "Toggle fullscreen state"
     ,(lambda (_button) (toggle-frame-fullscreen))
     ,(nerd-icons-octicon "nf-oct-screen_full" :height 1.1))))

(defun kawaii-splash/splash-screen ()
  "Generate a buffer for splash screen."

  (let ((splash-buffer (get-buffer-create "*splash*")))
    (with-current-buffer splash-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory command-line-default-directory)
        (make-local-variable 'startup-screen-inhibit-startup-screen)

        ;; Customized `fancy-splash-head'
        ;; Without this extra guard `(when window-system)',
        ;; Flycheck will fail on `init.el'.
        (when window-system
          (let* ((image-file (fancy-splash-image-file))
                 (img (create-image image-file))
                 (image-width (and img (car (image-size img))))
                 (window-width (window-width)))
            (when img
              (when (> window-width image-width)
                (let ((text-width 80)
                      (adjust-left 3))
                  (insert (propertize " " 'display
                                      `(spaced :align-to (+ ,(- (/ text-width 2)
                                                                adjust-left)
                                                            (-0.5 . ,img)))))))
              (when (and (memq 'xpm img)
                         (eq (frame-parameter nil 'background-mode) 'dark))
                (setq img (append img '(:color-symbols (("#000000" . "gray30"))))))
              (insert-image img)
              (insert "\n\n")
              (add-hook 'window-configuration-change-hook
                        #'(lambda ()
                            (when (equal (current-buffer)
                                         splash-buffer)
                              (setf (image-property img :max-width)
                                    (window-pixel-width))))))))
        ;; End of customized `fancy-splash-head'

        ;; Customized `fancy-startup-text'
        (fancy-splash-insert
         :face '(variable-pitch (:height 1.2) font-lock-comment-face)
         "Welcome to "
         :link
         `("GNU Emacs"
           ,(lambda (_button)
              (let ((browse-url-browser-function 'eww-browse-url))
                (browse-url "https://www.gnu.org/software/emacs/")))
           "Browse https://www.gnu.org/software/emacs/")
         ", one component of the "
         :link
         (lambda ()
           (if (eq system-type 'gnu/linux)
               `("GNU/Linux"
                 ,(lambda (_button)
                    (let ((browse-url-browser-function 'eww-browse-url))
                      (browse-url "https://www.gnu.org/gnu/linux-and-gnu.html")))
                 "Browse https://www.gnu.org/gnu/linux-and-gnu.html")
             `("GNU"
               ,(lambda (_button)
                  (let ((browse-url-browser-function 'eww-browse-url))
                    (browse-url "https://www.gnu.org/gnu/thegnuproject.html")))
               "Browse https://www.gnu.org/gnu/thegnuproject.html")))
         " operating system.\n")
        (fancy-splash-insert :face 'variable-pitch "\n")
        ;; End of customized `fancy-startup-text'

        (dolist (menu-item kawaii-splash/menu-items)
          (when menu-item
            (let ((name (nth 0 menu-item))
                  (description (nth 1 menu-item))
                  (function (nth 2 menu-item))
                  (bullet-icon (nth 3 menu-item)))
              (fancy-splash-insert :face '(variable-pitch (:height 1.1))
                                   "\t")
              (insert (if bullet-icon
                          bullet-icon
                        (nerd-icons-sucicon "nf-custom-emacs" :height 1.1)))
              (fancy-splash-insert :face '(variable-pitch (:height 1.1))
                                   "\t"
                                   :link
                                   `(,name ,function ,description)
                                   "\n"))))
        (skip-chars-backward "\n")
        (delete-region (point) (point-max))
        (fancy-splash-insert :face 'variable-pitch "\n\n")

        ;; Customized `fancy-start-up-tail'
        (fancy-splash-insert
         :face 'variable-pitch "To quit a partially entered command, type "
         :face 'default "Control-g"
         :face 'variable-pitch ".\n")
        (fancy-splash-insert :face '(variable-pitch bold) "New to Emacs?")
        (fancy-splash-insert
         :face 'variable-pitch
         "  Consider enabling "
         :link `("newcomer presets"
                 ,(lambda (_button) (info "(emacs) Newcomers Theme")))
         " by clicking this checkbox:  ")
        (let ((checked (create-image "checked.xpm"
                                     nil nil :ascent 'center))
              (unchecked (create-image "unchecked.xpm"
                                       nil nil :ascent 'center))
              (enabled (custom-theme-enabled-p 'newcomers-presets)))
          (insert-button
           " "
           :on-glyph checked
           :off-glyph unchecked
           'checked enabled
           'display (if enabled checked unchecked)
           'follow-link t
           'action (lambda (button)
                     (if (overlay-get button 'checked)
                         (progn (overlay-put button 'checked nil)
                                (overlay-put button 'display
                                             (overlay-get button :off-glyph))
                                (disable-theme 'newcomers-presets))
                       (overlay-put button 'checked t)
                       (overlay-put button 'display
                                    (overlay-get button :on-glyph))
                       (load-theme 'newcomers-presets)))))
        (fancy-splash-insert :face 'variable-pitch "\n")
        (when auto-save-list-file-prefix
          (let ((dir  (file-name-directory auto-save-list-file-prefix))
                (name (file-name-nondirectory auto-save-list-file-prefix))
                files)
            (and (file-directory-p dir)
                 (setq files (directory-files dir nil (concat "\\`" name) t))
                 (fancy-splash-insert :face '(variable-pitch font-lock-comment-face)
                                      (if (= (length files) 1)
                                          "\nAn auto-save file list was found.  "
                                        "\nAuto-save file lists were found.  ")
                                      "If an Emacs session crashed recently,\ntype "
                                      :link `("M-x recover-session RET"
                                              ,(lambda (_button)
                                                 (call-interactively
                                                  'recover-session)))
                                      " to recover the files you were editing.\n"))))
        (fancy-splash-insert :face 'variable-pitch "\n")
        (save-restriction
          (narrow-to-region (point) (point))
          (fancy-splash-insert :face '(variable-pitch font-lock-builtin-face)
                               "This is "
                               (emacs-version)
                               "\n")
          (fill-region (point-min) (point-max)))
        (let ((wsl-version (condition-case nil
                               (process-lines "/usr/bin/wslinfo"
                                              "--wsl-version")
                             (error nil)))
              (os-icon))
          (cond ((eq system-type 'gnu/linux)
                 (setq os-icon (nerd-icons-faicon "nf-fa-linux" :height 0.8)))
                ((eq system-type 'darwin)
                 (setq os-icon (nerd-icons-faicon "nf-fa-apple" :height 0.8)))
                (t (setq os-icon (nerd-icons-faicon "nf-fa-desktop" :height 0.8))))
          (fancy-splash-insert :face '(variable-pitch (:height 0.8) font-lock-builtin-face)
                               "\nSystem Type: ")
          (insert os-icon)
          (fancy-splash-insert :face '(variable-pitch (:height 0.8) font-lock-builtin-face)
                               (upcase (symbol-name system-type)))
          (when wsl-version
            (fancy-splash-insert :face '(variable-pitch (:height 0.8) font-lock-builtin-face)
                                 " on ")
            (insert (nerd-icons-faicon "nf-fa-windows" :height 0.8))
            (fancy-splash-insert :face '(variable-pitch (:height 0.8) font-lock-builtin-face)
                                 "WSL"))
          (fancy-splash-insert :face '(variable-pitch (:height 0.8) font-lock-builtin-face)
                               "\n")
          (fancy-splash-insert :face '(variable-pitch (:height 0.8) font-lock-builtin-face)
                               "Window System: ")
          (insert os-icon)
          (fancy-splash-insert :face '(variable-pitch (:height 0.8) font-lock-builtin-face)
                               (upcase (symbol-name window-system))
                               "\n\n"))
        (fancy-splash-insert :face '(variable-pitch (:height 0.9))
                             emacs-copyright
                             "\n")
        ;; End of customized `fancy-startup-tail'

        (use-local-map splash-screen-keymap)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (if (and view-read-only (not view-mode))
            (view-mode-enter nil 'kill-buffer))
        (goto-char (point-min))
        (forward-line 4)))
    splash-buffer))

(when (not fancy-splash-image)
  (setq fancy-splash-image
        (concat
         (file-name-directory load-file-name)
         "hacker.png")))
(setq initial-buffer-choice
      #'kawaii-splash/splash-screen)

(provide 'kawaii-splash)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; kawaii-splash.el ends here
