;;; tangotango-theme.el --- Tango Palette color theme for Emacs 24.

;; First derived from color-theme-tango.el,  created by danranx@gmail.com :
;; http://www.emacswiki.org/emacs/color-theme-tango.el

;; Copyright (C) 2011, 2012, 2013 Julien Barnier <julien@nozav.org>

;; Author: Julien Barnier
;; Adapted-By: Yesudeep Mangalapilly
;; Keywords: tango palette color theme emacs
;; URL: https://github.com/juba/color-theme-tangotango
;; Version: 20150113.517
;; X-Original-Version: 0.0.6

;; This file is NOT part of GNU Emacs.

;;; License:

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:

(deftheme tangotango
  "A color theme based on the Tango Palette colors.")

(custom-theme-set-faces
 'tangotango
 ;; Color codes :
 ;; - blue :       "dodger blue"
 ;; - yellow :     "#edd400"
 ;; - green :      "#6ac214"
 ;; - orange/red : "tomato"
 `(default ((t (:weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "#eeeeec" :background "#2e3434" :stipple nil :inherit nil))))
 `(cursor ((t (:foreground "#222222" :background "#fce94f"))))
 `(fixed-pitch ((t (:inherit (default)))))
 `(variable-pitch ((t (:family "Sans Serif"))))
 `(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 `(highlight ((t (:background "brown4"))))
 `(region ((t (:background "dark slate blue"))))
 `(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 `(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 `(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 `(font-lock-builtin-face ((t (:foreground "#729fcf"))))
 `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 `(font-lock-comment-face ((t (:foreground "#888a85"))))
 `(font-lock-constant-face ((t (:foreground "#8ae234"))))
 `(font-lock-doc-face ((t (:foreground "#888a85"))))
 `(font-lock-function-name-face ((t (:weight bold :foreground "#edd400"))))
 `(font-lock-keyword-face ((t (:weight bold :foreground "#729fcf"))))
 `(font-lock-negation-char-face ((t nil)))
 `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 `(font-lock-string-face ((t (:slant italic :foreground "#ad7fa8"))))
 `(font-lock-type-face ((t (:weight bold :foreground "#8ae234"))))
 `(font-lock-variable-name-face ((t (:foreground "tomato"))))
 `(font-lock-warning-face ((t (:weight bold :foreground "#f57900"))))
 `(button ((t (:inherit (link)))))
 `(link ((t (:foreground "dodger blue" :underline (:color foreground-color :style line)))))
 `(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 `(fringe ((t (:background "grey10"))))
 `(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 `(tooltip ((t (:background "lightyellow" :foreground "black" :inherit (quote variable-pitch)))))
 `(mode-line ((t (:box (:line-width 1 :color nil :style released-button) :background "#222222" :foreground "#bbbbbc"))))
 `(mode-line-buffer-id ((t (:weight bold :foreground "orange"))))
 `(mode-line-emphasis ((t (:weight bold))))
 `(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 `(mode-line-inactive ((t (:background "#555753" :foreground "#bbbbbc"))))
 `(isearch ((t (:foreground "#2e3436" :background "#f57900"))))
 `(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 `(lazy-highlight ((t (:foreground "#2e3436" :background "#e9b96e"))))
 `(match ((t (:weight bold :foreground "#2e3436" :background "#e9b96e"))))
 `(next-error ((t (:inherit (region)))))
 `(query-replace ((t (:inherit (isearch)))))
 `(show-paren-match-face ((t (:foreground "#2e3436" :background "#73d216"))))
 `(show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))
 `(info-xref ((t (:foreground "#729fcf"))))
 `(info-xref-visited ((t (:foreground "#ad7fa8"))))
 `(diary-face ((t (:bold t :foreground "IndianRed"))))
 `(eshell-ls-clutter-face ((t (:bold t :foreground "DimGray"))))
 `(eshell-ls-executable-face ((t (:bold t :foreground "Coral"))))
 `(eshell-ls-missing-face ((t (:bold t :foreground "black"))))
 `(eshell-ls-special-face ((t (:bold t :foreground "Gold"))))
 `(eshell-ls-symlink-face ((t (:bold t :foreground "White"))))
 `(widget-button ((t (:bold t))))
 `(widget-mouse-face ((t (:bold t :foreground "white" :background "brown4"))))
 `(widget-field ((t (:foreground "orange" :background "gray30"))))
 `(widget-single-line-field ((t (:foreground "orange" :background "gray30"))))
 `(custom-group-tag ((t (:bold t :foreground "#edd400" :height 1.3))))
 `(custom-variable-tag ((t (:bold t :foreground "#edd400" :height 1.1))))
 `(custom-face-tag ((t (:bold t :foreground "#edd400" :height 1.1))))
 `(custom-state-face ((t (:foreground "#729fcf"))))
 `(custom-button  ((t (:box (:line-width 1 :style released-button) :background "grey50" :foreground "black"))))
 `(custom-variable-button ((t (:inherit 'custom-button))))
 `(custom-button-mouse  ((t (:inherit 'custom-button :background "grey60"))))
 `(custom-button-unraised  ((t (:background "grey50" :foreground "black"))))
 `(custom-button-mouse-unraised  ((t (:inherit 'custom-button-unraised :background "grey60"))))
 `(custom-button-pressed  ((t (:inherit 'custom-button :box (:style pressed-button)))))
 `(custom-button-mouse-pressed-unraised  ((t (:inherit 'custom-button-unraised :background "grey60"))))
 `(custom-documentation ((t (:italic t))))
 `(message-cited-text ((t (:foreground "#edd400"))))
 `(gnus-cite-face-1 ((t (:foreground "#ad7fa8"))))
 `(gnus-cite-face-2 ((t (:foreground "sienna4"))))
 `(gnus-cite-face-3 ((t (:foreground "khaki4"))))
 `(gnus-cite-face-4 ((t (:foreground "PaleTurquoise4"))))
 `(gnus-group-mail-1-empty((t (:foreground "light cyan"))))
 `(gnus-group-mail-1((t (:bold t :foreground "light cyan"))))
 `(gnus-group-mail-2-empty((t (:foreground "turquoise"))))
 `(gnus-group-mail-2((t (:bold t :foreground "turquoise"))))
 `(gnus-group-mail-3-empty((t (:foreground "#729fcf"))))
 `(gnus-group-mail-3((t (:bold t :foreground "#edd400"))))
 `(gnus-group-mail-low-empty((t (:foreground "dodger blue"))))
 `(gnus-group-mail-low((t (:bold t :foreground "dodger blue"))))
 `(gnus-group-news-1-empty((t (:foreground "light cyan"))))
 `(gnus-group-news-1((t (:bold t :foreground "light cyan"))))
 `(gnus-group-news-2-empty((t (:foreground "turquoise"))))
 `(gnus-group-news-2((t (:bold t :foreground "turquoise"))))
 `(gnus-group-news-3-empty((t (:foreground "#729fcf"))))
 `(gnus-group-news-3((t (:bold t :foreground "#edd400"))))
 `(gnus-group-news-low-empty((t (:foreground "dodger blue"))))
 `(gnus-group-news-low((t (:bold t :foreground "dodger blue"))))
 `(gnus-header-name ((t (:bold t :foreground "#729fcf"))))
 `(gnus-header-from ((t (:bold t :foreground "#edd400"))))
 `(gnus-header-subject ((t (:foreground "#edd400"))))
 `(gnus-header-content ((t (:italic t :foreground "#8ae234"))))
 `(gnus-header-newsgroups((t (:italic t :bold t :foreground "LightSkyBlue3"))))
 `(gnus-signature((t (:italic t :foreground "dark grey"))))
 `(gnus-summary-cancelled((t (:background "black" :foreground "yellow"))))
 `(gnus-summary-high-ancient((t (:bold t :foreground "royal blue"))))
 `(gnus-summary-high-read((t (:bold t :foreground "lime green"))))
 `(gnus-summary-high-ticked((t (:bold t :foreground "tomato"))))
 `(gnus-summary-high-unread((t (:bold t :foreground "white"))))
 `(gnus-summary-low-ancient((t (:italic t :foreground "lime green"))))
 `(gnus-summary-low-read((t (:italic t :foreground "royal blue"))))
 `(gnus-summary-low-ticked((t (:italic t :foreground "dark red"))))
 `(gnus-summary-low-unread((t (:italic t :foreground "white"))))
 `(gnus-summary-normal-ancient((t (:foreground "royal blue"))))
 `(gnus-summary-normal-read((t (:foreground "lime green"))))
 `(gnus-summary-normal-ticked((t (:foreground "indian red"))))
 `(gnus-summary-normal-unread((t (:foreground "white"))))
 `(gnus-summary-selected ((t (:background "brown4" :foreground "white"))))
 `(message-header-name((t (:foreground "tomato"))))
 `(message-header-newsgroups((t (:italic t :bold t :foreground "LightSkyBlue3"))))
 `(message-header-other((t (:foreground "LightSkyBlue3"))))
 `(message-header-xheader((t (:foreground "DodgerBlue3"))))
 `(message-header-subject ((t (:foreground "white"))))
 `(message-header-to ((t (:foreground "white"))))
 `(message-header-cc ((t (:foreground "white"))))
 `(org-hide ((t (:foreground "#2e3436"))))
 `(org-level-1 ((t (:bold t :foreground "dodger blue" :height 1.5))))
 `(org-level-2 ((t (:bold t :foreground "#edd400" :height 1.2))))
 `(org-level-3 ((t (:bold t :foreground "#6ac214" :height 1.0))))
 `(org-level-4 ((t (:bold nil :foreground "tomato" :height 1.0))))
 `(org-date ((t (:underline t :foreground "magenta3"))))
 `(org-footnote  ((t (:underline t :foreground "magenta3"))))
 `(org-link ((t (:foreground "skyblue2" :background "#2e3436" :underline nil))))
 `(org-special-keyword ((t (:foreground "brown"))))
 `(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
 `(org-block ((t (:foreground "#bbbbbc"))))
 `(org-quote ((t (:inherit org-block :slant italic))))
 `(org-verse ((t (:inherit org-block :slant italic))))
 `(org-todo ((t (:bold t :foreground "Red"))))
 `(org-done ((t (:bold t :foreground "ForestGreen"))))
 `(org-agenda-structure ((t (:weight bold :foreground "tomato"))))
 `(org-agenda-date ((t (:foreground "#6ac214"))))
 `(org-agenda-date-weekend ((t (:weight normal :foreground "dodger blue"))))
 `(org-agenda-date-today ((t (:weight bold :foreground "#edd400"))))
 `(org-scheduled-previously ((t (:weight normal :foreground "#edd400"))))
 `(org-block-begin-line ((t ( :foreground "#888a85" :background "#252b2b"))))
 `(org-block-background ((t (:background "#252b2b"))))
 `(org-block-end-line ((t ( :foreground "#888a85" :background "#252b2b"))))
 `(anything-header ((t (:bold t :background "grey15" :foreground "#edd400"))))
 `(anything-candidate-number ((t (:background "#f57900" :foreground "black"))))
 `(ess-jb-comment-face ((t (:background "#2e3436" :foreground "#888a85" :slant italic))))
 `(ess-jb-hide-face ((t (:background "#2e3436" :foreground "#243436"))))
 `(ess-jb-h1-face ((t (:height 1.6 :foreground "dodger blue" :slant normal))))
 `(ess-jb-h2-face ((t (:height 1.4 :foreground "#6ac214" :slant normal))))
 `(ess-jb-h3-face ((t (:height 1.2 :foreground "#edd400" :slant normal))))
 `(ecb-default-highlight-face ((t (:background "#729fcf"))))
 `(ecb-tag-header-face ((t (:background "#f57900"))))
 `(magit-header ((t (:foreground "#edd400"))))
 `(magit-diff-add ((t (:foreground "#729fcf"))))
 `(magit-item-highlight ((t (:weight extra-bold :inverse-video t))))
 `(diff-header ((t (:background "gray30"))))
 `(diff-index ((t (:foreground "#edd400" :bold t))))
 `(diff-file-header ((t (:foreground "#eeeeec" :bold t))))
 `(diff-hunk-header ((t (:foreground "#edd400"))))
 `(diff-added ((t (:foreground "#8ae234"))))
 `(diff-removed ((t (:foreground "#f57900"))))
 `(diff-context ((t (:foreground "#888a85"))))
 `(diff-refine-change ((t (:bold t :background "gray30"))))
 `(ediff-current-diff-A ((t (:background "#555753"))))
 `(ediff-current-diff-Ancestor ((t (:background "#555753"))))
 `(ediff-current-diff-B ((t (:background "#555753"))))
 `(ediff-current-diff-C ((t (:background "#555753"))))
 `(ediff-even-diff-A ((t (:background "gray30"))))
 `(ediff-even-diff-Ancestor ((t (:background "gray30"))))
 `(ediff-even-diff-B ((t (:background "gray30"))))
 `(ediff-even-diff-C ((t (:background "gray30"))))
 `(ediff-odd-diff-A ((t (:background "gray30"))))
 `(ediff-odd-diff-Ancestor ((t (:background "gray30"))))
 `(ediff-odd-diff-B ((t (:background "gray30"))))
 `(ediff-odd-diff-C ((t (:background "gray30"))))
 `(ediff-fine-diff-A ((t (:background "#222222"))))
 `(ediff-fine-diff-Ancestor ((t (:background "#222222"))))
 `(ediff-fine-diff-B ((t (:background "#222222"))))
 `(ediff-fine-diff-C ((t (:background "#222222"))))
 `(minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
 `(mumamo-background-chunk-major ((t (:background nil))))
 `(mumamo-background-chunk-submode1 ((t (:background "#2E3440"))))
 `(mumamo-background-chunk-submode2 ((t (:background "#2E4034"))))
 `(mumamo-background-chunk-submode3 ((t (:background "#343434"))))
 `(rpm-spec-dir-face ((t (:foreground "#8ae234"))))
 `(rpm-spec-doc-face ((t (:foreground "#888a85"))))
 `(rpm-spec-ghost-face ((t (:foreground "tomato"))))
 `(rpm-spec-macro-face ((t (:foreground "#edd400"))))
 `(rpm-spec-obsolete-tag-face ((t (:background "#f57900" :foreground "#ee3436" :weight bold))))
 `(rpm-spec-package-face ((t (:foreground "tomato"))))
 `(rpm-spec-section-face ((t (:foreground "#8ae234" :underline t :weight bold))))
 `(rpm-spec-tag-face ((t (:foreground "dodger blue" :weight bold))))
 `(rpm-spec-var-face ((t (:foreground "tomato"))))
 ;; regexp metachars
 `(font-lock-negation-char-face ((t (:foreground "#6ac214"))))
 `(font-lock-regexp-grouping-construct ((t (:foreground "#edd400"))))
 `(font-lock-regexp-grouping-backslash ((t (:foreground "#888a85"))))
 `(which-func ((t (:inherit (font-lock-function-name-face) :weight normal))))
 )


;; Autoload for MELPA

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'tangotango)

;;; tangotango-theme.el ends here
