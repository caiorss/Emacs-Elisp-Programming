;;; cyberpunk-theme.el --- Cyberpunk Color Theme

;; Copyright 2012-2015, Nicholas M. Van Horn

;; Author: Nicholas M. Van Horn <vanhorn.nm@gmail.com>
;; Keywords: color theme cyberpunk
;; Package-Version: 20150326.1914
;; Version: 1.11

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;; "and he'd still see the matrix in his sleep, bright lattices of logic
;; unfolding across that colorless void..."
;; William Gibson, Neuromancer.

;;; Commentary:

;; This theme is a port of the overtone/emacs-live theme of the same name
;; (https://github.com/overtone/emacs-live). The original theme was
;; designed for use with the color-theme package. This theme adopts the
;; new built-in theme support deftheme. Additionally, this
;; theme strives to offer as many mode-specific customizations as
;; possible, with further tweaks that suit my fancy.

(deftheme cyberpunk "The Cyberpunk color theme")

(let ((class '((class color) (min-colors 89)))
      ;; Cyberpunk palette
      (cyberpunk-fg "#dcdccc")
      (cyberpunk-bg-1 "#2b2b2b")
      (cyberpunk-bg-05 "#383838")
      (cyberpunk-bg "#000000")
      (cyberpunk-bg+1 "#4f4f4f")
      (cyberpunk-bg+2 "#5f5f5f")
      (cyberpunk-bg+3 "#6f6f6f")
      (cyberpunk-red+1 "#dca3a3")
      (cyberpunk-red "#ff0000")
      (cyberpunk-red-1 "#8b0000")
      (cyberpunk-red-2 "#8b0000")
      (cyberpunk-red-3 "#9c6363")
      (cyberpunk-red-4 "#8c5353")
      (cyberpunk-red-5 "#7F073F")
      (cyberpunk-pink "#ff69b4")
      (cyberpunk-pink-1 "#ff1493")
      (cyberpunk-pink-2 "#cd1076")
      (cyberpunk-orange-2 "#FF6400")
      (cyberpunk-orange-1 "#ff8c00") ;; DarkOrange
      (cyberpunk-orange "#ffa500")
      (cyberpunk-yellow "#ffff00")
      (cyberpunk-yellow-1 "#FBDE2D")
      (cyberpunk-yellow-2 "#d0bf8f")
      (cyberpunk-yellow-3 "#D8FA3C")
      (cyberpunk-yellow-4 "#E9C062")
      (cyberpunk-yellow-5 "#ffd700")
      (cyberpunk-green-2 "#006400")
      (cyberpunk-green-1 "#2e8b57")
      (cyberpunk-green "#00ff00")
      (cyberpunk-green+1 "#61CE3C")
      (cyberpunk-green+2 "#9fc59f")
      (cyberpunk-green+3 "#afd8af")
      (cyberpunk-green+4 "#bfebbf")
      (cyberpunk-cyan "#93e0e3")
      (cyberpunk-blue+1 "#94bff3")
      (cyberpunk-blue "#0000ff")    ;; blue
      (cyberpunk-blue-1 "#7b68ee")  ;; medium slate blue
      (cyberpunk-blue-2 "#6a5acd")  ;; slate blue
      (cyberpunk-blue-3 "#add8e6")  ;; light blue
      (cyberpunk-blue-4 "#b2dfee")  ;; LightBlue2
      (cyberpunk-blue-5 "#4c83ff")
      (cyberpunk-blue-6 "#96CBFE")
      (cyberpunk-blue-7 "#00ffff")
      (cyberpunk-blue-8 "#4F94CD")
      (cyberpunk-magenta "#dc8cc3")
      (cyberpunk-black "#000000")
      (cyberpunk-black-2 "#0C1021")
      (cyberpunk-black-3 "#0A0A0A")
      (cyberpunk-gray "#d3d3d3")
      (cyberpunk-gray-2 "#8B8989")
      (cyberpunk-gray-3 "#919191")
      (cyberpunk-gray-4 "#999999")
      (cyberpunk-gray-5 "#333333")
      (cyberpunk-gray-6 "#1A1A1A")
      (cyberpunk-gray-7 "#4D4D4D")
      (cyberpunk-gray-8 "#262626")
      (cyberpunk-white "#ffffff")
      (cyberpunk-white-2 "#F8F8F8")
      (cyberpunk-white-3 "#fffafa"))

 (custom-theme-set-faces
   'cyberpunk
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,cyberpunk-yellow :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,cyberpunk-yellow-2 :underline t :weight normal))))
   `(blue ((,class (:foreground ,cyberpunk-blue))))
   `(bold ((,class (:bold t))))
   `(bold-italic ((,class (:bold t))))
   `(border-glyph ((,class (nil))))
   `(buffers-tab ((,class (:background ,cyberpunk-black-2 :foreground ,cyberpunk-white-2))))

   ;;; basic coloring
   `(default ((,class (:foreground ,cyberpunk-gray :background ,cyberpunk-black))))
   `(cursor ((,class (:background ,cyberpunk-fg))))
   `(escape-glyph-face ((,class (:foreground ,cyberpunk-red))))
   ;; `(fringe ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg+1))))
   `(header-line ((,class (:foreground ,cyberpunk-yellow
                                       :background ,cyberpunk-bg-1
                                       :box (:line-width -1 :style released-button)))))
   `(highlight ((,class (:background ,cyberpunk-gray-5))))

   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,cyberpunk-yellow))))
   `(compilation-enter-directory-face ((,class (:foreground ,cyberpunk-green))))
   `(compilation-error-face ((,class (:foreground ,cyberpunk-red-1 :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,cyberpunk-fg))))
   `(compilation-info-face ((,class (:foreground ,cyberpunk-blue))))
   `(compilation-info ((,class (:foreground ,cyberpunk-green+4 :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,cyberpunk-green))))
   `(compilation-line-face ((,class (:foreground ,cyberpunk-yellow))))
   `(compilation-line-number ((,class (:foreground ,cyberpunk-yellow))))
   `(compilation-message-face ((,class (:foreground ,cyberpunk-blue))))
   `(compilation-warning-face ((,class (:foreground ,cyberpunk-yellow-1 :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-pink-1))))
   `(grep-error-face ((,class (:foreground ,cyberpunk-red :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-red))))
   `(grep-match-face ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-pink-1))))
   `(match ((,class (:background ,cyberpunk-black :foreground ,cyberpunk-pink-1))))


   ;;; multiple-cursors
   `(mc/cursor-face ((,class (:inverse-video nil, :background ,cyberpunk-pink :foreground ,cyberpunk-black))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-pink-1))))
   `(isearch-fail ((,class (:background ,cyberpunk-red-1))))
   
   `(lazy-highlight ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-yellow))))
   `(query-replace ((,class (:background ,cyberpunk-gray-5))))
   `(Highline-face ((,class (:background ,cyberpunk-green-1))))
   `(italic ((,class (nil))))
   `(left-margin ((,class (nil))))
   `(toolbar ((,class (nil))))
   `(underline ((,class (:underline nil))))
   `(text-cursor ((,class (:background ,cyberpunk-yellow :foreground ,cyberpunk-black))))

   `(menu ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg))))
   `(minibuffer-prompt ((,class (:foreground ,cyberpunk-green+1 :background ,cyberpunk-black))))
   `(mode-line
     ((,class (:foreground ,cyberpunk-blue-5
                           :background ,cyberpunk-gray-5
                           :box (:line-width -1 :color ,cyberpunk-blue-5)))))
   ;; `(mode-line-buffer-id ((,class (:foreground ,cyberpunk-yellow :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,cyberpunk-gray-7
                           :background ,cyberpunk-gray-6
                           :box (:line-width -1 :color ,cyberpunk-blue-5)))))
   `(region ((,class (:background ,cyberpunk-red-5))))
   `(secondary-selection ((,class (:background ,cyberpunk-bg+2))))
   `(trailing-whitespace ((,class (:background ,cyberpunk-red))))
   `(vertical-border ((,class (:foreground ,cyberpunk-gray-5 :background ,cyberpunk-black))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,cyberpunk-orange-2))))
   `(font-lock-comment-face ((,class (:foreground ,cyberpunk-gray-2 :italic t))))
   ;; `(font-lock-comment-delimiter-face ((,class (:foreground ,cyberpunk-green)))) 
   `(font-lock-constant-face ((,class (:foreground ,cyberpunk-blue-5))))
   ;; `(font-lock-doc-face ((,class (:foreground ,cyberpunk-green+1))))
   `(font-lock-doc-string-face ((,class (:foreground ,cyberpunk-orange-1))))
   `(font-lock-function-name-face ((,class (:foreground ,cyberpunk-pink-1))))
   `(font-lock-keyword-face ((,class (:foreground ,cyberpunk-yellow-1))))
   ;; `(font-lock-negation-char-face ((,class (:foreground ,cyberpunk-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,cyberpunk-gray-3))))
   `(font-lock-string-face ((,class (:foreground ,cyberpunk-green+1))))
   `(font-lock-type-face ((,class (:foreground ,cyberpunk-yellow-3))))
   `(font-lock-variable-name-face ((,class (:foreground ,cyberpunk-yellow-3))))
   `(font-lock-warning-face ((,class (:foreground ,cyberpunk-pink))))
   `(font-lock-reference-face ((,class (:foreground ,cyberpunk-gray))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,cyberpunk-yellow-4))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,cyberpunk-red))))


   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   `(gui-element ((,class (:background ,cyberpunk-gray-5 :foreground ,cyberpunk-blue-6))))
   


   ;;; newsticker
   ;; These are currently placeholders that probably look terrible.
   ;; Someone who uses newsticker is welcome to change these
   `(newsticker-date-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-default-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-enclosure-face ((,class (:foreground ,cyberpunk-green+3))))
   `(newsticker-extra-face ((,class (:foreground ,cyberpunk-bg+2 :height 0.8))))
   `(newsticker-feed-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-immortal-item-face ((,class (:foreground ,cyberpunk-green))))
   `(newsticker-new-item-face ((,class (:foreground ,cyberpunk-blue))))
   `(newsticker-obsolete-item-face ((,class (:foreground ,cyberpunk-red))))
   `(newsticker-old-item-face ((,class (:foreground ,cyberpunk-bg+3))))
   `(newsticker-statistics-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-treeview-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-treeview-immortal-face ((,class (:foreground ,cyberpunk-green))))
   `(newsticker-treeview-listwindow-face ((,class (:foreground ,cyberpunk-fg))))
   `(newsticker-treeview-new-face ((,class (:foreground ,cyberpunk-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((,class (:foreground ,cyberpunk-red))))
   `(newsticker-treeview-old-face ((,class (:foreground ,cyberpunk-bg+3))))
   `(newsticker-treeview-selection-face ((,class (:foreground ,cyberpunk-yellow))))

   ;;; external

   ;; full-ack
   `(ack-separator ((,class (:foreground ,cyberpunk-fg))))
   `(ack-file ((,class (:foreground ,cyberpunk-blue))))
   `(ack-line ((,class (:foreground ,cyberpunk-yellow))))
   `(ack-match ((,class (:foreground ,cyberpunk-orange :background ,cyberpunk-bg-1 :weigth bold))))

   ;; auctex
   `(font-latex-bold ((,class (:inherit bold))))
   `(font-latex-warning ((,class (:inherit font-lock-warning))))
   `(font-latex-sedate ((,class (:foreground ,cyberpunk-yellow :weight bold))))
   `(font-latex-string ((,class (:foreground ,cyberpunk-green))))
   `(font-latex-title-4 ((,class (:inherit variable-pitch :weight bold))))
   `(font-latex-sectioning-0 ((,class (:foreground ,cyberpunk-blue :background ,cyberpunk-black :scale 1.5))))
   `(font-latex-sectioning-1 ((,class (:foreground ,cyberpunk-blue :background ,cyberpunk-black :scale 1.5))))

   ;; auto-complete
   `(ac-completion-face ((,class (:background ,cyberpunk-gray-2 :underline t))))
   `(ac-candidate-face ((,class (:background ,cyberpunk-gray-4 :foreground ,cyberpunk-black))))
   `(ac-selection-face ((,class (:background ,cyberpunk-pink-1 :foreground ,cyberpunk-black))))
   `(popup-tip-face ((,class (:background ,cyberpunk-gray-5 :foreground ,cyberpunk-white))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,cyberpunk-black-3))))
   `(popup-scroll-bar-background-face ((,class (:background ,cyberpunk-gray-5))))
   `(popup-isearch-match ((,class (:background ,cyberpunk-black :foreground ,cyberpunk-pink-1))))

   `(window-number-face ((,class (:background ,cyberpunk-gray-6 :foreground ,cyberpunk-blue-5))))

   ;; company-mode
   `(company-tooltip ((,class (:background ,cyberpunk-gray-2 :foreground ,cyberpunk-yellow))))
   `(company-tooltip-common ((,class (:inherit company-tooltip :foreground ,cyberpunk-blue))))
   `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection :foreground ,cyberpunk-blue))))
   `(company-tooltip-selection ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-pink-1))))
   `(company-tooltip-annotation ((,class (:foreground ,cyberpunk-black-3))))
   `(company-scrollbar-fg ((,class (:background ,cyberpunk-black-3))))
   `(company-scrollbar-bg ((,class (:background ,cyberpunk-gray-5))))
   `(company-preview ((,class (:foreground ,cyberpunk-gray :background ,cyberpunk-pink-1))))
   `(company-preview-common ((,class (:foreground ,cyberpunk-gray :background ,cyberpunk-pink-1))))
   
   ;; diff
   `(diff-added ((,class (:foreground ,cyberpunk-green))))
   `(diff-changed ((,class (:foreground ,cyberpunk-yellow))))
   `(diff-removed ((,class (:foreground ,cyberpunk-red))))
   `(diff-header ((,class (:background ,cyberpunk-bg+2))))
   `(diff-file-header ((,class (:background ,cyberpunk-bg+2 :foreground ,cyberpunk-fg :bold t))))

   ;; ediff
   `(ediff-current-diff-Ancestor ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-pink))))
   `(ediff-current-diff-A ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg-05))))
   `(ediff-current-diff-B ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg+1))))
   `(ediff-current-diff-C ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg+2))))
   `(ediff-even-diff-Ancestor ((,class (:foreground ,cyberpunk-white :background ,cyberpunk-bg-05))))
   `(ediff-even-diff-A ((,class (:foreground ,cyberpunk-white :background ,cyberpunk-bg+1))))
   `(ediff-even-diff-B ((,class (:foreground ,cyberpunk-white :background ,cyberpunk-bg+2))))
   `(ediff-even-diff-C ((,class (:foreground ,cyberpunk-white :background ,cyberpunk-bg+3))))
   `(ediff-fine-diff-Ancestor ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-pink))))
   `(ediff-fine-diff-A ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-blue-5))))
   `(ediff-fine-diff-B ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-blue-5))))
   `(ediff-fine-diff-C ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-blue-5))))
   `(ediff-odd-diff-Ancestor ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-gray-2))))
   `(ediff-odd-diff-A ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-gray-3))))
   `(ediff-odd-diff-B ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-gray-4))))
   `(ediff-odd-diff-C ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-gray))))

   ;; ert
   `(ert-test-result-expected ((,class (:foreground ,cyberpunk-green+4 :background ,cyberpunk-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,cyberpunk-red :background ,cyberpunk-bg))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,cyberpunk-yellow :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,cyberpunk-red-1 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,cyberpunk-blue+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,cyberpunk-red+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,cyberpunk-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,cyberpunk-yellow :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,cyberpunk-cyan :weight bold))))

   ;; flymake
   `(flymake-errline ((,class (:foreground ,cyberpunk-red-1 :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,cyberpunk-yellow-1 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,cyberpunk-yellow-1 :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,cyberpunk-orange-2 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,cyberpunk-blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,cyberpunk-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,cyberpunk-yellow))))
   `(erc-keyword-face ((,class (:foreground ,cyberpunk-blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,cyberpunk-yellow :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,cyberpunk-red :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,cyberpunk-green))))
   `(erc-pal-face ((,class (:foreground ,cyberpunk-orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,cyberpunk-orange :background ,cyberpunk-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,cyberpunk-green+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1 ((,class (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((,class (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((,class (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((,class (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((,class (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((,class (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((,class (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((,class (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((,class (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((,class (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((,class (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((,class (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((,class (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((,class (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-from ((,class (:inherit message-header-from))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((,class (:foreground ,cyberpunk-orange))))
   `(gnus-summary-high-ancient ((,class (:foreground ,cyberpunk-blue))))
   `(gnus-summary-high-read ((,class (:foreground ,cyberpunk-green :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,cyberpunk-orange :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,cyberpunk-fg :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,cyberpunk-blue))))
   `(gnus-summary-low-read ((t (:foreground ,cyberpunk-green))))
   `(gnus-summary-low-ticked ((,class (:foreground ,cyberpunk-orange :weight bold))))
   `(gnus-summary-low-unread ((,class (:foreground ,cyberpunk-fg))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,cyberpunk-blue+1))))
   `(gnus-summary-normal-read ((,class (:foreground ,cyberpunk-green))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,cyberpunk-orange :weight bold))))
   `(gnus-summary-normal-unread ((,class (:foreground ,cyberpunk-fg))))
   `(gnus-summary-selected ((,class (:foreground ,cyberpunk-yellow :weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,cyberpunk-yellow-2))))
   `(gnus-cite-10 ((,class (:foreground ,cyberpunk-yellow-1))))
   `(gnus-cite-11 ((,class (:foreground ,cyberpunk-yellow))))
   `(gnus-cite-2 ((,class (:foreground ,cyberpunk-blue-1))))
   `(gnus-cite-3 ((,class (:foreground ,cyberpunk-blue-2))))
   `(gnus-cite-4 ((,class (:foreground ,cyberpunk-green+2))))
   `(gnus-cite-5 ((,class (:foreground ,cyberpunk-green+1))))
   `(gnus-cite-6 ((,class (:foreground ,cyberpunk-green))))
   `(gnus-cite-7 ((,class (:foreground ,cyberpunk-red))))
   `(gnus-cite-8 ((,class (:foreground ,cyberpunk-red-1))))
   `(gnus-cite-9 ((,class (:foreground ,cyberpunk-red-2))))
   `(gnus-group-news-1-empty ((,class (:foreground ,cyberpunk-yellow))))
   `(gnus-group-news-2-empty ((,class (:foreground ,cyberpunk-green+3))))
   `(gnus-group-news-3-empty ((,class (:foreground ,cyberpunk-green+1))))
   `(gnus-group-news-4-empty ((,class (:foreground ,cyberpunk-blue-2))))
   `(gnus-group-news-5-empty ((,class (:foreground ,cyberpunk-blue-3))))
   `(gnus-group-news-6-empty ((,class (:foreground ,cyberpunk-bg+2))))
   `(gnus-group-news-low-empty ((,class (:foreground ,cyberpunk-bg+2))))
   `(gnus-signature ((,class (:foreground ,cyberpunk-yellow))))
   `(gnus-x ((,class (:background ,cyberpunk-fg :foreground ,cyberpunk-bg))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,cyberpunk-green
                           :background ,cyberpunk-bg
                           :underline nil
                           :box nil))))
   `(helm-source-header
     ((,class (:foreground ,cyberpunk-yellow
                           :background ,cyberpunk-bg-1
                           :underline nil
                           :weight bold
                           :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,cyberpunk-bg+1 :underline nil))))
   `(helm-selection-line ((,class (:background ,cyberpunk-bg+1))))
   `(helm-visible-mark ((,class (:foreground ,cyberpunk-bg :background ,cyberpunk-yellow-2))))
   `(helm-candidate-number ((,class (:foreground ,cyberpunk-green+4 :background ,cyberpunk-bg-1))))

   ;; hl-line-mode
   `(hl-sexp-face ((,class (:background ,cyberpunk-gray-5))))
   `(hl-line-face ((,class (:background ,cyberpunk-gray-5))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,cyberpunk-pink-1 :background ,cyberpunk-black))))
   `(ido-only-match ((,class (:foreground ,cyberpunk-pink-1 :background ,cyberpunk-black))))
   `(ido-subdir ((,class (:foreground ,cyberpunk-gray-4 :backgroun ,cyberpunk-black))))
   `(ido-indicator ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-pink-1))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,cyberpunk-orange))))
   `(js2-error-face ((,class (:foreground ,cyberpunk-red :weight bold))))
   `(js2-jsdoc-tag-face ((,class (:foreground ,cyberpunk-green-1))))
   `(js2-jsdoc-type-face ((,class (:foreground ,cyberpunk-green+2))))
   `(js2-jsdoc-value-face ((,class (:foreground ,cyberpunk-green+3))))
   `(js2-function-param-face ((,class (:foreground, cyberpunk-green+3))))
   `(js2-external-variable-face ((,class (:foreground ,cyberpunk-orange))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,cyberpunk-green+2))))
   `(jabber-roster-user-online ((,class (:foreground ,cyberpunk-blue-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,cyberpunk-red+1))))
   `(jabber-rare-time-face ((,class (:foreground ,cyberpunk-green+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,cyberpunk-blue-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,cyberpunk-red+1))))
   `(jabber-activity-face((,class (:foreground ,cyberpunk-red+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,cyberpunk-blue+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,cyberpunk-green+2 :background ,cyberpunk-bg))))

   ;;linum-relative
   `(linum-relative-current-face ((,class (:inherit linum :foreground ,cyberpunk-white :weight bold))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,cyberpunk-pink-1))))
   `(magit-branch ((,class (:foreground ,cyberpunk-yellow-5))))
   `(magit-item-highlight ((,class (:background ,cyberpunk-gray-8))))
   `(magit-diff-add ((,class (:foreground ,cyberpunk-green))))
   `(magit-diff-del ((,class (:foreground ,cyberpunk-red))))
   `(magit-diff-hunk-header ((,class (:foreground ,cyberpunk-orange))))

   `(eval-sexp-fu-flash ((,class (:background ,cyberpunk-gray-8 :foreground ,cyberpunk-pink-2))))

   ;; message-mode
   `(message-cited-text ((,class (:inherit font-lock-comment))))
   `(message-header-name ((,class (:foreground ,cyberpunk-blue-5))))
   `(message-header-other ((,class (:foreground ,cyberpunk-green))))
   `(message-header-to ((,class (:foreground ,cyberpunk-pink-1 :weight bold))))
   `(message-header-from ((,class (:foreground ,cyberpunk-yellow :weight bold))))
   `(message-header-cc ((,class (:foreground ,cyberpunk-yellow :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,cyberpunk-yellow :weight bold))))
   `(message-header-subject ((,class (:foreground ,cyberpunk-orange :weight bold))))
   `(message-header-xheader ((,class (:foreground ,cyberpunk-green))))
   `(message-mml ((,class (:foreground ,cyberpunk-yellow :weight bold))))
   `(message-separator ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,cyberpunk-orange))))
   `(mew-face-header-from ((,class (:foreground ,cyberpunk-yellow))))
   `(mew-face-header-date ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-header-to ((,class (:foreground ,cyberpunk-red))))
   `(mew-face-header-key ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-header-private ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-header-important ((,class (:foreground ,cyberpunk-blue))))
   `(mew-face-header-marginal ((,class (:foreground ,cyberpunk-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,cyberpunk-red))))
   `(mew-face-header-xmew ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,cyberpunk-red))))
   `(mew-face-body-url ((,class (:foreground ,cyberpunk-orange))))
   `(mew-face-body-comment ((,class (:foreground ,cyberpunk-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-body-cite2 ((,class (:foreground ,cyberpunk-blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,cyberpunk-orange))))
   `(mew-face-body-cite4 ((,class (:foreground ,cyberpunk-yellow))))
   `(mew-face-body-cite5 ((,class (:foreground ,cyberpunk-red))))
   `(mew-face-mark-review ((,class (:foreground ,cyberpunk-blue))))
   `(mew-face-mark-escape ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-mark-delete ((,class (:foreground ,cyberpunk-red))))
   `(mew-face-mark-unlink ((,class (:foreground ,cyberpunk-yellow))))
   `(mew-face-mark-refile ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-mark-unread ((,class (:foreground ,cyberpunk-red-2))))
   `(mew-face-eof-message ((,class (:foreground ,cyberpunk-green))))
   `(mew-face-eof-part ((,class (:foreground ,cyberpunk-yellow))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground ,cyberpunk-cyan :background ,cyberpunk-bg :weight bold))))
   `(paren-face-mismatch ((,class (:foreground ,cyberpunk-bg :background ,cyberpunk-magenta :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,cyberpunk-bg :background ,cyberpunk-red :weight bold))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,cyberpunk-yellow))))
   `(nav-face-button-num ((,class (:foreground ,cyberpunk-cyan))))
   `(nav-face-dir ((,class (:foreground ,cyberpunk-green))))
   `(nav-face-hdir ((,class (:foreground ,cyberpunk-red))))
   `(nav-face-file ((,class (:foreground ,cyberpunk-fg))))
   `(nav-face-hfile ((,class (:foreground ,cyberpunk-red-4))))

   ;; mumamo
   `(mumamo-background-chunk-major ((,class (:background ,cyberpunk-black))))
   `(mumamo-background-chunk-submode1 ((,class (:background ,cyberpunk-black))))
   `(mumamo-background-chunk-submode2 ((,class (:background ,cyberpunk-bg+2))))
   `(mumamo-background-chunk-submode3 ((,class (:background ,cyberpunk-bg+3))))
   `(mumamo-background-chunk-submode4 ((,class (:background ,cyberpunk-bg+1))))

   ;; org-mode
   `(org-document-title ((,class (:foreground ,cyberpunk-blue-3 :background ,cyberpunk-black :weight bold :height 1.5))))
   `(org-document-info ((,class (:foreground ,cyberpunk-blue-3 :background ,cyberpunk-black :weight bold))))
   `(org-document-info-keyword ((,class (:foreground ,cyberpunk-gray-2 :background ,cyberpunk-black))))
   `(org-agenda-date-today
     ((,class (:foreground ,cyberpunk-orange-2 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:slant italic))))
   `(org-checkbox ((,class (:background ,cyberpunk-gray-2 :foreground ,cyberpunk-black
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,cyberpunk-blue-7 :underline t))))
   `(org-done ((,class (:bold t :weight bold :foreground ,cyberpunk-green
                              :box (:line-width 1 :style none)))))
   `(org-todo ((,class (:bold t :foreground ,cyberpunk-orange :weight bold
                              :box (:line-width 1 :style none)))))
   `(org-level-1 ((,class (:foreground ,cyberpunk-pink-1 :height 1.3))))
   `(org-level-2 ((,class (:foreground ,cyberpunk-yellow :height 1.2))))
   `(org-level-3 ((,class (:foreground ,cyberpunk-blue-5 :height 1.1))))
   `(org-level-4 ((,class (:foreground ,cyberpunk-green))))
   `(org-level-5 ((,class (:foreground ,cyberpunk-orange))))
   `(org-level-6 ((,class (:foreground ,cyberpunk-pink))))
   `(org-level-7 ((,class (:foreground ,cyberpunk-green+3))))
   `(org-level-8 ((,class (:foreground ,cyberpunk-blue-1))))
   `(org-link ((,class (:foreground ,cyberpunk-blue-6 :underline t))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-column ((,class (:background ,cyberpunk-yellow-3 :foreground ,cyberpunk-black))))
   `(org-column-title ((,class (:background ,cyberpunk-bg-1 :underline t :weight bold))))
   `(org-block ((,class (:foreground ,cyberpunk-fg :background ,cyberpunk-bg-05))))
   `(org-block-begin-line 
     ((,class (:foreground "#008ED1" :background ,cyberpunk-bg-1))))
   `(org-block-background ((,class (:background ,cyberpunk-bg-05))))
   `(org-block-end-line 
     ((,class (:foreground "#008ED1" :background ,cyberpunk-bg-1))))

   ;; `(org-deadline-announce ((,class (:foreground ,cyberpunk-red-1))))
   ;; `(org-scheduled ((,class (:foreground ,cyberpunk-green+4))))
   ;; `(org-scheduled-previously ((,class (:foreground ,cyberpunk-red-4))))
   ;; `(org-scheduled-today ((,class (:foreground ,cyberpunk-blue+1))))
   ;; `(org-special-keyword ((,class (:foreground ,cyberpunk-yellow-1))))
   ;; `(org-table ((,class (:foreground ,cyberpunk-green+2))))
   ;; `(org-time-grid ((,class (:foreground ,cyberpunk-orange))))
   ;; `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   ;; `(org-warning ((,class (:bold t :foreground ,cyberpunk-red :weight bold :underline nil))))
   ;; `(org-formula ((,class (:foreground ,cyberpunk-yellow-2))))
   ;; `(org-headline-done ((,class (:foreground ,cyberpunk-green+3))))
   ;; `(org-hide ((,class (:foreground ,cyberpunk-bg-1))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,cyberpunk-red-1))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,cyberpunk-green-2))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,cyberpunk-pink-1))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,cyberpunk-yellow))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,cyberpunk-green))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,cyberpunk-blue-3))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,cyberpunk-orange))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,cyberpunk-blue-2))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,cyberpunk-gray))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,cyberpunk-white))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,cyberpunk-blue+1))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,cyberpunk-red-4))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,cyberpunk-green))))
   `(rpm-spec-doc-face ((,class (:foreground ,cyberpunk-green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,cyberpunk-red))))
   `(rpm-spec-macro-face ((,class (:foreground ,cyberpunk-yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,cyberpunk-red))))
   `(rpm-spec-package-face ((,class (:foreground ,cyberpunk-red))))
   `(rpm-spec-section-face ((,class (:foreground ,cyberpunk-yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,cyberpunk-blue))))
   `(rpm-spec-var-face ((,class (:foreground ,cyberpunk-red))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:foreground ,cyberpunk-orange))))
   `(rst-level-2-face ((,class (:foreground ,cyberpunk-green+1))))
   `(rst-level-3-face ((,class (:foreground ,cyberpunk-blue-1))))
   `(rst-level-4-face ((,class (:foreground ,cyberpunk-yellow-2))))
   `(rst-level-5-face ((,class (:foreground ,cyberpunk-cyan))))
   `(rst-level-6-face ((,class (:foreground ,cyberpunk-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,cyberpunk-red-3 :background ,cyberpunk-black))))
   `(show-paren-match ((,class (:foreground ,cyberpunk-black :background ,cyberpunk-pink-1))))

   `(naeu-green-face ((,class (:foreground ,cyberpunk-green :background ,cyberpunk-black))))
   `(naeu-pink-face ((,class (:foreground ,cyberpunk-pink-1 :background ,cyberpunk-black))))
   `(naeu-blue-face ((,class (:foreground ,cyberpunk-blue-1 :background ,cyberpunk-black))))
   `(naeu-orange-face ((,class (:foreground ,cyberpunk-yellow-1 :background ,cyberpunk-black))))
   `(naeu-red-face ((,class (:foreground ,cyberpunk-orange :background ,cyberpunk-black))))
   `(naeu-grey-face ((,class (:foreground ,cyberpunk-gray-7 :background ,cyberpunk-black))))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,cyberpunk-red))))

  ;;; ansi-term
   `(term-color-black ((,class (:foreground ,cyberpunk-bg
                                            :background ,cyberpunk-bg-1))))
   `(term-color-red ((,class (:foreground ,cyberpunk-red-2
                                          :background ,cyberpunk-red-4))))
   `(term-color-green ((,class (:foreground ,cyberpunk-green
                                            :background ,cyberpunk-green+2))))
   `(term-color-yellow ((,class (:foreground ,cyberpunk-orange
                                             :background ,cyberpunk-yellow))))
   `(term-color-blue ((,class (:foreground ,cyberpunk-blue-1
                                           :background ,cyberpunk-blue-4))))
   `(term-color-magenta ((,class (:foreground ,cyberpunk-magenta
                                              :background ,cyberpunk-red))))
   `(term-color-cyan ((,class (:foreground ,cyberpunk-cyan
                                           :background ,cyberpunk-blue))))
   `(term-color-white ((,class (:foreground ,cyberpunk-fg
                                            :background ,cyberpunk-bg-1))))
   `(term-default-fg-color ((,class (:inherit term-color-white))))
   `(term-default-bg-color ((,class (:inherit term-color-black))))

   ;; volatile-highlights
   `(vhl/default-face ((,class (:background ,cyberpunk-gray-5))))

   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,cyberpunk-pink-1 :background ,cyberpunk-black))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,cyberpunk-bg :foreground ,cyberpunk-bg+1))))
   `(whitespace-hspace ((,class (:background ,cyberpunk-bg :foreground ,cyberpunk-bg+1))))
   `(whitespace-tab ((,class (:background ,cyberpunk-bg :foreground ,cyberpunk-red))))
   `(whitespace-newline ((,class (:foreground ,cyberpunk-bg+1))))
   `(whitespace-trailing ((,class (:foreground ,cyberpunk-red :background ,cyberpunk-bg))))
   `(whitespace-line ((,class (:background ,cyberpunk-bg-05 :foreground ,cyberpunk-magenta))))
   `(whitespace-space-before-tab ((,class (:background ,cyberpunk-orange :foreground ,cyberpunk-orange))))
   `(whitespace-indentation ((,class (:background ,cyberpunk-yellow :foreground ,cyberpunk-red))))
   `(whitespace-empty ((,class (:background ,cyberpunk-yellow :foreground ,cyberpunk-red))))
   `(whitespace-space-after-tab ((,class (:background ,cyberpunk-yellow :foreground ,cyberpunk-red))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,cyberpunk-red-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,cyberpunk-red-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,cyberpunk-orange))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,cyberpunk-blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,cyberpunk-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,cyberpunk-blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,cyberpunk-red-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,cyberpunk-red))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,cyberpunk-green+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,cyberpunk-blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,cyberpunk-blue+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,cyberpunk-green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,cyberpunk-red+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,cyberpunk-green+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,cyberpunk-green+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,cyberpunk-green+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,cyberpunk-green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,cyberpunk-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,cyberpunk-blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,cyberpunk-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,cyberpunk-blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,cyberpunk-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,cyberpunk-yellow))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,cyberpunk-magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,cyberpunk-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((,class (:foreground ,cyberpunk-green+4))))

   ;; yasnippet
   `(yas/field-highlight-face ((,class (:background ,cyberpunk-pink-1 :foreground ,cyberpunk-black))))

   ;; enh-ruby-mode enh-ruby-op-face
   `(enh-ruby-op-face ((,class (:foreground ,cyberpunk-blue-7))))
   `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,cyberpunk-green+2))))
   `(enh-ruby-string-delimiter-face ((,class (:foreground ,cyberpunk-green+2))))
   `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,cyberpunk-blue-1))))

   ;; yascroll
   `(yascroll:thumb-text-area ((,class (:background ,cyberpunk-bg-1))))
   `(yascroll:thumb-fringe ((,class (:background ,cyberpunk-bg-1 :foreground ,cyberpunk-bg-1))))
   )

  ;;; custom theme variables
  (custom-theme-set-variables
   'cyberpunk
   `(ansi-color-names-vector [,cyberpunk-bg ,cyberpunk-red-2 ,cyberpunk-green ,cyberpunk-orange
                                          ,cyberpunk-blue-1 ,cyberpunk-magenta ,cyberpunk-cyan ,cyberpunk-fg])
   ;; fill-column-indicator
   `(fci-rule-color ,cyberpunk-bg-05)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'cyberpunk)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; cyberpunk-theme.el ends here.
