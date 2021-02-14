;;; gruvbox-black.el --- Custom variant of grubox with black background

;;; Commentary:

;;; Custom variant of gruvbox with a black background

;;; Code:

(require 'autothemer)

(autothemer-deftheme gruvbox-black "A theme that uses gruvbox colors but is a black background"

 ((((class color) (min-colors #xFFFFFF))) ;; Only concerned about graphical emacs

  ;; Define our color palette
  (gb-background  "#000000")
  (gb-foreground  "#ebdbb2")
  (gb-black       "#181818")
  (gb-red         "#cc241d")
  (gb-green       "#98971a")
  (gb-yellow      "#d79921")
  (gb-blue        "#458588")
  (gb-magenta     "#b16286")
  (gb-cyan        "#689d6a")
  (gb-lgt-grey    "#a89984")
  (gb-drk-grey    "#928374")
  (gb-lgt-red     "#fb4934")
  (gb-lgt-green   "#b8bb26")
  (gb-lgt-yellow  "#fabd2f")
  (gb-lgt-blue    "#83a598")
  (gb-lgt-magenta "#d3869b")
  (gb-lgt-cyan    "#8ec07c")
  (gb-white       "#ebdbb2"))

 (
  ;; Basic UI Components
  (default (:foregroud gb-foreground :background gb-background))
  (cursor  (:background gb-foreground))
  (line-number (:foreground gb-black))
  (line-number-current-line (:foreground gb-yellow))
  (window-divider (:foreground gb-black))

  ;; Code Syntax
  (font-lock-keyword-face (:foreground gb-magenta))
  (font-lock-function-name-face (:foreground gb-blue))
  (font-lock-variable-name-face (:foreground gb-yellow))
  (font-lock-constant-face (:foreground gb-blue))
  (font-lock-string-face (:foreground gb-green))
  (font-lock-builtin-face (:foreground gb-cyan))
  (font-lock-comment-face (:foreground gb-drk-grey))

  ;; Modeline
  (mode-line (:background gb-black))
  (mode-line-inactive (:background "#090909"))

  ;; Org Mode
  (org-document-title (:foreground gb-green))
  (org-document-info (:foreground gb-green))
  (org-level-1 (:foreground gb-blue))
  (org-level-2 (:foreground gb-green))
  (org-level-3 (:foreground gb-yellow))
  (org-level-4 (:foreground gb-cyan))
  (org-level-5 (:foreground gb-magenta))
  (org-ellipsis (:foreground gb-drk-grey :underline nil))
  (org-block-begin-line (:background gb-black))
  (org-block-end-line (:background gb-black))
  (org-block (:background "#090909"))

  ))

(provide-theme 'gruvbox-black)

;;; gruvbox-black.el ends here
