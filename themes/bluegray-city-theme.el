;;; bluegray-city-theme.el --- inspired by Atom City Lights -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup bluegray-city-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom bluegray-city-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'bluegray-city-theme
  :type 'boolean)

(defcustom bluegray-city-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'bluegray-city-theme
  :type 'boolean)

(defcustom bluegray-city-comment-bg bluegray-city-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'bluegray-city-theme
  :type 'boolean)

(defcustom bluegray-city-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'bluegray-city-theme
  :type '(choice integer boolean))

;;
(def-doom-theme bluegray-city
  "A dark theme inspired by Atom City Lights"

  ;; name        default   256       16
  ((bg         '("#0A0D0F" nil       nil            ))
   (bg-alt     '("#000000" nil       nil            ))
   (base0      '("#10151C" "black"   "black"        ))
   (base1      '("#171D22" "#111122" "brightblack"  ))
   (base2      '("#20282F" "#222222" "brightblack"  ))
   (base3      '("#28323B" "#223333" "brightblack"  ))
   (base4      '("#384551" "#334455" "brightblack"  ))
   (base5      '("#56697A" "#556677" "brightblack"  ))
   (base6      '("#688094" "#668899" "brightblack"  ))
   (base7      '("#7FA0B7" "#77AABB" "brightblack"  ))
   (base8      '("#9CAABB" "#99AABB" "white"        ))
   (fg-alt     '("#728CA0" "#7788AA" "brightwhite"  ))
   (fg         '("#ffffff" "#ffffff" "white"        ))

   (grey        '("#546778" "#ff6655" "red"          ))
   (red         '("#D95468" "#ff6655" "red"          ))
   (orange      '("#D98E48" "#dd8844" "brightred"    ))
   (green       '("#8BD49C" "#99bb66" "green"        ))
   (teal        '("#33CED8" "#33CCDD" "brightgreen"  ))
   (yellow      '("#EBBF83" "#EEBB88" "yellow"       ))
   (blue        '("#5EC4FF" "#55CCFF" "brightblue"   ))
   (bright-blue '("#539AFC" "#5599FF" "blue"         ))
   (dark-blue   '("#7a9db8" "#7788AA" "blue"         ))
   (magenta     '("#E27E8D" "#EE7788" "magenta"      ))
   (violet      '("#CD7297" "#CC7799" "brightmagenta"))
   (cyan        '("#70E1E8" "#77EEEE" "brightcyan"   ))
   (dark-cyan   '("#008B94" "#008899" "cyan"   ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if bluegray-city-brighter-comments dark-cyan grey))
   (doc-comments   (doom-lighten (if bluegray-city-brighter-comments dark-cyan grey) 0.25))
   (constants      magenta)
   (functions      teal)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        bright-blue)
   (variables      blue)
   (numbers        magenta)
   (region         base5)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright bluegray-city-brighter-modeline)
   (-modeline-pad
    (when bluegray-city-padded-modeline
      (if (integerp bluegray-city-padded-modeline) bluegray-city-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if bluegray-city-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; magit-mode
   (magit-diff-hunk-heading-highlight :foreground fg :background base4 :weight 'bold)
   (magit-diff-hunk-heading :foreground fg-alt :background base3 :weight 'normal)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;; org-mode
   ((org-block &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   ;; tooltip and company
   (tooltip              :background bg-alt :foreground fg)
   (company-tooltip-selection     :background base3)

   ;; Ivy
   (ivy-minibuffer-match-face-2 :foreground blue :weight 'bold)

   ;; js2-mode
   (js2-object-property :foreground dark-blue)
   (js2-object-property-access :foreground dark-cyan) 
   
   ;; rjsx-mode
   (rjsx-tag :foreground dark-cyan)
   (rjsx-attr :foreground cyan :slant 'italic :weight 'medium)

   ;; clojure-mode
   (clojure-keyword-face :foreground violet)
   (clojure-character-face :foreground teal)

   ;;rainbow
   (rainbow-delimiters-depth-1-face :foreground "#f00")
   (rainbow-delimiters-depth-2-face :foreground "#66e")
   (rainbow-delimiters-depth-3-face :foreground "#e66")
   (rainbow-delimiters-depth-4-face :foreground "#6f6")
   (rainbow-delimiters-depth-5-face :foreground "#ee6")
   (rainbow-delimiters-depth-6-face :foreground "#6ee")
   (rainbow-delimiters-depth-7-face :foreground "#e6e")
   (rainbow-delimiters-depth-8-face :foreground "#ff0")
   (rainbow-delimiters-depth-9-face :foreground "#0f0")
   (rainbow-delimiters-unmatched-face :foreground "#FFFF00" :background violet)
   (rainbow-delimiters-mismatched-face :foreground "#FFFF00" :background red)

   (show-paren-match :foreground "#ff0000" :background "#5982a6")

   (escape-glyph :foreground "#ff0000" :background "#ff0")
   )

  ;; --- extra variables ---------------------
  ()
  )

;;; bluegray-city-theme.el ends here
