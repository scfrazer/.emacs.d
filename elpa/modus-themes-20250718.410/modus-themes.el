;;; modus-themes.el --- Elegant, highly legible and customizable themes -*- lexical-binding:t -*-

;; Copyright (C) 2019-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/modus-themes
;; Package-Version: 20250718.410
;; Package-Revision: f4e67c80c94f
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  Please refer to the official Info manual for
;; further documentation (distributed with the themes, or available
;; at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:



(eval-when-compile (require 'subr-x))

(defgroup modus-themes ()
  "User options for the Modus themes.
The Modus themes conform with the WCAG AAA standard for color
contrast between background and foreground combinations (a
minimum contrast of 7:1---the highest standard of its kind).

The Modus themes collection includes themes that are optimized
for people with red-green or blue-yellow color
deficiency (deuteranopia or tritanopia, respectively)."
  :group 'faces
  :link '(info-link "(modus-themes) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/modus-themes")
  :link '(url-link :tag "Sample pictures" "https://protesilaos.com/emacs/modus-themes-pictures")
  :prefix "modus-themes-"
  :tag "Modus Themes")

(defgroup modus-themes-faces ()
  "Faces defined by the Modus themes."
  :group 'modus-themes
  :link '(info-link "(modus-themes) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/modus-themes")
  :link '(url-link :tag "Sample pictures" "https://protesilaos.com/emacs/modus-themes-pictures")
  :prefix "modus-themes-"
  :tag "Modus Themes Faces")



;;;; Custom faces

;; These faces are used internally to ensure consistency between various
;; groups and to streamline the evaluation of relevant customization
;; options.

(dolist (color '( red green blue yellow magenta cyan
                  red-warmer green-warmer blue-warmer yellow-warmer magenta-warmer cyan-warmer
                  red-cooler green-cooler blue-cooler yellow-cooler magenta-cooler cyan-cooler
                  red-faint green-faint blue-faint yellow-faint magenta-faint cyan-faint
                  red-intense green-intense blue-intense yellow-intense magenta-intense cyan-intense))
  (custom-declare-face
   (intern (format "modus-themes-fg-%s" color))
   nil (format "Face with %s foreground." color)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (color '(red green yellow blue magenta cyan))
  (custom-declare-face
   (intern (format "modus-themes-nuanced-%s" color))
   nil (format "Nuanced %s background." color)
   :package-version '(modus-themes . "4.1.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (color '(red green yellow blue magenta cyan))
  (custom-declare-face
   (intern (format "modus-themes-subtle-%s" color))
   nil (format "Subtle %s background." color)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (color '(red green yellow blue magenta cyan))
  (custom-declare-face
   (intern (format "modus-themes-intense-%s" color))
   nil (format "Intense %s background." color)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (scope '(alt del sel))
  (custom-declare-face
   (intern (format "modus-themes-mark-%s" scope))
   nil (format "Mark of type %s." scope)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (scope '(note warning error))
  (custom-declare-face
   (intern (format "modus-themes-lang-%s" scope))
   nil (format "Linter or spell check of type %s." scope)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (scope '(note warning error))
  (custom-declare-face
   (intern (format "modus-themes-prominent-%s" scope))
   nil (format "Prominent notification of type %s." scope)
   :package-version '(modus-themes . "4.2.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (scope '(current lazy replace))
  (custom-declare-face
   (intern (format "modus-themes-search-%s" scope))
   nil (format "Search of type %s." scope)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dotimes (n 4)
  (custom-declare-face
   (intern (format "modus-themes-search-rx-group-%s" n))
   nil (format "Search regexp group number %s." n)
   :package-version '(modus-themes . "4.4.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dolist (scope '(code macro verbatim))
  (custom-declare-face
   (intern (format "modus-themes-prose-%s" scope))
   nil (format "Construct of type %s for prose." scope)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(dotimes (n 9)
  (custom-declare-face
   (intern (format "modus-themes-heading-%d" n))
   nil (format "Level %d heading." n)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(defface modus-themes-bold nil
  "Generic face for applying a conditional bold weight.
This behaves in accordance with `modus-themes-bold-constructs'."
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-slant nil
  "Generic face for applying a conditional slant (italics).
This behaves in accordance with `modus-themes-italic-constructs'."
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-key-binding nil
  "Face for key bindings."
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-fixed-pitch nil
  "Face for `fixed-pitch' if `modus-themes-mixed-fonts' is non-nil."
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-ui-variable-pitch nil
  "Face for `variable-pitch' if `modus-themes-variable-pitch-ui' is non-nil."
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-reset-soft nil
  "Generic face to set most face properties to nil.

This is intended to be inherited by faces that should not retain
properties from their context (e.g. an overlay over an underlined
text should not be underlined as well) yet still blend in."
  :group 'modus-themes-faces)

(defface modus-themes-prompt nil
  "Generic face for command prompts."
  :group 'modus-themes-faces)

(defface modus-themes-completion-selected nil
  "Face for current selection in completion UIs."
  :group 'modus-themes-faces)

(defface modus-themes-button nil
  "Face for graphical buttons."
  :group 'modus-themes-faces)

(dotimes (n 4)
  (custom-declare-face
   (intern (format "modus-themes-completion-match-%d" n))
   nil (format "Completions match level %d." n)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))



;;;; Customization variables

(defcustom modus-themes-custom-auto-reload t
  "Automatically reload theme after setting options with Customize.

All theme user options take effect when a theme is loaded.  Any
subsequent changes require the theme to be reloaded.

When this variable has a non-nil value, any change made via the
Custom UI or related functions such as `customize-set-variable'
and `setopt' (Emacs 29), will trigger a reload automatically.

With a nil value, changes to user options have no further
consequences.  The user must manually reload the theme."
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Custom reload theme"))

(defun modus-themes--set-option (sym val)
  "Custom setter for theme related user options.
Will set SYM to VAL, and reload the current theme, unless
`modus-themes-custom-auto-reload' is nil."
  (set-default sym val)
  (when (and modus-themes-custom-auto-reload
             ;; Check if a theme is being loaded, in which case we
             ;; don't want to reload a theme if the setter is
             ;; invoked. `custom--inhibit-theme-enable' is set to nil
             ;; by `enable-theme'.
             (bound-and-true-p custom--inhibit-theme-enable))
    (when-let* ((modus-themes-custom-auto-reload t)
                (theme (modus-themes--current-theme)))
      (modus-themes-load-theme theme))))

(defcustom modus-themes-disable-other-themes t
  "Disable all other themes when loading a Modus theme.

When the value is non-nil, the commands `modus-themes-toggle' and
`modus-themes-select', as well as the `modus-themes-load-theme'
function, will disable all other themes while loading the
specified Modus theme.  This is done to ensure that Emacs does
not blend two or more themes: such blends lead to awkward results
that undermine the work of the designer.

When the value is nil, the aforementioned commands and function
will only disable other themes within the Modus collection.

This option is provided because Emacs themes are not necessarily
limited to colors/faces: they can consist of an arbitrary set of
customizations.  Users who use such customization bundles must
set this variable to a nil value."
  :group 'modus-themes
  :package-version '(modus-themes . "4.1.0")
  :version "30.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Disable other themes"))

(defvaralias 'modus-themes-collection 'modus-themes-items
  "Alias of `modus-themes-items'.")

(defconst modus-themes-items
  '( modus-operandi
     modus-operandi-tinted
     modus-operandi-deuteranopia
     modus-operandi-tritanopia
     modus-vivendi
     modus-vivendi-tinted
     modus-vivendi-deuteranopia
     modus-vivendi-tritanopia)
  "Symbols of the Modus themes.")

(defcustom modus-themes-to-toggle '(modus-operandi modus-vivendi)
  "Specify two Modus themes for `modus-themes-toggle' command.
The variable `modus-themes-items' contains the symbols of all
official themes that form part of this collection.

The default value of this user option includes the original
themes: `modus-operandi' (light) and `modus-vivendi' (dark).

If the value is nil or otherwise does not specify two valid Modus
themes, the command `modus-themes-toggle' reverts to selecting a
theme from the list of available Modus themes.  In effect, it is
the same as using the command `modus-themes-select'."
  :type (let ((themes (mapcar (lambda (theme) (list 'const theme)) modus-themes-items)))
          `(choice
            (const :tag "No toggle" nil)
            (list :tag "Pick two themes to toggle between"
                  (choice :tag "Theme one of two" ,@themes)
                  (choice :tag "Theme two of two" ,@themes))))
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes)

(defcustom modus-themes-to-rotate modus-themes-items
  "List of Modus themes to rotate among, per `modus-themes-rotate'."
  :type `(repeat
          (choice :tag "A theme among the `modus-themes-items'"
                  ,@(mapcar (lambda (theme) (list 'const theme)) modus-themes-items)))
  :package-version '(modus-themes . "4.6.0")
  :version "31.1"
  :group 'modus-themes)

(defvaralias 'modus-themes-post-load-hook 'modus-themes-after-load-theme-hook
  "Alias for `modus-themes-after-load-theme-hook'.")

(defcustom modus-themes-after-load-theme-hook nil
  "Hook that runs after loading a Modus theme.
This is used by the commands `modus-themes-toggle',
`modus-themes-rotate', `modus-themes-select', as well as the function
`modus-themes-load-theme'."
  :type 'hook
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes)

(defvaralias 'modus-themes-slanted-constructs 'modus-themes-italic-constructs)

(defcustom modus-themes-italic-constructs nil
  "Use italic font forms in more code constructs."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Italic constructs"))

(defcustom modus-themes-bold-constructs nil
  "Use bold text in more code constructs."
  :group 'modus-themes
  :package-version '(modus-themes . "1.0.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Bold constructs"))

(defcustom modus-themes-variable-pitch-ui nil
  "Use proportional fonts (variable-pitch) in UI elements.
This includes the mode line, header line, tab bar, and tab line."
  :group 'modus-themes
  :package-version '(modus-themes . "1.1.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) UI typeface"))

(defcustom modus-themes-mixed-fonts nil
  "Non-nil to enable inheritance from `fixed-pitch' in some faces.

This is done to allow spacing-sensitive constructs, such as Org
tables and code blocks, to remain monospaced when users opt for
something like the command `variable-pitch-mode'.

Users may need to explicitly configure the font family of
`fixed-pitch' in order to get a consistent experience with their
typography (also check the `fontaine' package on GNU ELPA (by
Protesilaos))."
  :group 'modus-themes
  :package-version '(modus-themes . "1.7.0")
  :version "29.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Mixed fonts"))

(defconst modus-themes--weight-widget
  '(choice :tag "Font weight (must be supported by the typeface)"
           (const :tag "Unspecified (use whatever the default is)" nil)
           (const :tag "Thin" thin)
           (const :tag "Ultra-light" ultralight)
           (const :tag "Extra-light" extralight)
           (const :tag "Light" light)
           (const :tag "Semi-light" semilight)
           (const :tag "Regular" regular)
           (const :tag "Medium" medium)
           (const :tag "Semi-bold" semibold)
           (const :tag "Bold" bold)
           (const :tag "Extra-bold" extrabold)
           (const :tag "Ultra-bold" ultrabold))
  "List of supported font weights used by `defcustom' forms.")

(defconst modus-themes--headings-widget
  `(set :tag "Properties" :greedy t
        (const :tag "Proportionately spaced font (variable-pitch)" variable-pitch)
        ,modus-themes--weight-widget
        (radio :tag "Height"
               (float :tag "Floating point to adjust height by")
               (cons :tag "Cons cell of `(height . FLOAT)'"
                     (const :tag "The `height' key (constant)" height)
                     (float :tag "Floating point"))))
  "Refer to the doc string of `modus-themes-headings'.
This is a helper variable intended for internal use.")

(defcustom modus-themes-headings nil
  "Heading styles with optional list of values per heading level.

This is an alist that accepts a (KEY . LIST-OF-VALUES)
combination.  The KEY is either a number, representing the
heading's level (0-8) or t, which pertains to the fallback style.
The named keys `agenda-date' and `agenda-structure' apply to the
Org agenda.

Level 0 is used for what counts as a document title or
equivalent, such as the #+title construct we find in Org files.
Levels 1-8 are regular headings.

The LIST-OF-VALUES covers symbols that refer to properties, as
described below.  Here is a complete sample with various
stylistic combinations, followed by a presentation of all
available properties:

    (setq modus-themes-headings
          (quote ((1 . (variable-pitch 1.5))
                  (2 . (1.3))
                  (agenda-date . (1.3))
                  (agenda-structure . (variable-pitch light 1.8))
                  (t . (1.1)))))

By default (a nil value for this variable), all headings have a
bold typographic weight, use a desaturated text color, have a
font family that is the same as the `default' face (typically
monospaced), and a height that is equal to the `default' face's
height.

A `variable-pitch' property changes the font family of the
heading to that of the `variable-pitch' face (normally a
proportionately spaced typeface).

The symbol of a weight attribute adjusts the font of the heading
accordingly, such as `light', `semibold', etc.  Valid symbols are
defined in the variable `modus-themes-weights'.  The absence of a
weight means that bold will be used by virtue of inheriting the
`bold' face (check the manual for tweaking bold and italic
faces).

A number, expressed as a floating point (e.g. 1.5), adjusts the
height of the heading to that many times the base font size.  The
default height is the same as 1.0, though it need not be
explicitly stated.  Instead of a floating point, an acceptable
value can be in the form of a cons cell like (height . FLOAT)
or (height FLOAT), where FLOAT is the given number.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (semibold)
    (variable-pitch semibold 1.3)
    (variable-pitch semibold (height 1.3)) ; same as above
    (variable-pitch semibold (height . 1.3)) ; same as above

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-headings
          (quote ((1 . (variable-pitch 1.5))
                  (2 . (1.3))
                  (agenda-date . (1.3))
                  (agenda-structure . (variable-pitch light 1.8))
                  (t . (1.1)))))

When defining the styles per heading level, it is possible to
pass a non-nil value (t) instead of a list of properties.  This
will retain the original aesthetic for that level.  For example:

    (setq modus-themes-headings
          (quote ((1 . t)           ; keep the default style
                  (2 . (semibold 1.2))
                  (t . (variable-pitch))))) ; style for all other headings

    (setq modus-themes-headings
          (quote ((1 . (variable-pitch extrabold 1.5))
                  (2 . (semibold))
                  (t . t)))) ; default style for all other levels

Note that the text color of headings, of their background, and
overline can all be set via the overrides.  It is possible to
have any color combination for any heading level (something that
could not be done in older versions of the themes).

Read Info node `(modus-themes) Option for palette overrides' as
well as Info node `(modus-themes) Make headings more or less
colorful'.  Else check `modus-themes-common-palette-overrides'
and related user options."
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type `(alist
          :options ,(mapcar (lambda (el)
                              (list el modus-themes--headings-widget))
                            '(0 1 2 3 4 5 6 7 8 t agenda-date agenda-structure))
          :key-type symbol
          :value-type ,modus-themes--headings-widget)
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Heading styles"))

(make-obsolete-variable 'modus-themes-org-blocks nil "4.4.0: Use palette overrides")

(defcustom modus-themes-completions nil
  "Control the style of completion user interfaces.

This affects Company, Corfu, Flx, Icomplete/Fido, Ido, Ivy,
Orderless, Vertico, and the standard *Completions* buffer.  The
value is an alist of expressions, each of which takes the form
of (KEY . LIST-OF-PROPERTIES).  KEY is a symbol, while PROPERTIES
is a list.  Here is a sample, followed by a description of the
particularities:

    (setq modus-themes-completions
          (quote ((matches . (extrabold underline))
                  (selection . (semibold italic)))))

The `matches' key refers to the highlighted characters that
correspond to the user's input.  When its properties are nil or
an empty list, matching characters in the user interface will
have a bold weight and a colored foreground.  The list of
properties may include any of the following symbols regardless of
the order they may appear in:

- `underline' to draw a line below the characters;

- `italic' to use a slanted font (italic or oblique forms);

- The symbol of a font weight attribute such as `light',
  `semibold', et cetera.  Valid symbols are defined in the
  variable `modus-themes-weights'.  The absence of a weight means
  that bold will be used.

The `selection' key applies to the current line or currently
matched candidate, depending on the specifics of the user
interface.  When its properties are nil or an empty list, it has
a subtle gray background, a bold weight, and the base foreground
value for the text.  The list of properties it accepts is as
follows (order is not significant):

- `underline' to draw a line below the characters;

- `italic' to use a slanted font (italic or oblique forms);

- The symbol of a font weight attribute such as `light',
  `semibold', et cetera.  Valid symbols are defined in the
  variable `modus-themes-weights'.  The absence of a weight means
  that bold will be used.

Apart from specifying each key separately, a catch-all list is
accepted.  This is only useful when the desired aesthetic is the
same across all keys that are not explicitly referenced.  For
example, this:

    (setq modus-themes-completions
          (quote ((t . (extrabold underline)))))

Is the same as:

    (setq modus-themes-completions
          (quote ((matches . (extrabold underline))
                  (selection . (extrabold underline)))))"
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type `(set
          (cons :tag "Matches"
                (const matches)
                (set :tag "Style of matches" :greedy t
                     ,modus-themes--weight-widget
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline)))
          (cons :tag "Selection"
                (const selection)
                (set :tag "Style of selection" :greedy t
                     ,modus-themes--weight-widget
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline)))
          (cons :tag "Fallback for both matches and selection"
                (const t)
                (set :tag "Style of both matches and selection" :greedy t
                     ,modus-themes--weight-widget
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline))))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Completion UIs"))

(defcustom modus-themes-prompts nil
  "Use subtle or intense styles for minibuffer and REPL prompts.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) means to only use a
subtle colored foreground color.

The `italic' property adds a slant to the font's forms (italic or
oblique forms, depending on the typeface).

The symbol of a font weight attribute such as `light', `semibold',
et cetera, adds the given weight to links.  Valid symbols are
defined in the variable `modus-themes-weights'.  The absence of a
weight means that the one of the underlying text will be used.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (bold italic)
    (italic semibold)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-prompts (quote (extrabold italic)))"
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type `(set :tag "Properties" :greedy t
              (const :tag "Italic font slant" italic)
              ,modus-themes--weight-widget)
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Command prompts"))


(defcustom modus-themes-common-palette-user nil
  "Common user-defined colors to extend all the themes' palettes.
This is meant to extend the palette of the active Modus theme with
custom named colors and/or semantic palette mappings.  Those may then be
used in combination with palette overrides (see
`modus-themes-common-palette-overrides')."
  :group 'modus-themes
  :package-version '(modus-themes . "4.5.0")
  :type '(repeat (list symbol (choice symbol string)))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Extend the palette for use with overrides"))

(defcustom modus-themes-common-palette-overrides nil
  "Set palette overrides for all the Modus themes.

Mirror the elements of a theme's palette, overriding their value.
The palette variables are named THEME-NAME-palette, while
individual theme overrides are THEME-NAME-palette-overrides.  The
THEME-NAME is one of the symbols in `modus-themes-items'.  For
example:

- `modus-operandi-palette'
- `modus-operandi-palette-overrides'

Individual theme overrides take precedence over these common
overrides.

The idea of common overrides is to change semantic color
mappings, such as to make the cursor red.  Wherea theme-specific
overrides can also be used to change the value of a named color,
such as what hexadecimal RGB value the red-warmer symbol
represents."
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type '(repeat (list symbol (choice symbol string)))
  ;; ;; NOTE 2023-01-07: The following is a functioning version of the
  ;; ;; intended :type.  However, I think the Custom UI is really
  ;; ;; awkward for this specific case.  Maybe the generic type I have
  ;; ;; above is better, as it encourages the user to write out the
  ;; ;; code and read the manual.  Counter-arguments are welcome.
  ;;
  ;; :type `(repeat (list (radio :tag "Palette key to override"
  ;;                             ,@(mapcar (lambda (x)
  ;;                                         (list 'const x))
  ;;                                       (mapcar #'car (modus-themes--current-theme-palette))))
  ;;                      (choice :tag "Value to assign" :value unspecified
  ;;                              (const :tag "`unspecified' (remove the original color)" unspecified)
  ;;                              (string :tag "String with color name (e.g. \"gray50\") or hex RGB (e.g. \"#123456\")"
  ;;                                      :match-inline (color-supported-p val))
  ;;                              (radio :tag "Palette key to map to"
  ;;                                     ,@(mapcar (lambda (x)
  ;;                                                 (list 'const x))
  ;;                                               (mapcar #'car (modus-themes--current-theme-palette)))))))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Palette overrides"))



;;;; Presets of palette overrides

(defvar modus-themes-preset-overrides-faint
  '((bg-completion       bg-inactive)
    (bg-hl-line          bg-dim)
    (bg-paren-match      bg-cyan-subtle)
    (bg-region           bg-active)

    (bg-mode-line-active        bg-inactive)
    (border-mode-line-active    fg-dim)
    (bg-mode-line-inactive      bg-dim)
    (border-mode-line-inactive  bg-active)

    (bg-tab-bar      bg-inactive)
    (bg-tab-current  bg-main)
    (bg-tab-other    bg-active)

    (fringe unspecified)
    (builtin maroon)
    (comment fg-dim)
    (constant blue-faint)
    (docstring fg-alt)
    (docmarkup magenta-faint)
    (fnname pink)
    (keyword indigo)
    (preprocessor rust)
    (string slate)
    (type cyan-faint)
    (variable cyan-faint)
    (rx-construct gold)
    (rx-backslash olive)

    (underline-err red-faint)
    (underline-warning yellow-faint)
    (underline-note cyan-faint)

    (bg-button-active bg-main)
    (fg-button-active fg-main)
    (bg-button-inactive bg-inactive)
    (fg-button-inactive "gray50")

    (date-common cyan-faint)
    (date-deadline red-faint)
    (date-event fg-alt)
    (date-holiday magenta)
    (date-now fg-main)
    (date-scheduled yellow-faint)
    (date-weekday fg-dim)
    (date-weekend fg-dim)

    (name maroon)
    (identifier fg-dim)

    (fg-line-number-active fg-main)
    (fg-line-number-inactive "gray50")
    (bg-line-number-active unspecified)
    (bg-line-number-inactive unspecified)

    (fg-link blue-faint)
    (bg-link unspecified)
    (underline-link bg-active)

    (fg-link-symbolic cyan-faint)
    (bg-link-symbolic unspecified)
    (underline-link-symbolic bg-active)

    (fg-link-visited magenta-faint)
    (bg-link-visited unspecified)
    (underline-link-visited bg-active)

    (mail-cite-0 cyan-faint)
    (mail-cite-1 yellow-faint)
    (mail-cite-2 green-faint)
    (mail-cite-3 red-faint)
    (mail-part olive)
    (mail-recipient indigo)
    (mail-subject maroon)
    (mail-other slate)

    (fg-prompt cyan-faint)

    (fg-prose-code olive)
    (fg-prose-macro indigo)
    (fg-prose-verbatim maroon)

    (prose-done green-faint)
    (prose-tag rust)
    (prose-todo red-faint)

    (rainbow-0 fg-main)
    (rainbow-1 magenta)
    (rainbow-2 cyan)
    (rainbow-3 red-faint)
    (rainbow-4 yellow-faint)
    (rainbow-5 magenta-cooler)
    (rainbow-6 green)
    (rainbow-7 blue-warmer)
    (rainbow-8 magenta-faint))
  "Preset for palette overrides with faint coloration.

This changes many parts of the theme to make them look less
colorful/intense.  Grays are toned down, gray backgrounds are
removed from some contexts, and almost all accent colors are
desaturated.

All the preset overrides the themes provide (including this one):

- `modus-themes-preset-overrides-faint'
- `modus-themes-preset-overrides-intense'
- `modus-themes-preset-overrides-cooler'
- `modus-themes-preset-overrides-warmer'

To set a preset, assign its symbol without a quote as the value
of the `modus-themes-common-palette-overrides' or as the value of
theme-specific options such as `modus-operandi-palette-overrides'.

For overriding named colors and/or semantic color mappings read
Info node `(modus-themes) Option for palette overrides'.")

(defvar modus-themes-preset-overrides-intense
  '((bg-region bg-cyan-intense)

    (bg-completion       bg-cyan-subtle)
    (bg-hover            bg-yellow-intense)
    (bg-hover-secondary  bg-magenta-intense)
    (bg-hl-line          bg-cyan-subtle)

    (bg-mode-line-active      bg-blue-subtle)
    (fg-mode-line-active      fg-main)
    (border-mode-line-active  blue-intense)

    (fringe bg-inactive)
    (comment red-faint)

    (date-common cyan)
    (date-deadline red)
    (date-event blue)
    (date-holiday magenta-warmer)
    (date-now blue-faint)
    (date-range blue)
    (date-scheduled yellow-warmer)
    (date-weekday fg-main)
    (date-weekend red-faint)

    (keybind blue-intense)

    (mail-cite-0 blue)
    (mail-cite-1 yellow-cooler)
    (mail-cite-2 green-warmer)
    (mail-cite-3 magenta)
    (mail-part cyan)
    (mail-recipient magenta-cooler)
    (mail-subject red-warmer)
    (mail-other cyan-cooler)

    (fg-prompt blue-intense)

    (bg-prose-block-delimiter bg-dim)
    (fg-prose-block-delimiter red-faint)
    (prose-done green-intense)
    (prose-metadata magenta-faint)
    (prose-metadata-value blue-cooler)
    (prose-table blue)
    (prose-todo red-intense)

    (fg-heading-0 blue-cooler)
    (fg-heading-1 magenta-cooler)
    (fg-heading-2 magenta-warmer)
    (fg-heading-3 blue)
    (fg-heading-4 cyan)
    (fg-heading-5 green-warmer)
    (fg-heading-6 yellow)
    (fg-heading-7 red)
    (fg-heading-8 magenta)

    (bg-heading-0 unspecified)
    (bg-heading-1 bg-magenta-nuanced)
    (bg-heading-2 bg-red-nuanced)
    (bg-heading-3 bg-blue-nuanced)
    (bg-heading-4 bg-cyan-nuanced)
    (bg-heading-5 bg-green-nuanced)
    (bg-heading-6 bg-yellow-nuanced)
    (bg-heading-7 bg-red-nuanced)
    (bg-heading-8 bg-magenta-nuanced)

    (overline-heading-0 unspecified)
    (overline-heading-1 magenta-cooler)
    (overline-heading-2 magenta-warmer)
    (overline-heading-3 blue)
    (overline-heading-4 cyan)
    (overline-heading-5 green)
    (overline-heading-6 yellow-cooler)
    (overline-heading-7 red-cooler)
    (overline-heading-8 magenta))
  "Preset for palette overrides with intense coloration.

This changes many parts of the theme to make them look more
colorful/intense.  Many background colors are accented and
coloration is increased to pop out more.

All the preset overrides the themes provide (including this one):

- `modus-themes-preset-overrides-faint'
- `modus-themes-preset-overrides-intense'
- `modus-themes-preset-overrides-cooler'
- `modus-themes-preset-overrides-warmer'

To set a preset, assign its symbol without a quote as the value
of the `modus-themes-common-palette-overrides' or as the value of
theme-specific options such as `modus-operandi-palette-overrides'.

For overriding named colors and/or semantic color mappings read
Info node `(modus-themes) Option for palette overrides'.")

(defvar modus-themes-preset-overrides-cooler
  '((fg-prompt blue-cooler)

    (builtin magenta-faint)
    (constant blue-cooler)
    (fnname cyan-cooler)
    (keyword magenta-cooler)
    (preprocessor blue)
    (string blue-warmer)
    (type green-cooler)
    (variable cyan)
    (rx-construct blue-cooler)
    (rx-backslash red)

    (name blue-warmer)
    (identifier magenta-faint)

    (date-deadline magenta-cooler)
    (date-scheduled yellow-cooler)
    (date-weekday blue-faint)
    (date-weekend red-faint)

    (mail-cite-0 blue-faint)
    (mail-cite-1 cyan-cooler)
    (mail-cite-2 magenta-faint)
    (mail-cite-3 yellow-cooler)
    (mail-part cyan)
    (mail-recipient blue-warmer)
    (mail-subject magenta-cooler)
    (mail-other blue)

    (prose-tag fg-dim)
    (fg-prose-verbatim blue-cooler))
  "Preset of palette overrides with cooler colors.

This changes parts of the palette to use more blue and
blue-tinted colors.

All the preset overrides the themes provide (including this one):

- `modus-themes-preset-overrides-faint'
- `modus-themes-preset-overrides-intense'
- `modus-themes-preset-overrides-cooler'
- `modus-themes-preset-overrides-warmer'

To set a preset, assign its symbol without a quote as the value
of the `modus-themes-common-palette-overrides' or as the value of
theme-specific options such as `modus-operandi-palette-overrides'.

For overriding named colors and/or semantic color mappings read
Info node `(modus-themes) Option for palette overrides'.")

(defvar modus-themes-preset-overrides-warmer
  '((fg-prompt magenta-warmer)

    (builtin magenta)
    (constant blue-warmer)
    (fnname magenta-cooler)
    (keyword magenta-warmer)
    (preprocessor red-cooler)
    (string green-warmer)
    (type cyan-cooler)
    (variable cyan)
    (rx-construct blue-cooler)
    (rx-backslash red-warmer)

    (name blue-warmer)
    (identifier magenta)
    (keybind magenta-warmer)

    (accent-0 magenta-warmer)
    (accent-1 cyan)
    (accent-2 blue-warmer)
    (accent-3 red-cooler)

    (date-common cyan-cooler)
    (date-holiday magenta-warmer)

    (mail-cite-0 magenta-faint)
    (mail-cite-1 cyan-cooler)
    (mail-cite-2 green-warmer)
    (mail-cite-3 red-faint)
    (mail-part cyan)
    (mail-recipient magenta)
    (mail-subject blue-warmer)
    (mail-other magenta-warmer)

    (fg-prose-macro red-cooler)
    (prose-tag fg-dim))
  "Preset of palette overrides with warmer colors.

This changes many parts of the theme to use warmer colors,
including green and yellow.

All the preset overrides the themes provide (including this one):

- `modus-themes-preset-overrides-faint'
- `modus-themes-preset-overrides-intense'
- `modus-themes-preset-overrides-cooler'
- `modus-themes-preset-overrides-warmer'

To set a preset, assign its symbol without a quote as the value
of the `modus-themes-common-palette-overrides' or as the value of
theme-specific options such as `modus-operandi-palette-overrides'.

For overriding named colors and/or semantic color mappings read
Info node `(modus-themes) Option for palette overrides'.")



;;;; Helper functions for theme setup

;; This is the WCAG formula: https://www.w3.org/TR/WCAG20-TECHS/G18.html
(defun modus-themes--wcag-contribution (channel weight)
  "Return the CHANNEL contribution to overall luminance given WEIGHT."
  (* weight
     (if (<= channel 0.03928)
         (/ channel 12.92)
       (expt (/ (+ channel 0.055) 1.055) 2.4))))

(defun modus-themes-wcag-formula (hex)
  "Get WCAG value of color value HEX.
The value is defined in hexadecimal RGB notation, such #123456."
  (let ((channels (color-name-to-rgb hex))
        (weights '(0.2126 0.7152 0.0722))
        contribution)
    (while channels
      (push (modus-themes--wcag-contribution (pop channels) (pop weights)) contribution))
    (apply #'+ contribution)))

;;;###autoload
(defun modus-themes-contrast (c1 c2)
  "Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB."
  (let ((ct (/ (+ (modus-themes-wcag-formula c1) 0.05)
               (+ (modus-themes-wcag-formula c2) 0.05))))
    (max ct (/ ct))))

(defun modus-themes--modus-p (theme)
  "Return non-nil if THEME name has a modus- prefix."
  (string-prefix-p "modus-" (symbol-name theme)))

(defun modus-themes--list-enabled-themes ()
  "Return list of `custom-enabled-themes' with modus- prefix."
  (seq-filter #'modus-themes--modus-p custom-enabled-themes))

(defun modus-themes--load-no-enable (theme)
  "Load but do not enable THEME if it belongs to `custom-known-themes'."
  (unless (memq theme custom-known-themes)
    (load-theme theme :no-confirm :no-enable)))

(defun modus-themes--enable-themes ()
  "Enable the Modus themes."
  (mapc #'modus-themes--load-no-enable modus-themes-items))

(defun modus-themes--list-known-themes ()
  "Return list of `custom-known-themes' with modus- prefix."
  (modus-themes--enable-themes)
  (seq-filter #'modus-themes--modus-p custom-known-themes))

(defun modus-themes--current-theme ()
  "Return first enabled Modus theme."
  (car (or (modus-themes--list-enabled-themes)
           (modus-themes--list-known-themes))))

(defun modus-themes--palette-symbol (theme &optional suffix)
  "Return THEME palette as a symbol of the form THEME-palette.
With optional SUFFIX, return THEME-palette-SUFFIX as a symbol."
  (when theme
    (intern
     (if suffix
         (format "%s-palette-%s" theme suffix)
       (format "%s-palette" theme)))))

(defun modus-themes--palette-value (theme &optional overrides)
  "Return palette value of THEME with optional OVERRIDES."
  (let* ((core-palette (symbol-value (modus-themes--palette-symbol theme)))
         (user-palette (symbol-value (modus-themes--palette-symbol theme "user")))
         (base-value (append user-palette modus-themes-common-palette-user core-palette)))
    (if overrides
        (append (symbol-value (modus-themes--palette-symbol theme "overrides"))
                modus-themes-common-palette-overrides
                base-value)
      base-value)))

(defun modus-themes--current-theme-palette (&optional overrides)
  "Return palette value of active Modus theme, else produce `user-error'.
With optional OVERRIDES return palette value plus whatever
overrides."
  (if-let* ((theme (modus-themes--current-theme)))
      (if overrides
          (modus-themes--palette-value theme :overrides)
        (modus-themes--palette-value theme))
    (user-error "No enabled Modus theme could be found")))

(defun modus-themes--disable-themes ()
  "Disable themes per `modus-themes-disable-other-themes'."
  (mapc #'disable-theme
        (if modus-themes-disable-other-themes
            custom-enabled-themes
          (modus-themes--list-known-themes))))

(defun modus-themes-load-theme (theme)
  "Load THEME while disabling other themes.

Which themes are disabled is determined by the user option
`modus-themes-disable-other-themes'.

Run the `modus-themes-after-load-theme-hook' as the final step
after loading the THEME.

Return THEME."
  (modus-themes--disable-themes)
  (load-theme theme :no-confirm)
  (run-hooks 'modus-themes-after-load-theme-hook)
  theme)

(defun modus-themes--retrieve-palette-value (color palette)
  "Return COLOR from PALETTE.
Use recursion until COLOR is retrieved as a string.  Refrain from
doing so if the value of COLOR is not a key in the PALETTE.

Return `unspecified' if the value of COLOR cannot be determined.
This symbol is accepted by faces and is thus harmless.

This function is used in the macros `modus-themes-theme',
`modus-themes-with-colors'."
  (let ((value (car (alist-get color palette))))
    (cond
     ((or (stringp value)
          (eq value 'unspecified))
      value)
     ((and (symbolp value)
           value)
      (modus-themes--retrieve-palette-value value palette))
     (t
      'unspecified))))

(defun modus-themes-get-color-value (color &optional overrides theme)
  "Return color value of named COLOR for current Modus theme.

COLOR is a symbol that represents a named color entry in the
palette.

If the value is the name of another color entry in the
palette (so a mapping), recur until you find the underlying color
value.

With optional OVERRIDES as a non-nil value, account for palette
overrides.  Else use the default palette.

With optional THEME as a symbol among `modus-themes-items', use
the palette of that item.  Else use the current Modus theme.

If COLOR is not present in the palette, return the `unspecified'
symbol, which is safe when used as a face attribute's value."
  (if-let* ((palette (if theme
                         (modus-themes--palette-value theme overrides)
                       (modus-themes--current-theme-palette overrides)))
            (value (modus-themes--retrieve-palette-value color palette)))
      value
    'unspecified))

;;;; Commands

;;;;; Select a theme with completion

(defvar modus-themes--select-theme-history nil
  "Minibuffer history of `modus-themes--select-prompt'.")

(defun modus-themes--annotate-theme (theme)
  "Return completion annotation for THEME."
  (when-let* ((symbol (intern-soft theme))
              (doc-string (get symbol 'theme-documentation)))
    (format " -- %s"
            (propertize (car (split-string doc-string "\\."))
                        'face 'completions-annotations))))

(defun modus-themes--completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

(defun modus-themes--completion-table-candidates ()
  "Render `modus-themes--list-known-themes' as completion with theme category."
  (modus-themes--completion-table 'theme (modus-themes--list-known-themes)))

(defun modus-themes--select-prompt (&optional prompt)
  "Minibuffer prompt to select a Modus theme.
With optional PROMPT string, use it.  Else use a generic prompt."
  (let ((completion-extra-properties `(:annotation-function ,#'modus-themes--annotate-theme)))
    (intern
     (completing-read
      (or prompt "Select Modus theme: ")
      (modus-themes--completion-table-candidates)
      nil t nil 'modus-themes--select-theme-history))))

;;;###autoload
(defun modus-themes-select (theme)
  "Load a Modus THEME using minibuffer completion.
Run `modus-themes-after-load-theme-hook' after loading the theme.
Disable other themes per `modus-themes-disable-other-themes'."
  (interactive (list (modus-themes--select-prompt)))
  (modus-themes-load-theme theme))

;;;;; Toggle between two themes

(defun modus-themes--toggle-theme-p ()
  "Return non-nil if `modus-themes-to-toggle' are valid."
  (condition-case nil
      (dolist (theme modus-themes-to-toggle)
        (or (memq theme modus-themes-items)
            (memq theme (modus-themes--list-known-themes))
            (error "`%s' is not part of `modus-themes-items'" theme)))
    (error nil)
    (:success modus-themes-to-toggle)))

;;;###autoload
(defun modus-themes-toggle ()
  "Toggle between the two `modus-themes-to-toggle'.
If `modus-themes-to-toggle' does not specify two Modus themes,
prompt with completion for a theme among our collection (this is
practically the same as the `modus-themes-select' command).

Run `modus-themes-after-load-theme-hook' after loading the theme.
Disable other themes per `modus-themes-disable-other-themes'."
  (declare (interactive-only t))
  (interactive)
  (if-let* ((themes (modus-themes--toggle-theme-p))
            (one (car themes))
            (two (cadr themes)))
      (modus-themes-load-theme (if (eq (car custom-enabled-themes) one) two one))
    (modus-themes-load-theme (modus-themes--select-prompt))))

;;;;; Rotate through a list of themes

(defun modus-themes--next-in-rotation (themes &optional reverse)
  "Return a new theme among THEMES if it is possible to rotate to it.
The argument REVERSE controls the direction of rotation."
  (if-let* ((index (seq-position themes (modus-themes--current-theme)))
            (offset (mod (if reverse (1- index) (1+ index))
                         (length themes)))
            (new-theme (nth offset themes)))
      new-theme
    (error "Cannot determine a theme among `%s'" themes)))

;;;###autoload
(defun modus-themes-rotate (themes &optional reverse)
  "Rotate to the next theme among THEMES.
When called interactively THEMES is the value of `modus-themes-to-rotate'
and REVERSE is the prefix argument.

If the current theme is already the next in line, then move to the one
after.  The rotation is performed rightwards if REVERSE is nil (the
default), and leftwards if REVERSE is non-nil.  Perform the rotation
such that the current element in the list becomes the last.  Do not
modify THEMES in the process."
  (interactive (list modus-themes-to-rotate current-prefix-arg))
  (unless (proper-list-p themes)
    "This is not a list of themes: `%s'" themes)
  (let ((candidate (modus-themes--next-in-rotation themes reverse)))
    (if (modus-themes--modus-p candidate)
        (progn
          (message "Rotating to `%s'" (propertize (symbol-name candidate) 'face 'success))
          (modus-themes-load-theme candidate))
      (user-error "`%s' is not part of the Modus collection" candidate))))

;;;;; Preview a theme palette

(defun modus-themes--list-colors-get-mappings (palette)
  "Get the semantic palette entries in PALETTE.
PALETTE is the value of a variable like `modus-operandi-palette'."
  (seq-remove
   (lambda (cell)
     (stringp (cadr cell)))
   palette))

(defun modus-themes--list-colors-tabulated (theme &optional mappings)
  "Return a data structure of THEME palette or MAPPINGS for tabulated list."
  (let* ((current-palette (modus-themes--palette-value theme mappings))
         (palette (if mappings
                      (modus-themes--list-colors-get-mappings current-palette)
                    current-palette)))
    (mapcar (lambda (cell)
              (pcase-let* ((`(,name ,value) cell)
                           (name-string (format "%s" name))
                           (value-string (format "%s" value))
                           (value-string-padded (format "%-30s" value-string))
                           (color (modus-themes-get-color-value name mappings theme))) ; resolve a semantic mapping
                (list name
                      (vector
                       (if (and (symbolp value)
                                (not (eq value 'unspecified)))
                           "Yes"
                         "")
                       name-string
                       (propertize value-string 'face `( :foreground ,color))
                       (propertize value-string-padded 'face (list :background color
                                                                   :foreground (if (string= color "unspecified")
                                                                                   (readable-foreground-color (modus-themes-get-color-value 'bg-main nil theme))
                                                                                 (readable-foreground-color color))))))))
            palette)))

(defvar modus-themes-current-preview nil)
(defvar modus-themes-current-preview-show-mappings nil)

(defun modus-themes--set-tabulated-entries ()
  "Set the value of `tabulated-list-entries' with palette entries."
  (setq-local tabulated-list-entries
              (modus-themes--list-colors-tabulated modus-themes-current-preview modus-themes-current-preview-show-mappings)))

(defun modus-themes-list-colors (theme &optional mappings)
  "Preview the palette of the Modus THEME of choice.
With optional prefix argument for MAPPINGS preview only the semantic
color mappings instead of the complete palette."
  (interactive
   (let ((prompt (if current-prefix-arg
                     "Preview palette mappings of THEME: "
                   "Preview palette of THEME: ")))
     (list
      (modus-themes--select-prompt prompt)
      current-prefix-arg)))
  (let ((buffer (get-buffer-create (format (if mappings "*%s-list-mappings*" "*%s-list-all*") theme))))
    (with-current-buffer buffer
      (let ((modus-themes-current-preview theme)
            (modus-themes-current-preview-show-mappings mappings))
        (modus-themes-preview-mode)))
    (pop-to-buffer buffer)))

(defalias 'modus-themes-preview-colors 'modus-themes-list-colors
  "Alias for `modus-themes-list-colors'.")

(defun modus-themes-list-colors-current (&optional mappings)
  "Like `modus-themes-list-colors' with optional MAPPINGS for the current theme."
  (interactive "P")
  (modus-themes-list-colors (modus-themes--current-theme) mappings))

(defalias 'modus-themes-preview-colors-current 'modus-themes-list-colors-current
  "Alias for `modus-themes-list-colors-current'.")

(define-derived-mode modus-themes-preview-mode tabulated-list-mode "Modus palette"
  "Major mode to display a Modus themes palette."
  :interactive nil
  (setq-local tabulated-list-format
              [("Mapping?" 10 t)
               ("Symbol name" 30 t)
               ("As foreground" 30 t)
               ("As background" 0 t)])
  (modus-themes--set-tabulated-entries)
  (tabulated-list-init-header)
  (tabulated-list-print))



;;;; Internal functions

(defun modus-themes--warn (option)
  "Warn that OPTION has changed."
  (prog1 nil
    (display-warning
     'modus-themes
     (format "`%s' has changed; please read the updated documentation" option)
     :warning)))

(defun modus-themes--list-or-warn (option)
  "Return list or nil value of OPTION, else `modus-themes--warn'."
  (let* ((value (symbol-value option)))
    (if (or (null value) (listp value))
        value
      (modus-themes--warn option))))

(defun modus-themes--property-lookup (properties alist-key list-pred default)
  "Return value from property alist or list.
Check PROPERTIES for an alist value that corresponds to
ALIST-KEY.  If no alist is present, search the PROPERTIES
list given LIST-PRED, using DEFAULT as a fallback."
  (if-let* ((val (or (alist-get alist-key properties)
                     (seq-filter (lambda (x) (funcall list-pred x)) properties)
                     default))
            ((listp val)))
      (car val)
    val))

;; Helper functions that are meant to ease the implementation of the
;; above customization variables.
(defun modus-themes--bold-weight ()
  "Conditional use of a heavier text weight."
  (when modus-themes-bold-constructs
    (list :inherit 'bold)))

(defun modus-themes--slant ()
  "Conditional use of italics for slant attribute."
  (when modus-themes-italic-constructs
    (list :inherit 'italic)))

(defun modus-themes--fixed-pitch ()
  "Conditional application of `fixed-pitch' inheritance."
  (when modus-themes-mixed-fonts
    (list :inherit 'fixed-pitch)))

(defun modus-themes--variable-pitch-ui ()
  "Conditional use of `variable-pitch' in UI elements."
  (when modus-themes-variable-pitch-ui
    (list :inherit 'variable-pitch)))

(defun modus-themes--prompt (fg bg)
  "Conditional use of colors for text prompt faces.
FG is the prompt's standard foreground.  BG is a background
color that is combined with FG-FOR-BG."
  (let* ((properties (modus-themes--list-or-warn 'modus-themes-prompts))
         (weight (modus-themes--weight properties)))
    (list :inherit
          (cond
           ((and (memq 'bold properties)
                 (memq 'italic properties))
            'bold-italic)
           ((memq 'italic properties)
            'italic)
           ((memq 'bold properties)
            'bold)
           ('unspecified))
          :background bg
          :foreground fg
          :weight
          ;; If we have `bold' specifically, we inherit the face of
          ;; the same name.  This allows the user to customise that
          ;; face, such as to change its font family.
          (if (and weight (not (eq weight 'bold)))
              weight
            'unspecified))))

(defconst modus-themes-weights
  '( thin ultralight extralight light semilight regular medium
     semibold bold heavy extrabold ultrabold)
  "List of font weights.")

(defun modus-themes--weight (list)
  "Search for `modus-themes-weights' weight in LIST."
  (catch 'found
    (dolist (elt list)
      (when (memq elt modus-themes-weights)
        (throw 'found elt)))))

(defun modus-themes--heading (level fg &optional bg ol)
  "Conditional styles for `modus-themes-headings'.

LEVEL is the heading's position in their order.  FG is the
default text color.  Optional BG is an appropriate background.
Optional OL is the color of an overline."
  (let* ((key (alist-get level modus-themes-headings))
         (style (or key (alist-get t modus-themes-headings)))
         (style-listp (listp style))
         (properties style)
         (var (when (and style-listp (memq 'variable-pitch properties)) 'variable-pitch))
         (weight (when style-listp (modus-themes--weight style))))
    (list :inherit (cond
                    ((not style-listp) 'bold)
                    ;; `no-bold' is for backward compatibility because we cannot
                    ;; deprecate a variable's value.
                    ((or weight (memq 'no-bold properties))
                     var)
                    (var (append (list 'bold) (list var)))
                    (t 'bold))
          :background (or bg 'unspecified)
          :foreground fg
          :overline (or ol 'unspecified)
          :height (if style-listp
                      (modus-themes--property-lookup properties 'height #'floatp 'unspecified)
                    'unspecified)
          :weight (or weight 'unspecified))))

(defun modus-themes--completion-line (bg)
  "Styles for `modus-themes-completions' with BG as the background."
  (let* ((var (modus-themes--list-or-warn 'modus-themes-completions))
         (properties (or (alist-get 'selection var) (alist-get t var)))
         (italic (memq 'italic properties))
         (weight (modus-themes--weight properties))
         (bold (when (and weight (eq weight 'bold)) 'bold)))
    (list
     :inherit
     (cond
      ((and italic weight (not (eq weight 'bold)))
       'italic)
      ((and weight (not (eq weight 'bold)))
       'unspecified)
      (italic 'bold-italic)
      ('bold))
     :background bg
     :foreground 'unspecified
     :underline
     (if (memq 'underline properties) t 'unspecified)
     :weight
     (if (and weight (null bold)) weight 'unspecified))))

(defun modus-themes--completion-match (fg bg)
  "Styles for `modus-themes-completions'.
FG and BG are the main colors."
  (let* ((var (modus-themes--list-or-warn 'modus-themes-completions))
         (properties (or (alist-get 'matches var) (alist-get t var)))
         (italic (memq 'italic properties))
         (weight (modus-themes--weight properties))
         (bold (when (and weight (eq weight 'bold)) 'bold)))
    (list
     :inherit
     (cond
      ((and italic weight (not (eq weight 'bold)))
       'italic)
      ((and weight (not (eq weight 'bold)))
       'unspecified)
      (italic 'bold-italic)
      ('bold))
     :background bg
     :foreground fg
     :underline
     (if (memq 'underline properties) t 'unspecified)
     :weight
     (if (and weight (null bold)) weight 'unspecified))))



;;;; Face specifications

(defconst modus-themes-faces
  '(
;;;; custom faces
    ;; these bespoke faces are inherited by other constructs below
;;;;; just the foregrounds
    `(modus-themes-fg-red ((,c :foreground ,red)))
    `(modus-themes-fg-red-warmer ((,c :foreground ,red-warmer)))
    `(modus-themes-fg-red-cooler ((,c :foreground ,red-cooler)))
    `(modus-themes-fg-red-faint ((,c :foreground ,red-faint)))
    `(modus-themes-fg-red-intense ((,c :foreground ,red-intense)))
    `(modus-themes-fg-green ((,c :foreground ,green)))
    `(modus-themes-fg-green-warmer ((,c :foreground ,green-warmer)))
    `(modus-themes-fg-green-cooler ((,c :foreground ,green-cooler)))
    `(modus-themes-fg-green-faint ((,c :foreground ,green-faint)))
    `(modus-themes-fg-green-intense ((,c :foreground ,green-intense)))
    `(modus-themes-fg-yellow ((,c :foreground ,yellow)))
    `(modus-themes-fg-yellow-warmer ((,c :foreground ,yellow-warmer)))
    `(modus-themes-fg-yellow-cooler ((,c :foreground ,yellow-cooler)))
    `(modus-themes-fg-yellow-faint ((,c :foreground ,yellow-faint)))
    `(modus-themes-fg-yellow-intense ((,c :foreground ,yellow-intense)))
    `(modus-themes-fg-blue ((,c :foreground ,blue)))
    `(modus-themes-fg-blue-warmer ((,c :foreground ,blue-warmer)))
    `(modus-themes-fg-blue-cooler ((,c :foreground ,blue-cooler)))
    `(modus-themes-fg-blue-faint ((,c :foreground ,blue-faint)))
    `(modus-themes-fg-blue-intense ((,c :foreground ,blue-intense)))
    `(modus-themes-fg-magenta ((,c :foreground ,magenta)))
    `(modus-themes-fg-magenta-warmer ((,c :foreground ,magenta-warmer)))
    `(modus-themes-fg-magenta-cooler ((,c :foreground ,magenta-cooler)))
    `(modus-themes-fg-magenta-faint ((,c :foreground ,magenta-faint)))
    `(modus-themes-fg-magenta-intense ((,c :foreground ,magenta-intense)))
    `(modus-themes-fg-cyan ((,c :foreground ,cyan)))
    `(modus-themes-fg-cyan-warmer ((,c :foreground ,cyan-warmer)))
    `(modus-themes-fg-cyan-cooler ((,c :foreground ,cyan-cooler)))
    `(modus-themes-fg-cyan-faint ((,c :foreground ,cyan-faint)))
    `(modus-themes-fg-cyan-intense ((,c :foreground ,cyan-intense)))
;;;;; nuanced colored backgrounds
    `(modus-themes-nuanced-red ((,c :background ,bg-red-nuanced :extend t)))
    `(modus-themes-nuanced-green ((,c :background ,bg-green-nuanced :extend t)))
    `(modus-themes-nuanced-yellow ((,c :background ,bg-yellow-nuanced :extend t)))
    `(modus-themes-nuanced-blue ((,c :background ,bg-blue-nuanced :extend t)))
    `(modus-themes-nuanced-magenta ((,c :background ,bg-magenta-nuanced :extend t)))
    `(modus-themes-nuanced-cyan ((,c :background ,bg-cyan-nuanced :extend t)))
;;;;; subtle colored backgrounds
    `(modus-themes-subtle-red ((,c :background ,bg-red-subtle :foreground ,fg-main)))
    `(modus-themes-subtle-green ((,c :background ,bg-green-subtle :foreground ,fg-main)))
    `(modus-themes-subtle-yellow ((,c :background ,bg-yellow-subtle :foreground ,fg-main)))
    `(modus-themes-subtle-blue ((,c :background ,bg-blue-subtle :foreground ,fg-main)))
    `(modus-themes-subtle-magenta ((,c :background ,bg-magenta-subtle :foreground ,fg-main)))
    `(modus-themes-subtle-cyan ((,c :background ,bg-cyan-subtle :foreground ,fg-main)))
;;;;; intense colored backgrounds
    `(modus-themes-intense-red ((,c :background ,bg-red-intense :foreground ,fg-main)))
    `(modus-themes-intense-green ((,c :background ,bg-green-intense :foreground ,fg-main)))
    `(modus-themes-intense-yellow ((,c :background ,bg-yellow-intense :foreground ,fg-main)))
    `(modus-themes-intense-blue ((,c :background ,bg-blue-intense :foreground ,fg-main)))
    `(modus-themes-intense-magenta ((,c :background ,bg-magenta-intense :foreground ,fg-main)))
    `(modus-themes-intense-cyan ((,c :background ,bg-cyan-intense :foreground ,fg-main)))
;;;;; mark indicators
    ;; color combinations intended for Dired, Ibuffer, or equivalent
    `(modus-themes-mark-alt ((,c :inherit bold :background ,bg-mark-other :foreground ,fg-mark-other)))
    `(modus-themes-mark-del ((,c :inherit bold :background ,bg-mark-delete :foreground ,fg-mark-delete)))
    `(modus-themes-mark-sel ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
;;;;; heading levels
    ;; styles for regular headings used in Org, Markdown, Info, etc.
    `(modus-themes-heading-0 ((,c ,@(modus-themes--heading 0 fg-heading-0 bg-heading-0 overline-heading-0))))
    `(modus-themes-heading-1 ((,c ,@(modus-themes--heading 1 fg-heading-1 bg-heading-1 overline-heading-1))))
    `(modus-themes-heading-2 ((,c ,@(modus-themes--heading 2 fg-heading-2 bg-heading-2 overline-heading-2))))
    `(modus-themes-heading-3 ((,c ,@(modus-themes--heading 3 fg-heading-3 bg-heading-3 overline-heading-3))))
    `(modus-themes-heading-4 ((,c ,@(modus-themes--heading 4 fg-heading-4 bg-heading-4 overline-heading-4))))
    `(modus-themes-heading-5 ((,c ,@(modus-themes--heading 5 fg-heading-5 bg-heading-5 overline-heading-5))))
    `(modus-themes-heading-6 ((,c ,@(modus-themes--heading 6 fg-heading-6 bg-heading-6 overline-heading-6))))
    `(modus-themes-heading-7 ((,c ,@(modus-themes--heading 7 fg-heading-7 bg-heading-7 overline-heading-7))))
    `(modus-themes-heading-8 ((,c ,@(modus-themes--heading 8 fg-heading-8 bg-heading-8 overline-heading-8))))
;;;;; language checkers
    `(modus-themes-lang-error ((,c :underline (:style wave :color ,underline-err))))
    `(modus-themes-lang-note ((,c :underline (:style wave :color ,underline-note))))
    `(modus-themes-lang-warning ((,c :underline (:style wave :color ,underline-warning))))
;;;;; prominent semantic notes
    `(modus-themes-prominent-error ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
    `(modus-themes-prominent-note ((,c :background ,bg-prominent-note :foreground ,fg-prominent-note)))
    `(modus-themes-prominent-warning ((,c :background ,bg-prominent-warning :foreground ,fg-prominent-warning)))
;;;;; markup
    `(modus-themes-prose-code ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-code :foreground ,fg-prose-code)))
    `(modus-themes-prose-macro ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-macro :foreground ,fg-prose-macro)))
    `(modus-themes-prose-verbatim ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
;;;;; search
    `(modus-themes-search-current ((,c :background ,bg-search-current :foreground ,fg-main)))
    `(modus-themes-search-lazy ((,c :background ,bg-search-lazy :foreground ,fg-main)))
    `(modus-themes-search-replace ((,c :background ,bg-search-replace :foreground ,fg-main)))
;;;;; search regexp groups
    `(modus-themes-search-rx-group-0 ((,c :background ,bg-search-rx-group-0 :foreground ,fg-main)))
    `(modus-themes-search-rx-group-1 ((,c :background ,bg-search-rx-group-1 :foreground ,fg-main)))
    `(modus-themes-search-rx-group-2 ((,c :background ,bg-search-rx-group-2 :foreground ,fg-main)))
    `(modus-themes-search-rx-group-3 ((,c :background ,bg-search-rx-group-3 :foreground ,fg-main)))
;;;;; completion frameworks
    `(modus-themes-completion-match-0 ((,c ,@(modus-themes--completion-match fg-completion-match-0 bg-completion-match-0))))
    `(modus-themes-completion-match-1 ((,c ,@(modus-themes--completion-match fg-completion-match-1 bg-completion-match-1))))
    `(modus-themes-completion-match-2 ((,c ,@(modus-themes--completion-match fg-completion-match-2 bg-completion-match-2))))
    `(modus-themes-completion-match-3 ((,c ,@(modus-themes--completion-match fg-completion-match-3 bg-completion-match-3))))
    `(modus-themes-completion-selected ((,c ,@(modus-themes--completion-line bg-completion))))
;;;;; typography
    `(modus-themes-bold ((,c ,@(modus-themes--bold-weight))))
    `(modus-themes-fixed-pitch ((,c ,@(modus-themes--fixed-pitch))))
    `(modus-themes-slant ((,c ,@(modus-themes--slant))))
    `(modus-themes-ui-variable-pitch ((,c ,@(modus-themes--variable-pitch-ui))))
;;;;; other custom faces
    `(modus-themes-button ((,c :inherit variable-pitch
                               :box (:line-width 1 :color ,border :style released-button)
                               :background ,bg-button-active
                               :foreground ,fg-button-active)))
    `(modus-themes-key-binding ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(modus-themes-prompt ((,c ,@(modus-themes--prompt fg-prompt bg-prompt))))
    `(modus-themes-reset-soft ((,c :background ,bg-main :foreground ,fg-main
                                   :weight normal :slant normal :strike-through nil
                                   :box nil :underline nil :overline nil :extend nil)))
;;;; standard faces
;;;;; absolute essentials
    `(default ((,c :background ,bg-main :foreground ,fg-main)))
    `(cursor ((,c :background ,cursor)))
    `(fringe ((,c :background ,fringe :foreground ,fg-main)))
    `(menu ((,c :background ,bg-dim :foreground ,fg-main)))
    `(scroll-bar ((,c :background ,fringe :foreground ,border)))
    `(tool-bar ((,c :background ,bg-dim :foreground ,fg-main)))
    `(vertical-border ((,c :foreground ,border)))
;;;;; basic and/or ungrouped styles
    `(abbrev-table-name ((,c :inherit bold)))
    `(appt-notification ((,c :inherit bold :foreground ,modeline-err)))
    `(blink-matching-paren-offscreen ((,c :background ,bg-paren-match)))
    `(bold ((,c :weight bold)))
    `(bold-italic ((,c :inherit (bold italic))))
    `(underline ((,c :underline ,fg-dim)))
    `(buffer-menu-buffer ((,c :inherit bold)))
    `(child-frame-border ((,c :background ,border)))
    `(comint-highlight-input ((,c :inherit bold)))
    `(comint-highlight-prompt ((,c :inherit modus-themes-prompt)))
    `(confusingly-reordered ((,c :inherit modus-themes-lang-error)))
    `(edmacro-label ((,c :inherit bold :foreground ,accent-0)))
    `(elisp-shorthand-font-lock-face ((,c :inherit (italic font-lock-preprocessor-face))))
    `(error ((,c :inherit bold :foreground ,err)))
    `(escape-glyph ((,c :foreground ,err)))
    `(file-name-shadow ((,c :inherit shadow)))
    `(header-line ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-dim)))
    `(header-line-inactive ((,c :inherit (modus-themes-ui-variable-pitch shadow))))
    `(header-line-highlight ((,c :background ,bg-hover :foreground ,fg-main :box ,fg-main)))
    `(help-argument-name ((,c :inherit modus-themes-slant :foreground ,variable)))
    `(help-key-binding ((,c :inherit modus-themes-key-binding)))
    `(highlight ((,c :background ,bg-hover :foreground ,fg-main)))
    `(homoglyph ((,c :foreground ,warning)))
    `(ibuffer-locked-buffer ((,c :foreground ,warning)))
    `(icon-button ((,c :inherit modus-themes-button)))
    `(italic ((,c :slant italic)))
    `(nobreak-hyphen ((,c :foreground ,err)))
    `(nobreak-space ((,c :foreground ,err :underline t)))
    `(menu ((,c :inverse-video unspecified :background ,bg-active :foreground ,fg-main)))
    `(minibuffer-prompt ((,c :inherit modus-themes-prompt)))
    `(mm-command-output ((,c :foreground ,mail-part)))
    `(mm-uu-extract ((,c :foreground ,mail-part)))
    `(next-error ((,c :inherit modus-themes-prominent-error :extend t)))
    `(pgtk-im-0 ((,c :inherit modus-themes-prominent-note)))
    `(read-multiple-choice-face ((,c :inherit modus-themes-mark-sel)))
    `(rectangle-preview ((,c :inherit secondary-selection)))
    `(region ((,c :background ,bg-region :foreground ,fg-region)))
    `(secondary-selection ((,c :background ,bg-hover-secondary :foreground ,fg-main)))
    `(separator-line ((,c :underline ,bg-active)))
    `(shadow ((,c :foreground ,fg-dim)))
    `(success ((,c :inherit bold :foreground ,info)))
    `(trailing-whitespace ((,c :background ,bg-space-err)))
    ;; NOTE 2024-06-22: I use `list' here to suppress a bogus warning
    ;; from the compiler: it says I should depend on Emacs 29 to use
    ;; vtable.
    (list 'vtable `((,c :inherit modus-themes-fixed-pitch)))
    `(warning ((,c :inherit bold :foreground ,warning)))
;;;;; buttons, links, widgets
    `(button ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(link ((,c :inherit button)))
    `(link-visited ((,c :background ,bg-link-visited :foreground ,fg-link-visited :underline ,underline-link-visited)))
    `(tooltip ((,c :background ,bg-active :foreground ,fg-main)))
;;;;; adoc-mode
    `(adoc-code-face ((,c :inherit font-lock-constant-face)))
    `(adoc-command-face ((,c :inherit modus-themes-prose-macro)))
    `(adoc-complex-replacement-face ((,c :background ,bg-magenta-subtle :foreground ,magenta)))
    `(adoc-emphasis-face ((t (:inherit bold))))
    `(adoc-gen-face ((,c :foreground ,blue)))
    `(adoc-meta-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(adoc-meta-hide-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(adoc-replacement-face ((,c :inherit font-lock-escape-face)))
    `(adoc-secondary-text-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata-value)))
    `(adoc-table-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-table)))
    `(adoc-title-0-face ((,c :inherit modus-themes-heading-0)))
    `(adoc-title-1-face ((,c :inherit modus-themes-heading-1)))
    `(adoc-title-2-face ((,c :inherit modus-themes-heading-2)))
    `(adoc-title-3-face ((,c :inherit modus-themes-heading-3)))
    `(adoc-title-4-face ((,c :inherit modus-themes-heading-4)))
    `(adoc-title-5-face ((,c :inherit modus-themes-heading-5)))
    `(adoc-typewriter-face ((,c :inherit modus-themes-prose-verbatim)))
    `(adoc-verbatim-face ((,c :inherit modus-themes-prose-verbatim)))
;;;;; agda2-mode
    `(agda2-highlight-bound-variable-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-catchall-clause-face ((,c :background ,bg-inactive)))
    `(agda2-highlight-coinductive-constructor-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-coverage-problem-face ((,c :inherit modus-themes-lang-error)))
    `(agda2-highlight-datatype-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-deadcode-face ((,c :background ,bg-active)))
    `(agda2-highlight-dotted-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-error-face ((,c :inherit modus-themes-lang-error)))
    `(agda2-highlight-field-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-function-face ((,c :inherit font-lock-function-name-face)))
    `(agda2-highlight-generalizable-variable-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-incomplete-pattern-face ((,c :inherit modus-themes-lang-warning)))
    `(agda2-highlight-inductive-constructor-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-keyword-face ((,c :inherit font-lock-keyword-face)))
    `(agda2-highlight-macro-face ((,c :inherit font-lock-keyword-face)))
    `(agda2-highlight-module-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-number-face ((,c :inherit shadow)))
    `(agda2-highlight-operator-face ((,c :inherit font-lock-variable-name-face)))
    `(agda2-highlight-positivity-problem-face ((,c :inherit modus-themes-lang-warning)))
    `(agda2-highlight-postulate-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-pragma-face ((,c :inherit font-lock-preprocessor-face)))
    `(agda2-highlight-primitive-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-primitive-type-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-record-face ((,c :inherit font-lock-type-face)))
    `(agda2-highlight-string-face ((,c :inherit font-lock-string-face)))
    `(agda2-highlight-symbol-face ((,c :inherit font-lock-constant-face)))
    `(agda2-highlight-termination-problem-face ((,c :inherit modus-themes-lang-warning)))
    `(agda2-highlight-typechecks-face ((,c :inherit font-lock-warning-face)))
    `(agda2-highlight-unsolved-constraint-face ((,c :inherit modus-themes-lang-warning)))
    `(agda2-highlight-unsolved-meta-face ((,c :inherit modus-themes-lang-warning)))
;;;;; all-the-icons
    `(all-the-icons-blue ((,c :foreground ,blue-cooler)))
    `(all-the-icons-blue-alt ((,c :foreground ,blue-warmer)))
    `(all-the-icons-cyan ((,c :foreground ,cyan)))
    `(all-the-icons-cyan-alt ((,c :foreground ,cyan-warmer)))
    `(all-the-icons-dblue ((,c :foreground ,blue-faint)))
    `(all-the-icons-dcyan ((,c :foreground ,cyan-faint)))
    `(all-the-icons-dgreen ((,c :foreground ,green-faint)))
    `(all-the-icons-dmaroon ((,c :foreground ,magenta-faint)))
    `(all-the-icons-dorange ((,c :foreground ,red-faint)))
    `(all-the-icons-dpink ((,c :foreground ,magenta-faint)))
    `(all-the-icons-dpurple ((,c :foreground ,magenta-cooler)))
    `(all-the-icons-dred ((,c :foreground ,red)))
    `(all-the-icons-dsilver ((,c :foreground ,cyan-faint)))
    `(all-the-icons-dyellow ((,c :foreground ,yellow-faint)))
    `(all-the-icons-green ((,c :foreground ,green)))
    `(all-the-icons-lblue ((,c :foreground ,blue-cooler)))
    `(all-the-icons-lcyan ((,c :foreground ,cyan)))
    `(all-the-icons-lgreen ((,c :foreground ,green-warmer)))
    `(all-the-icons-lmaroon ((,c :foreground ,magenta-warmer)))
    `(all-the-icons-lorange ((,c :foreground ,red-warmer)))
    `(all-the-icons-lpink ((,c :foreground ,magenta)))
    `(all-the-icons-lpurple ((,c :foreground ,magenta-faint)))
    `(all-the-icons-lred ((,c :foreground ,red-faint)))
    `(all-the-icons-lsilver ((,c :foreground "gray50")))
    `(all-the-icons-lyellow ((,c :foreground ,yellow-warmer)))
    `(all-the-icons-maroon ((,c :foreground ,magenta)))
    `(all-the-icons-orange ((,c :foreground ,yellow-warmer)))
    `(all-the-icons-pink ((,c :foreground ,magenta-warmer)))
    `(all-the-icons-purple ((,c :foreground ,magenta-cooler)))
    `(all-the-icons-purple-alt ((,c :foreground ,blue-warmer)))
    `(all-the-icons-red ((,c :foreground ,red)))
    `(all-the-icons-red-alt ((,c :foreground ,red-cooler)))
    `(all-the-icons-silver ((,c :foreground "gray50")))
    `(all-the-icons-yellow ((,c :foreground ,yellow)))
;;;;; all-the-icons-dired
    `(all-the-icons-dired-dir-face ((,c :foreground ,accent-0)))
;;;;; all-the-icons-ibuffer
    `(all-the-icons-ibuffer-dir-face ((,c :foreground ,accent-0)))
    `(all-the-icons-ibuffer-file-face ((,c :foreground ,docstring)))
    `(all-the-icons-ibuffer-mode-face ((,c :foreground ,type)))
    `(all-the-icons-ibuffer-size-face ((,c :foreground ,variable)))
;;;;; annotate
    `(annotate-annotation ((,c :inherit modus-themes-subtle-blue)))
    `(annotate-annotation-secondary ((,c :inherit modus-themes-subtle-magenta)))
    `(annotate-highlight ((,c :background ,bg-blue-subtle :underline ,blue-intense)))
    `(annotate-highlight-secondary ((,c :background ,bg-magenta-subtle :underline ,magenta-intense)))
;;;;; ansi-color
    ;; Those are in Emacs28.
    `(ansi-color-black ((,c :background ,bg-term-black :foreground ,fg-term-black)))
    `(ansi-color-blue ((,c :background ,bg-term-blue :foreground ,fg-term-blue)))
    `(ansi-color-bold ((,c :inherit bold)))
    `(ansi-color-bright-black ((,c :background ,bg-term-black-bright :foreground ,fg-term-black-bright)))
    `(ansi-color-bright-blue ((,c :background ,bg-term-blue-bright :foreground ,fg-term-blue-bright)))
    `(ansi-color-bright-cyan ((,c :background ,bg-term-cyan-bright :foreground ,fg-term-cyan-bright)))
    `(ansi-color-bright-green ((,c :background ,bg-term-green-bright :foreground ,fg-term-green-bright)))
    `(ansi-color-bright-magenta ((,c :background ,bg-term-magenta-bright :foreground ,fg-term-magenta-bright)))
    `(ansi-color-bright-red ((,c :background ,bg-term-red-bright :foreground ,fg-term-red-bright)))
    `(ansi-color-bright-white ((,c :background ,bg-term-white-bright :foreground ,fg-term-white-bright)))
    `(ansi-color-bright-yellow ((,c :background ,bg-term-yellow-bright :foreground ,fg-term-yellow-bright)))
    `(ansi-color-cyan ((,c :background ,bg-term-cyan :foreground ,fg-term-cyan)))
    `(ansi-color-green ((,c :background ,bg-term-green :foreground ,fg-term-green)))
    `(ansi-color-magenta ((,c :background ,bg-term-magenta :foreground ,fg-term-magenta)))
    `(ansi-color-red ((,c :background ,bg-term-red :foreground ,fg-term-red)))
    `(ansi-color-white ((,c :background ,bg-term-white :foreground ,fg-term-white)))
    `(ansi-color-yellow ((,c :background ,bg-term-yellow :foreground ,fg-term-yellow)))
;;;;; anzu
    `(anzu-match-1 ((,c :inherit modus-themes-subtle-cyan)))
    `(anzu-match-2 ((,c :inherit modus-themes-search-current)))
    `(anzu-match-3 ((,c :inherit modus-themes-subtle-yellow)))
    `(anzu-mode-line ((,c :inherit bold)))
    `(anzu-mode-line-no-match ((,c :inherit error)))
    `(anzu-replace-highlight ((,c :inherit modus-themes-search-replace)))
    `(anzu-replace-to ((,c :inherit modus-themes-search-current)))
;;;;; auctex and Tex
    `(font-latex-bold-face ((,c :inherit bold)))
    `(font-latex-doctex-documentation-face ((,c :inherit font-lock-doc-face)))
    `(font-latex-doctex-preprocessor-face ((,c :inherit font-lock-preprocessor-face)))
    `(font-latex-italic-face ((,c :inherit italic)))
    `(font-latex-math-face ((,c :inherit font-lock-constant-face)))
    `(font-latex-script-char-face ((,c :inherit font-lock-builtin-face)))
    `(font-latex-sectioning-5-face ((,c :inherit bold :foreground ,fg-alt)))
    `(font-latex-sedate-face ((,c :inherit font-lock-keyword-face)))
    `(font-latex-slide-title-face ((,c :inherit modus-themes-heading-1)))
    `(font-latex-string-face ((,c :inherit font-lock-string-face)))
    `(font-latex-subscript-face ((,c :height 0.95)))
    `(font-latex-superscript-face ((,c :height 0.95)))
    `(font-latex-underline-face ((,c :inherit underline)))
    `(font-latex-verbatim-face ((,c :inherit modus-themes-prose-verbatim)))
    `(font-latex-warning-face ((,c :inherit font-lock-warning-face)))
    `(tex-verbatim ((,c :inherit modus-themes-prose-verbatim)))
    ;; `(texinfo-heading ((,c :foreground ,magenta)))
    `(TeX-error-description-error ((,c :inherit error)))
    `(TeX-error-description-help ((,c :inherit success)))
    `(TeX-error-description-tex-said ((,c :inherit success)))
    `(TeX-error-description-warning ((,c :inherit warning)))
;;;;; auto-dim-other-buffers
    `(auto-dim-other-buffers-face ((,c :background ,bg-inactive)))
    `(auto-dim-other-buffers-hide-face ((,c :foreground ,bg-inactive :background ,bg-inactive)))
;;;;; avy
    `(avy-background-face ((,c :background ,bg-dim :foreground ,fg-dim :extend t)))
    `(avy-goto-char-timer-face ((,c :inherit (bold modus-themes-search-lazy modus-themes-reset-soft))))
    `(avy-lead-face ((,c :inherit (bold modus-themes-search-current modus-themes-reset-soft))))
    `(avy-lead-face-0 ((,c :inherit (bold modus-themes-search-current modus-themes-reset-soft))))
    `(avy-lead-face-1 ((,c :inherit (bold modus-themes-search-current modus-themes-reset-soft))))
    `(avy-lead-face-2 ((,c :inherit (bold modus-themes-search-current modus-themes-reset-soft))))
;;;;; aw (ace-window)
    `(aw-background-face ((,c :foreground "gray50")))
    `(aw-key-face ((,c :inherit modus-themes-key-binding)))
    `(aw-leading-char-face ((,c :inherit (bold modus-themes-reset-soft) :height 1.5 :foreground ,red-intense)))
    `(aw-minibuffer-leading-char-face ((,c :inherit modus-themes-key-binding)))
    `(aw-mode-line-face ((,c :inherit bold)))
;;;;; binder
    `(binder-sidebar-highlight ((,c :inherit modus-themes-hl-line)))
    `(binder-sidebar-marked ((,c :inherit modus-themes-mark-sel)))
    `(binder-sidebar-missing ((,c :inherit modus-themes-mark-del)))
    `(binder-sidebar-tags ((,c :foreground ,variable)))
;;;;; breadcrumb
    `(breadcrumb-face ((,c :foreground ,fg-alt)))
    `(breadcrumb-imenu-leaf-face ((,c :inherit bold :foreground ,modeline-info))) ; same as `which-func'
    `(breadcrumb-project-leaf-face ((,c :inherit bold)))
;;;;; bongo
    `(bongo-album-title (( )))
    `(bongo-artist ((,c :foreground ,accent-0)))
    `(bongo-currently-playing-track ((,c :inherit bold)))
    `(bongo-elapsed-track-part ((,c :background ,bg-inactive :underline t)))
    `(bongo-filled-seek-bar ((,c :background ,bg-hover)))
    `(bongo-marked-track ((,c :inherit modus-themes-mark-alt)))
    `(bongo-marked-track-line ((,c :background ,bg-dim)))
    `(bongo-played-track ((,c :inherit shadow :strike-through t)))
    `(bongo-track-length ((,c :inherit shadow)))
    `(bongo-track-title ((,c :foreground ,accent-1)))
    `(bongo-unfilled-seek-bar ((,c :background ,bg-dim)))
;;;;; boon
    `(boon-modeline-cmd ((,c :inherit modus-themes-intense-blue)))
    `(boon-modeline-ins ((,c :inherit modus-themes-intense-red)))
    `(boon-modeline-off ((,c :inherit modus-themes-intense-yellow)))
    `(boon-modeline-spc ((,c :inherit modus-themes-intense-green)))
;;;;; bookmark
    `(bookmark-face ((,c :inherit success)))
    `(bookmark-menu-bookmark ((,c :inherit bold)))
;;;;; calendar and diary
    `(calendar-month-header ((,c :inherit bold)))
    `(calendar-today ((,c :inherit bold :underline t)))
    `(calendar-weekday-header ((,c :foreground ,date-weekday)))
    `(calendar-weekend-header ((,c :foreground ,date-weekend)))
    `(diary ((,c :foreground ,date-common)))
    `(diary-anniversary ((,c :foreground ,date-holiday)))
    `(diary-time ((,c :foreground ,date-common)))
    `(holiday ((,c :foreground ,date-holiday)))
;;;;; calibredb
    ;; NOTE 2022-12-27: Calibredb needs to be reviewed.  I had to
    ;; change the applicable colors for the transition to
    ;; modus-themes version 4, but I cannot test this currently (it
    ;; depends on an external program).
    `(calibredb-archive-face ((,c :foreground ,accent-3)))
    `(calibredb-author-face ((,c :foreground ,name)))
    `(calibredb-comment-face ((,c :inherit shadow)))
    `(calibredb-date-face ((,c :foreground ,date-common)))
    `(calibredb-edit-annotation-header-title-face ((,c :inherit bold)))
    `(calibredb-favorite-face ((,c :foreground ,red-warmer)))
    `(calibredb-file-face (( )))
    `(calibredb-format-face ((,c :foreground ,fg-alt)))
    `(calibredb-highlight-face ((,c :inherit success)))
    `(calibredb-id-face (( )))
    `(calibredb-ids-face (( )))
    `(calibredb-search-header-highlight-face ((,c :background ,bg-hl-line :extend t)))
    `(calibredb-search-header-library-name-face ((,c :foreground ,accent-2)))
    `(calibredb-search-header-library-path-face ((,c :inherit bold)))
    `(calibredb-search-header-sort-face ((,c :inherit bold :foreground ,accent-1)))
    `(calibredb-search-header-total-face ((,c :inherit bold :foreground ,accent-0)))
    `(calibredb-search-header-filter-face ((,c :inherit bold)))
    `(calibredb-mark-face ((,c :inherit modus-themes-mark-sel)))
    `(calibredb-size-face (( )))
    `(calibredb-tag-face ((,c :foreground ,fg-alt)))
;;;;; centaur-tabs
    `(centaur-tabs-active-bar-face ((,c :background ,blue)))
    `(centaur-tabs-close-mouse-face ((,c :inherit bold :foreground ,red :underline t)))
    `(centaur-tabs-close-selected ((,c :inherit centaur-tabs-selected)))
    `(centaur-tabs-close-unselected ((,c :inherit centaur-tabs-unselected)))
    `(centaur-tabs-modified-marker-selected ((,c :inherit centaur-tabs-selected)))
    `(centaur-tabs-modified-marker-unselected ((,c :inherit centaur-tabs-unselected)))
    `(centaur-tabs-default ((,c :background ,bg-main)))
    `(centaur-tabs-selected ((,c :inherit bold :box (:line-width -2 :color ,bg-tab-current) :background ,bg-tab-current)))
    `(centaur-tabs-selected-modified ((,c :inherit (italic centaur-tabs-selected))))
    `(centaur-tabs-unselected ((,c :box (:line-width -2 :color ,bg-tab-other) :background ,bg-tab-other)))
    `(centaur-tabs-unselected-modified ((,c :inherit (italic centaur-tabs-unselected))))
;;;;; change-log and log-view (`vc-print-log' and `vc-print-root-log')
    `(change-log-acknowledgment ((,c :foreground ,identifier)))
    `(change-log-conditionals ((,c :inherit error)))
    `(change-log-date ((,c :foreground ,date-common)))
    `(change-log-email ((,c :foreground ,fg-alt)))
    `(change-log-file ((,c :inherit bold)))
    `(change-log-function ((,c :inherit warning)))
    `(change-log-list ((,c :inherit bold)))
    `(change-log-name ((,c :foreground ,name)))
    `(log-edit-header ((,c :inherit bold)))
    `(log-edit-headers-separator ((,c :height 1 :background ,border :extend t)))
    `(log-edit-summary ((,c :inherit success)))
    `(log-edit-unknown-header ((,c :inherit shadow)))
    `(log-view-commit-body (( )))
    `(log-view-file ((,c :inherit bold)))
    `(log-view-message ((,c :foreground ,identifier)))
;;;;; cider
    `(cider-deprecated-face ((,c :inherit warning)))
    `(cider-enlightened-face ((,c :box ,warning)))
    `(cider-enlightened-local-face ((,c :inherit warning)))
    `(cider-error-highlight-face ((,c :inherit modus-themes-lang-error)))
    `(cider-fringe-good-face ((,c :foreground ,info)))
    `(cider-instrumented-face ((,c :box ,err)))
    `(cider-reader-conditional-face ((,c :inherit font-lock-type-face)))
    `(cider-repl-prompt-face ((,c :inherit minibuffer-prompt)))
    `(cider-repl-stderr-face ((,c :foreground ,err)))
    `(cider-repl-stdout-face (( )))
    `(cider-warning-highlight-face ((,c :inherit modus-themes-lang-warning)))
;;;;; circe (and lui)
    `(circe-fool-face ((,c :inherit shadow)))
    `(circe-highlight-nick-face ((,c :inherit error)))
    `(circe-prompt-face ((,c :inherit modus-themes-prompt)))
    `(circe-server-face ((,c :inherit shadow)))
    `(lui-button-face ((,c :inherit button)))
    `(lui-highlight-face ((,c :inherit error)))
    `(lui-time-stamp-face ((,c :foreground ,date-common)))
;;;;; citar
    `(citar ((,c :inherit shadow)))
    `(citar-highlight (( )))
;;;;; clojure-mode
    `(clojure-keyword-face ((,c :inherit font-lock-builtin-face)))
;;;;; column-enforce-mode
    `(column-enforce-face ((,c :inherit modus-themes-prominent-error)))
;;;;; company-mode
    `(company-echo-common ((,c :inherit modus-themes-completion-match-0)))
    `(company-preview ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(company-preview-common ((,c :inherit company-echo-common)))
    `(company-preview-search ((,c :background ,bg-yellow-intense)))
    `(company-scrollbar-bg ((,c :background ,bg-active)))
    `(company-scrollbar-fg ((,c :background ,fg-main)))
    `(company-template-field ((,c :background ,bg-active)))
    `(company-tooltip ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim)))
    `(company-tooltip-annotation ((,c :inherit completions-annotations)))
    `(company-tooltip-common ((,c :inherit company-echo-common)))
    `(company-tooltip-deprecated ((,c :inherit company-tooltip :strike-through t)))
    `(company-tooltip-mouse ((,c :inherit highlight)))
    `(company-tooltip-scrollbar-thumb ((,c :background ,fg-alt)))
    `(company-tooltip-scrollbar-track ((,c :background ,bg-inactive)))
    `(company-tooltip-search ((,c :inherit secondary-selection)))
    `(company-tooltip-search-selection ((,c :inherit secondary-selection :underline t)))
    `(company-tooltip-selection ((,c :inherit modus-themes-completion-selected)))
;;;;; compilation
    `(compilation-column-number ((,c :inherit compilation-line-number)))
    `(compilation-error ((,c :inherit modus-themes-bold :foreground ,err)))
    `(compilation-info ((,c :inherit modus-themes-bold :foreground ,info)))
    `(compilation-line-number ((,c :inherit shadow)))
    `(compilation-mode-line-exit ((,c :inherit bold)))
    `(compilation-mode-line-fail ((,c :inherit bold :foreground ,modeline-err)))
    `(compilation-mode-line-run ((,c :inherit bold :foreground ,modeline-warning)))
    `(compilation-warning ((,c :inherit modus-themes-bold :foreground ,warning)))
;;;;; completions
    `(completions-annotations ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(completions-common-part ((,c :inherit modus-themes-completion-match-0)))
    `(completions-first-difference ((,c :inherit modus-themes-completion-match-1)))
    `(completions-highlight ((,c :inherit modus-themes-completion-selected)))
;;;;; consult
    `(consult-async-split ((,c :inherit error)))
    `(consult-file ((,c :inherit modus-themes-bold :foreground ,info)))
    `(consult-key ((,c :inherit modus-themes-key-binding)))
    `(consult-imenu-prefix ((,c :inherit shadow)))
    `(consult-line-number ((,c :inherit shadow)))
    `(consult-line-number-prefix ((,c :inherit shadow)))
    `(consult-preview-insertion ((,c :background ,bg-dim)))
;;;;; corfu
    `(corfu-current ((,c :inherit modus-themes-completion-selected)))
    `(corfu-bar ((,c :background ,fg-dim)))
    `(corfu-border ((,c :background ,bg-active)))
    `(corfu-default ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim)))
;;;;; corfu-candidate-overlay
    `(corfu-candidate-overlay-face ((t :inherit shadow)))
;;;;; corfu-quick
    `(corfu-quick1 ((,c :inherit (bold modus-themes-search-current))))
    `(corfu-quick2 ((,c :inherit (bold modus-themes-search-current))))
;;;;; counsel
    `(counsel-active-mode ((,c :foreground ,keyword)))
    `(counsel-application-name ((,c :foreground ,name)))
    `(counsel-key-binding ((,c :inherit modus-themes-key-binding)))
    `(counsel-outline-default ((,c :foreground ,fg-main)))
    `(counsel-variable-documentation ((,c :inherit font-lock-doc-face)))
;;;;; cperl-mode
    `(cperl-nonoverridable-face ((,c :foreground unspecified)))
    `(cperl-array-face ((,c :inherit font-lock-keyword-face)))
    `(cperl-hash-face ((,c :inherit font-lock-variable-name-face)))
;;;;; crontab-mode
    `(crontab-minute ((,c :foreground ,string)))
    `(crontab-hour ((,c :foreground ,keyword)))
    `(crontab-month-day ((,c :foreground ,builtin)))
    `(crontab-month ((,c :foreground ,constant)))
    `(crontab-week-day ((,c :foreground ,variable)))
    `(crontab-predefined ((,c :foreground ,string)))
;;;;; csv-mode
    `(csv-separator-face ((,c :foreground ,red-intense)))
;;;;; ctrlf
    `(ctrlf-highlight-active ((,c :inherit modus-themes-search-current)))
    `(ctrlf-highlight-line ((,c :background ,bg-hl-line :extend t)))
    `(ctrlf-highlight-passive ((,c :inherit modus-themes-search-lazy)))
;;;;; custom (M-x customize)
    `(custom-button ((,c :inherit modus-themes-button)))
    `(custom-button-mouse ((,c :inherit (highlight custom-button))))
    `(custom-button-pressed ((,c :inherit (secondary-selection custom-button))))
    `(custom-changed ((,c :background ,bg-changed)))
    `(custom-comment ((,c :inherit shadow)))
    `(custom-comment-tag ((,c :inherit (bold shadow))))
    `(custom-invalid ((,c :inherit error :strike-through t)))
    `(custom-modified ((,c :inherit custom-changed)))
    `(custom-rogue ((,c :inherit custom-invalid)))
    `(custom-set ((,c :inherit success)))
    `(custom-state ((,c :foreground ,warning)))
    `(custom-themed ((,c :inherit custom-changed)))
    `(custom-variable-obsolete ((,c :inherit shadow)))
    `(custom-face-tag ((,c :inherit bold :foreground ,type)))
    `(custom-group-tag ((,c :inherit bold :foreground ,builtin)))
    `(custom-group-tag-1 ((,c :inherit bold :foreground ,constant)))
    `(custom-variable-tag ((,c :inherit bold :foreground ,variable)))
;;;;; dashboard
    `(dashboard-heading ((,c :foreground ,name)))
    `(dashboard-items-face (( ))) ; use the underlying style of all-the-icons
;;;;; deadgrep
    `(deadgrep-filename-face ((,c :inherit bold :foreground ,name)))
    `(deadgrep-match-face ((,c :inherit match)))
    `(deadgrep-meta-face ((,c :inherit shadow)))
    `(deadgrep-regexp-metachar-face ((,c :inherit font-lock-regexp-grouping-construct)))
    `(deadgrep-search-term-face ((,c :inherit success)))
;;;;; debbugs
    `(debbugs-gnu-archived ((,c :background ,bg-inactive :foreground ,fg-dim)))
    `(debbugs-gnu-done ((,c :inherit success)))
    `(debbugs-gnu-forwarded ((,c :inherit modus-themes-slant :foreground ,info)))
    `(debbugs-gnu-handled (( )))
    `(debbugs-gnu-marked ((,c :inherit modus-themes-mark-sel)))
    `(debbugs-gnu-marked-stale ((,c :inherit modus-themes-mark-alt)))
    `(debbugs-gnu-new ((,c :inherit error)))
    `(debbugs-gnu-pending ((,c :inherit modus-themes-slant :foreground ,fg-alt)))
    `(debbugs-gnu-stale-1 ((,c :foreground ,red-cooler)))
    `(debbugs-gnu-stale-2 ((,c :foreground ,yellow-warmer)))
    `(debbugs-gnu-stale-3 ((,c :foreground ,magenta-warmer)))
    `(debbugs-gnu-stale-4 ((,c :foreground ,magenta-cooler)))
    `(debbugs-gnu-stale-5 ((,c :foreground ,cyan-faint)))
    `(debbugs-gnu-tagged ((,c :inherit modus-themes-mark-alt)))
    `(debbugs-gnu-title ((,c :inherit bold)))
;;;;; deft
    `(deft-filter-string-face ((,c :inherit success)))
    `(deft-header-face ((,c :inherit shadow)))
    `(deft-separator-face ((,c :foreground "gray50")))
    `(deft-summary-face ((,c :inherit (shadow modus-themes-slant))))
    `(deft-time-face ((,c :foreground ,date-common)))
    `(deft-title-face ((,c :inherit bold)))
;;;;; denote
    `(denote-faces-date ((,c :foreground ,date-common)))
    `(denote-faces-delimiter ((,c :inherit shadow)))
    `(denote-faces-extension ((,c :inherit shadow)))
    `(denote-faces-keywords ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(denote-faces-link ((,c :inherit link)))
    `(denote-faces-prompt-current-name ((,c :inherit modus-themes-slant :foreground ,fg-changed-intense)))
    `(denote-faces-prompt-new-name ((,c :inherit modus-themes-slant :foreground ,fg-added-intense)))
    `(denote-faces-prompt-old-name ((,c :inherit modus-themes-slant :foreground ,fg-removed-intense)))
    `(denote-faces-signature ((,c :inherit modus-themes-bold :foreground ,string)))
    `(denote-faces-subdirectory ((,c :inherit modus-themes-bold :foreground ,fg-alt)))
    `(denote-faces-time ((,c :inherit denote-faces-date)))
    `(denote-faces-time-delimiter ((,c :inherit shadow)))
    `(denote-faces-title (( )))
;;;;; devdocs
    `(devdocs-code-block ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim :extend t)))
;;;;; dictionary
    `(dictionary-button-face ((,c :inherit bold)))
    `(dictionary-reference-face ((,c :inherit link)))
    `(dictionary-word-definition-face (( )))
    `(dictionary-word-entry-face ((,c :inherit font-lock-comment-face)))
;;;;; diff-hl
    `(diff-hl-change ((,c :background ,bg-changed-fringe)))
    `(diff-hl-delete ((,c :background ,bg-removed-fringe)))
    `(diff-hl-insert ((,c :background ,bg-added-fringe)))
    `(diff-hl-reverted-hunk-highlight ((,c :background ,fg-main :foreground ,bg-main)))
;;;;; diff-mode
    `(diff-added ((,c :background ,bg-added :foreground ,fg-added)))
    `(diff-changed ((,c :background ,bg-changed :foreground ,fg-changed :extend t)))
    `(diff-changed-unspecified ((,c :inherit diff-changed)))
    `(diff-removed ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(diff-refine-added ((,c :background ,bg-added-refine :foreground ,fg-added)))
    `(diff-refine-changed ((,c :background ,bg-changed-refine :foreground ,fg-changed)))
    `(diff-refine-removed ((,c :background ,bg-removed-refine :foreground ,fg-removed)))
    `(diff-indicator-added ((,c :inherit diff-added :foreground ,fg-added-intense)))
    `(diff-indicator-changed ((,c :inherit diff-changed :foreground ,fg-changed-intense)))
    `(diff-indicator-removed ((,c :inherit diff-removed :foreground ,fg-removed-intense)))
    `(diff-context (( )))
    `(diff-error ((,c :inherit error)))
    `(diff-file-header ((,c :inherit bold)))
    `(diff-function ((,c :background ,bg-inactive)))
    `(diff-header (( )))
    `(diff-hunk-header ((,c :inherit bold :background ,bg-inactive)))
    `(diff-index ((,c :inherit italic)))
    `(diff-nonexistent ((,c :inherit bold)))
;;;;; dim-autoload
    `(dim-autoload-cookie-line ((,c :inherit font-lock-comment-face)))
;;;;; dired
    `(dired-broken-symlink ((,c :inherit button :foreground ,err)))
    `(dired-directory ((,c :foreground ,accent-0)))
    `(dired-flagged ((,c :inherit modus-themes-mark-del)))
    `(dired-header ((,c :inherit bold)))
    `(dired-ignored ((,c :inherit shadow)))
    `(dired-mark ((,c :inherit bold)))
    `(dired-marked ((,c :inherit modus-themes-mark-sel)))
    `(dired-perm-write ((,c :inherit shadow)))
    `(dired-symlink ((,c :inherit button :background ,bg-link-symbolic :foreground ,fg-link-symbolic :underline ,underline-link-symbolic)))
    `(dired-warning ((,c :inherit warning)))
;;;;; dired-async
    `(dired-async-failures ((,c :inherit error)))
    `(dired-async-message ((,c :inherit bold)))
    `(dired-async-mode-message ((,c :inherit bold)))
;;;;; dired-git
    `(dired-git-branch-else ((,c :inherit bold :foreground ,accent-0)))
    `(dired-git-branch-master ((,c :inherit bold :foreground ,accent-1)))
;;;;; dired-git-info
    `(dgi-commit-message-face ((,c :foreground ,docstring)))
;;;;; dired-narrow
    `(dired-narrow-blink ((,c :inherit (modus-themes-prominent-warning bold))))
;;;;; dired-subtree
    ;; remove backgrounds from dired-subtree faces, else they break
    ;; dired-{flagged,marked} and any other face that sets a background
    ;; such as hl-line.  Also, denoting depth by varying shades of gray
    ;; is not good for accessibility.
    `(dired-subtree-depth-1-face (()))
    `(dired-subtree-depth-2-face (()))
    `(dired-subtree-depth-3-face (()))
    `(dired-subtree-depth-4-face (()))
    `(dired-subtree-depth-5-face (()))
    `(dired-subtree-depth-6-face (()))
;;;;; diredfl
    `(diredfl-autofile-name ((,c :background ,bg-inactive)))
    `(diredfl-compressed-file-name ((,c :foreground ,warning)))
    `(diredfl-compressed-file-suffix ((,c :foreground ,err)))
    `(diredfl-date-time ((,c :foreground ,date-common)))
    `(diredfl-deletion ((,c :inherit dired-flagged)))
    `(diredfl-deletion-file-name ((,c :inherit diredfl-deletion)))
    `(diredfl-dir-heading ((,c :inherit bold)))
    `(diredfl-dir-name ((,c :inherit dired-directory)))
    `(diredfl-dir-priv ((,c :inherit dired-directory)))
    `(diredfl-exec-priv ((,c :foreground ,accent-1)))
    `(diredfl-executable-tag ((,c :inherit diredfl-exec-priv)))
    `(diredfl-file-name ((,c :foreground ,fg-main)))
    `(diredfl-file-suffix ((,c :foreground ,variable)))
    `(diredfl-flag-mark ((,c :inherit dired-marked)))
    `(diredfl-flag-mark-line ((,c :inherit dired-marked)))
    `(diredfl-ignored-file-name ((,c :inherit shadow)))
    `(diredfl-link-priv ((,c :foreground ,fg-link)))
    `(diredfl-no-priv ((,c :inherit shadow)))
    `(diredfl-number ((,c :inherit shadow)))
    `(diredfl-other-priv ((,c :foreground ,accent-2)))
    `(diredfl-rare-priv ((,c :foreground ,accent-3)))
    `(diredfl-read-priv ((,c :foreground ,fg-main)))
    `(diredfl-symlink ((,c :inherit dired-symlink)))
    `(diredfl-tagged-autofile-name ((,c :inherit (diredfl-autofile-name dired-marked))))
    `(diredfl-write-priv ((,c :foreground ,accent-0)))
;;;;; disk-usage
    `(disk-usage-inaccessible ((,c :inherit error)))
    `(disk-usage-percent ((,c :foreground ,accent-0)))
    `(disk-usage-size ((,c :foreground ,accent-1)))
    `(disk-usage-symlink ((,c :inherit dired-symlink)))
    `(disk-usage-symlink-directory ((,c :inherit dired-symlink)))
;;;;; display-fill-column-indicator-mode
    `(fill-column-indicator ((,c :height 1 :background ,bg-active :foreground ,bg-active)))
;;;;; doom-modeline
    `(doom-modeline-bar ((,c :background ,blue)))
    `(doom-modeline-bar-inactive ((,c :background ,border)))
    `(doom-modeline-battery-charging ((,c :foreground ,modeline-info)))
    `(doom-modeline-battery-critical ((,c :underline t :foreground ,modeline-err)))
    `(doom-modeline-battery-error ((,c :underline t :foreground ,modeline-err)))
    `(doom-modeline-battery-full (( )))
    `(doom-modeline-battery-warning ((,c :inherit warning)))
    `(doom-modeline-buffer-file ((,c :inherit bold)))
    `(doom-modeline-buffer-major-mode (( )))
    `(doom-modeline-buffer-minor-mode (( )))
    `(doom-modeline-buffer-modified ((,c :foreground ,modeline-err)))
    `(doom-modeline-buffer-path (( )))
    `(doom-modeline-evil-emacs-state ((,c :inherit italic)))
    `(doom-modeline-evil-insert-state ((,c :foreground ,modeline-info)))
    `(doom-modeline-evil-motion-state (( )))
    `(doom-modeline-evil-normal-state (( )))
    `(doom-modeline-evil-operator-state ((,c :inherit bold)))
    `(doom-modeline-evil-replace-state ((,c :inherit error)))
    `(doom-modeline-evil-visual-state ((,c :inherit warning)))
    `(doom-modeline-info ((,c :inherit success)))
    `(doom-modeline-input-method (( )))
    `(doom-modeline-lsp-error ((,c :inherit bold)))
    `(doom-modeline-lsp-running (( )))
    `(doom-modeline-lsp-success ((,c :inherit success)))
    `(doom-modeline-lsp-warning ((,c :inherit warning)))
    `(doom-modeline-notification ((,c :inherit error)))
    `(doom-modeline-project-dir (( )))
    `(doom-modeline-project-parent-dir (( )))
    `(doom-modeline-project-root-dir (( )))
    `(doom-modeline-repl-success ((,c :inherit success)))
    `(doom-modeline-repl-warning ((,c :inherit warning)))
    `(doom-modeline-time (( )))
    `(doom-modeline-urgent ((,c :inherit bold :foreground ,modeline-err)))
    `(doom-modeline-warning ((,c :inherit warning)))
;;;;; ediff
    `(ediff-current-diff-A ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(ediff-current-diff-Ancestor ((,c :background ,bg-region)))
    `(ediff-current-diff-B ((,c :background ,bg-added :foreground ,fg-added)))
    `(ediff-current-diff-C ((,c :background ,bg-changed :foreground ,fg-changed)))
    `(ediff-even-diff-A ((,c :background ,bg-diff-context)))
    `(ediff-even-diff-Ancestor ((,c :background ,bg-diff-context)))
    `(ediff-even-diff-B ((,c :background ,bg-diff-context)))
    `(ediff-even-diff-C ((,c :background ,bg-diff-context)))
    `(ediff-fine-diff-A ((,c :background ,bg-removed-refine :foreground ,fg-removed)))
    `(ediff-fine-diff-Ancestor ((,c :inherit modus-themes-subtle-cyan)))
    `(ediff-fine-diff-B ((,c :background ,bg-added-refine :foreground ,fg-added)))
    `(ediff-fine-diff-C ((,c :background ,bg-changed-refine :foreground ,fg-changed)))
    `(ediff-odd-diff-A ((,c :inherit ediff-even-diff-A)))
    `(ediff-odd-diff-Ancestor ((,c :inherit ediff-even-diff-Ancestor)))
    `(ediff-odd-diff-B ((,c :inherit ediff-even-diff-B)))
    `(ediff-odd-diff-C ((,c :inherit ediff-even-diff-C)))
;;;;; ein (Emacs IPython Notebook)
    `(ein:basecell-input-area-face ((,c :background ,bg-dim :extend t)))
    `(ein:cell-output-area (( )))
    `(ein:cell-output-area-error ((,c :background ,bg-removed :extend t)))
    `(ein:cell-output-stderr ((,c :background ,bg-removed :extend t)))
    `(ein:markdowncell-input-area-face (( )))
    `(ein:notification-tab-normal ((,c :underline t)))
;;;;; eglot
    `(eglot-mode-line ((,c :inherit modus-themes-bold :foreground ,modeline-info)))
    `(eglot-diagnostic-tag-unnecessary-face ((,c :inherit modus-themes-lang-note)))
;;;;; el-search
    `(el-search-highlight-in-prompt-face ((,c :inherit italic)))
    `(el-search-match ((,c :inherit modus-themes-search-current)))
    `(el-search-other-match ((,c :inherit modus-themes-search-lazy)))
    `(el-search-occur-match ((,c :inherit match)))
;;;;; eldoc
    ;; NOTE: see https://github.com/purcell/package-lint/issues/187
    (list 'eldoc-highlight-function-argument `((,c :inherit bold :background ,bg-active-argument :foreground ,fg-active-argument)))
;;;;; eldoc-box
    `(eldoc-box-body ((,c :background ,bg-dim :foreground ,fg-main)))
    `(eldoc-box-border ((,c :background ,border)))
;;;;; elfeed
    `(elfeed-log-date-face ((,c :inherit elfeed-search-date-face)))
    `(elfeed-log-debug-level-face ((,c :inherit elfeed-search-filter-face)))
    `(elfeed-log-error-level-face ((,c :inherit error)))
    `(elfeed-log-info-level-face ((,c :inherit success)))
    `(elfeed-log-warn-level-face ((,c :inherit warning)))
    `(elfeed-search-date-face ((,c :foreground ,date-common)))
    `(elfeed-search-feed-face ((,c :foreground ,accent-1)))
    `(elfeed-search-filter-face ((,c :inherit bold)))
    `(elfeed-search-last-update-face ((,c :inherit bold :foreground ,date-common)))
    `(elfeed-search-tag-face ((,c :foreground ,accent-0)))
    `(elfeed-search-title-face ((,c :foreground ,fg-dim)))
    `(elfeed-search-unread-count-face (( )))
    `(elfeed-search-unread-title-face ((,c :inherit bold :foreground ,fg-main)))
;;;;; elfeed-score
    `(elfeed-score-date-face ((,c :foreground ,date-common)))
    `(elfeed-score-debug-level-face ((,c :inherit bold)))
    `(elfeed-score-error-level-face ((,c :inherit error)))
    `(elfeed-score-info-level-face ((,c :inherit success)))
    `(elfeed-score-warn-level-face ((,c :inherit warning)))
;;;;; elpher
    `(elpher-gemini-heading1 ((,c :inherit modus-themes-heading-1)))
    `(elpher-gemini-heading2 ((,c :inherit modus-themes-heading-2)))
    `(elpher-gemini-heading3 ((,c :inherit modus-themes-heading-3)))
;;;;; embark
    `(embark-collect-group-title ((,c :inherit bold :foreground ,name)))
    `(embark-keybinding ((,c :inherit modus-themes-key-binding)))
    `(embark-keybinding-repeat ((,c :inherit bold)))
    `(embark-selected ((,c :inherit modus-themes-mark-sel)))
;;;;; ement (ement.el)
    `(ement-room-fully-read-marker ((,c :inherit success)))
    `(ement-room-membership ((,c :inherit shadow)))
    `(ement-room-mention ((,c :inherit highlight)))
    `(ement-room-name ((,c :inherit bold)))
    `(ement-room-reactions ((,c :inherit shadow)))
    `(ement-room-read-receipt-marker ((,c :inherit match)))
    `(ement-room-self ((,c :inherit bold :foreground ,accent-1)))
    `(ement-room-self-message ((,c :foreground ,fg-alt)))
    `(ement-room-timestamp ((,c :inherit shadow)))
    `(ement-room-timestamp-header ((,c :inherit bold :foreground ,date-common)))
    `(ement-room-user ((,c :inherit bold :foreground ,accent-0)))
;;;;; emms
    `(emms-browser-album-face ((,c :foreground ,keyword)))
    `(emms-browser-artist-face ((,c :foreground ,variable)))
    `(emms-browser-composer-face ((,c :foreground ,builtin)))
    `(emms-browser-performer-face ((,c :inherit emms-browser-artist-face)))
    `(emms-browser-track-face ((,c :inherit emms-playlist-track-face)))
    `(emms-browser-year/genre-face ((,c :foreground ,type)))
    `(emms-playlist-track-face ((,c :foreground ,string)))
    `(emms-playlist-selected-face ((,c :inherit bold :foreground ,constant)))
    `(emms-metaplaylist-mode-current-face ((,c :inherit emms-playlist-selected-face)))
    `(emms-metaplaylist-mode-face ((,c :foreground ,variable)))
;;;;; enh-ruby-mode (enhanced-ruby-mode)
    `(enh-ruby-heredoc-delimiter-face ((,c :inherit font-lock-constant-face)))
    `(enh-ruby-op-face ((,c :foreground ,fg-main)))
    `(enh-ruby-regexp-delimiter-face ((,c :inherit font-lock-regexp-grouping-construct)))
    `(enh-ruby-regexp-face ((,c :inherit font-lock-string-face)))
    `(enh-ruby-string-delimiter-face ((,c :inherit font-lock-string-face)))
    `(erm-syn-errline ((,c :inherit modus-themes-lang-error)))
    `(erm-syn-warnline ((,c :inherit modus-themes-lang-warning)))
;;;;; epa
    `(epa-field-body (( )))
    `(epa-field-name ((,c :inherit bold :foreground ,fg-dim)))
    `(epa-mark ((,c :inherit bold)))
    `(epa-string ((,c :foreground ,string)))
    `(epa-validity-disabled ((,c :foreground ,err)))
    `(epa-validity-high ((,c :inherit success)))
    `(epa-validity-low ((,c :inherit shadow)))
    `(epa-validity-medium ((,c :foreground ,info)))
;;;;; erc
    `(erc-action-face ((,c :foreground ,accent-2)))
    `(erc-bold-face ((,c :inherit bold)))
    `(erc-button ((,c :inherit button)))
    `(erc-command-indicator-face ((,c :inherit bold :foreground ,accent-3)))
    `(erc-current-nick-face ((,c :inherit match)))
    `(erc-dangerous-host-face ((,c :inherit error)))
    `(erc-direct-msg-face ((,c :inherit shadow)))
    `(erc-error-face ((,c :inherit error)))
    `(erc-fill-wrap-merge-indicator-face ((,c :foreground ,fg-dim)))
    `(erc-fool-face ((,c :inherit shadow)))
    `(erc-input-face ((,c :foreground ,fnname)))
    `(erc-inverse-face ((,c :inherit erc-default-face :inverse-video t)))
    `(erc-keep-place-indicator-arrow ((,c :foreground ,info)))
    `(erc-keyword-face ((,c :inherit bold :foreground ,keyword)))
    `(erc-my-nick-face ((,c :inherit bold :foreground ,name)))
    `(erc-my-nick-prefix-face ((,c :inherit erc-my-nick-face)))
    `(erc-nick-default-face ((,c :inherit bold :foreground ,accent-0)))
    `(erc-nick-msg-face ((,c :inherit warning)))
    `(erc-nick-prefix-face ((,c :inherit erc-nick-default-face)))
    `(erc-notice-face ((,c :inherit font-lock-comment-face)))
    `(erc-pal-face ((,c :inherit bold :foreground ,accent-1)))
    `(erc-prompt-face ((,c :inherit modus-themes-prompt)))
    `(erc-timestamp-face ((,c :foreground ,date-common)))
    `(erc-underline-face ((,c :underline t)))
;;;;; ert
    `(ert-test-result-expected ((,c :inherit modus-themes-prominent-note)))
    `(ert-test-result-unexpected ((,c :inherit modus-themes-prominent-error)))
;;;;; erts-mode
    `(erts-mode-end-test ((,c :inherit error)))
    `(erts-mode-specification-name ((,c :inherit bold)))
    `(erts-mode-specification-value ((,c :foreground ,string)))
    `(erts-mode-start-test ((,c :inherit success)))
;;;;; eshell
    `(eshell-ls-archive ((,c :foreground ,accent-2)))
    `(eshell-ls-backup ((,c :inherit shadow)))
    `(eshell-ls-clutter ((,c :inherit shadow)))
    `(eshell-ls-directory ((,c :foreground ,accent-0)))
    `(eshell-ls-executable ((,c :foreground ,accent-1)))
    `(eshell-ls-missing ((,c :inherit error)))
    `(eshell-ls-product ((,c :inherit shadow)))
    `(eshell-ls-readonly ((,c :foreground ,warning)))
    `(eshell-ls-special ((,c :foreground ,accent-3)))
    `(eshell-ls-symlink ((,c :inherit link)))
    `(eshell-ls-unreadable ((,c :inherit shadow)))
    `(eshell-prompt ((,c :inherit modus-themes-prompt)))
;;;;; eshell-fringe-status
    `(eshell-fringe-status-failure ((,c :inherit error)))
    `(eshell-fringe-status-success ((,c :inherit success)))
;;;;; evil-mode
    `(evil-ex-commands ((,c :inherit font-lock-keyword-face)))
    `(evil-ex-info ((,c :inherit font-lock-type-face)))
    `(evil-ex-lazy-highlight ((,c :inherit modus-themes-search-lazy)))
    `(evil-ex-search ((,c :inherit modus-themes-search-current)))
    `(evil-ex-substitute-matches ((,c :inherit modus-themes-search-replace)))
    `(evil-ex-substitute-replacement ((,c :inherit modus-themes-search-current)))
;;;;; eww
    `(eww-invalid-certificate ((,c :foreground ,err)))
    `(eww-valid-certificate ((,c :foreground ,info)))
    `(eww-form-checkbox ((,c :inherit eww-form-text)))
    `(eww-form-file ((,c :inherit eww-form-submit)))
    `(eww-form-select ((,c :inherit eww-form-submit)))
    `(eww-form-submit ((,c :inherit modus-themes-button)))
    `(eww-form-text ((,c :inherit widget-field)))
    `(eww-form-textarea ((,c :inherit eww-form-text)))
;;;;; eyebrowse
    `(eyebrowse-mode-line-active ((,c :inherit mode-line-emphasis)))
;;;;; flycheck
    `(flycheck-error ((,c :inherit modus-themes-lang-error)))
    `(flycheck-fringe-error ((,c :inherit modus-themes-prominent-error)))
    `(flycheck-fringe-info ((,c :inherit modus-themes-prominent-note)))
    `(flycheck-fringe-warning ((,c :inherit modus-themes-prominent-warning)))
    `(flycheck-info ((,c :inherit modus-themes-lang-note)))
    `(flycheck-warning ((,c :inherit modus-themes-lang-warning)))
;;;;; flycheck-color-mode-line
    `(flycheck-color-mode-line-error-face ((,c :inherit flycheck-fringe-error)))
    `(flycheck-color-mode-line-info-face ((,c :inherit flycheck-fringe-info)))
    `(flycheck-color-mode-line-running-face ((,c :inherit italic)))
    `(flycheck-color-mode-line-info-face ((,c :inherit flycheck-fringe-warning)))
;;;;; flycheck-indicator
    `(flycheck-indicator-disabled ((,c :inherit modus-themes-slant :foreground ,fg-dim)))
    `(flycheck-indicator-error ((,c :inherit error)))
    `(flycheck-indicator-info ((,c :inherit bold)))
    `(flycheck-indicator-running ((,c :inherit modus-themes-slant)))
    `(flycheck-indicator-success ((,c :inherit success)))
    `(flycheck-indicator-warning ((,c :inherit warning)))
;;;;; flymake
    `(flymake-end-of-line-diagnostics-face ((,c :inherit modus-themes-slant :height 0.85 :box ,border)))
    `(flymake-error ((,c :inherit modus-themes-lang-error)))
    `(flymake-error-echo ((,c :inherit error)))
    `(flymake-error-echo-at-eol ((,c :inherit flymake-end-of-line-diagnostics-face :foreground ,err)))
    `(flymake-note ((,c :inherit modus-themes-lang-note)))
    `(flymake-note-echo ((,c :inherit success)))
    `(flymake-note-echo-at-eol ((,c :inherit flymake-end-of-line-diagnostics-face :foreground ,info)))
    `(flymake-warning ((,c :inherit modus-themes-lang-warning)))
    `(flymake-warning-echo ((,c :inherit warning)))
    `(flymake-warning-echo-at-eol ((,c :inherit flymake-end-of-line-diagnostics-face :foreground ,warning)))
;;;;; flyspell
    `(flyspell-duplicate ((,c :inherit modus-themes-lang-warning)))
    `(flyspell-incorrect ((,c :inherit modus-themes-lang-error)))
;;;;; flx
    `(flx-highlight-face ((,c :inherit modus-themes-completion-match-0)))
;;;;; focus
    `(focus-unfocused ((,c :foreground "gray50")))
;;;;; fold-this
    `(fold-this-overlay ((,c :background ,bg-inactive)))
;;;;; font-lock
    `(font-lock-bracket-face ((,c :foreground ,bracket)))
    `(font-lock-builtin-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(font-lock-comment-delimiter-face ((,c :inherit font-lock-comment-face)))
    `(font-lock-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(font-lock-constant-face ((,c :foreground ,constant)))
    `(font-lock-delimiter-face ((,c :foreground ,delimiter)))
    `(font-lock-doc-face ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(font-lock-doc-markup-face ((,c :inherit modus-themes-slant :foreground ,docmarkup)))
    `(font-lock-function-name-face ((,c :foreground ,fnname)))
    `(font-lock-keyword-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(font-lock-negation-char-face ((,c :inherit error)))
    `(font-lock-number-face ((,c :foreground ,number)))
    `(font-lock-operator-face ((,c :foreground ,operator)))
    `(font-lock-preprocessor-face ((,c :foreground ,preprocessor)))
    `(font-lock-property-name-face ((,c :foreground ,property)))
    `(font-lock-punctuation-face ((,c :foreground ,punctuation)))
    `(font-lock-regexp-grouping-backslash ((,c :inherit modus-themes-bold :foreground ,rx-backslash)))
    `(font-lock-regexp-grouping-construct ((,c :inherit modus-themes-bold :foreground ,rx-construct)))
    `(font-lock-string-face ((,c :foreground ,string)))
    `(font-lock-type-face ((,c :inherit modus-themes-bold :foreground ,type)))
    `(font-lock-variable-name-face ((,c :foreground ,variable)))
    `(font-lock-warning-face ((,c :inherit modus-themes-bold :foreground ,warning)))
;;;;; forge
    `(forge-dimmed ((,c :inherit shadow)))
    `(forge-issue-completed ((,c :inherit shadow)))
    `(forge-issue-open (( )))
    `(forge-issue-unplanned ((,c :inherit forge-dimmed :strike-through t)))
    `(forge-post-author ((,c :inherit bold :foreground ,name)))
    `(forge-post-date ((,c :inherit bold :foreground ,date-common)))
    `(forge-pullreq-merged ((,c :foreground ,fg-alt)))
    `(forge-pullreq-open ((,c :foreground ,info)))
    `(forge-pullreq-rejected ((,c :foreground ,err :strike-through t)))
    `(forge-topic-done (( )))
    `(forge-topic-pending ((,c :inherit italic)))
    `(forge-topic-slug-completed ((,c :inherit forge-dimmed)))
    `(forge-topic-slug-open ((,c :inherit forge-dimmed)))
    `(forge-topic-slug-saved ((,c :inherit success)))
    `(forge-topic-slug-unplanned ((,c :inherit forge-dimmed :strike-through t)))
    `(forge-topic-unread ((,c :inherit bold)))
;;;;; geiser
    `(geiser-font-lock-autodoc-current-arg ((,c :inherit bold :background ,bg-active-argument :foreground ,fg-active-argument)))
    `(geiser-font-lock-autodoc-identifier ((,c :foreground ,docstring)))
    `(geiser-font-lock-doc-button ((,c :inherit button)))
    `(geiser-font-lock-doc-link ((,c :inherit button)))
    `(geiser-font-lock-error-link ((,c :inherit button :foreground ,err)))
    `(geiser-font-lock-image-button ((,c :inherit button :foreground ,info)))
    `(geiser-font-lock-repl-input ((,c :inherit bold)))
    `(geiser-font-lock-repl-output ((,c :inherit font-lock-keyword-face)))
    `(geiser-font-lock-repl-prompt ((,c :inherit modus-themes-prompt)))
    `(geiser-font-lock-xref-header ((,c :inherit bold)))
    `(geiser-font-lock-xref-link ((,c :inherit button)))
;;;;; git-commit
    `(git-commit-comment-action ((,c :inherit font-lock-comment-face)))
    `(git-commit-comment-branch-local ((,c :inherit (bold font-lock-comment-face) :foreground ,accent-0)))
    `(git-commit-comment-branch-remote ((,c :inherit (bold font-lock-comment-face) :foreground ,accent-1)))
    `(git-commit-comment-heading ((,c :inherit (bold font-lock-comment-face))))
    `(git-commit-comment-file ((,c :inherit font-lock-comment-face :foreground ,accent-2))) ; like `magit-filename'
    `(git-commit-keyword ((,c :foreground ,keyword)))
    `(git-commit-nonempty-second-line ((,c :inherit error)))
    `(git-commit-overlong-summary ((,c :inherit warning)))
    `(git-commit-summary ((,c :inherit success)))
;;;;; git-gutter
    `(git-gutter:added ((,c :background ,bg-added-fringe)))
    `(git-gutter:deleted ((,c :background ,bg-removed-fringe)))
    `(git-gutter:modified ((,c :background ,bg-changed-fringe)))
    `(git-gutter:separator ((,c :inherit modus-themes-intense-cyan)))
    `(git-gutter:unchanged ((,c :inherit modus-themes-intense-magenta)))
;;;;; git-gutter-fr
    `(git-gutter-fr:added ((,c :background ,bg-added-fringe)))
    `(git-gutter-fr:deleted ((,c :background ,bg-removed-fringe)))
    `(git-gutter-fr:modified ((,c :background ,bg-changed-fringe)))
;;;;; git-rebase
    `(git-rebase-comment-hash ((,c :inherit (bold font-lock-comment-face) :foreground ,identifier)))
    `(git-rebase-comment-heading  ((,c :inherit (bold font-lock-comment-face))))
    `(git-rebase-description ((,c :foreground ,fg-main)))
    `(git-rebase-hash ((,c :foreground ,identifier)))
;;;;; git-timemachine
    `(git-timemachine-commit ((,c :inherit warning)))
    `(git-timemachine-minibuffer-author-face ((,c :foreground ,name)))
    `(git-timemachine-minibuffer-detail-face ((,c :foreground ,fg-main)))
;;;;; gnus
    `(gnus-button ((,c :inherit button :underline nil)))
    `(gnus-cite-1 ((,c :inherit message-cited-text-1)))
    `(gnus-cite-2 ((,c :inherit message-cited-text-2)))
    `(gnus-cite-3 ((,c :inherit message-cited-text-3)))
    `(gnus-cite-4 ((,c :inherit message-cited-text-4)))
    `(gnus-cite-5 ((,c :inherit message-cited-text-1)))
    `(gnus-cite-6 ((,c :inherit message-cited-text-2)))
    `(gnus-cite-7 ((,c :inherit message-cited-text-3)))
    `(gnus-cite-8 ((,c :inherit message-cited-text-4)))
    `(gnus-cite-9 ((,c :inherit message-cited-text-1)))
    `(gnus-cite-10 ((,c :inherit message-cited-text-2)))
    `(gnus-cite-11 ((,c :inherit message-cited-text-3)))
    `(gnus-cite-attribution ((,c :inherit italic)))
    `(gnus-emphasis-bold ((,c :inherit bold)))
    `(gnus-emphasis-bold-italic ((,c :inherit bold-italic)))
    `(gnus-emphasis-highlight-words ((,c :inherit warning)))
    `(gnus-emphasis-italic ((,c :inherit italic)))
    `(gnus-emphasis-underline-bold ((,c :inherit gnus-emphasis-bold :underline t)))
    `(gnus-emphasis-underline-bold-italic ((,c :inherit gnus-emphasis-bold-italic :underline t)))
    `(gnus-emphasis-underline-italic ((,c :inherit gnus-emphasis-italic :underline t)))
    `(gnus-group-mail-1 ((,c :inherit (bold gnus-group-mail-1-empty))))
    `(gnus-group-mail-1-empty ((,c :foreground ,magenta-warmer)))
    `(gnus-group-mail-2 ((,c :inherit (bold gnus-group-mail-2-empty))))
    `(gnus-group-mail-2-empty ((,c :foreground ,magenta)))
    `(gnus-group-mail-3 ((,c :inherit (bold gnus-group-mail-3-empty))))
    `(gnus-group-mail-3-empty ((,c :foreground ,magenta-cooler)))
    `(gnus-group-mail-low ((,c :inherit (bold gnus-group-mail-low-empty))))
    `(gnus-group-mail-low-empty ((,c :foreground ,fg-dim)))
    `(gnus-group-news-1 ((,c :inherit (bold gnus-group-news-1-empty))))
    `(gnus-group-news-1-empty ((,c :foreground ,green)))
    `(gnus-group-news-2 ((,c :inherit (bold gnus-group-news-2-empty))))
    `(gnus-group-news-2-empty ((,c :foreground ,cyan)))
    `(gnus-group-news-3 ((,c :inherit (bold gnus-group-news-3-empty))))
    `(gnus-group-news-3-empty ((,c :foreground ,yellow-faint)))
    `(gnus-group-news-4 ((,c :inherit (bold gnus-group-news-4-empty))))
    `(gnus-group-news-4-empty ((,c :foreground ,magenta-faint)))
    `(gnus-group-news-5 ((,c :inherit (bold gnus-group-news-5-empty))))
    `(gnus-group-news-5-empty ((,c :foreground ,fg-alt)))
    `(gnus-group-news-6 ((,c :inherit (bold gnus-group-news-6-empty))))
    `(gnus-group-news-6-empty ((,c :foreground ,fg-dim)))
    `(gnus-group-news-low ((,c :inherit (bold gnus-group-news-low-empty))))
    `(gnus-group-news-low-empty ((,c :foreground ,fg-dim)))
    `(gnus-header-content ((,c :inherit message-header-other)))
    `(gnus-header-from ((,c :inherit message-header-to :underline nil)))
    `(gnus-header-name ((,c :inherit message-header-name)))
    `(gnus-header-newsgroups ((,c :inherit message-header-newsgroups)))
    `(gnus-header-subject ((,c :inherit message-header-subject)))
    `(gnus-server-agent ((,c :inherit bold)))
    `(gnus-server-closed ((,c :inherit italic)))
    `(gnus-server-cloud ((,c :inherit bold :foreground ,fg-alt)))
    `(gnus-server-cloud-host ((,c :inherit bold :foreground ,fg-alt :underline t)))
    `(gnus-server-denied ((,c :inherit error)))
    `(gnus-server-offline ((,c :inherit shadow)))
    `(gnus-server-opened ((,c :inherit success)))
    `(gnus-summary-cancelled ((,c :inherit italic :foreground ,warning)))
    `(gnus-summary-high-ancient ((,c :inherit bold :foreground ,fg-alt)))
    `(gnus-summary-high-read ((,c :inherit bold :foreground ,fg-dim)))
    `(gnus-summary-high-ticked ((,c :inherit bold :foreground ,err)))
    `(gnus-summary-high-undownloaded ((,c :inherit bold-italic :foreground ,warning)))
    `(gnus-summary-high-unread ((,c :inherit bold)))
    `(gnus-summary-low-ancient ((,c :inherit italic)))
    `(gnus-summary-low-read ((,c :inherit (shadow italic))))
    `(gnus-summary-low-ticked ((,c :inherit italic :foreground ,err)))
    `(gnus-summary-low-undownloaded ((,c :inherit italic :foreground ,warning)))
    `(gnus-summary-low-unread ((,c :inherit italic)))
    `(gnus-summary-normal-ancient ((,c :inherit shadow)))
    `(gnus-summary-normal-read ((,c :inherit shadow)))
    `(gnus-summary-normal-ticked ((,c :foreground ,err)))
    `(gnus-summary-normal-undownloaded ((,c :foreground ,warning)))
    `(gnus-summary-normal-unread (( )))
    `(gnus-summary-selected ((,c :inherit highlight)))
;;;;; gotest
    `(go-test--ok-face ((,c :inherit success)))
    `(go-test--error-face ((,c :inherit error)))
    `(go-test--warning-face ((,c :inherit warning)))
    `(go-test--pointer-face ((,c :foreground ,accent-0)))
    `(go-test--standard-face (( )))
;;;;; golden-ratio-scroll-screen
    `(golden-ratio-scroll-highlight-line-face ((,c :background ,bg-cyan-subtle :foreground ,fg-main)))
;;;;; helpful
    `(helpful-heading ((,c :inherit modus-themes-heading-1)))
;;;;; hexl-mode
    `(hexl-address-region ((,c :foreground ,constant)))
    `(hexl-ascii-region ((,c :foreground ,variable)))
;;;;; hideshow
    `(hs-ellipsis (( )))
;;;;; highlight region or ad-hoc regexp
    ;; HACK 2022-06-23: The :inverse-video prevents hl-line-mode from
    ;; overriding the background.  Such an override really defeats the
    ;; purpose of setting those highlights.
    ;;
    ;; NOTE 2022-10-04: We do not use the ,c here but instead
    ;; hardcode color values.  We have to do this as the themes lack
    ;; entries in their palette for such an edge case.  Defining those
    ;; entries is not appropriate.
    `(hi-aquamarine ((((class color) (min-colors 88) (background light))
                      :background "#ffffff" :foreground "#227f9f" :inverse-video t)
                     (((class color) (min-colors 88) (background dark))
                      :background "#000000" :foreground "#66cbdc" :inverse-video t)))
    `(hi-black-b ((,c :inverse-video t)))
    `(hi-black-hb ((,c :background ,bg-main :foreground ,fg-dim :inverse-video t)))
    `(hi-blue ((((class color) (min-colors 88) (background light))
                :background "#ffffff" :foreground "#3366dd" :inverse-video t)
               (((class color) (min-colors 88) (background dark))
                :background "#000000" :foreground "#aaccff" :inverse-video t)))
    `(hi-blue-b ((,c :inherit (bold hi-blue))))
    `(hi-green ((((class color) (min-colors 88) (background light))
                 :background "#ffffff" :foreground "#008a00" :inverse-video t)
                (((class color) (min-colors 88) (background dark))
                 :background "#000000" :foreground "#66dd66" :inverse-video t)))
    `(hi-green-b ((,c :inherit (bold hi-green))))
    `(hi-pink ((((class color) (min-colors 88) (background light))
                :background "#ffffff" :foreground "#bd30aa" :inverse-video t)
               (((class color) (min-colors 88) (background dark))
                :background "#000000" :foreground "#ff88ee" :inverse-video t)))
    `(hi-red-b ((((class color) (min-colors 88) (background light))
                 :background "#ffffff" :foreground "#dd0000" :inverse-video t)
                (((class color) (min-colors 88) (background dark))
                 :background "#000000" :foreground "#f06666" :inverse-video t)))
    `(hi-salmon ((((class color) (min-colors 88) (background light))
                  :background "#ffffff" :foreground "#bf555a" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "#000000" :foreground "#e08a50" :inverse-video t)))
    `(hi-yellow ((((class color) (min-colors 88) (background light))
                  :background "#ffffff" :foreground "#af6400" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "#000000" :foreground "#faea00" :inverse-video t)))
    `(highlight-changes ((,c :foreground ,warning :underline nil)))
    `(highlight-changes-delete ((,c :foreground ,err :underline t)))
    `(hl-line ((,c :background ,bg-hl-line :extend t)))
;;;;; highlight-numbers
    `(highlight-numbers-number ((,c :foreground ,constant)))
;;;;; highlight-thing
    `(highlight-thing ((,c :inherit match)))
;;;;; hl-fill-column
    `(hl-fill-column-face ((,c :background ,bg-active)))
;;;;; hl-todo
    `(hl-todo ((,c :inherit (bold font-lock-comment-face) :foreground ,err)))
;;;;; howm
    `(action-lock-face ((,c :inherit button)))
    `(howm-mode-keyword-face (( )))
    `(howm-mode-ref-face ((,c :inherit link)))
    `(howm-mode-title-face ((,c :inherit modus-themes-heading-0)))
    `(howm-mode-wiki-face ((,c :inherit link)))
    `(howm-reminder-deadline-face ((,c :foreground ,date-deadline)))
    `(howm-reminder-late-deadline-face ((,c :inherit bold :foreground ,date-deadline)))
    `(howm-reminder-defer-face ((,c :foreground ,date-scheduled)))
    `(howm-reminder-scheduled-face ((,c :foreground ,date-scheduled)))
    `(howm-reminder-done-face ((,c :foreground ,prose-done)))
    `(howm-reminder-todo-face ((,c :foreground ,prose-todo)))
    `(howm-reminder-normal-face ((,c :foreground ,date-common)))
    `(howm-reminder-today-face ((,c :inherit bold :foreground ,date-common)))
    `(howm-reminder-tomorrow-face ((,c :inherit bold :foreground ,date-scheduled)))
    `(howm-simulate-todo-mode-line-face ((,c :inherit bold)))
    `(howm-view-empty-face (( )))
    `(howm-view-hilit-face ((,c :inherit match)))
    `(howm-view-name-face ((,c :inherit bold)))
    `(iigrep-counts-face1 ((,c :foreground ,rainbow-1)))
    `(iigrep-counts-face2 ((,c :foreground ,rainbow-2)))
    `(iigrep-counts-face3 ((,c :foreground ,rainbow-3)))
    `(iigrep-counts-face4 ((,c :foreground ,rainbow-4)))
    `(iigrep-counts-face5 ((,c :foreground ,rainbow-5)))
;;;;; hydra
    `(hydra-face-amaranth ((,c :inherit bold :foreground ,yellow-warmer)))
    `(hydra-face-blue ((,c :inherit bold :foreground ,blue)))
    `(hydra-face-pink ((,c :inherit bold :foreground ,magenta)))
    `(hydra-face-red ((,c :inherit bold :foreground ,red-faint)))
    `(hydra-face-teal ((,c :inherit bold :foreground ,cyan-cooler)))
;;;;; hyperbole
    `(hbut-item-face ((,c :foreground ,info)))
    `(hbut-face ((,c :inherit modus-themes-button)))
    `(hbut-flash ((,c :background ,bg-search-replace)))
    `(ibut-face ((,c :inherit button :background ,bg-link-symbolic :foreground ,fg-link-symbolic :underline ,underline-link-symbolic)))
;;;;; icomplete
    `(icomplete-first-match ((,c :inherit modus-themes-completion-match-0)))
    `(icomplete-vertical-selected-prefix-indicator-face ((,c :inherit bold :foreground ,keybind)))
    `(icomplete-vertical-unselected-prefix-indicator-face ((,c :inherit shadow)))
    `(icomplete-selected-match ((,c :inherit modus-themes-completion-selected)))
;;;;; ido-mode
    `(ido-first-match ((,c :inherit modus-themes-completion-match-0)))
    `(ido-incomplete-regexp ((,c :inherit error)))
    `(ido-indicator ((,c :inherit bold)))
    `(ido-only-match ((,c :inherit ido-first-match)))
    `(ido-subdir ((,c :foreground ,keyword)))
    `(ido-virtual ((,c :foreground ,warning)))
;;;;; iedit
    `(iedit-occurrence ((,c :inherit modus-themes-search-lazy)))
    `(iedit-read-only-occurrence ((,c :inherit modus-themes-search-current)))
;;;;; iflipb
    `(iflipb-current-buffer-face ((,c :inherit bold :foreground ,name)))
    `(iflipb-other-buffer-face ((,c :inherit shadow)))
;;;;; image-dired
    `(image-dired-thumb-flagged ((,c :inherit modus-themes-mark-del :box (:line-width -3))))
    `(image-dired-thumb-header-file-name ((,c :inherit bold)))
    `(image-dired-thumb-header-file-size ((,c :foreground ,constant)))
    `(image-dired-thumb-mark ((,c :inherit modus-themes-mark-sel :box (:line-width -3))))
;;;;; imenu-list
    `(imenu-list-entry-face-0 ((,c :foreground ,fg-heading-1)))
    `(imenu-list-entry-face-1 ((,c :foreground ,fg-heading-2)))
    `(imenu-list-entry-face-2 ((,c :foreground ,fg-heading-3)))
    `(imenu-list-entry-face-3 ((,c :foreground ,fg-heading-4)))
    `(imenu-list-entry-subalist-face-0 ((,c :inherit bold :foreground ,fg-heading-1 :underline t)))
    `(imenu-list-entry-subalist-face-1 ((,c :inherit bold :foreground ,fg-heading-2 :underline t)))
    `(imenu-list-entry-subalist-face-2 ((,c :inherit bold :foreground ,fg-heading-3 :underline t)))
    `(imenu-list-entry-subalist-face-3 ((,c :inherit bold :foreground ,fg-heading-4 :underline t)))
;;;;; indium
    `(indium-breakpoint-face ((,c :foreground ,err)))
    `(indium-frame-url-face ((,c :inherit (shadow button))))
    `(indium-keyword-face ((,c :inherit font-lock-keyword-face)))
    `(indium-litable-face ((,c :inherit modus-themes-slant)))
    `(indium-repl-error-face ((,c :inherit error)))
    `(indium-repl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(indium-repl-stdout-face (( )))
;;;;; info
    `(Info-quoted ((,c :inherit modus-themes-prose-verbatim))) ; the capitalization is canonical
    `(info-header-node ((,c :inherit (shadow bold))))
    `(info-header-xref ((,c :foreground ,fg-link)))
    `(info-index-match ((,c :inherit match)))
    `(info-menu-header ((,c :inherit bold)))
    `(info-menu-star ((,c :inherit error)))
    `(info-node ((,c :inherit bold)))
    `(info-title-1 ((,c :inherit modus-themes-heading-1)))
    `(info-title-2 ((,c :inherit modus-themes-heading-2)))
    `(info-title-3 ((,c :inherit modus-themes-heading-3)))
    `(info-title-4 ((,c :inherit modus-themes-heading-4)))
;;;;; info+ (info-plus)
    `(info-command-ref-item ((,c :inherit font-lock-function-name-face)))
    `(info-constant-ref-item ((,c :inherit font-lock-constant-face)))
    `(info-custom-delimited ((,c :inherit modus-themes-prose-verbatim)))
    `(info-double-quoted-name ((,c :inherit font-lock-string-face)))
    `(info-file (( )))
    `(info-function-ref-item ((,c :inherit font-lock-function-name-face)))
    `(info-glossary-word ((,c :inherit modus-themes-button)))
    `(info-indented-text (( )))
    `(info-isolated-backquote (( )))
    `(info-isolated-quote (( )))
    `(info-macro-ref-item ((,c :inherit font-lock-keyword-face)))
    `(info-menu ((,c :inherit bold)))
    `(info-quoted-name ((,c :inherit modus-themes-prose-verbatim)))
    `(info-reference-item ((,c :inherit bold)))
    `(info-special-form-ref-item ((,c :inherit warning)))
    `(info-string ((,c :inherit font-lock-string-face)))
    `(info-syntax-class-item ((,c :inherit modus-themes-prose-code)))
    `(info-user-option-ref-item ((,c :inherit font-lock-variable-name-face)))
    `(info-variable-ref-item ((,c :inherit font-lock-variable-name-face)))
;;;;; info-colors
    `(info-colors-lisp-code-block ((,c :inherit modus-themes-fixed-pitch)))
    `(info-colors-ref-item-command ((,c :inherit font-lock-function-name-face)))
    `(info-colors-ref-item-constant ((,c :inherit font-lock-constant-face)))
    `(info-colors-ref-item-function ((,c :inherit font-lock-function-name-face)))
    `(info-colors-ref-item-macro ((,c :inherit font-lock-keyword-face)))
    `(info-colors-ref-item-other ((,c :inherit font-lock-doc-face)))
    `(info-colors-ref-item-special-form ((,c :inherit font-lock-keyword-face)))
    `(info-colors-ref-item-syntax-class ((,c :inherit font-lock-builtin-face)))
    `(info-colors-ref-item-type ((,c :inherit font-lock-type-face)))
    `(info-colors-ref-item-user-option ((,c :inherit font-lock-variable-name-face)))
    `(info-colors-ref-item-variable ((,c :inherit font-lock-variable-name-face)))
;;;;; ioccur
    `(ioccur-cursor ((,c :foreground ,fg-main)))
    `(ioccur-invalid-regexp ((,c :inherit error)))
    `(ioccur-match-face ((,c :inherit match)))
    `(ioccur-match-overlay-face ((,c :background ,bg-inactive :extend t)))
    `(ioccur-num-line-face ((,c :inherit shadow)))
    `(ioccur-overlay-face ((,c :background ,bg-hl-line :extend t)))
    `(ioccur-regexp-face ((,c :inherit (modus-themes-search-current bold))))
    `(ioccur-title-face ((,c :inherit bold :foreground ,name)))
;;;;; isearch, occur, and the like
    `(isearch ((,c :inherit modus-themes-search-current)))
    `(isearch-fail ((,c :inherit modus-themes-prominent-error)))
    `(isearch-group-1 ((,c :inherit modus-themes-search-rx-group-0)))
    `(isearch-group-2 ((,c :inherit modus-themes-search-rx-group-1)))
    `(lazy-highlight ((,c :inherit modus-themes-search-lazy)))
    `(match ((,c :background ,bg-magenta-subtle :foreground ,fg-main)))
    `(query-replace ((,c :inherit modus-themes-search-replace)))
;;;;; ivy
    `(ivy-action ((,c :inherit modus-themes-key-binding)))
    `(ivy-confirm-face ((,c :inherit success)))
    `(ivy-current-match ((,c :inherit modus-themes-completion-selected)))
    `(ivy-match-required-face ((,c :inherit error)))
    `(ivy-minibuffer-match-face-1 (( )))
    `(ivy-minibuffer-match-face-2 ((,c :inherit modus-themes-completion-match-0)))
    `(ivy-minibuffer-match-face-3 ((,c :inherit modus-themes-completion-match-1)))
    `(ivy-minibuffer-match-face-4 ((,c :inherit modus-themes-completion-match-2)))
    `(ivy-remote ((,c :inherit italic)))
    `(ivy-separator ((,c :inherit shadow)))
    `(ivy-subdir ((,c :foreground ,keyword)))
    `(ivy-virtual ((,c :foreground ,warning)))
;;;;; ivy-posframe
    `(ivy-posframe-border ((,c :background ,border)))
    `(ivy-posframe-cursor ((,c :background ,fg-main :foreground ,bg-main)))
;;;;; jabber
    `(jabber-activity-face ((,c :foreground ,modeline-info)))
    `(jabber-roster-user-away ((,c :foreground ,red-faint)))
    `(jabber-roster-user-xa ((,c :foreground ,magenta :italic t)))
    `(jabber-roster-user-dnd ((,c :foreground ,red :bold t)))
    `(jabber-roster-user-chatty ((,c :foreground ,cyan-intense)))
    `(jabber-roster-user-error ((,c :inherit error)))
    `(jabber-roster-user-offline ((,c :foreground ,fg-dim :italic t)))
    `(jabber-roster-user-online ((,c :foreground ,cyan :weight bold)))
    `(jabber-chat-prompt-foreign ((,c :foreground ,red :weight bold)))
    `(jabber-chat-prompt-system ((,c :foreground ,green)))
    `(jabber-chat-prompt-local ((,c :foreground ,cyan)))
    `(jabber-chat-error ((,c :inherit error)))
    `(jabber-activity-personal-face ((,c :foreground ,blue-intense)))
    `(jabber-rare-time-face ((,c :foreground ,green-faint :underline t)))
    `(jabber-title-small ((,c :weight bold :height 1.0 :foreground ,fg-heading-3)))
    `(jabber-title-medium ((,c :weight bold :width expanded :height 2.0 :foreground ,fg-heading-2)))
    `(jabber-title-large ((,c :weight bold :width ultra-expanded :height 3.0 :foreground ,fg-heading-1)))
;;;;; japanese-holidays
    `(japanese-holiday-saturday ((,c :foreground ,date-holiday-other)))
;;;;; jira (org-jira)
    `(jiralib-comment-face ((,c :background ,bg-inactive)))
    `(jiralib-comment-header-face ((,c :inherit bold)))
    `(jiralib-issue-info-face ((,c :background ,bg-inactive)))
    `(jiralib-issue-info-header-face ((,c :inherit bold :background ,bg-inactive)))
    `(jiralib-issue-summary-face ((,c :inherit bold)))
    `(jiralib-link-filter-face ((,c :underline t)))
    `(jiralib-link-issue-face ((,c :underline t)))
    `(jiralib-link-project-face ((,c :underline t)))
;;;;; jit-spell
    `(jit-spell-misspelling ((,c :inherit modus-themes-lang-error)))
;;;;; jinx
    `(jinx-misspelled ((,c :inherit modus-themes-lang-warning)))
;;;;; journalctl-mode
    `(journalctl-error-face ((,c :inherit error)))
    `(journalctl-finished-face ((,c :inherit success)))
    `(journalctl-host-face ((,c :foreground ,name)))
    `(journalctl-process-face ((,c :foreground ,warning)))
    `(journalctl-starting-face ((,c :foreground ,info)))
    `(journalctl-timestamp-face ((,c :foreground ,date-common)))
    `(journalctl-warning-face ((,c :inherit warning)))
;;;;; js2-mode
    `(js2-error ((,c :inherit modus-themes-lang-error)))
    `(js2-external-variable ((,c :inherit font-lock-variable-name-face)))
    `(js2-function-call ((,c :inherit font-lock-function-name-face)))
    `(js2-function-param ((,c :inherit font-lock-constant-face)))
    `(js2-instance-member ((,c :inherit font-lock-keyword-face)))
    `(js2-jsdoc-html-tag-delimiter ((,c :foreground ,fg-main)))
    `(js2-jsdoc-html-tag-name ((,c :inherit font-lock-function-name-face)))
    `(js2-jsdoc-tag ((,c :inherit (font-lock-builtin-face font-lock-comment-face) :weight normal)))
    `(js2-jsdoc-type ((,c :inherit (font-lock-type-face font-lock-comment-face) :weight normal)))
    `(js2-jsdoc-value ((,c :inherit (font-lock-constant-face font-lock-comment-face) :weight normal)))
    `(js2-object-property ((,c :foreground ,fg-main)))
    `(js2-object-property-access ((,c :foreground ,fg-main)))
    `(js2-private-function-call ((,c :inherit font-lock-preprocessor-face)))
    `(js2-private-member ((,c :inherit font-lock-warning-face)))
    `(js2-warning ((,c :inherit modus-themes-lang-warning)))
;;;;; julia
    `(julia-macro-face ((,c :inherit font-lock-builtin-face)))
    `(julia-quoted-symbol-face ((,c :inherit font-lock-constant-face)))
;;;;; kaocha-runner
    `(kaocha-runner-error-face ((,c :inherit error)))
    `(kaocha-runner-success-face ((,c :inherit success)))
    `(kaocha-runner-warning-face ((,c :inherit warning)))
;;;;; keycast
    `(keycast-command ((,c :inherit bold)))
    `(keycast-key ((,c :inherit modus-themes-bold :background ,keybind :foreground ,bg-main)))
;;;;; kmacro-menu
    ;; Use `list' here to avoid a spurious warning about `kmacro-menu-mark'.
    (list 'kmacro-menu-mark `((,c :inherit bold)))
    `(kmacro-menu-marked ((,c :inherit modus-themes-mark-sel)))
    `(kmacro-menu-flagged ((,c :inherit modus-themes-mark-del)))
;;;;; ledger-mode
    `(ledger-font-auto-xact-face ((,c :inherit font-lock-builtin-face)))
    `(ledger-font-account-name-face ((,c :foreground ,name)))
    `(ledger-font-directive-face ((,c :inherit font-lock-keyword-face)))
    `(ledger-font-posting-date-face ((,c :inherit modus-themes-bold :foreground ,date-common)))
    `(ledger-font-periodic-xact-face ((,c :inherit font-lock-variable-name-face)))
    `(ledger-font-posting-amount-face ((,c :inherit font-lock-constant-face)))
    `(ledger-font-payee-cleared-face ((,c :inherit success)))
    `(ledger-font-payee-pending-face ((,c :inherit warning)))
    `(ledger-font-payee-uncleared-face ((,c :inherit error)))
    `(ledger-font-xact-highlight-face ((,c :background ,bg-hl-line :extend t)))
;;;;; leerzeichen
    `(leerzeichen ((,c :background ,bg-inactive)))
;;;;; line numbers (display-line-numbers-mode and global variant)
    ;; Here we cannot inherit `modus-themes-fixed-pitch'.  We need to
    ;; fall back to `default' otherwise line numbers do not scale when
    ;; using `text-scale-adjust'.
    `(line-number ((,c :inherit ,(if modus-themes-mixed-fonts '(fixed-pitch default) 'default) :background ,bg-line-number-inactive :foreground ,fg-line-number-inactive)))
    `(line-number-current-line ((,c :inherit (bold line-number) :background ,bg-line-number-active :foreground ,fg-line-number-active)))
    `(line-number-major-tick ((,c :inherit line-number :foreground ,err)))
    `(line-number-minor-tick ((,c :inherit line-number :foreground ,fg-alt)))
;;;;; lsp
    `(lsp-details-face ((t :height 0.9 :inherit shadow)))
    `(lsp-face-rename ((t :inherit modus-themes-search-replace)))
    `(lsp-headerline-breadcrumb-separator-face ((t :inherit shadow)))
    `(lsp-headerline-breadcrumb-path-face ((t :inherit font-lock-string-face)))
    `(lsp-headerline-breadcrumb-path-error-face ((,c :inherit (lsp-headerline-breadcrumb-path-face modus-themes-lang-error))))
    `(lsp-headerline-breadcrumb-path-warning-face ((,c :inherit (lsp-headerline-breadcrumb-path-face modus-themes-lang-warning))))
    `(lsp-headerline-breadcrumb-path-info-face ((,c :inherit (lsp-headerline-breadcrumb-path-face modus-themes-lang-note))))
    `(lsp-headerline-breadcrumb-path-hint-face ((,c :inherit (lsp-headerline-breadcrumb-path-face modus-themes-lang-note))))
    `(lsp-headerline-breadcrumb-project-prefix-face ((t :inherit bold :foreground ,name)))
    `(lsp-headerline-breadcrumb-unknown-project-prefix-face ((t :inherit (shadow bold))))
    `(lsp-headerline-breadcrumb-symbols-face ((t :inherit (bold font-lock-doc-face))))
    `(lsp-headerline-breadcrumb-symbols-error-face ((,c :inherit (lsp-headerline-breadcrumb-symbols-face modus-themes-lang-error))))
    `(lsp-headerline-breadcrumb-symbols-warning-face ((,c :inherit (lsp-headerline-breadcrumb-symbols-face modus-themes-lang-warning))))
    `(lsp-headerline-breadcrumb-symbols-info-face ((,c :inherit (lsp-headerline-breadcrumb-symbols-face modus-themes-lang-note))))
    `(lsp-headerline-breadcrumb-symbols-hint-face ((,c :inherit (lsp-headerline-breadcrumb-symbols-face modus-themes-lang-note))))
    `(lsp-installation-buffer-face ((,c :foreground ,info)))
    `(lsp-installation-finished-buffer-face ((,c :foreground ,info)))
    `(lsp-lens-mouse-face ((t :height 0.9 :inherit link)))
    `(lsp-modeline-code-actions-face ((,c :foreground ,modeline-warning)))
    `(lsp-modeline-code-actions-preferred-face ((,c :foreground ,modeline-warning)))
    `(lsp-signature-posframe ((,c :background ,bg-dim :foreground ,fg-main)))
    (list 'lsp-signature-highlight-function-argument `((,c :inherit bold :background ,bg-active-argument :foreground ,fg-active-argument)))
;;;;; lsp-ui
    `(lsp-ui-doc-background ((,c :background ,bg-dim)))
    `(lsp-ui-doc-header ((t :inherit bold)))
    `(lsp-ui-peek-peek ((,c :background ,bg-dim)))
    `(lsp-ui-peek-list ((,c :background ,bg-dim)))
    `(lsp-ui-peek-filename ((,c :inherit bold :foreground ,name)))
    `(lsp-ui-peek-line-number ((t :inherit line-number)))
    `(lsp-ui-peek-highlight ((,c :background ,bg-dim :box (:line-width -1 :color ,border))))
    `(lsp-ui-peek-header ((t :inherit bold)))
    `(lsp-ui-peek-selection ((t :inherit region)))
    `(lsp-ui-sideline-symbol ((,c :foreground ,fg-dim :box (:line-width -1 :color ,border))))
    `(lsp-ui-sideline-current-symbol ((,c :inherit bold :foreground ,fg-main :box (:line-width -1 :color ,border))))
    `(lsp-ui-sideline-code-action ((t :inherit lsp-modeline-code-actions-face)))
    `(lsp-ui-sideline-symbol-info ((,c :inherit modus-themes-slant)))
;;;;; magit
    `(magit-bisect-bad ((,c :inherit error)))
    `(magit-bisect-good ((,c :inherit success)))
    `(magit-bisect-skip ((,c :inherit warning)))
    `(magit-blame-date (( )))
    `(magit-blame-dimmed ((,c :inherit shadow)))
    `(magit-blame-hash (( )))
    `(magit-blame-highlight ((,c :background ,bg-active :foreground ,fg-main)))
    `(magit-blame-name (( )))
    `(magit-blame-summary ((  )))
    `(magit-branch-local ((,c :foreground ,accent-0)))
    `(magit-branch-remote ((,c :foreground ,accent-1)))
    `(magit-branch-upstream ((,c :inherit italic)))
    `(magit-branch-warning ((,c :inherit warning)))
    `(magit-cherry-equivalent ((,c :foreground ,magenta)))
    `(magit-cherry-unmatched ((,c :foreground ,cyan)))
    `(magit-diff-added ((,c :background ,bg-added-faint :foreground ,fg-added)))
    `(magit-diff-added-highlight ((,c :background ,bg-added :foreground ,fg-added)))
    `(magit-diff-base ((,c :background ,bg-changed-faint :foreground ,fg-changed)))
    `(magit-diff-base-highlight ((,c :background ,bg-changed :foreground ,fg-changed)))
    `(magit-diff-context ((,c :inherit shadow)))
    `(magit-diff-context-highlight ((,c :background ,bg-diff-context)))
    `(magit-diff-file-heading ((,c :inherit bold :foreground ,accent-0)))
    `(magit-diff-file-heading-highlight ((,c :inherit magit-diff-file-heading :background ,bg-inactive)))
    `(magit-diff-file-heading-selection ((,c :inherit bold :background ,bg-hover-secondary)))
    `(magit-diff-hunk-heading ((,c :background ,bg-inactive)))
    `(magit-diff-hunk-heading-highlight ((,c :inherit bold :background ,bg-active)))
    `(magit-diff-hunk-heading-selection ((,c :inherit bold :background ,bg-hover-secondary)))
    `(magit-diff-hunk-region ((,c :inherit bold)))
    `(magit-diff-lines-boundary ((,c :background ,fg-main)))
    `(magit-diff-lines-heading ((,c :background ,fg-dim :foreground ,bg-main)))
    `(magit-diff-removed ((,c :background ,bg-removed-faint :foreground ,fg-removed)))
    `(magit-diff-removed-highlight ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(magit-diffstat-added ((,c :foreground ,fg-added-intense)))
    `(magit-diffstat-removed ((,c :foreground ,fg-removed-intense)))
    `(magit-dimmed ((,c :inherit shadow)))
    `(magit-filename ((,c :foreground ,accent-2)))
    `(magit-hash ((,c :foreground ,identifier)))
    `(magit-head ((,c :inherit magit-branch-local)))
    `(magit-header-line ((,c :inherit bold)))
    `(magit-header-line-key ((,c :inherit modus-themes-key-binding)))
    `(magit-header-line-log-select ((,c :inherit bold)))
    `(magit-keyword ((,c :foreground ,keyword)))
    `(magit-keyword-squash ((,c :inherit bold :foreground ,warning)))
    `(magit-log-author ((,c :foreground ,name)))
    `(magit-log-date ((,c :foreground ,date-common)))
    `(magit-log-graph ((,c :inherit shadow)))
    `(magit-mode-line-process ((,c :inherit bold :foreground ,modeline-info)))
    `(magit-mode-line-process-error ((,c :inherit bold :foreground ,modeline-err)))
    `(magit-process-ng ((,c :inherit error)))
    `(magit-process-ok ((,c :inherit success)))
    `(magit-reflog-amend ((,c :inherit warning)))
    `(magit-reflog-checkout ((,c :inherit bold :foreground ,blue)))
    `(magit-reflog-cherry-pick ((,c :inherit success)))
    `(magit-reflog-commit ((,c :inherit bold)))
    `(magit-reflog-merge ((,c :inherit success)))
    `(magit-reflog-other ((,c :inherit bold :foreground ,cyan)))
    `(magit-reflog-rebase ((,c :inherit bold :foreground ,magenta)))
    `(magit-reflog-remote ((,c :inherit (bold magit-branch-remote))))
    `(magit-reflog-reset ((,c :inherit error)))
    `(magit-refname ((,c :inherit shadow)))
    `(magit-refname-pullreq ((,c :inherit shadow)))
    `(magit-refname-stash ((,c :inherit shadow)))
    `(magit-refname-wip ((,c :inherit shadow)))
    `(magit-section ((,c :background ,bg-dim :foreground ,fg-main)))
    `(magit-section-heading ((,c :inherit bold)))
    `(magit-section-heading-selection ((,c :inherit bold :background ,bg-hover-secondary)))
    `(magit-section-highlight ((,c :background ,bg-dim)))
    `(magit-sequence-done ((,c :inherit success)))
    `(magit-sequence-drop ((,c :inherit error)))
    `(magit-sequence-exec ((,c :inherit bold :foreground ,magenta)))
    `(magit-sequence-head ((,c :inherit bold :foreground ,cyan)))
    `(magit-sequence-onto ((,c :inherit (bold shadow))))
    `(magit-sequence-part ((,c :inherit warning)))
    `(magit-sequence-pick ((,c :inherit bold)))
    `(magit-sequence-stop ((,c :inherit error)))
    `(magit-signature-bad ((,c :inherit error)))
    `(magit-signature-error ((,c :inherit error)))
    `(magit-signature-expired ((,c :inherit warning)))
    `(magit-signature-expired-key ((,c :foreground ,warning)))
    `(magit-signature-good ((,c :inherit success)))
    `(magit-signature-revoked ((,c :inherit bold :foreground ,warning)))
    `(magit-signature-untrusted ((,c :inherit (bold shadow))))
    `(magit-tag ((,c :foreground ,accent-3))) ; compare with branches
;;;;; make-mode (makefiles)
    `(makefile-makepp-perl ((,c :background ,bg-dim)))
    `(makefile-space ((,c :background ,bg-inactive)))
;;;;; man
    `(Man-overstrike ((,c :inherit bold :foreground ,accent-0)))
    `(Man-underline ((,c :foreground ,accent-1 :underline t)))
;;;;; marginalia
    `(marginalia-archive ((,c :foreground ,accent-0)))
    `(marginalia-char ((,c :foreground ,accent-2)))
    `(marginalia-date ((,c :foreground ,date-common)))
    `(marginalia-documentation ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(marginalia-file-name (( )))
    `(marginalia-file-owner ((,c :inherit shadow)))
    `(marginalia-file-priv-dir ((,c :foreground ,accent-0)))
    `(marginalia-file-priv-exec ((,c :foreground ,accent-1)))
    `(marginalia-file-priv-link ((,c :foreground ,fg-link)))
    `(marginalia-file-priv-no ((,c :inherit shadow)))
    `(marginalia-file-priv-other ((,c :foreground ,accent-2)))
    `(marginalia-file-priv-rare ((,c :foreground ,accent-3)))
    `(marginalia-file-priv-read ((,c :foreground ,fg-main)))
    `(marginalia-file-priv-write ((,c :foreground ,accent-0)))
    `(marginalia-function ((,c :foreground ,fnname)))
    `(marginalia-key ((,c :inherit modus-themes-key-binding)))
    `(marginalia-lighter ((,c :inherit shadow)))
    `(marginalia-liqst ((,c :inherit shadow)))
    `(marginalia-mode ((,c :foreground ,constant)))
    `(marginalia-modified ((,c :inherit warning)))
    `(marginalia-null ((,c :inherit shadow)))
    `(marginalia-number ((,c :foreground ,constant)))
    `(marginalia-size ((,c :foreground ,variable)))
    `(marginalia-string ((,c :foreground ,string)))
    `(marginalia-symbol ((,c :foreground ,builtin)))
    `(marginalia-true (( )))
    `(marginalia-type ((,c :foreground ,type)))
    `(marginalia-value ((,c :inherit shadow)))
    `(marginalia-version ((,c :foreground ,date-common)))
;;;;; markdown-mode
    `(markdown-blockquote-face ((,c :inherit font-lock-doc-face)))
    `(markdown-bold-face ((,c :inherit bold)))
    `(markdown-code-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim :extend t)))
    `(markdown-gfm-checkbox-face ((,c :foreground ,warning)))
    `(markdown-header-face (( )))
    `(markdown-header-face-1 ((,c :inherit modus-themes-heading-1)))
    `(markdown-header-face-2 ((,c :inherit modus-themes-heading-2)))
    `(markdown-header-face-3 ((,c :inherit modus-themes-heading-3)))
    `(markdown-header-face-4 ((,c :inherit modus-themes-heading-4)))
    `(markdown-header-face-5 ((,c :inherit modus-themes-heading-5)))
    `(markdown-header-face-6 ((,c :inherit modus-themes-heading-6)))
    `(markdown-highlighting-face ((,c :inherit secondary-selection)))
    `(markdown-inline-code-face ((,c :inherit modus-themes-prose-code)))
    `(markdown-italic-face ((,c :inherit italic)))
    `(markdown-language-keyword-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-delimiter :foreground ,fg-prose-block-delimiter)))
    `(markdown-line-break-face ((,c :inherit nobreak-space)))
    `(markdown-link-face ((,c :inherit link)))
    `(markdown-markup-face ((,c :inherit shadow)))
    `(markdown-metadata-key-face ((,c :inherit bold)))
    `(markdown-metadata-value-face ((,c :foreground ,string)))
    `(markdown-missing-link-face ((,c :inherit warning)))
    `(markdown-pre-face ((,c :inherit markdown-code-face)))
    `(markdown-table-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-table)))
    `(markdown-url-face ((,c :foreground ,fg-alt)))
;;;;; markup-faces (`adoc-mode')
    `(markup-attribute-face ((,c :inherit (modus-themes-slant markup-meta-face))))
    `(markup-bold-face ((,c :inherit bold)))
    `(markup-code-face ((,c :inherit modus-themes-prose-code)))
    `(markup-comment-face ((,c :inherit font-lock-comment-face)))
    `(markup-complex-replacement-face ((,c :inherit modus-themes-prose-macro)))
    `(markup-emphasis-face ((,c :inherit markup-italic-face)))
    `(markup-error-face ((,c :inherit error)))
    `(markup-gen-face ((,c :inherit modus-themes-prose-verbatim)))
    `(markup-internal-reference-face ((,c :inherit (shadow modus-themes-slant))))
    `(markup-italic-face ((,c :inherit italic)))
    `(markup-list-face ((,c :background ,bg-inactive)))
    `(markup-meta-face ((,c :inherit (modus-themes-fixed-pitch shadow))))
    `(markup-meta-hide-face ((,c :foreground "gray50")))
    `(markup-reference-face ((,c :inherit modus-themes-slant :foreground ,fg-alt)))
    `(markup-replacement-face ((,c :inherit modus-themes-fixed-pitch :foreground ,err)))
    `(markup-secondary-text-face ((,c :height 0.9 :foreground ,fg-alt)))
    `(markup-small-face ((,c :inherit markup-gen-face :height 0.9)))
    `(markup-strong-face ((,c :inherit markup-bold-face)))
    `(markup-subscript-face ((,c :height 0.9 :foreground ,fg-alt)))
    `(markup-superscript-face ((,c :height 0.9 :foreground ,fg-alt)))
    `(markup-table-cell-face (( )))
    `(markup-table-face ((,c :foreground ,prose-table)))
    `(markup-table-row-face (( )))
    `(markup-title-0-face ((,c :inherit modus-themes-heading-1)))
    `(markup-title-1-face ((,c :inherit modus-themes-heading-2)))
    `(markup-title-2-face ((,c :inherit modus-themes-heading-3)))
    `(markup-title-3-face ((,c :inherit modus-themes-heading-4)))
    `(markup-title-4-face ((,c :inherit modus-themes-heading-5)))
    `(markup-title-5-face ((,c :inherit modus-themes-heading-6)))
    `(markup-verbatim-face ((,c :inherit modus-themes-prose-verbatim)))
;;;;; mbdepth
    `(minibuffer-depth-indicator ((,c :inherit modus-themes-mark-alt)))
;;;;; mct
    `(mct-highlight-candidate ((,c :inherit modus-themes-completion-selected)))
;;;;; messages
    `(message-cited-text-1 ((,c :foreground ,mail-cite-0)))
    `(message-cited-text-2 ((,c :foreground ,mail-cite-1)))
    `(message-cited-text-3 ((,c :foreground ,mail-cite-2)))
    `(message-cited-text-4 ((,c :foreground ,mail-cite-3)))
    `(message-header-name ((,c :inherit bold)))
    `(message-header-newsgroups ((,c :inherit message-header-other)))
    `(message-header-to ((,c :inherit bold :foreground ,mail-recipient)))
    `(message-header-cc ((,c :foreground ,mail-recipient)))
    `(message-header-subject ((,c :inherit bold :foreground ,mail-subject)))
    `(message-header-xheader ((,c :inherit message-header-other)))
    `(message-header-other ((,c :foreground ,mail-other)))
    `(message-mml ((,c :foreground ,mail-part)))
    `(message-separator ((,c :background ,bg-inactive :foreground ,fg-main)))
;;;;; minimap
    `(minimap-active-region-background ((,c :background ,bg-active)))
    `(minimap-current-line-face ((,c :background ,bg-cyan-intense :foreground ,fg-main)))
;;;;; mode-line
    `(mode-line ((,c :inherit modus-themes-ui-variable-pitch
                     :box ,border-mode-line-active
                     :background ,bg-mode-line-active
                     :foreground ,fg-mode-line-active)))
    `(mode-line-active ((,c :inherit mode-line)))
    `(mode-line-buffer-id ((,c :inherit bold)))
    `(mode-line-emphasis ((,c :inherit bold :foreground ,modeline-info)))
    `(mode-line-highlight ((,c :background ,bg-hover :foreground ,fg-main :box ,fg-main)))
    `(mode-line-inactive ((,c :inherit modus-themes-ui-variable-pitch
                              :box ,border-mode-line-inactive
                              :background ,bg-mode-line-inactive
                              :foreground ,fg-mode-line-inactive)))
;;;;; mood-line
    `(mood-line-modified ((,c :inherit italic)))
    `(mood-line-status-error ((,c :inherit error)))
    `(mood-line-status-info ((,c :foreground ,info)))
    `(mood-line-status-neutral (( )))
    `(mood-line-status-success ((,c :inherit success)))
    `(mood-line-status-warning ((,c :inherit warning)))
    `(mood-line-unimportant ((,c :inherit shadow)))
;;;;; mpdel
    `(mpdel-browser-directory-face ((,c :foreground ,accent-0)))
    `(mpdel-playlist-current-song-face ((,c :inherit bold :foreground ,accent-0)))
;;;;; mu4e
    `(mu4e-attach-number-face ((,c :inherit bold :foreground ,fg-dim)))
    `(mu4e-cited-1-face ((,c :inherit message-cited-text-1)))
    `(mu4e-cited-2-face ((,c :inherit message-cited-text-2)))
    `(mu4e-cited-3-face ((,c :inherit message-cited-text-3)))
    `(mu4e-cited-4-face ((,c :inherit message-cited-text-4)))
    `(mu4e-cited-5-face ((,c :inherit message-cited-text-1)))
    `(mu4e-cited-6-face ((,c :inherit message-cited-text-2)))
    `(mu4e-cited-7-face ((,c :inherit message-cited-text-3)))
    `(mu4e-compose-header-face ((,c :inherit mu4e-compose-separator-face)))
    `(mu4e-compose-separator-face ((,c :inherit message-separator)))
    `(mu4e-contact-face ((,c :inherit message-header-to)))
    `(mu4e-context-face ((,c :inherit bold)))
    `(mu4e-draft-face ((,c :foreground ,warning)))
    `(mu4e-flagged-face ((,c :foreground ,keyword)))
    `(mu4e-footer-face ((,c :inherit italic :foreground ,fg-alt)))
    `(mu4e-forwarded-face ((,c :inherit italic :foreground ,info)))
    `(mu4e-header-face ((,c :inherit shadow)))
    `(mu4e-header-highlight-face ((,c :background ,bg-hl-line :extend t)))
    `(mu4e-header-key-face ((,c :inherit message-header-name)))
    `(mu4e-header-marks-face ((,c :inherit mu4e-special-header-value-face)))
    `(mu4e-header-title-face ((,c :foreground ,fg-alt)))
    `(mu4e-header-value-face ((,c :inherit message-header-other)))
    `(mu4e-highlight-face ((,c :inherit modus-themes-key-binding)))
    `(mu4e-link-face ((,c :inherit link)))
    `(mu4e-modeline-face (( )))
    `(mu4e-moved-face ((,c :inherit italic :foreground ,warning)))
    `(mu4e-ok-face ((,c :inherit success)))
    `(mu4e-region-code ((,c :foreground ,builtin)))
    `(mu4e-related-face ((,c :inherit (italic shadow))))
    `(mu4e-replied-face ((,c :foreground ,info)))
    `(mu4e-special-header-value-face ((,c :inherit message-header-subject)))
    `(mu4e-system-face ((,c :inherit italic)))
    `(mu4e-thread-fold-face ((,c :foreground ,border)))
    `(mu4e-title-face (( )))
    `(mu4e-trashed-face ((,c :foreground ,err)))
    `(mu4e-unread-face ((,c :inherit bold)))
    `(mu4e-url-number-face ((,c :inherit shadow)))
    `(mu4e-view-body-face (( )))
    `(mu4e-warning-face ((,c :inherit warning)))
;;;;; multiple-cursors
    `(mc/cursor-bar-face ((,c :height 1 :foreground ,fg-main :background ,bg-main)))
    `(mc/cursor-face ((,c :inverse-video t)))
    `(mc/region-face ((,c :inherit region)))
;;;;; nerd-icons
    `(nerd-icons-blue ((,c :foreground ,blue-cooler)))
    `(nerd-icons-blue-alt ((,c :foreground ,blue-warmer)))
    `(nerd-icons-cyan ((,c :foreground ,cyan)))
    `(nerd-icons-cyan-alt ((,c :foreground ,cyan-warmer)))
    `(nerd-icons-dblue ((,c :foreground ,blue-faint)))
    `(nerd-icons-dcyan ((,c :foreground ,cyan-faint)))
    `(nerd-icons-dgreen ((,c :foreground ,green-faint)))
    `(nerd-icons-dmaroon ((,c :foreground ,magenta-faint)))
    `(nerd-icons-dorange ((,c :foreground ,red-faint)))
    `(nerd-icons-dpink ((,c :foreground ,magenta-faint)))
    `(nerd-icons-dpurple ((,c :foreground ,magenta-cooler)))
    `(nerd-icons-dred ((,c :foreground ,red)))
    `(nerd-icons-dsilver ((,c :foreground ,cyan-faint)))
    `(nerd-icons-dyellow ((,c :foreground ,yellow-faint)))
    `(nerd-icons-green ((,c :foreground ,green)))
    `(nerd-icons-lblue ((,c :foreground ,blue-cooler)))
    `(nerd-icons-lcyan ((,c :foreground ,cyan)))
    `(nerd-icons-lgreen ((,c :foreground ,green-warmer)))
    `(nerd-icons-lmaroon ((,c :foreground ,magenta-warmer)))
    `(nerd-icons-lorange ((,c :foreground ,red-warmer)))
    `(nerd-icons-lpink ((,c :foreground ,magenta)))
    `(nerd-icons-lpurple ((,c :foreground ,magenta-faint)))
    `(nerd-icons-lred ((,c :foreground ,red-faint)))
    `(nerd-icons-lsilver ((,c :foreground "gray50")))
    `(nerd-icons-lyellow ((,c :foreground ,yellow-warmer)))
    `(nerd-icons-maroon ((,c :foreground ,magenta)))
    `(nerd-icons-orange ((,c :foreground ,yellow-warmer)))
    `(nerd-icons-pink ((,c :foreground ,magenta-warmer)))
    `(nerd-icons-purple ((,c :foreground ,magenta-cooler)))
    `(nerd-icons-purple-alt ((,c :foreground ,blue-warmer)))
    `(nerd-icons-red ((,c :foreground ,red)))
    `(nerd-icons-red-alt ((,c :foreground ,red-cooler)))
    `(nerd-icons-silver ((,c :foreground "gray50")))
    `(nerd-icons-yellow ((,c :foreground ,yellow)))
;;;;; nerd-icons-completion
    `(nerd-icons-completion-dir-face ((,c :foreground ,accent-0)))
;;;;; nerd-icons-dired
    `(nerd-icons-dired-dir-face ((,c :foreground ,accent-0)))
;;;;; nerd-icons-ibuffer
    `(nerd-icons-ibuffer-dir-face ((,c :foreground ,accent-0)))
    `(nerd-icons-ibuffer-file-face ((,c :foreground ,docstring)))
    `(nerd-icons-ibuffer-mode-face ((,c :foreground ,type)))
    `(nerd-icons-ibuffer-size-face ((,c :foreground ,variable)))
;;;;; neotree
    `(neo-banner-face ((,c :foreground ,accent-0)))
    `(neo-button-face ((,c :inherit button)))
    `(neo-dir-link-face (( )))
    `(neo-expand-btn-face (( )))
    `(neo-file-link-face (( )))
    `(neo-header-face ((,c :inherit bold)))
    `(neo-root-dir-face ((,c :inherit bold :foreground ,accent-0)))
    `(neo-vc-added-face ((,c :inherit success)))
    `(neo-vc-conflict-face ((,c :inherit error)))
    `(neo-vc-default-face (( )))
    `(neo-vc-edited-face ((,c :inherit italic)))
    `(neo-vc-ignored-face ((,c :inherit shadow)))
    `(neo-vc-missing-face ((,c :inherit error)))
    `(neo-vc-needs-merge-face ((,c :inherit italic)))
    `(neo-vc-needs-update-face ((,c :underline t)))
    `(neo-vc-removed-face ((,c :strike-through t)))
    `(neo-vc-unlocked-changes-face ((,c :inherit success)))
    `(neo-vc-up-to-date-face (( )))
    `(neo-vc-user-face ((,c :inherit warning)))
;;;;; notmuch
    `(notmuch-crypto-decryption ((,c :inherit bold)))
    `(notmuch-crypto-part-header ((,c :foreground ,mail-part))) ; like `message-mml'
    `(notmuch-crypto-signature-bad ((,c :inherit error)))
    `(notmuch-crypto-signature-good ((,c :inherit success)))
    `(notmuch-crypto-signature-good-key ((,c :inherit success)))
    `(notmuch-crypto-signature-unknown ((,c :inherit warning)))
    `(notmuch-jump-key ((,c :inherit modus-themes-key-binding)))
    `(notmuch-message-summary-face ((,c :inherit bold :background ,bg-inactive)))
    `(notmuch-search-count ((,c :foreground ,fg-dim)))
    `(notmuch-search-date ((,c :foreground ,date-common)))
    `(notmuch-search-flagged-face ((,c :foreground ,keyword)))
    `(notmuch-search-matching-authors ((,c :foreground ,mail-recipient)))
    `(notmuch-search-non-matching-authors ((,c :inherit shadow)))
    `(notmuch-search-subject ((,c :foreground ,fg-main)))
    `(notmuch-search-unread-face ((,c :inherit bold)))
    `(notmuch-tag-added ((,c :underline ,info)))
    `(notmuch-tag-deleted ((,c :strike-through ,err)))
    `(notmuch-tag-face ((,c :foreground ,accent-0)))
    `(notmuch-tag-flagged ((,c :foreground ,keyword)))
    `(notmuch-tag-unread ((,c :foreground ,accent-1)))
    `(notmuch-tree-match-author-face ((,c :inherit notmuch-search-matching-authors)))
    `(notmuch-tree-match-date-face ((,c :inherit notmuch-search-date)))
    `(notmuch-tree-match-face ((,c :foreground ,fg-dim)))
    `(notmuch-tree-match-subject-face ((,c :foreground ,fg-main)))
    `(notmuch-tree-match-tag-face ((,c :inherit notmuch-tag-face)))
    `(notmuch-tree-no-match-face ((,c :inherit shadow)))
    `(notmuch-tree-no-match-date-face ((,c :inherit shadow)))
    `(notmuch-wash-cited-text ((,c :inherit message-cited-text-1)))
    `(notmuch-wash-toggle-button ((,c :background ,bg-dim)))
;;;;; num3-mode
    `(num3-face-even ((,c :inherit bold :background ,bg-inactive)))
;;;;; nxml-mode
    `(nxml-attribute-colon ((,c :foreground ,fg-main)))
    `(nxml-attribute-local-name ((,c :inherit font-lock-variable-name-face)))
    `(nxml-attribute-prefix ((,c :inherit font-lock-type-face)))
    `(nxml-attribute-value ((,c :inherit font-lock-constant-face)))
    `(nxml-cdata-section-CDATA ((,c :inherit error)))
    `(nxml-cdata-section-delimiter ((,c :inherit error)))
    `(nxml-char-ref-delimiter ((,c :inherit shadow)))
    `(nxml-char-ref-number ((,c :inherit (shadow modus-themes-bold))))
    `(nxml-delimited-data ((,c :inherit (shadow modus-themes-slant))))
    `(nxml-delimiter ((,c :foreground ,fg-dim)))
    `(nxml-element-colon ((,c :foreground ,fg-main)))
    `(nxml-element-local-name ((,c :inherit font-lock-function-name-face)))
    `(nxml-element-prefix ((,c :inherit font-lock-builtin-face)))
    `(nxml-entity-ref-delimiter ((,c :inherit shadow)))
    `(nxml-entity-ref-name ((,c :inherit (shadow modus-themes-bold))))
    `(nxml-glyph ((,c :background ,bg-active :foreground ,fg-main)))
    `(nxml-hash ((,c :inherit (bold font-lock-string-face))))
    `(nxml-heading ((,c :inherit bold)))
    `(nxml-name ((,c :inherit font-lock-builtin-face)))
    `(nxml-namespace-attribute-colon ((,c :foreground ,fg-main)))
    `(nxml-namespace-attribute-prefix ((,c :inherit font-lock-variable-name-face)))
    `(nxml-processing-instruction-target ((,c :inherit font-lock-keyword-face)))
    `(nxml-prolog-keyword ((,c :inherit font-lock-keyword-face)))
    `(nxml-ref ((,c :inherit (shadow modus-themes-bold))))
    `(rng-error ((,c :inherit error)))
;;;;; olivetti
    `(olivetti-fringe ((,c :background ,fringe)))
;;;;; orderless
    `(orderless-match-face-0 ((,c :inherit modus-themes-completion-match-0)))
    `(orderless-match-face-1 ((,c :inherit modus-themes-completion-match-1)))
    `(orderless-match-face-2 ((,c :inherit modus-themes-completion-match-2)))
    `(orderless-match-face-3 ((,c :inherit modus-themes-completion-match-3)))
;;;;; org
    `(org-agenda-calendar-daterange ((,c :foreground ,date-range)))
    `(org-agenda-calendar-event ((,c :foreground ,date-event)))
    `(org-agenda-calendar-sexp ((,c :inherit (modus-themes-slant org-agenda-calendar-event))))
    `(org-agenda-clocking ((,c :inherit bold :background ,bg-active-argument :foreground ,fg-active-argument)))
    `(org-agenda-column-dateline ((,c :background ,bg-inactive)))
    `(org-agenda-current-time ((,c :foreground ,date-now)))
    `(org-agenda-date ((,c ,@(modus-themes--heading 'agenda-date date-weekday))))
    `(org-agenda-date-today ((,c :inherit org-agenda-date :underline t)))
    `(org-agenda-date-weekend ((,c :inherit org-agenda-date :foreground ,date-weekend)))
    `(org-agenda-date-weekend-today ((,c :inherit org-agenda-date-today :foreground ,date-weekend)))
    `(org-agenda-diary ((,c :inherit org-agenda-calendar-sexp)))
    `(org-agenda-dimmed-todo-face ((,c :inherit shadow)))
    `(org-agenda-done ((,c :inherit org-done)))
    `(org-agenda-filter-category ((,c :inherit bold :foreground ,modeline-err)))
    `(org-agenda-filter-effort ((,c :inherit bold :foreground ,modeline-err)))
    `(org-agenda-filter-regexp ((,c :inherit bold :foreground ,modeline-err)))
    `(org-agenda-filter-tags ((,c :inherit bold :foreground ,modeline-err)))
    `(org-agenda-restriction-lock ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(org-agenda-structure ((,c ,@(modus-themes--heading 'agenda-structure fg-alt))))
    `(org-agenda-structure-filter ((,c :inherit org-agenda-structure :foreground ,warning)))
    `(org-agenda-structure-secondary ((,c :inherit font-lock-doc-face)))
    `(org-archived ((,c :background ,bg-inactive :foreground ,fg-main)))
    `(org-block ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-contents :extend t)))
    `(org-block-begin-line ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-delimiter :foreground ,fg-prose-block-delimiter :extend t)))
    `(org-block-end-line ((,c :inherit org-block-begin-line)))
    `(org-checkbox ((,c :inherit modus-themes-fixed-pitch :foreground ,warning)))
    `(org-checkbox-statistics-done ((,c :inherit org-done)))
    `(org-checkbox-statistics-todo ((,c :inherit org-todo)))
    `(org-clock-overlay ((,c :inherit secondary-selection)))
    `(org-code ((,c :inherit modus-themes-prose-code)))
    `(org-column ((,c :inherit default :background ,bg-dim)))
    `(org-column-title ((,c :inherit (modus-themes-fixed-pitch bold default) :underline t :background ,bg-dim)))
    `(org-date ((,c :inherit modus-themes-fixed-pitch :foreground ,date-common)))
    `(org-date-selected ((,c :foreground ,date-common :inverse-video t)))
    ;; NOTE 2024-03-17: Normally we do not want to add this padding
    ;; with the :box, but I do it here because the keys are otherwise
    ;; very hard to read.  The square brackets around them are not
    ;; colored, which is what is causing the problem.
    `(org-dispatcher-highlight ((,c :inherit modus-themes-bold :box (:line-width 2 :color ,bg-hover-secondary) :background ,bg-hover-secondary :foreground ,fg-main)))
    `(org-document-info ((,c :foreground ,prose-metadata-value)))
    `(org-document-info-keyword ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(org-document-title ((,c :inherit modus-themes-heading-0)))
    `(org-done ((,c :foreground ,prose-done)))
    `(org-drawer ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(org-ellipsis (( ))) ; inherits from the heading's color
    `(org-footnote ((,c :inherit link)))
    `(org-formula ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-table-formula)))
    `(org-headline-done ((,c :inherit org-done)))
    `(org-headline-todo ((,c :inherit org-todo)))
    `(org-hide ((,c :foreground ,bg-main)))
    `(org-indent ((,c :inherit (fixed-pitch org-hide))))
    `(org-imminent-deadline ((,c :inherit bold :foreground ,date-deadline)))
    `(org-latex-and-related ((,c :foreground ,type)))
    `(org-level-1 ((,c :inherit modus-themes-heading-1)))
    `(org-level-2 ((,c :inherit modus-themes-heading-2)))
    `(org-level-3 ((,c :inherit modus-themes-heading-3)))
    `(org-level-4 ((,c :inherit modus-themes-heading-4)))
    `(org-level-5 ((,c :inherit modus-themes-heading-5)))
    `(org-level-6 ((,c :inherit modus-themes-heading-6)))
    `(org-level-7 ((,c :inherit modus-themes-heading-7)))
    `(org-level-8 ((,c :inherit modus-themes-heading-8)))
    `(org-link ((,c :inherit link)))
    `(org-list-dt ((,c :inherit bold)))
    `(org-macro ((,c :inherit modus-themes-prose-macro)))
    `(org-meta-line ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(org-mode-line-clock (( )))
    `(org-mode-line-clock-overrun ((,c :inherit bold :foreground ,modeline-err)))
    `(org-priority ((,c :foreground ,prose-tag)))
    `(org-property-value ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata-value)))
    `(org-quote ((,c :inherit org-block)))
    `(org-scheduled ((,c :foreground ,date-scheduled-subtle)))
    `(org-scheduled-previously ((,c :inherit (bold org-scheduled-today))))
    `(org-scheduled-today ((,c :foreground ,date-scheduled)))
    `(org-sexp-date ((,c :foreground ,date-common)))
    `(org-special-keyword ((,c :inherit org-drawer)))
    `(org-table ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-table)))
    `(org-table-header ((,c :inherit (bold org-table))))
    `(org-tag ((,c :foreground ,prose-tag)))
    `(org-tag-group ((,c :inherit (bold org-tag))))
    `(org-target ((,c :underline t)))
    `(org-time-grid ((,c :foreground ,fg-dim)))
    `(org-todo ((,c :foreground ,prose-todo)))
    `(org-upcoming-deadline ((,c :foreground ,date-deadline-subtle)))
    `(org-upcoming-distant-deadline ((,c :foreground ,fg-main)))
    `(org-verbatim ((,c :inherit modus-themes-prose-verbatim)))
    `(org-verse ((,c :inherit org-block)))
    `(org-warning ((,c :inherit warning)))
;;;;; org-habit
    `(org-habit-alert-face ((,c :background ,bg-graph-yellow-0 :foreground "#000000"))) ; fg is special case
    `(org-habit-alert-future-face ((,c :background ,bg-graph-yellow-1)))
    `(org-habit-clear-face ((,c :background ,bg-graph-blue-0 :foreground "#000000"))) ; fg is special case
    `(org-habit-clear-future-face ((,c :background ,bg-graph-blue-1)))
    `(org-habit-overdue-face ((,c :background ,bg-graph-red-0)))
    `(org-habit-overdue-future-face ((,c :background ,bg-graph-red-1)))
    `(org-habit-ready-face ((,c :background ,bg-graph-green-0 :foreground "#000000"))) ; fg is special case
    `(org-habit-ready-future-face ((,c :background ,bg-graph-green-1)))
;;;;; org-journal
    `(org-journal-calendar-entry-face ((,c :inherit modus-themes-slant :foreground ,date-common)))
    `(org-journal-calendar-scheduled-face ((,c :inherit (modus-themes-slant org-scheduled))))
    `(org-journal-highlight ((,c :foreground ,err)))
;;;;; org-noter
    `(org-noter-no-notes-exist-face ((,c :inherit error)))
    `(org-noter-notes-exist-face ((,c :inherit success)))
;;;;; org-pomodoro
    `(org-pomodoro-mode-line ((,c :foreground ,err)))
    `(org-pomodoro-mode-line-break ((,c :foreground ,info)))
    `(org-pomodoro-mode-line-overtime ((,c :inherit error)))
;;;;; org-recur
    `(org-recur ((,c :foreground ,fg-alt)))
;;;;; org-roam
    `(org-roam-dim ((,c :foreground "gray50")))
    `(org-roam-olp ((,c :inherit shadow)))
    `(org-roam-preview-heading ((,c :background ,bg-inactive)))
    `(org-roam-preview-heading-highlight ((,c :background ,bg-active :foreground ,fg-main)))
    `(org-roam-preview-region ((,c :inherit bold)))
    `(org-roam-title ((,c :inherit bold)))
;;;;; org-superstar
    `(org-superstar-item ((,c :foreground ,fg-main)))
;;;;; org-tree-slide
    `(org-tree-slide-header-overlay-face ((,c :inherit org-document-title)))
;;;;; origami
    `(origami-fold-header-face ((,c :background ,bg-dim :foreground ,fg-dim :box t)))
    `(origami-fold-replacement-face ((,c :background ,bg-inactive :foreground ,fg-dim)))
;;;;; outline-mode
    `(outline-1 ((,c :inherit modus-themes-heading-1)))
    `(outline-2 ((,c :inherit modus-themes-heading-2)))
    `(outline-3 ((,c :inherit modus-themes-heading-3)))
    `(outline-4 ((,c :inherit modus-themes-heading-4)))
    `(outline-5 ((,c :inherit modus-themes-heading-5)))
    `(outline-6 ((,c :inherit modus-themes-heading-6)))
    `(outline-7 ((,c :inherit modus-themes-heading-7)))
    `(outline-8 ((,c :inherit modus-themes-heading-8)))
;;;;; outline-minor-faces
    `(outline-minor-0 (()))
;;;;; package (M-x list-packages)
    `(package-description ((,c :foreground ,docstring)))
    `(package-help-section-name ((,c :inherit bold)))
    `(package-mark-delete-line ((,c :inherit modus-themes-mark-del)))
    `(package-mark-install-line ((,c :inherit modus-themes-mark-sel)))
    `(package-name ((,c :inherit link)))
    `(package-status-available ((,c :foreground ,date-common)))
    `(package-status-avail-obso ((,c :inherit error)))
    `(package-status-built-in ((,c :foreground ,builtin)))
    `(package-status-dependency ((,c :foreground ,warning)))
    `(package-status-disabled ((,c :inherit error :strike-through t)))
    `(package-status-from-source ((,c :foreground ,type)))
    `(package-status-held ((,c :foreground ,warning)))
    `(package-status-incompat ((,c :inherit warning)))
    `(package-status-installed ((,c :foreground ,fg-alt)))
    `(package-status-new ((,c :inherit success)))
    `(package-status-unsigned ((,c :inherit error)))
;;;;; page-break-lines
    `(page-break-lines ((,c :inherit default :foreground "gray50")))
;;;;; pandoc-mode
    `(pandoc-citation-key-face ((,c :inherit font-lock-builtin-face)))
    `(pandoc-directive-@@-face ((,c :inherit font-lock-keyword-face)))
    `(pandoc-directive-braces-face ((,c :inherit font-lock-constant-face)))
    `(pandoc-directive-contents-face ((,c :inherit font-lock-string-face)))
    `(pandoc-directive-type-face ((,c :inherit font-lock-type-face)))
;;;;; paren-face
    `(parenthesis ((,c :inherit shadow)))
;;;;; pass
    `(pass-mode-directory-face ((,c :inherit bold :foreground ,accent-0)))
    `(pass-mode-entry-face ((,c :background ,bg-main :foreground ,fg-main)))
    `(pass-mode-header-face ((,c :inherit shadow)))
;;;;; pdf-tools
    `(pdf-links-read-link ((,c :background ,fg-main :foreground ,bg-magenta-intense :inherit bold))) ; Foreground is background and vice versa
    `(pdf-occur-document-face ((,c :inherit shadow)))
    `(pdf-occur-page-face ((,c :inherit shadow)))
;;;;; persp-mode
    `(persp-face-lighter-buffer-not-in-persp ((,c :inherit error)))
    `(persp-face-lighter-default ((,c :inherit bold :foreground ,name)))
    `(persp-face-lighter-nil-persp ((,c :inherit bold)))
;;;;; perspective
    `(persp-selected-face ((,c :inherit bold :foreground ,name)))
;;;;; proced
    `(proced-cpu ((,c :foreground ,keyword)))
    `(proced-emacs-pid ((,c :foreground ,identifier :underline t)))
    `(proced-executable ((,c :foreground ,name)))
    `(proced-interruptible-sleep-status-code ((,c :inherit shadow)))
    `(proced-mem ((,c :foreground ,type)))
    `(proced-memory-high-usage ((,c :foreground ,err)))
    `(proced-memory-low-usage ((,c :foreground ,info)))
    `(proced-memory-medium-usage ((,c :foreground ,warning)))
    `(proced-pgrp ((,c :inherit proced-pid)))
    `(proced-pid ((,c :foreground ,identifier)))
    `(proced-ppid ((,c :inherit proced-pid)))
    `(proced-run-status-code ((,c :inherit success)))
    `(proced-sess ((,c :inherit proced-pid)))
    `(proced-session-leader-pid ((,c :inherit bold :foreground ,identifier)))
    `(proced-time-colon (( )))
    `(proced-uninterruptible-sleep-status-code ((,c :inherit error)))
    `(proced-user (( )))
;;;;; popup
    `(popup-face ((,c :background ,bg-inactive :foreground ,fg-main)))
    `(popup-isearch-match ((,c :inherit modus-themes-search-current)))
    `(popup-menu-mouse-face ((,c :inherit highlight)))
    `(popup-menu-selection-face ((,c :inherit modus-themes-completion-selected)))
    `(popup-scroll-bar-background-face ((,c :background ,bg-active)))
    `(popup-scroll-bar-foreground-face (( )))
    `(popup-summary-face ((,c :background ,bg-active :foreground ,fg-dim)))
    `(popup-tip-face ((,c :inherit modus-themes-intense-yellow)))
;;;;; powerline
    `(powerline-active0 ((,c :background ,fg-dim :foreground ,bg-main)))
    `(powerline-active1 ((,c :inherit mode-line)))
    `(powerline-active2 ((,c :inherit mode-line-inactive)))
    `(powerline-inactive0 ((,c :background ,bg-active :foreground ,fg-dim)))
    `(powerline-inactive1 ((,c :background ,bg-main :foreground ,fg-dim)))
    `(powerline-inactive2 ((,c :inherit mode-line-inactive)))
;;;;; powerline-evil
    `(powerline-evil-base-face ((,c :background ,fg-main :foreground ,bg-main)))
    `(powerline-evil-emacs-face ((,c :inherit bold :background ,bg-main)))
    `(powerline-evil-insert-face ((,c :inherit success :background ,bg-main)))
    `(powerline-evil-motion-face ((,c :inherit italic :background ,bg-main)))
    `(powerline-evil-normal-face ((,c :background ,bg-main :foreground ,fg-alt)))
    `(powerline-evil-operator-face ((,c :inherit warning :background ,bg-main)))
    `(powerline-evil-replace-face ((,c :inherit error :background ,bg-main)))
    `(powerline-evil-visual-face ((,c :inherit bold :background ,bg-main)))
;;;;; prescient
    `(prescient-primary-highlight ((,c :inherit modus-themes-completion-match-0)))
    `(prescient-secondary-highlight ((,c :inherit modus-themes-completion-match-1)))
;;;;; proced
    `(proced-mark ((,c :inherit bold)))
    `(proced-marked ((,c :inherit modus-themes-mark-alt)))
    `(proced-sort-header ((,c :inherit bold :underline t)))
;;;;; prodigy
    `(prodigy-green-face ((,c :inherit success)))
    `(prodigy-red-face ((,c :inherit error)))
    `(prodigy-yellow-face ((,c :inherit warning)))
;;;;; pulse
    `(pulse-highlight-start-face ((,c :background ,bg-blue-intense :extend t)))
;;;;; pyim
    `(pyim-page ((,c :background ,bg-active)))
    `(pyim-page-selection ((,c :inherit bold :background ,bg-active :foreground ,info)))
    `(pyim-page-subword ((,c :background ,bg-inactive)))
;;;;; quick-peek
    `(quick-peek-background-face ((,c :background ,bg-inactive)))
    `(quick-peek-border-face ((,c :background ,border :height 1)))
    `(quick-peek-padding-face ((,c :background ,bg-inactive :height 0.15)))
;;;;; rainbow-delimiters
    `(rainbow-delimiters-base-error-face ((,c :inherit modus-themes-prominent-error)))
    `(rainbow-delimiters-base-face ((,c :foreground ,fg-main)))
    `(rainbow-delimiters-depth-1-face ((,c :foreground ,rainbow-0)))
    `(rainbow-delimiters-depth-2-face ((,c :foreground ,rainbow-1)))
    `(rainbow-delimiters-depth-3-face ((,c :foreground ,rainbow-2)))
    `(rainbow-delimiters-depth-4-face ((,c :foreground ,rainbow-3)))
    `(rainbow-delimiters-depth-5-face ((,c :foreground ,rainbow-4)))
    `(rainbow-delimiters-depth-6-face ((,c :foreground ,rainbow-5)))
    `(rainbow-delimiters-depth-7-face ((,c :foreground ,rainbow-6)))
    `(rainbow-delimiters-depth-8-face ((,c :foreground ,rainbow-7)))
    `(rainbow-delimiters-depth-9-face ((,c :foreground ,rainbow-8)))
    `(rainbow-delimiters-mismatched-face ((,c :inherit (bold modus-themes-prominent-warning))))
    `(rainbow-delimiters-unmatched-face ((,c :inherit (bold modus-themes-prominent-error))))
;;;;; rcirc
    `(rcirc-bright-nick ((,c :inherit bold :foreground ,accent-2)))
    `(rcirc-dim-nick ((,c :inherit shadow)))
    `(rcirc-monospace-text ((,c :inherit fixed-pitch)))
    `(rcirc-my-nick ((,c :inherit bold :foreground ,accent-1)))
    `(rcirc-nick-in-message ((,c :inherit rcirc-my-nick)))
    `(rcirc-nick-in-message-full-line ((,c :inherit rcirc-my-nick)))
    `(rcirc-other-nick ((,c :inherit bold :foreground ,accent-0)))
    `(rcirc-prompt ((,c :inherit minibuffer-prompt)))
    `(rcirc-server ((,c :inherit font-lock-comment-face)))
    `(rcirc-timestamp ((,c :foreground ,date-common)))
    `(rcirc-track-keyword ((,c :inherit bold :foreground ,modeline-warning)))
    `(rcirc-track-nick ((,c :inherit rcirc-my-nick)))
    `(rcirc-url ((,c :inherit link)))
;;;;; recursion-indicator
    `(recursion-indicator-general ((,c :foreground ,modeline-err)))
    `(recursion-indicator-minibuffer ((,c :foreground ,modeline-info)))
;;;;; regexp-builder (re-builder)
    `(reb-match-0 ((,c :inherit modus-themes-search-rx-group-0)))
    `(reb-match-1 ((,c :inherit modus-themes-search-rx-group-1)))
    `(reb-match-2 ((,c :inherit modus-themes-search-rx-group-2)))
    `(reb-match-3 ((,c :inherit modus-themes-search-rx-group-3)))
    `(reb-regexp-grouping-backslash ((,c :inherit font-lock-regexp-grouping-backslash)))
    `(reb-regexp-grouping-construct ((,c :inherit font-lock-regexp-grouping-construct)))
;;;;; rg (rg.el)
    `(rg-column-number-face ((,c :inherit shadow)))
    `(rg-context-face ((,c :inherit shadow)))
    `(rg-error-face ((,c :inherit error)))
    `(rg-file-tag-face ((,c :inherit font-lock-builtin-face)))
    `(rg-filename-face ((,c :inherit bold :foreground ,name)))
    `(rg-line-number-face ((,c :inherit shadow)))
    `(rg-literal-face ((,c :inherit font-lock-constant-face)))
    `(rg-match-face ((,c :inherit match)))
    `(rg-regexp-face ((,c :foreground ,name)))
    `(rg-toggle-off-face ((,c :inherit (shadow bold))))
    `(rg-toggle-on-face ((,c :inherit success)))
    `(rg-warning-face ((,c :inherit warning)))
;;;;; ripgrep
    `(ripgrep-context-face ((,c :inherit shadow)))
    `(ripgrep-error-face ((,c :inherit error)))
    `(ripgrep-hit-face ((,c :inherit success)))
    `(ripgrep-match-face ((,c :inherit match)))
;;;;; rmail
    `(rmail-header-name ((,c :inherit bold)))
    `(rmail-highlight ((,c :inherit bold :foreground ,mail-other)))
;;;;; rst-mode
    `(rst-level-1 ((,c :inherit modus-themes-heading-1)))
    `(rst-level-2 ((,c :inherit modus-themes-heading-2)))
    `(rst-level-3 ((,c :inherit modus-themes-heading-3)))
    `(rst-level-4 ((,c :inherit modus-themes-heading-4)))
    `(rst-level-5 ((,c :inherit modus-themes-heading-5)))
    `(rst-level-6 ((,c :inherit modus-themes-heading-6)))
;;;;; ruler-mode
    `(ruler-mode-column-number ((,c :inherit ruler-mode-default)))
    `(ruler-mode-comment-column ((,c :inherit ruler-mode-default :foreground ,red)))
    `(ruler-mode-current-column ((,c :inherit ruler-mode-default :background ,bg-active :foreground ,fg-main)))
    `(ruler-mode-default ((,c :inherit default :background ,bg-dim :foreground ,fg-dim)))
    `(ruler-mode-fill-column ((,c :inherit ruler-mode-default :foreground ,green)))
    `(ruler-mode-fringes ((,c :inherit ruler-mode-default :foreground ,cyan)))
    `(ruler-mode-goal-column ((,c :inherit ruler-mode-default :foreground ,blue)))
    `(ruler-mode-margins ((,c :inherit ruler-mode-default :foreground ,bg-main)))
    `(ruler-mode-pad ((,c :inherit ruler-mode-default :background ,bg-inactive :foreground ,fg-dim)))
    `(ruler-mode-tab-stop ((,c :inherit ruler-mode-default :foreground ,yellow)))
;;;;; sesman
    `(sesman-browser-button-face ((,c :inherit button)))
    `(sesman-browser-highligh-face ((,c :inherit highlight)))
    `(sesman-buffer-face ((,c :foreground ,accent-1)))
    `(sesman-directory-face ((,c :inherit bold :foreground ,accent-0)))
    `(sesman-project-face ((,c :inherit bold :foreground ,accent-2)))
;;;;; shell-script-mode
    `(sh-heredoc ((,c :inherit font-lock-string-face)))
    `(sh-quoted-exec ((,c :inherit font-lock-builtin-face)))
;;;;; shortdoc
    `(shortdoc-heading ((,c :inherit bold)))
    `(shortdoc-section (())) ; remove the default's variable-pitch style
;;;;; show-paren-mode
    `(show-paren-match ((,c :background ,bg-paren-match :foreground ,fg-paren-match :underline ,underline-paren-match)))
    `(show-paren-match-expression ((,c :background ,bg-paren-expression)))
    `(show-paren-mismatch ((,c :inherit modus-themes-prominent-error)))
;;;;; shr
    `(shr-abbreviation ((,c :inherit modus-themes-lang-note)))
    `(shr-code ((,c :inherit modus-themes-prose-verbatim)))
    `(shr-h1 ((,c :inherit modus-themes-heading-1)))
    `(shr-h2 ((,c :inherit modus-themes-heading-2)))
    `(shr-h3 ((,c :inherit modus-themes-heading-3)))
    `(shr-h4 ((,c :inherit modus-themes-heading-4)))
    `(shr-h5 ((,c :inherit modus-themes-heading-5)))
    `(shr-h6 ((,c :inherit modus-themes-heading-6)))
    `(shr-mark ((,c :inherit match)))
    `(shr-selected-link ((,c :inherit modus-themes-mark-sel)))
;;;;; side-notes
    `(side-notes ((,c :background ,bg-dim :foreground ,fg-dim)))
;;;;; sieve-mode
    `(sieve-action-commands ((,c :inherit font-lock-builtin-face)))
    `(sieve-control-commands ((,c :inherit font-lock-keyword-face)))
    `(sieve-tagged-arguments ((,c :inherit font-lock-type-face)))
    `(sieve-test-commands ((,c :inherit font-lock-function-name-face)))
;;;;; skewer-mode
    `(skewer-error-face ((,c :inherit modus-themes-lang-error)))
;;;;; slime (sldb)
    `(sldb-condition-face ((,c :inherit font-lock-preprocessor-face)))
    `(sldb-restart-number-face ((,c :inherit bold)))
    `(sldb-restart-type-face ((,c :inherit font-lock-type-face)))
    `(sldb-restartable-frame-line-face ((,c :inherit success)))
    `(sldb-section-face ((,c :inherit bold)))
    `(slime-error-face ((,c :inherit modus-themes-lang-error)))
    `(slime-note-face ((,c :underline t)))
    `(slime-repl-input-face ((,c :inherit bold)))
    `(slime-repl-inputed-output-face ((,c :inherit font-lock-string-face)))
    `(slime-repl-output-mouseover-face ((,c :inherit highlight)))
    `(slime-repl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(slime-style-warning-face ((,c :inherit modus-themes-lang-note)))
    `(slime-warning-face ((,c :inherit modus-themes-lang-warning)))
;;;;; sly
    `(sly-action-face ((,c :inherit font-lock-type-face)))
    `(sly-db-condition-face ((,c :inherit font-lock-preprocessor-face)))
    `(sly-db-restartable-frame-line-face ((,c :inherit success)))
    `(sly-error-face ((,c :inherit modus-themes-lang-error)))
    `(sly-mode-line ((,c :inherit mode-line-emphasis)))
    `(sly-mrepl-output-face ((,c :inherit font-lock-string-face)))
    `(sly-mrepl-output-face ((,c :inherit font-lock-string-face)))
    `(sly-mrepl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(sly-note-face ((,c :inherit modus-themes-lang-note)))
    `(sly-stickers-placed-face ((,c :background ,bg-inactive)))
    `(sly-style-warning-face ((,c :inherit modus-themes-lang-note)))
    `(sly-warning-face ((,c :inherit modus-themes-lang-warning)))
;;;;; smart-mode-line
    `(sml/charging ((,c :foreground ,info)))
    `(sml/discharging ((,c :foreground ,err)))
    `(sml/filename ((,c :inherit bold :foreground ,name)))
    `(sml/folder (( )))
    `(sml/git ((,c :inherit success)))
    `(sml/global (( )))
    `(sml/line-number ((,c :inherit sml/global)))
    `(sml/minor-modes ((,c :inherit sml/global)))
    `(sml/modes ((,c :inherit bold)))
    `(sml/modified ((,c :inherit italic)))
    `(sml/mule-info ((,c :inherit sml/global)))
    `(sml/name-filling ((,c :inherit warning)))
    `(sml/not-modified ((,c :inherit sml/global)))
    `(sml/numbers-separator ((,c :inherit sml/global)))
    `(sml/outside-modified ((,c :inherit modus-themes-prominent-error)))
    `(sml/position-percentage ((,c :inherit sml/global)))
    `(sml/prefix ((,c :foreground ,fg-alt)))
    `(sml/process ((,c :inherit sml/prefix)))
    `(sml/projectile ((,c :inherit sml/git)))
    `(sml/read-only (( )))
    `(sml/remote ((,c :inherit sml/global)))
    `(sml/sudo ((,c :inherit warning)))
    `(sml/time ((,c :inherit sml/global)))
    `(sml/vc ((,c :inherit sml/git)))
    `(sml/vc-edited ((,c :inherit italic)))
;;;;; smerge
    `(smerge-base ((,c :inherit diff-changed)))
    `(smerge-lower ((,c :inherit diff-added)))
    `(smerge-markers ((,c :inherit diff-header)))
    `(smerge-refined-added ((,c :inherit diff-refine-added)))
    `(smerge-refined-changed (()))
    `(smerge-refined-removed ((,c :inherit diff-refine-removed)))
    `(smerge-upper ((,c :inherit diff-removed)))
;;;;; spacious-padding
    `(spacious-padding-line-active ((,c :foreground ,accent-0)))
    `(spacious-padding-line-inactive ((,c :foreground ,border)))
    `(spacious-padding-subtle-mode-line-active ((,c :foreground ,accent-0)))
    `(spacious-padding-subtle-mode-line-inactive ((,c :foreground ,border)))
;;;;; speedbar
    `(speedbar-button-face ((,c :inherit button)))
    `(speedbar-directory-face ((,c :inherit bold :foreground ,accent-0)))
    `(speedbar-file-face ((,c :foreground ,fg-main)))
    `(speedbar-highlight-face ((,c :inherit highlight)))
    `(speedbar-selected-face ((,c :inherit modus-themes-mark-sel)))
    `(speedbar-separator-face ((,c :background ,bg-active :foreground ,fg-main)))
    `(speedbar-tag-face ((,c :foreground ,accent-1)))
;;;;; spell-fu
    `(spell-fu-incorrect-face ((,c :inherit modus-themes-lang-error)))
;;;;; stripes
    `(stripes ((,c :background ,bg-inactive)))
;;;;; suggest
    `(suggest-heading ((,c :inherit warning)))
;;;;; switch-window
    `(switch-window-background ((,c :background ,bg-inactive)))
    `(switch-window-label ((,c :height 3.0 :foreground ,red-intense)))
;;;;; swiper
    `(swiper-background-match-face-1 (( )))
    `(swiper-background-match-face-2 ((,c :inherit modus-themes-completion-match-0)))
    `(swiper-background-match-face-3 ((,c :inherit modus-themes-completion-match-1)))
    `(swiper-background-match-face-4 ((,c :inherit modus-themes-completion-match-2)))
    `(swiper-line-face ((,c :background ,bg-hl-line :extend t)))
    `(swiper-match-face-1 (( )))
    `(swiper-match-face-2 ((,c :inherit modus-themes-completion-match-0)))
    `(swiper-match-face-3 ((,c :inherit modus-themes-completion-match-1)))
    `(swiper-match-face-4 ((,c :inherit modus-themes-completion-match-2)))
;;;;; symbol-overlay
    `(symbol-overlay-default-face ((,c :background ,bg-inactive)))
    `(symbol-overlay-face-1 ((,c :inherit modus-themes-intense-blue)))
    `(symbol-overlay-face-2 ((,c :inherit modus-themes-intense-magenta)))
    `(symbol-overlay-face-3 ((,c :inherit modus-themes-intense-yellow)))
    `(symbol-overlay-face-4 ((,c :inherit modus-themes-intense-magenta)))
    `(symbol-overlay-face-5 ((,c :inherit modus-themes-intense-red)))
    `(symbol-overlay-face-6 ((,c :inherit modus-themes-intense-red)))
    `(symbol-overlay-face-7 ((,c :inherit modus-themes-intense-cyan)))
    `(symbol-overlay-face-8 ((,c :inherit modus-themes-intense-cyan)))
;;;;; syslog-mode
    `(syslog-debug ((,c :inherit italic)))
    `(syslog-error ((,c :inherit error)))
    `(syslog-file ((,c :inherit bold :foreground ,name)))
    `(syslog-hide ((,c :background ,bg-main :foreground ,fg-main)))
    `(syslog-hour ((,c :inherit bold :foreground ,date-common)))
    `(syslog-info ((,c :inherit success)))
    `(syslog-ip ((,c :inherit bold :foreground ,name :underline t)))
    `(syslog-su ((,c :inherit error :underline t)))
    `(syslog-warn ((,c :inherit warning)))
;;;;; tab-bar-mode
    `(tab-bar ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-tab-bar)))
    `(tab-bar-tab-group-current ((,c :inherit bold :background ,bg-tab-current :box (:line-width -2 :color ,bg-tab-current) :foreground ,fg-alt)))
    `(tab-bar-tab-group-inactive ((,c :background ,bg-tab-bar :box (:line-width -2 :color ,bg-tab-bar) :foreground ,fg-alt)))
    `(tab-bar-tab ((,c :inherit bold :box (:line-width -2 :color ,bg-tab-current) :background ,bg-tab-current)))
    `(tab-bar-tab-highlight ((,c :inherit highlight :box t)))
    `(tab-bar-tab-inactive ((,c :box (:line-width -2 :color ,bg-tab-other) :background ,bg-tab-other)))
    `(tab-bar-tab-ungrouped ((,c :inherit tab-bar-tab-inactive)))
;;;;; tab-line-mode
    `(tab-line ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-tab-bar :height 0.95)))
    `(tab-line-close-highlight ((,c :foreground ,err)))
    `(tab-line-highlight ((,c :inherit highlight)))
    `(tab-line-tab (( )))
    `(tab-line-tab-current ((,c :inherit bold :box (:line-width -2 :color ,bg-tab-current) :background ,bg-tab-current)))
    `(tab-line-tab-inactive ((,c :box (:line-width -2 :color ,bg-tab-other) :background ,bg-tab-other)))
    `(tab-line-tab-inactive-alternate ((,c :inherit tab-line-tab-inactive :foreground ,fg-alt)))
    `(tab-line-tab-modified ((,c :foreground ,warning)))
;;;;; table (built-in table.el)
    `(table-cell ((,c :background ,bg-dim)))
;;;;; telega
    `(telega-button ((,c :box t :foreground ,fg-link)))
    `(telega-button-active ((,c :box ,fg-link :background ,fg-link :foreground ,bg-main)))
    `(telega-button-highlight ((,c :inherit secondary-selection)))
    `(telega-chat-prompt ((,c :inherit modus-themes-prompt)))
    `(telega-entity-type-code ((,c :inherit modus-themes-prose-verbatim)))
    `(telega-entity-type-mention ((,c :foreground ,cyan)))
    `(telega-entity-type-pre ((,c :inherit modus-themes-prose-code)))
    `(telega-entity-type-spoiler ((,c :background ,fg-main :foreground ,fg-main)))
    `(telega-msg-heading ((,c :background ,bg-inactive)))
    `(telega-msg-self-title ((,c :inherit bold)))
    `(telega-root-heading ((,c :background ,bg-inactive)))
    `(telega-secret-title ((,c :foreground ,magenta-warmer)))
    `(telega-unmuted-count ((,c :foreground ,blue-cooler)))
    `(telega-user-online-status ((,c :foreground ,cyan)))
    `(telega-username ((,c :foreground ,cyan-cooler)))
    `(telega-webpage-chat-link ((,c :background ,bg-inactive)))
    `(telega-webpage-fixed ((,c :inherit modus-themes-fixed-pitch :height 0.85)))
    `(telega-webpage-header ((,c :height 1.3)))
    `(telega-webpage-preformatted ((,c :inherit modus-themes-fixed-pitch :background ,bg-inactive)))
    `(telega-webpage-subheader ((,c :height 1.15)))
;;;;; terraform-mode
    `(terraform--resource-name-face ((,c :foreground ,keyword)))
    `(terraform--resource-type-face ((,c :foreground ,type)))
;;;;; term
    ;; NOTE 2023-08-10: `term-color-black' and `term-color-white' use
    ;; the "bright" semantic color mappings to make sure they are
    ;; distinct from `term'.
    `(term ((,c :background ,bg-main :foreground ,fg-main)))
    `(term-bold ((,c :inherit bold)))
    `(term-color-black ((,c :background ,bg-term-black-bright :foreground ,fg-term-black-bright)))
    `(term-color-blue ((,c :background ,bg-term-blue :foreground ,fg-term-blue)))
    `(term-color-cyan ((,c :background ,bg-term-cyan :foreground ,fg-term-cyan)))
    `(term-color-green ((,c :background ,bg-term-green :foreground ,fg-term-green)))
    `(term-color-magenta ((,c :background ,bg-term-magenta :foreground ,fg-term-magenta)))
    `(term-color-red ((,c :background ,bg-term-red :foreground ,fg-term-red)))
    `(term-color-white ((,c :background ,bg-term-white-bright :foreground ,fg-term-white-bright)))
    `(term-color-yellow ((,c :background ,bg-term-yellow :foreground ,fg-term-yellow)))
    `(term-underline ((,c :underline t)))
;;;;; textsec
    `(textsec-suspicious (( )))
;;;;; tldr
    `(tldr-code-block (( )))
    `(tldr-command-argument ((,c :inherit font-lock-string-face)))
    `(tldr-command-itself ((,c :inherit font-lock-builtin-face)))
    `(tldr-description ((,c :inherit font-lock-doc-face)))
    `(tldr-introduction ((,c :inherit font-lock-comment-face)))
    `(tldr-title ((,c :inherit bold)))
;;;;; tmr
    `(tmr-mode-line-active ((,c :inherit bold :foreground ,modeline-info)))
    `(tmr-mode-line-soon ((,c :inherit bold :foreground ,modeline-warning)))
    `(tmr-mode-line-urgent ((,c :inherit bold :foreground ,modeline-err)))
    `(tmr-tabulated-description ((,c :foreground ,docstring)))
    `(tmr-tabulated-end-time ((,c :foreground ,date-deadline)))
    `(tmr-tabulated-remaining-time ((,c :foreground ,date-scheduled)))
    `(tmr-tabulated-start-time ((,c :foreground ,date-common)))
;;;;; transient
    `(transient-active-infix ((,c :inherit highlight)))
    `(transient-amaranth ((,c :inherit bold :foreground ,yellow-warmer)))
    ;; Placate the compiler for what is a spurious warning.  We also
    ;; have to do this with `eldoc-highlight-function-argument'.
    (list 'transient-argument `((,c :inherit bold :background ,bg-active-argument :foreground ,fg-active-argument)))
    `(transient-blue ((,c :inherit bold :foreground ,blue)))
    `(transient-disabled-suffix ((,c :inherit modus-themes-mark-del)))
    `(transient-enabled-suffix ((,c :inherit modus-themes-subtle-cyan)))
    `(transient-heading ((,c :inherit bold :foreground ,fg-main)))
    `(transient-inactive-argument ((,c :inherit shadow)))
    `(transient-inactive-value ((,c :inherit shadow)))
    ;; NOTE 2023-12-09 10:30:09 +0200: The new user option
    ;; `transient-semantic-coloring' is enabled by default.  This is
    ;; not good for us, because we are making it harder for users who
    ;; need accessible colors to use the transient interfaces.  I
    ;; could set that user option to nil, but I think it is less
    ;; intrusive to enforce uniformity among the relevant faces.
    ;; Those who want semantic coloring can modify these faces.
    `(transient-key ((,c :inherit modus-themes-key-binding)))
    `(transient-key-exit ((,c :inherit modus-themes-key-binding)))
    `(transient-key-noop ((,c :inherit (shadow modus-themes-key-binding))))
    `(transient-key-recurse ((,c :inherit modus-themes-key-binding)))
    `(transient-key-return ((,c :inherit modus-themes-key-binding)))
    `(transient-key-stack ((,c :inherit modus-themes-key-binding)))
    `(transient-key-stay ((,c :inherit modus-themes-key-binding)))
    `(transient-mismatched-key ((,c :underline t)))
    `(transient-nonstandard-key ((,c :underline t)))
    `(transient-pink ((,c :inherit bold :foreground ,magenta)))
    `(transient-purple ((,c :inherit bold :foreground ,magenta-cooler)))
    `(transient-red ((,c :inherit bold :foreground ,red-faint)))
    `(transient-teal ((,c :inherit bold :foreground ,cyan-cooler)))
    `(transient-unreachable ((,c :inherit shadow)))
    `(transient-unreachable-key ((,c :inherit shadow)))
    `(transient-value ((,c :inherit bold :background ,bg-active-value :foreground ,fg-active-value)))
;;;;; trashed
    `(trashed-deleted ((,c :inherit modus-themes-mark-del)))
    `(trashed-directory ((,c :foreground ,accent-0)))
    `(trashed-mark ((,c :inherit bold)))
    `(trashed-marked ((,c :inherit modus-themes-mark-alt)))
    `(trashed-restored ((,c :inherit modus-themes-mark-sel)))
;;;;; treemacs
    `(treemacs-async-loading-face ((,c :foreground ,fg-main)))
    `(treemacs-directory-face ((,c :foreground ,accent-0)))
    `(treemacs-directory-collapsed-face ((,c :foreground ,accent-0)))
    `(treemacs-file-face ((,c :foreground ,fg-main)))
    `(treemacs-fringe-indicator-face ((,c :foreground ,fg-main)))
    `(treemacs-git-added-face ((,c :inherit success)))
    `(treemacs-git-commit-diff-face ((,c :foreground ,err)))
    `(treemacs-git-conflict-face ((,c :inherit error)))
    `(treemacs-git-ignored-face ((,c :inherit shadow)))
    `(treemacs-git-modified-face ((,c :inherit warning)))
    `(treemacs-git-renamed-face ((,c :inherit italic)))
    `(treemacs-git-unmodified-face ((,c :foreground ,fg-main)))
    `(treemacs-git-untracked-face ((,c :inherit success)))
    `(treemacs-header-button-face ((,c :foreground ,fg-main)))
    `(treemacs-help-column-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(treemacs-help-title-face ((,c :foreground ,fg-main)))
    `(treemacs-hl-line-face ((,c :background ,bg-hl-line :extend t)))
    `(treemacs-marked-file-face ((,c :inherit modus-themes-mark-alt)))
    `(treemacs-nerd-icons-face ((,c :foreground ,accent-0)))
    `(treemacs-on-failure-pulse-face ((,c :foreground ,fg-main)))
    `(treemacs-on-success-pulse-face ((,c :foreground ,fg-main)))
    `(treemacs-peek-mode-indicator-face ((,c :foreground ,fg-main)))
    `(treemacs-remote-face ((,c :foreground ,fg-main)))
    `(treemacs-root-face ((,c :foreground ,accent-0)))
    `(treemacs-root-remote-disconnected-face ((,c :inherit warning)))
    `(treemacs-root-remote-unreadable-face ((,c :inherit warning)))
    `(treemacs-root-unreadable-face ((,c :inherit error)))
    `(treemacs-tags-face ((,c :foreground ,fg-main)))
    `(treemacs-term-node-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(treemacs-window-background-face ((,c :background ,bg-main)))
    `(treemacs-nerd-icons-root-face ((,c :foreground ,accent-0)))
    `(treemacs-nerd-icons-file-face ((,c :foreground ,accent-0)))
;;;;; tree-sitter
    `(tree-sitter-hl-face:attribute ((,c :inherit font-lock-variable-name-face)))
    `(tree-sitter-hl-face:constant.builtin ((,c :inherit tree-sitter-hl-face:constant)))
    `(tree-sitter-hl-face:escape ((,c :inherit font-lock-regexp-grouping-backslash)))
    `(tree-sitter-hl-face:function ((,c :inherit font-lock-function-name-face)))
    `(tree-sitter-hl-face:function.call ((,c :inherit tree-sitter-hl-face:function)))
    `(tree-sitter-hl-face:label (( )))
    `(tree-sitter-hl-face:method.call (( )))
    `(tree-sitter-hl-face:operator ((,c :inherit modus-themes-bold)))
    `(tree-sitter-hl-face:property (( )))
    `(tree-sitter-hl-face:property.definition ((,c :inherit font-lock-variable-name-face)))
    `(tree-sitter-hl-face:punctuation (( )))
    `(tree-sitter-hl-face:punctuation.bracket (( )))
    `(tree-sitter-hl-face:punctuation.delimiter (( )))
    `(tree-sitter-hl-face:punctuation.special ((,c :inherit font-lock-regexp-grouping-construct)))
    `(tree-sitter-hl-face:string.special ((,c :inherit tree-sitter-hl-face:string)))
    `(tree-sitter-hl-face:tag ((,c :inherit font-lock-function-name-face)))
    `(tree-sitter-hl-face:type.argument (( )))
;;;;; tty-menu
    `(tty-menu-disabled-face ((,c :background ,bg-inactive :foreground ,fg-dim)))
    `(tty-menu-enabled-face ((,c :inherit bold :background ,bg-inactive :foreground ,fg-main)))
    `(tty-menu-selected-face ((,c :inherit modus-themes-intense-blue)))
;;;;; tuareg
    `(caml-types-def-face ((,c :inherit modus-themes-subtle-red)))
    `(caml-types-expr-face ((,c :inherit modus-themes-subtle-green)))
    `(caml-types-occ-face ((,c :inherit modus-themes-subtle-green)))
    `(caml-types-scope-face ((,c :inherit modus-themes-subtle-blue)))
    `(caml-types-typed-face ((,c :inherit modus-themes-subtle-magenta)))
    `(tuareg-font-double-semicolon-face ((,c :inherit font-lock-preprocessor-face)))
    `(tuareg-font-lock-attribute-face ((,c :inherit font-lock-function-name-face)))
    `(tuareg-font-lock-constructor-face ((,c :foreground ,fg-main)))
    `(tuareg-font-lock-error-face ((,c :inherit (modus-themes-intense-red bold))))
    ;; `(tuareg-font-lock-extension-node-face ((,c :background ,bg-inactive :foreground ,magenta)))
    `(tuareg-font-lock-governing-face ((,c :inherit bold :foreground ,fg-main)))
    `(tuareg-font-lock-infix-extension-node-face ((,c :inherit font-lock-function-name-face)))
    `(tuareg-font-lock-interactive-directive-face ((,c :inherit font-lock-preprocessor-face)))
    `(tuareg-font-lock-interactive-error-face ((,c :inherit error)))
    `(tuareg-font-lock-interactive-output-face ((,c :inherit font-lock-constant-face)))
    `(tuareg-font-lock-label-face ((,c :inherit font-lock-type-face)))
    `(tuareg-font-lock-line-number-face ((,c :inherit shadow)))
    `(tuareg-font-lock-module-face ((,c :inherit font-lock-builtin-face)))
    ;; `(tuareg-font-lock-multistage-face ((,c :inherit bold :background ,bg-inactive :foreground ,blue)))
    `(tuareg-font-lock-operator-face ((,c :inherit font-lock-preprocessor-face)))
    `(tuareg-opam-error-face ((,c :inherit error)))
    `(tuareg-opam-pkg-variable-name-face ((,c :inherit font-lock-variable-name-face)))
;;;;; typescript
    `(typescript-jsdoc-tag ((,c :inherit (font-lock-builtin-face font-lock-comment-face) :weight normal)))
    `(typescript-jsdoc-type ((,c :inherit (font-lock-type-face font-lock-comment-face) :weight normal)))
    `(typescript-jsdoc-value ((,c :inherit (font-lock-constant-face font-lock-comment-face) :weight normal)))
;;;;; undo-tree
    `(undo-tree-visualizer-active-branch-face ((,c :inherit bold :foreground ,fg-main)))
    `(undo-tree-visualizer-current-face ((,c :foreground ,blue-intense)))
    `(undo-tree-visualizer-default-face ((,c :inherit shadow)))
    `(undo-tree-visualizer-register-face ((,c :foreground ,magenta-intense)))
    `(undo-tree-visualizer-unmodified-face ((,c :foreground ,green-intense)))
;;;;; vc (vc-dir.el, vc-hooks.el)
    `(vc-dir-directory (( )))
    `(vc-dir-file ((,c :foreground ,name)))
    `(vc-dir-header ((,c :inherit bold)))
    `(vc-dir-header-value ((,c :foreground ,string)))
    `(vc-dir-mark-indicator (( )))
    `(vc-dir-status-edited ((,c :inherit italic)))
    `(vc-dir-status-ignored ((,c :inherit shadow)))
    `(vc-dir-status-up-to-date ((,c :foreground ,info)))
    `(vc-dir-status-warning ((,c :inherit error)))
    `(vc-conflict-state ((,c :inherit error)))
    `(vc-edited-state ((,c :inherit italic)))
    `(vc-git-log-edit-summary-max-warning ((,c :inherit error)))
    `(vc-git-log-edit-summary-target-warning ((,c :inherit warning)))
    `(vc-locally-added-state ((,c :inherit italic)))
    `(vc-locked-state ((,c :inherit success)))
    `(vc-missing-state ((,c :inherit error)))
    `(vc-needs-update-state ((,c :inherit error)))
    `(vc-removed-state ((,c :inherit error)))
    `(vc-state-base (( )))
    `(vc-up-to-date-state (( )))
;;;;; vertico
    `(vertico-current ((,c :inherit modus-themes-completion-selected)))
;;;;; vertico-quick
    `(vertico-quick1 ((,c :inherit (bold modus-themes-search-current))))
    `(vertico-quick2 ((,c :inherit (bold modus-themes-search-current))))
;;;;; vimish-fold
    `(vimish-fold-fringe ((,c :foreground ,cyan)))
    `(vimish-fold-mouse-face ((,c :inherit modus-themes-intense-blue)))
    `(vimish-fold-overlay ((,c :background ,bg-inactive)))
;;;;; viper
    `(viper-search ((,c :inherit modus-themes-search-current)))
    `(viper-replace-overlay ((,c :inherit modus-themes-search-replace)))
    `(viper-minibuffer-emacs (( )))
    `(viper-minibuffer-insert (( )))
    `(viper-minibuffer-vi (( )))
;;;;; visible-mark
    `(visible-mark-active ((,c :background ,bg-blue-intense)))
    `(visible-mark-face1 ((,c :background ,bg-cyan-intense)))
    `(visible-mark-face2 ((,c :background ,bg-yellow-intense)))
    `(visible-mark-forward-face1 ((,c :background ,bg-magenta-intense)))
    `(visible-mark-forward-face2 ((,c :background ,bg-green-intense)))
;;;;; visual-regexp
    `(vr/group-0 ((,c :inherit modus-themes-search-rx-group-0)))
    `(vr/group-1 ((,c :inherit modus-themes-search-rx-group-1)))
    `(vr/group-2 ((,c :inherit modus-themes-search-rx-group-2)))
    `(vr/match-0 ((,c :inherit modus-themes-search-current)))
    `(vr/match-1 ((,c :inherit modus-themes-search-lazy)))
    `(vr/match-separator-face ((,c :inherit bold :background ,bg-active)))
;;;;; vterm
    `(vterm-color-black ((,c :background ,bg-term-black :foreground ,fg-term-black)))
    `(vterm-color-bright-black ((,c :background ,bg-term-black-bright :foreground ,fg-term-black-bright)))
    `(vterm-color-red ((,c :background ,bg-term-red :foreground ,fg-term-red)))
    `(vterm-color-bright-red ((,c :background ,bg-term-red-bright :foreground ,fg-term-red-bright)))
    `(vterm-color-green ((,c :background ,bg-term-green :foreground ,fg-term-green)))
    `(vterm-color-bright-green ((,c :background ,bg-term-green-bright :foreground ,fg-term-green-bright)))
    `(vterm-color-yellow ((,c :background ,bg-term-yellow :foreground ,fg-term-yellow)))
    `(vterm-color-bright-yellow ((,c :background ,bg-term-yellow-bright :foreground ,fg-term-yellow-bright)))
    `(vterm-color-blue ((,c :background ,bg-term-blue :foreground ,fg-term-blue)))
    `(vterm-color-bright-blue ((,c :background ,bg-term-blue-bright :foreground ,fg-term-blue-bright)))
    `(vterm-color-magenta ((,c :background ,bg-term-magenta :foreground ,fg-term-magenta)))
    `(vterm-color-bright-magenta ((,c :background ,bg-term-magenta-bright :foreground ,fg-term-magenta-bright)))
    `(vterm-color-cyan ((,c :background ,bg-term-cyan :foreground ,fg-term-cyan)))
    `(vterm-color-bright-cyan ((,c :background ,bg-term-cyan-bright :foreground ,fg-term-cyan-bright)))
    `(vterm-color-white ((,c :background ,bg-term-white :foreground ,fg-term-white)))
    `(vterm-color-bright-white ((,c :background ,bg-term-white-bright :foreground ,fg-term-white-bright)))
    `(vterm-color-inverse-video ((,c :background ,bg-main :inverse-video t)))
    `(vterm-color-underline ((,c :underline t)))
;;;;; vundo
    `(vundo-default ((,c :inherit shadow)))
    `(vundo-highlight ((,c :inherit (bold vundo-node) :foreground ,red)))
    `(vundo-last-saved ((,c :inherit (bold vundo-node) :foreground ,blue)))
    `(vundo-saved ((,c :inherit vundo-node :foreground ,blue-intense)))
;;;;; wcheck-mode
    `(wcheck-default-face ((,c :foreground ,red :underline t)))
;;;;; web-mode
    `(web-mode-annotation-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-annotation-html-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-annotation-tag-face ((,c :inherit web-mode-comment-face :underline t)))
    `(web-mode-block-attr-name-face ((,c :inherit font-lock-constant-face)))
    `(web-mode-block-attr-value-face ((,c :inherit font-lock-type-face)))
    `(web-mode-block-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-block-control-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-block-delimiter-face ((,c :foreground ,fg-main)))
    `(web-mode-block-face ((,c :background ,bg-dim)))
    `(web-mode-block-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-bold-face ((,c :inherit bold)))
    `(web-mode-builtin-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-comment-face ((,c :inherit font-lock-comment-face)))
    `(web-mode-comment-keyword-face ((,c :inherit font-lock-warning-face)))
    `(web-mode-constant-face ((,c :inherit font-lock-constant-face)))
    `(web-mode-css-at-rule-face ((,c :inherit font-lock-constant-face)))
    `(web-mode-css-color-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-css-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-css-function-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-css-priority-face ((,c :inherit font-lock-warning-face)))
    `(web-mode-css-property-name-face ((,c :inherit font-lock-keyword-face)))
    `(web-mode-css-pseudo-class-face ((,c :inherit font-lock-doc-face)))
    `(web-mode-css-selector-face ((,c :inherit font-lock-keyword-face)))
    `(web-mode-css-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-css-variable-face ((,c :inherit font-lock-variable-name-face)))
    `(web-mode-current-column-highlight-face ((,c :background ,bg-inactive)))
    `(web-mode-current-element-highlight-face ((,c :inherit modus-themes-subtle-cyan)))
    `(web-mode-doctype-face ((,c :inherit font-lock-doc-face)))
    `(web-mode-error-face ((,c :inherit error)))
    `(web-mode-filter-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-folded-face ((,c :underline t)))
    `(web-mode-function-call-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-function-name-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-html-attr-custom-face ((,c :inherit font-lock-variable-name-face)))
    `(web-mode-html-attr-engine-face ((,c :foreground ,fg-main)))
    `(web-mode-html-attr-equal-face ((,c :foreground ,fg-main)))
    `(web-mode-html-attr-name-face ((,c :inherit font-lock-variable-name-face)))
    `(web-mode-html-attr-value-face ((,c :inherit font-lock-constant-face)))
    `(web-mode-html-entity-face ((,c :inherit font-lock-negation-char-face)))
    `(web-mode-html-tag-bracket-face ((,c :foreground ,fg-dim)))
    `(web-mode-html-tag-custom-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-html-tag-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-html-tag-namespaced-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-html-tag-unclosed-face ((,c :inherit error :underline t)))
    `(web-mode-inlay-face ((,c :background ,bg-inactive)))
    `(web-mode-italic-face ((,c :inherit italic)))
    `(web-mode-javascript-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-javascript-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-json-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-json-context-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-json-key-face ((,c :foreground ,blue-faint)))
    `(web-mode-json-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-keyword-face ((,c :inherit font-lock-keyword-face)))
    `(web-mode-param-name-face ((,c :inherit font-lock-function-name-face)))
    `(web-mode-part-comment-face ((,c :inherit web-mode-comment-face)))
    `(web-mode-part-face ((,c :inherit web-mode-block-face)))
    `(web-mode-part-string-face ((,c :inherit web-mode-string-face)))
    `(web-mode-preprocessor-face ((,c :inherit font-lock-preprocessor-face)))
    `(web-mode-script-face ((,c :inherit web-mode-part-face)))
    `(web-mode-sql-keyword-face ((,c :inherit font-lock-negation-char-face)))
    `(web-mode-string-face ((,c :inherit font-lock-string-face)))
    `(web-mode-style-face ((,c :inherit web-mode-part-face)))
    `(web-mode-symbol-face ((,c :inherit font-lock-constant-face)))
    `(web-mode-type-face ((,c :inherit font-lock-builtin-face)))
    `(web-mode-underline-face ((,c :underline t)))
    `(web-mode-variable-name-face ((,c :inherit font-lock-variable-name-face)))
    `(web-mode-warning-face ((,c :inherit warning)))
    `(web-mode-whitespace-face ((,c :background ,bg-inactive)))
;;;;; wgrep
    `(wgrep-delete-face ((,c :inherit warning)))
    `(wgrep-done-face ((,c :inherit success)))
    `(wgrep-face ((,c :inherit bold)))
    `(wgrep-file-face ((,c :foreground ,fg-alt)))
    `(wgrep-reject-face ((,c :inherit error)))
;;;;; which-function-mode
    `(which-func ((,c :inherit bold :foreground ,modeline-info))) ; same as `breadcrumb-imenu-leaf-face'
;;;;; which-key
    `(which-key-command-description-face ((,c :foreground ,fg-main)))
    `(which-key-group-description-face ((,c :foreground ,type)))
    `(which-key-highlighted-command-face ((,c :foreground ,warning :underline t)))
    `(which-key-key-face ((,c :inherit modus-themes-key-binding)))
    `(which-key-local-map-description-face ((,c :foreground ,fg-main)))
    `(which-key-note-face ((,c :inherit shadow)))
    `(which-key-separator-face ((,c :inherit shadow)))
    `(which-key-special-key-face ((,c :inherit error)))
;;;;; whitespace-mode
    `(whitespace-big-indent ((,c :background ,bg-space-err)))
    `(whitespace-empty ((,c :background ,bg-space)))
    `(whitespace-hspace ((,c :background ,bg-space :foreground ,fg-space)))
    `(whitespace-indentation ((,c :background ,bg-space :foreground ,fg-space)))
    `(whitespace-line ((,c :background ,bg-space :foreground ,warning)))
    `(whitespace-newline ((,c :background ,bg-space :foreground ,fg-space)))
    `(whitespace-space ((,c :background ,bg-space :foreground ,fg-space)))
    `(whitespace-space-after-tab ((,c :inherit warning :background ,bg-space)))
    `(whitespace-space-before-tab ((,c :inherit warning :background ,bg-space)))
    `(whitespace-tab ((,c :background ,bg-space :foreground ,fg-space)))
    `(whitespace-trailing ((,c :background ,bg-space-err)))
;;;;; window-divider-mode
    `(window-divider ((,c :foreground ,border)))
    `(window-divider-first-pixel ((,c :foreground ,bg-inactive)))
    `(window-divider-last-pixel ((,c :foreground ,bg-inactive)))
;;;;; window-tool-bar-mode
    `(window-tool-bar-button ((,c :inherit modus-themes-button)))
    `(window-tool-bar-button-hover ((,c :inherit (highlight modus-themes-button))))
    `(window-tool-bar-button-disabled ((,c :inherit modus-themes-button :background ,bg-button-inactive :foreground ,fg-button-inactive)))
;;;;; widget
    `(widget-button ((,c :inherit bold :foreground ,fg-link)))
    `(widget-button-pressed ((,c :inherit widget-button :foreground ,fg-link-visited)))
    `(widget-documentation ((,c :inherit font-lock-doc-face)))
    `(widget-field ((,c :background ,bg-inactive :foreground ,fg-main :extend nil)))
    `(widget-inactive ((,c :background ,bg-button-inactive :foreground ,fg-button-inactive)))
    `(widget-single-line-field ((,c :inherit widget-field)))
;;;;; writegood-mode
    `(writegood-duplicates-face ((,c :inherit modus-themes-lang-error)))
    `(writegood-passive-voice-face ((,c :inherit modus-themes-lang-warning)))
    `(writegood-weasels-face ((,c :inherit modus-themes-lang-warning)))
;;;;; woman
    `(woman-addition ((,c :foreground ,accent-2)))
    `(woman-bold ((,c :inherit bold :foreground ,accent-0)))
    `(woman-italic ((,c :inherit italic :foreground ,accent-1)))
    `(woman-unknown ((,c :foreground ,accent-3)))
;;;;; xah-elisp-mode
    `(xah-elisp-at-symbol ((,c :inherit font-lock-warning-face)))
    `(xah-elisp-cap-variable ((,c :inherit font-lock-preprocessor-face)))
    `(xah-elisp-command-face ((,c :inherit font-lock-type-face)))
    `(xah-elisp-dollar-symbol ((,c :inherit font-lock-variable-name-face)))
;;;;; yaml-mode
    `(yaml-tab-face ((,c :background ,bg-space-err)))
;;;;; yasnippet
    `(yas-field-highlight-face ((,c :inherit highlight)))
;;;;; ztree
    `(ztreep-arrow-face ((,c :inherit shadow)))
    `(ztreep-diff-header-face ((,c :inherit modus-themes-heading-0)))
    `(ztreep-diff-header-small-face ((,c :inherit font-lock-doc-face)))
    `(ztreep-diff-model-add-face ((,c :foreground ,info)))
    `(ztreep-diff-model-diff-face ((,c :foreground ,err)))
    `(ztreep-diff-model-ignored-face ((,c :foreground ,fg-dim :strike-through t)))
    `(ztreep-diff-model-normal-face (( )))
    `(ztreep-expand-sign-face ((,c :inherit shadow)))
    `(ztreep-header-face ((,c :inherit modus-themes-heading-0)))
    `(ztreep-leaf-face (( )))
    `(ztreep-node-count-children-face ((,c :inherit (shadow italic))))
    `(ztreep-node-face ((,c :foreground ,accent-0))))
  "Face specs for use with `modus-themes-theme'.")

(defconst modus-themes-custom-variables
  '(
;;;; ansi-colors
    `(ansi-color-faces-vector [default bold shadow italic underline success warning error])
    `(ansi-color-names-vector ["#595959" ,red ,green ,yellow ,blue ,magenta ,cyan "#a6a6a6"])
;;;; chart
    `(chart-face-color-list
      '( ,bg-graph-red-0 ,bg-graph-green-0 ,bg-graph-yellow-0 ,bg-graph-blue-0 ,bg-graph-magenta-0 ,bg-graph-cyan-0
         ,bg-graph-red-1 ,bg-graph-green-1 ,bg-graph-yellow-1 ,bg-graph-blue-1 ,bg-graph-magenta-1 ,bg-graph-cyan-1))
;;;; exwm
    `(exwm-floating-border-color ,border)
;;;; flymake fringe indicators
    `(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-prominent-error))
    `(flymake-warning-bitmap '(exclamation-mark modus-themes-prominent-warning))
    `(flymake-note-bitmap '(exclamation-mark modus-themes-prominent-note))
;;;; highlight-changes
    `(highlight-changes-colors nil)
    `(highlight-changes-face-list '(success warning error bold bold-italic))
;;;; ibuffer
    `(ibuffer-deletion-face 'modus-themes-mark-del)
    `(ibuffer-filter-group-name-face 'bold)
    `(ibuffer-marked-face 'modus-themes-mark-sel)
    `(ibuffer-title-face 'default)
;;;; hl-todo
    `(hl-todo-keyword-faces
      '(("HOLD" . ,warning)
        ("TODO" . ,err)
        ("NEXT" . ,fg-alt)
        ("THEM" . ,fg-alt)
        ("PROG" . ,info)
        ("OKAY" . ,info)
        ("DONT" . ,warning)
        ("FAIL" . ,err)
        ("BUG" . ,err)
        ("DONE" . ,info)
        ("NOTE" . ,warning)
        ("KLUDGE" . ,warning)
        ("HACK" . ,warning)
        ("TEMP" . ,warning)
        ("FIXME" . ,err)
        ("XXX+" . ,err)
        ("REVIEW" . ,info)
        ("DEPRECATED" . ,info)))
;;;; pdf-tools
    `(pdf-view-midnight-colors '(,fg-main . ,bg-dim))
;;;; rcirc-color
    `(rcirc-colors
      '(modus-themes-fg-red
        modus-themes-fg-green
        modus-themes-fg-blue
        modus-themes-fg-yellow
        modus-themes-fg-magenta
        modus-themes-fg-cyan
        modus-themes-fg-red-warmer
        modus-themes-fg-green-warmer
        modus-themes-fg-blue-warmer
        modus-themes-fg-yellow-warmer
        modus-themes-fg-magenta-warmer
        modus-themes-fg-cyan-warmer
        modus-themes-fg-red-cooler
        modus-themes-fg-green-cooler
        modus-themes-fg-blue-cooler
        modus-themes-fg-yellow-cooler
        modus-themes-fg-magenta-cooler
        modus-themes-fg-cyan-cooler
        modus-themes-fg-red-faint
        modus-themes-fg-green-faint
        modus-themes-fg-blue-faint
        modus-themes-fg-yellow-faint
        modus-themes-fg-magenta-faint
        modus-themes-fg-cyan-faint
        modus-themes-fg-red-intense
        modus-themes-fg-green-intense
        modus-themes-fg-blue-intense
        modus-themes-fg-yellow-intense
        modus-themes-fg-magenta-intense
        modus-themes-fg-cyan-intense))
;;;; rustic-ansi-faces
    `(rustic-ansi-faces
      [,fg-term-black
       ,fg-term-red
       ,fg-term-green
       ,fg-term-yellow
       ,fg-term-blue
       ,fg-term-magenta
       ,fg-term-cyan
       ,fg-term-white])
;;;; viper
    `(viper-replace-overlay-cursor-color ,err)
    `(viper-insert-state-cursor-color ,info)
    `(viper-emacs-state-cursor-color ,fg-main)
;;;; xterm-color
    `(xterm-color-names
      [,fg-term-black
       ,fg-term-red
       ,fg-term-green
       ,fg-term-yellow
       ,fg-term-blue
       ,fg-term-magenta
       ,fg-term-cyan
       ,fg-term-white])
    `(xterm-color-names-bright
      [,fg-term-black-bright
       ,fg-term-red-bright
       ,fg-term-green-bright
       ,fg-term-yellow-bright
       ,fg-term-blue-bright
       ,fg-term-magenta-bright
       ,fg-term-cyan-bright
       ,fg-term-white-bright]))
  "Custom variables for `modus-themes-theme'.")

;;; Theme macros

;;;; Instantiate a Modus theme

;;;###autoload
(defmacro modus-themes-theme (name palette &optional overrides)
  "Bind NAME's color PALETTE around face specs and variables.
Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.
Those are stored in `modus-themes-faces' and
`modus-themes-custom-variables' respectively.

Optional OVERRIDES are appended to PALETTE, overriding
corresponding entries."
  (declare (indent 0))
  (let ((sym (gensym))
        (colors (mapcar #'car (symbol-value palette))))
    `(let* ((c '((class color) (min-colors 256)))
            (,sym (modus-themes--palette-value ',name ',overrides))
            ,@(mapcar (lambda (color)
                        (list color
                              `(modus-themes--retrieve-palette-value ',color ,sym)))
                      colors))
       (ignore c ,@colors)            ; Silence unused variable warnings
       (custom-theme-set-faces ',name ,@modus-themes-faces)
       (custom-theme-set-variables ',name ,@modus-themes-custom-variables))))

;;;; Use theme colors

(defmacro modus-themes-with-colors (&rest body)
  "Evaluate BODY with colors from current palette bound."
  (declare (indent 0))
  (let* ((sym (gensym))
         ;; NOTE 2022-08-23: We just give it a sample palette at this
         ;; stage.  It only needs to collect each car.  Then we
         ;; instantiate the actual theme's palette.  We have to do this
         ;; otherwise the macro does not work properly when called from
         ;; inside a function.
         (colors (mapcar #'car (modus-themes--current-theme-palette))))
    `(let* ((c '((class color) (min-colors 256)))
            (,sym (modus-themes--current-theme-palette :overrides))
            ,@(mapcar (lambda (color)
                        (list color
                              `(modus-themes--retrieve-palette-value ',color ,sym)))
                      colors))
       (ignore c ,@colors)            ; Silence unused variable warnings
       ,@body)))

;;;; Add themes from package to path

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (equal dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'modus-themes)
;;; modus-themes.el ends here
