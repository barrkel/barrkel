   Extensions to `isearch.el' (incremental search).

 The Isearch+ libraries are these:

 `isearch+.el' (this file)    - General extensions to `isearch.el'.
                                Can be used with Emacs 20 or later.
 `isearch-prop.el' (optional) - Commands to search within contexts,
                                which are character-property zones:
                                regions of text that have certain
                                text or overlay properties.  Can be
                                Used with Emacs 23 or later.

 You can use either of the Isearch+ files without the other, but I
 recommend that you use them together.


 This file should be loaded *AFTER* loading the standard GNU file
 `isearch.el'.  So in your `~/.emacs' file, do this:

 (eval-after-load "isearch" '(require 'isearch+))

 Library `isearch-prop.el' is optional.  If you do not want to use
 it then do not put it in your `load-path'.  If it is in your
 `load-path' then it will automatically be loaded when you load
 library `isearch+.el'.

 More description below - see Overview of Features.


 Index
 -----

 If you have library `linkd.el' and Emacs 22 or later, load
 `linkd.el' and turn on `linkd-mode' now.  It lets you easily
 navigate around the sections of this doc.  Linkd mode will
 highlight this Index, as well as the cross-references and section
 headings throughout this file.  You can get `linkd.el' here:
 http://dto.freeshell.org/notebook/Linkd.html.

 (@> "Overview of Features")
 (@> "Change log")
 (@> "Faces and Variables")
 (@> "Keys and Hooks")
 (@> "Commands")
 (@> "Non-Interactive Functions")


 Macros defined here:

   `with-isearch-suspended' (Emacs 22-24.3).

 Commands defined here:

   `isearch-char-by-name' (Emacs 23-24.3),
   `isearchp-cycle-mismatch-removal',
   `isearchp-fontify-buffer-now', `isearchp-init-edit',
   `isearchp-open-recursive-edit' (Emacs 22+),
   `isearchp-retrieve-last-quit-search',
   `isearchp-set-region-around-search-target',
   `isearchp-toggle-search-invisible',
   `isearchp-toggle-option-toggle',
   `isearchp-toggle-regexp-quote-yank',
   `isearchp-toggle-set-region', `isearchp-yank-char' (Emacs 22+),
   `isearchp-yank-line' (Emacs 22+),
   `isearchp-yank-sexp-symbol-or-char' (Emacs 22+),
   `isearchp-yank-sexp-symbol-or-char-1' (Emacs 22+),
   `isearchp-yank-symbol-or-char' (Emacs 22+),
   `isearchp-yank-symbol-or-char-1' (Emacs 22+),
   `isearchp-yank-word-or-char' (Emacs 22+).

 User options defined here:

   `isearchp-case-fold', `isearchp-deactivate-region-flag' (Emacs
   24.3+), `isearchp-drop-mismatch',
   `isearchp-initiate-edit-commands' (Emacs 22+),
   `isearchp-mouse-2-flag', `isearchp-regexp-quote-yank-flag',
   `isearchp-restrict-to-region-flag' (Emacs 24.3+),
   `isearchp-set-region-flag', `isearchp-toggle-option-flag'.

 Faces defined here:

   `isearch-fail'.

 Non-interactive functions defined here:

   `isearchp-fail-pos', `isearchp-highlight-lighter',
   `isearchp-message-prefix', `isearchp-message-suffix',
   `isearchp-read-face-names', `isearchp-read-face-names--read',
   `isearchp-read-sexps', `isearchp-remove-duplicates',
   `isearchp-remove-mismatch', `isearchp-repeat-command',
   `isearchp-set-region', `isearchp-set-sel-and-yank',
   `isearchp-update-edit-init-commands' (Emacs
   22+).

 Internal variables defined here:

   `isearchp-last-non-nil-invisible',
   `isearchp-last-quit-regexp-search', `isearchp-last-quit-search',
   `isearchp-reg-beg', `isearchp-reg-end', `isearchp-win-pt-line'.


 ***** NOTE: The following functions defined in `isearch.el' have
             been REDEFINED OR ADVISED HERE:

 `isearch-abort'       - Save search string when `C-g'.
 `isearch-cancel'      - Restore cursor position relative to window.
 `isearch-edit-string' - Put point at mismatch position.
 `isearch-lazy-highlight-search' - Can limit to region (24.3+)
 `isearch-lazy-highlight-update' - Can limit to region (24.3+)
 `isearch-mode'        - Save cursor position relative to window.
 `isearch-mode-help'   - End isearch.  List bindings.
 `isearch-message'     - Highlight failed part of search string in
                         echo area, in face `isearch-fail'.
 `isearch-message-prefix' - Highlight prompt keywords:
                            wrapped, regexp, word, multi
 `isearch-mouse-2'     - Respect `isearchp-mouse-2-flag'(Emacs 21+)
 `isearch-search'      - Can limit to active region (Emacs 24.3+)
 `isearch-repeat'      - Can limit to active region (Emacs 24.3+)
 `isearch-printing-char' - Respect option `isearchp-drop-mismatch'
 `isearch-toggle-case-fold' - Respect `isearchp-toggle-option-flag'
                              Show case sensitivity in mode-line.
                              Message.
 `isearch-toggle-invisible' - Respect `isearchp-toggle-option-flag'
                              Message.
 `isearch-toggle-word' - Message, and turn off regexp search.
 `isearch-update' (Emacs 20-23) - Run `isearch-update-post-hook'.
 `isearch-yank-string' - Respect `isearchp-regexp-quote-yank-flag'.


 ***** NOTE: The following internal variables defined in
             `isearch.el' have been REDEFINED HERE:

 `isearch-invisible'   - defined for Emacs<24.4 & added doc string.


 Keys bound in `isearch-mode-map' here:

   `C-`'        `isearchp-toggle-regexp-quote-yank'
   `C-+'        `isearchp-toggle-search-invisible'
   `C-_'        `isearchp-yank-symbol-or-char' (Emacs 22+)
   `C-('        `isearchp-yank-sexp-symbol-or-char' (Emacs 22+)
   `C-SPC'      `isearchp-toggle-set-region'
   `C-end'      `goto-longest-line' (requires `misc-cmds.el')
   `C-h'        `isearch-mode-help'
   `C-x n'      `isearchp-toggle-region-restriction' (Emacs 24.3+)
   `C-x o'      `isearchp-open-recursive-edit' (Emacs 22+)
   `C-x 8 RET'  `isearch-char-by-name' (Emacs 23-24.3)
   `C-y C-_'    `isearchp-yank-symbol-or-char' (Emacs 22+)
   `C-y C-('    `isearchp-yank-sexp-symbol-or-char' (Emacs 22+)
   `C-y C-2'    `isearch-yank-secondary'
   `C-y C-c'    `isearchp-yank-char' (Emacs 22+)
   `C-y C-e'    `isearchp-yank-line'
   `C-y C-w'    `isearchp-yank-word-or-char' (Emacs 22+)
   `C-y C-y'    `isearch-yank-kill'
   `C-y M-y'    `isearch-yank-pop' (Emacs 24+)
   `M-c'        `isearch-toggle-case-fold'
   `M-e'        `isearch-edit-string'
   `M-g'        `isearchp-retrieve-last-quit-search'
   `M-k'        `isearchp-cycle-mismatch-removal'
   `M-r'        `isearch-toggle-regexp'
   `M-s i'      `isearch-toggle-invisible'
   `M-s w'      `isearch-toggle-word'
   `M-w'        `isearchp-kill-ring-save'
   `C-M-y'      `isearch-yank-secondary'
   `C-M-tab'    `isearch-complete' (on MS Windows)
   `next'       `isearch-repeat-forward'
   `prior'      `isearch-repeat-backward'


 User option `isearchp-initiate-edit-commands' causes certain keys
 not to exit Isearch but rather to edit the search string.
 Customize it to `nil' if you do not want this behavior at all.


 The following bindings are made here for incremental search edit
 mode:

   `C-x 8 RET'  `insert-char' (Emacs 23+)
   `C-M-tab'    `isearch-complete-edit' (MS Windows only)

(@* "Overview of Features")

Overview of Features ---------------------------------------------

 * Case-sensitivity is indicated in the mode line minor-mode
   lighter: `ISEARCH' for case-insensitive; `Isearch' for
   case-sensitive.

 * Highlighting of the mode-line minor-mode lighter when search has
   wrapped around (Emacs 24+ only).

 * Highlighting of parts of the prompt, to indicate the type of
   search: regexp, word, multiple-buffer, and whether searching has
   wrapped around the buffer (Emacs 22+ only).

 * Optional limiting of search to the active region, controlled by
   option `isearchp-restrict-to-region-flag'.  Deactivation of the
   active region, controlled by option
   `isearchp-deactivate-region-flag'.  Both of these are available
   for Emacs 24.3 and later.  You can use `C-x n' (command
   `isearchp-toggle-region-restriction') during search to toggle
   `isearchp-restrict-to-region-flag'.

 * Option and commands to let you select the last target occurrence
   (set the region around it):

   - Option `isearchp-set-region-flag' - Non-`nil' means
     automatically set the region around the last search target.
   - Command `isearchp-toggle-set-region', bound to `C-SPC' during
     isearch - toggle `isearchp-set-region-flag'.
   - Command `set-region-around-search-target' - manually set the
     region around the last search target.

 * Option (`isearchp-regexp-quote-yank-flag') and command
   (`isearchp-toggle-regexp-quote-yank', bound to `C-`') to toggle
   quoting (escaping) of regexp special characters.  With escaping
   turned off, you can yank text such as `^\*.*' without it being
   transformed to `\^\\\*\.\*'.

 * `M-g' (`isearchp-retrieve-last-quit-search') yanks the last
   successful search string (regexp or plain) from when you last
   hit `C-g' in Isearch.  Sometimes you search for something but
   abandon the search - you just want to check the locations of
   something, without staying at any of them.  Afterward, if you
   want to find them again, use `M-g'.  This yanks that search
   string, so you can append it to whatever you are already
   searching for.

 * `C-M-y' (`isearch-yank-secondary') yanks the secondary selection
   into the search string, if you also use library `second-sel.el'.

 * `C-c' (`isearchp-yank-char') yanks successive characters onto
   the search string.

 * `C-_' (`isearchp-yank-symbol-or-char') yanks successive symbols
   (or words or subwords or chars) into the search string.

 * `C-(' (`isearchp-yank-sexp-symbol-or-char') yanks successive
   sexps (or symbols or words or subwords or chars) into the search
   string.

 * `M-w' (`isearchp-kill-ring-save') copies the current search
   string to the kill ring.  You can then, for example, use `C-s
   M-y' to search for the same thing in another Emacs session.

   (I use this all the time, but you might not use multiple Emacs
   sessions.)  Note that if you did not have this feature then you
   would need to select the search-string text (in the text buffer
   or in the `M-e' Isearch edit buffer) and copy it to the kill
   ring. (Note: `M-w' used to toggle word search, but
   `isearch-toggle-word' is now `M-s w'.)

 * All commands that yank text onto the search string are bound to
   keys with prefix `C-y' (in addition to any other Isearch
   bindings):

     `C-y C-_'   isearchp-yank-symbol-or-char
     `C-y C-('   isearchp-yank-sexp-symbol-or-char
     `C-y C-2'   isearch-yank-secondary
     `C-y C-c'   isearchp-yank-char
     `C-y C-e'   isearchp-yank-line
     `C-y C-w'   isearchp-yank-word-or-char
     `C-y C-y'   isearch-yank-kill
     `C-y M-y'   isearch-yank-pop

   You can repeat any of these for which it makes sense (i.e., all
   except `isearch-yank-secondary', `isearch-yank-kill', and
   `isearch-yank-pop') by just repeating the last key.  For
   example: `C-y C-e C-e C-e' adds the text up to the end of three
   lines.

 * `C-x 8 RET' (`isearch-char-by-name') reads the name of a Unicode
   character with completion and appends it to the search string.
   Same thing when editing the search string (i.e., after `M-e').
   This is part of GNU Emacs starting with Emacs 24.4.

 * `C-x o' (`isearchp-open-recursive-edit') opens a recursive
   editing session, where you can do anything you like (including
   search for something different).  Using `C-M-c' closes the
   recursive editing session and resumes the search (from the
   current position when you hit `C-M-c').

 * Highlighting of the mismatched portion of your search string in
   the minibuffer.  This is the portion that is removed if you do
   `C-g', or removed/replaced automatically if you use `M-k' (see
   next).  I added this feature to GNU Emacs 23.1.

 * `C-g' after successfully finding matches restores not only the
   original position but also its relative position in the window.
   IOW, you get back to what you saw before searching.  Fixes Emacs
   bug #12253 for Isearch.

 * `M-k' (`isearchp-cycle-mismatch-removal') cycles automatic
   removal or replacement of the input portion that does not match,
   bound to .  The behavior is controlled by the value of option
   `isearchp-drop-mismatch':

   `replace-last' - Your current input replaces the last mismatched
                    text.  You can always see your last input, even
                    if it is a mismatch.  And it is available for
                    editing using `M-e'.
   nil            - Your current input is appended, even if the
                    previous input has a mismatched portion.
   anything else  - Your current input is ignored (removed) if it
                    causes a mismatch.  The search string always
                    has successful matches.

 * Non-`nil' option `isearchp-toggle-option-flag', which you can
   toggle using `M-s v' (`isearchp-toggle-option-toggle'),
   determines whether commands that toggle behavior also toggle an
   associated user option.  For such commands, a prefix argument
   flips the behavior, as if `isearchp-toggle-option-flag' were
   toggled temporarily.  Currently this feature applies to toggles
   `M-c' (case-sensitivity) and `M-s i' (matching hidden text).

 * `M-c' (`isearch-toggle-case-fold') toggles case sensitivity.  If
   option `isearchp-toggle-option-flag' is non-nil then it toggles
   option `isearchp-case-fold' to change the sensitivity from now
   on.  Otherwise, the option value is not changed, so the effect
   is for the current search only.

 * `M-s i' (`isearch-toggle-invisible') toggles invisible-text
   sensitivity.  If option `isearchp-toggle-option-flag' is non-nil
   then it toggles option `search-invisible' to change the
   sensitivity from now on.  Otherwise, the option value is not
   changed, so the effect is for the current search only.

 * `C-+' (`isearchp-toggle-search-invisible') toggles the value of
   option `search-invisible'.  The effect is like that of `M-s i'
   with no prefix argument and with non-nil
   `isearchp-toggle-option-flag'.

 * Other bindings during Isearch:

   - `next', `prior' repeat the last Isearch forward and backward
     (easier than using the chords `C-s', `C-r').
   - `C-end' - go to the longest line.  Repeat to go to the longest
     line following that one in the buffer.  As usual, `C-g' puts
     you back where you started.  This binding is made only if you
     also use `misc-cmds.el'.
   - `C-h' provides help on Isearch while searsching.  This library
     also redefines `isearch-mode-help' so that it lists all
     Isearch bindings and ends Isearch properly.

 * `M-e' (`isearch-edit-string') automatically puts the cursor at
   the first mismatch position in the search string, for easy
   editing.  Whereas `C-g' (see also `M-k') removes all of the
   mismatch, this feature lets you change or insert a character or
   two, without losing the rest of the search string.

 * A user option, `isearchp-initiate-edit-commands', that specifies
   commands whose keys will not exit Isearch but will instead
   initiate editing of the search string.  For example, if
   `backward-char' is included in the list then `C-b' and `left'
   will just move the cursor backward over the search string so you
   can change, delete, or insert chars in the middle somewhere.
   This makes the search string more minibuffer-like.

 * You can, by default, select text with the mouse, then hit `C-s'
   etc. to search for it.  This is controlled by user option
   `isearchp-mouse-2-flag'.

 If you have Emacs 23 or later then I recommend that you also use
 the companion library, `isearch-prop.el'.  If it is in your
 `load-path' then it will be loaded by `isearch+.el'.  It lets you
 limit incremental searching to contexts that you define.

 Example: search within zones having a `face' text property with a
 value of `font-lock-comment-face' or `font-lock-string-face'.
 Search overlays or text properties.

 Besides relying on existing text properties such as `face' for
 contexts to search, you can use command
 `isearchp-put-prop-on-region' to add any text property to the
 region.  This gives you an easy way to set up contexts for
 text-property search.  For property `face', empty input to
 `isearchp-put-prop-on-region' removes all faces from the region.
