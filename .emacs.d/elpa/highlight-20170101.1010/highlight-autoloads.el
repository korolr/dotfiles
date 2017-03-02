;;; highlight-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "highlight" "highlight.el" (22688 13531 839980
;;;;;;  503000))
;;; Generated autoloads from highlight.el

(defvar hlt-auto-face-foreground nil "\
*Foreground color for pseudo faces created from a chosen background.
The value is either a color (name or #RGB hex triplet) or nil.  A nil
value means that highlighting does not change the existing foreground
color.")

(custom-autoload 'hlt-auto-face-foreground "highlight" t)

(defvar hlt-auto-faces-flag nil "\
*Non-nil means highlighting can automatically choose faces.
Highlighting action can use the next background color or face in
`hlt-auto-face-backgrounds'.  When a given item in the list is a color
name, not a face, `hlt-auto-face-foreground' is used as the
corresponding foreground.

This option has no effect on unhighlighting.")

(custom-autoload 'hlt-auto-faces-flag "highlight" t)

(defvar hlt-default-copy/yank-props '(face) "\
*Properties that `hlt-copy-props' and `hlt-yank-props' use by default.
You can use a prefix argument with those commands to override the
default behavior.
Either a list of properties (symbols) or `t', meaning all properties.")

(custom-autoload 'hlt-default-copy/yank-props "highlight" t)

(defvar hlt-face-prop (if (< emacs-major-version 22) 'face 'font-lock-face) "\
*Face property used for highlighting: `face' or `font-lock-face'.
If the value is `font-lock-face' then highlighting by library
`highlight.el' disappears when `font-lock-mode' is turned off.

If the value is `face', and if library`font-lock+.el' has been loaded,
then highlighting persists - it is independent of font-locking.

For Emacs 20 or 21, font-locking does not recognize property
`font-lock-face', so just use `face' (the default value for these
versions), not `font-lock-face' - do not change the value.")

(custom-autoload 'hlt-face-prop "highlight" t)

(defvar hlt-line-dups-ignore-regexp "[ 	]*" "\
Regexp to ignore leading and trailing text for duplicate lines.
Or nil if no such text is to be ignored.
Used by `hlt-highlight-line-dups-region' to determine whether two
lines are duplicates.")

(custom-autoload 'hlt-line-dups-ignore-regexp "highlight" t)

(defvar hlt-max-region-no-warning 100000 "\
*Maximum size (chars) of region to highlight without confirmation.
This is used only for highlighting of a regexp, which can be slow.")

(custom-autoload 'hlt-max-region-no-warning "highlight" t)

(defvar hlt-overlays-priority 0 "\
*Priority of the overlays created by `hlt-*' functions.")

(custom-autoload 'hlt-overlays-priority "highlight" t)

(defvar hlt-use-overlays-flag 'only "\
*Non-nil means use overlays to highlight; nil means use text properties.
This value also affects some actions, such as unhighlighting, for text
that is highlighted.  If the value is `only' (the default value), then
those actions only affect overlay highlighting.  Otherwise, they
affect both kinds of highlighting.")

(custom-autoload 'hlt-use-overlays-flag "highlight" t)

(autoload 'hlt-choose-default-face "highlight" "\
Choose a face for highlighting.
Set `hlt-last-face' to the face, and return it.

You can choose a face name or a color name.  If a color is chosen, it
is used for the face background.  The face foreground is determined by
the value of `hlt-auto-face-foreground'.

\(fn FACE)" t nil)

(autoload 'hlt-next-face "highlight" "\
Choose the next face for highlighting and unhighlighting.
Use `hlt-auto-face-backgrounds' and `hlt-auto-face-foreground'.

A non-negative numeric prefix arg N means use the Nth entry of
`hlt-auto-face-backgrounds'.  Counting is 0-based.

A negative numeric prefix arg N means count from the end, not the
beginning, of `hlt-auto-face-backgrounds': -1 means the last entry, -2
means the next-to-last, etc. (counting from the end is 1-based, not
0-based).

From LISP, if the first argument is `previous' then choose the
previous face, not the next one.

\(fn &optional FACE-NB MSGP)" t nil)

(autoload 'hlt-previous-face "highlight" "\
Like `hlt-next-face', but previous, not next.
Use of a numeric prefix arg is the same as for `hlt-next-face'.

\(fn &optional FACE-NB MSGP)" t nil)

(autoload 'hlt-highlighter "highlight" "\
Highlight the text you drag the mouse over.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose the default
face to use.

If `hlt-auto-faces-flag' is non-nil then this command cycles to the
next color/face.  This is the case even if you do not drag the
mouse (empty highlight).  A message tells you what the face is.

A numeric prefix arg N means use the face represented by the Nth entry
of `hlt-auto-face-backgrounds' (uses `hlt-next-face').

\(fn START-EVENT &optional FACE-NB)" t nil)

(autoload 'hlt-eraser "highlight" "\
Erase highlights that you click or drag the mouse over.
If `hlt-use-overlays-flag' is non-nil, then remove overlay
highlighting for the last face that was used for highlighting.  (You
can use command `hlt-choose-default-face' first to choose a different
face.)

If `hlt-use-overlays-flag' is not `only', then remove text-property
highlighting for *ALL* faces (not just highlighting faces).  This
means, in particular, that a value of nil erases both overlays for the
last face and text properties for all faces.

With a numeric prefix arg N, if `hlt-use-overlays-flag' is `only',
erase the face represented by the Nth entry of
`hlt-auto-face-backgrounds' (uses `hlt-next-face').

\(fn START-EVENT &optional FACE-NB)" t nil)

(autoload 'hlt-highlighter-mouse "highlight" "\
Same as `hlt-highlighter', but for binding to a menu item.

\(fn)" t nil)

(autoload 'hlt-eraser-mouse "highlight" "\
Same as `hlt-eraser', but for binding to a menu item.

\(fn)" t nil)

(autoload 'hlt-highlight "highlight" "\
Highlight or unhighlight.
If the region is not active or it is empty, then use the whole buffer.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face.

This is several commands rolled into one, depending on the prefix arg:

* No prefix arg: highlight all text in region/buffer
* Plain prefix arg (`C-u') or zero prefix arg (`C-0'): UNhighlight all
* Positive prefix arg (`C-1'): highlight regexp matches
* Negative prefix arg (`C--'): UNhighlight regexp matches

You can also used the individual commands:

* `hlt-highlight-region'          - same as no prefix arg
* `hlt-unhighlight-region'        - same as `C-u' or `C-0'
* `hlt-highlight-regexp-region'   - same as `C-1'
* `hlt-unhighlight-regexp-region' - same as `C--'

\(fn &optional PREFIX)" t nil)

(autoload 'hlt-highlight-region-in-buffers "highlight" "\
Use `hlt-highlight-region' in each buffer of list BUFFERS.
A prefix arg >= 0 means highlight with `mouse-face', not `face'.
A prefix arg <= 0 means highlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to highlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is highlighted.

Non-nil optional arg MSGP means show status messages.

\(fn BUFFERS &optional MSGP)" t nil)

(autoload 'hlt-highlight-region "highlight" "\
Highlight either the region/buffer or new input that you type.
Use the region if active, or the buffer otherwise.

If *ALL* of the following are true then apply the last-used face as a
text property to the next and subsequent characters that you type, and
add that face to a Facemenu menu (`Text Properties' or one of its
submenus):

 * You call this command interactively.
 * You use no prefix arg.
 * Option `hlt-use-overlays-flag' is nil
 * The last property used for highlighting was `face'.

Otherwise, the behavior respects `hlt-use-overlays-flag' and depends
on the optional arguments, as follows:

Optional args START and END are the limits of the area to act on.
  They default to the region limits.  If the region is not active or
  it is empty, then use the whole buffer.  (But see BUFFERS, below.)

Optional 3rd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)

Optional 4th arg MSGP non-nil means to display a progress message.
  Interactively, MSGP is t.

Optional 5th arg MOUSEP non-nil means use `mouse-face', not `face'.
  Interactively, MOUSEP is provided by the prefix arg.

Optional 6th arg BUFFERS is the list of buffers to highlight.
  If non-nil and this command is called interactively then explicit
  START and END values are ignored, and the actual values are
  determined automatically for each buffer, based on whether the
  region is active there.

\(fn &optional START END FACE MSGP MOUSEP BUFFERS)" t nil)

(autoload 'hlt-unhighlight-region-in-buffers "highlight" "\
Use `hlt-unhighlight-region' in each buffer of list BUFFERS.
A prefix arg >= 0 means unhighlight `mouse-face', not `face'.
A prefix arg <= 0 means unhighlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to unhighlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is unhighlighted.

Non-nil optional arg MSGP means show status messages.

\(fn BUFFERS &optional MSGP)" t nil)

(autoload 'hlt-unhighlight-region "highlight" "\
Remove all highlighting in region or buffer.
Use the region if active, or the buffer otherwise.
The arguments are the same as for `hlt-highlight-region'.

If `hlt-use-overlays-flag' is non-nil, then remove overlay highlighting.
If `hlt-use-overlays-flag' is not `only', then remove text-property
highlighting.  This means, in particular, that a value of nil removes
both overlays and text properties.

\(fn &optional START END FACE MSGP MOUSEP BUFFERS)" t nil)

(autoload 'hlt-highlight-regexp-region-in-buffers "highlight" "\
Use `hlt-highlight-regexp-region' in each buffer of list BUFFERS.
A prefix arg >= 0 means highlight with `mouse-face', not `face'.
A prefix arg <= 0 means highlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to highlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is highlighted.

See `hlt-highlight-regexp-region' for other arguments.

\(fn REGEXP BUFFERS &optional FACE MSGP MOUSEP NTH)" t nil)

(autoload 'hlt-highlight-regexp-region "highlight" "\
Highlight regular expression REGEXP in region/buffer.
Use the region if active, or the buffer otherwise.

Optional args START and END are the limits of the area to act on.
  They default to the region limits.  (But see BUFFERS, below.)
Optional 4th arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 5th arg MSGP:
  t means to treat this as an interactive call when deciding to
    display all messages.
  non-nil & non-t means to display only error and warning messages.
Optional 6th arg MOUSEP non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg.
Optional 7th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)
Optional 6th arg BUFFERS is the list of buffers to highlight.
  If non-nil then explicit START and END values are ignored, and the
  actual values are determined automatically for each buffer, based on
  whether the region is active there.

\(fn &optional START END REGEXP FACE MSGP MOUSEP NTH BUFFERS)" t nil)

(autoload 'hlt-unhighlight-regexp-region-in-buffers "highlight" "\
Use `hlt-unhighlight-regexp-region' in each buffer of list BUFFERS.
A prefix arg >= 0 means unhighlight `mouse-face', not `face'.
A prefix arg <= 0 means unhighlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to unhighlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is unhighlighted.

See `hlt-highlight-regexp-region' for other arguments.

\(fn REGEXP BUFFERS &optional NTH MSGP)" t nil)

(autoload 'hlt-unhighlight-regexp-region "highlight" "\
Unhighlight text matching regular expression REGEXP in region/buffer.
This is like `hlt-highlight-regexp-region' (which see), but opposite.
Where `hlt-highlight-regexp-region' highlights REGEXP matches, this
unhighlights the matches.

\(fn &optional START END REGEXP FACE MSGP MOUSEP NTH BUFFERS)" t nil)

(autoload 'hlt-highlight-regexp-to-end "highlight" "\
Highlight text after cursor that matches REGEXP.
The behavior respects `hlt-use-overlays-flag' and depends on the
optional arguments, as follows:

 Optional 2nd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different
  face.)

 Optional 3rd arg MSGP non-nil means to display a progress message.
  Interactively, MSGP is t.

 Optional 4th arg MOUSEP non-nil means use property `mouse-face', not
 `face'.  Interactively, MOUSEP is provided by the prefix arg.

 Optional 5th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)

\(fn REGEXP &optional FACE MSGP MOUSEP NTH)" t nil)

(autoload 'hlt-unhighlight-regexp-to-end "highlight" "\
UNhighlight text after cursor that matches REGEXP.
This is like `hlt-highlight-regexp-to-end' (which see), but opposite.
Where `hlt-highlight-regexp-to-end' highlights REGEXP matches, this
unhighlights the matches.

\(fn REGEXP &optional FACE MSGP MOUSEP NTH)" t nil)

(autoload 'hlt-highlight-regexp-groups-region "highlight" "\
Like `hlt-highlight-regexp-region', but highlight regexp groups.
Highlight regular expression REGEXP in region/buffer.
Use the region if active, or the buffer otherwise.

Up to 8 group levels are highlighted, using faces `hlt-regexp-level-1'
through `hlt-regexp-level-8'.  The current default highlighting face
is used to highlight the non-group matches. You can change the default
face using command `hlt-choose-default-face'.

Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 4th arg MSGP:
  t means to treat this as an interactive call when deciding to
    display all messages.
  non-nil & non-t means to display only error and warning messages.
Optional 5th arg MOUSEP non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg.

\(fn &optional START END REGEXP MSGP MOUSEP)" t nil)

(autoload 'hlt-unhighlight-regexp-groups-region "highlight" "\
Like `hlt-highlight-regexp-groups-region', but opposite.
Where `hlt-highlight-regexp-groups-region' highlights REGEXP matches,
this unhighlights the matches.

\(fn &optional START END REGEXP MSGP MOUSEP)" t nil)

(autoload 'hlt-unhighlight-region-for-face-in-buffers "highlight" "\
Use `hlt-unhighlight-region-for-face' in each buffer of list BUFFERS.
A prefix arg >= 0 means unhighlight `mouse-face', not `face'.
A prefix arg <= 0 means unhighlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to unhighlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is unhighlighted.

See `hlt-unhighlight-region-for-face' for other arguments.

\(fn FACE BUFFERS &optional MSGP)" t nil)

(autoload 'hlt-unhighlight-region-for-face "highlight" "\
Remove any highlighting in the region that uses FACE.
Same as `hlt-unhighlight-region', but removes only highlighting
that uses FACE.  Interactively, you are prompted for the face.

This works only for overlay highlighting, not text-property
highlighting.

Note: When text in the region has been highlighted using more than one
face, unhighlighting for one of those faces can mean that adjacent
highlighting outside the region appears to change.  That outside text
still has the same multiple-overlay face highlighting, but the overlay
stacking order is not the same as it was.

Optional arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional args START and END are the limits of the area to act on.
  They default to the region limits.  (But see BUFFERS, below.)
Optional arg MSGP non-nil means show status messages.
Optional arg MOUSEP non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSEP is provided by the prefix arg.
Optional arg BUFFERS is the list of buffers to unhighlight for FACE.
  If non-nil then explicit START and END values are ignored, and the
  actual values are determined automatically for each buffer, based on
  whether the region is active there.

\(fn &optional FACE START END MSGP MOUSEP BUFFERS)" t nil)

(autoload 'hlt-replace-highlight-face-in-buffers "highlight" "\
Use `hlt-replace-highlight-face' in each buffer of list BUFFERS.
A prefix arg >= 0 means highlight with `mouse-face', not `face'.
A prefix arg <= 0 means highlight all visible or iconified buffers.
Otherwise, you are prompted for the BUFFERS to highlight, one at a
 time.  Use `C-g' to end prompting.
If you specify no BUFFERS then the current buffer is highlighted.

See `hlt-replace-highlight-face' for other arguments.

\(fn OLD-FACE NEW-FACE BUFFERS &optional MSGP)" t nil)

(autoload 'hlt-replace-highlight-face "highlight" "\
Replace OLD-FACE by NEW-FACE in overlay highlighting in the region.
This command applies only to overlay highlighting created by library
`highlight.el'.

Update the last-used highlighting face.

With a prefix argument, replace OLD-FACE as the `mouse-face' property,
 not the `face' property.

Other arguments:
 Optional args START and END are the limits of the area to act on.
  They default to the region limits.  If the region is not active or
  it is empty, then use the whole buffer.
 Optional arg MSGP non-nil means display a progress message.
 Optional arg MOUSEP non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSEP is provided by the prefix arg.

\(fn OLD-FACE NEW-FACE &optional START END MSGP MOUSEP BUFFERS)" t nil)

(autoload 'hlt-highlight-symbol "highlight" "\
Highlight occurrences of SYMBOL.
The symbol at point is used by default, or the symbol under the mouse
pointer if the command is invoked using the mouse.

Highlight over the region if active, or the buffer otherwise.
With a prefix arg, use all buffers that are visible or iconified.
\(This first unhighlights occurrences, to prevent stacking up multiple
highlighting on the same occurrences.)

When called from Lisp:
* ALL-BUFFERS-P corresponds to the prefix-argument behavior.
* FACE is the face to use.

\(fn SYMBOL &optional ALL-BUFFERS-P FACE)" t nil)

(autoload 'hlt-unhighlight-symbol "highlight" "\
Unhighlight occurrences of SYMBOL.
The symbol at point is used by default, or the symbol under the mouse
pointer if the command is invoked using the mouse.

Unhighlight over the region if active, or the buffer otherwise.
With a prefix arg, use all buffers that are visible or iconified.

When called from Lisp:
* ALL-BUFFERS-P corresponds to the prefix-argument behavior.
* FACE is the face to unhighlight.

\(fn SYMBOL &optional ALL-BUFFERS-P FACE)" t nil)

(autoload 'hlt-highlight-enclosing-list "highlight" "\
Highlight the ARGth level sexp enclosing point.
ARG is the numeric prefix value.

A negative prefix arg prompts you for the face to use.  This face is
used by default from then on.  You can also choose the default face
using command `hlt-choose-default-face'.  The same face is used as the
default for all `hlt-*' functions.

When used in Lisp code:
 MOUSEP non-nil means use property `mouse-face', not `face'.

\(fn ARG &optional FACE MOUSEP)" t nil)

(autoload 'hlt-highlight-single-quotations "highlight" "\
Highlight single-quoted text in the region.
This means, for example, commands and keys between `'s: `foobar'.
If the region is not active or it is empty, then use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a
 different face.

\(fn &optional FACE)" t nil)

(autoload 'hlt-highlight-line-dups-region "highlight" "\
Highlight sets of duplicate lines in the region.
Each set is given a different background, according to user option
`hlt-auto-face-backgrounds'.

Whether leading and trailing whitespace is ignored is controlled by
option `hlt-line-dups-ignore-regexp'.  But a prefix argument reverses
this: if the option value is \"\" then whitespace defined by
\"[ 	]*\" is ignored, and otherwise whitespace is not ignored.

\(fn &optional START END MSGP FLIP)" t nil)

(autoload 'hlt-highlight-lines "highlight" "\
Highlight full lines in region, window-wide.

\(fn START END FACE MSGP)" t nil)

(autoload 'hlt-mouse-face-each-line "highlight" "\
Put `mouse-face' on each line of buffer in region.
If the region is active and not empty, then limit mouse-face
highlighting to the region.  Otherwise, use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a
 different face.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MSGP non-nil means display a progress message.

\(fn &optional START END FACE MSGP)" t nil)

(autoload 'hlt-toggle-use-overlays-flag "highlight" "\
Toggle `hlt-use-overlays-flag'.
If the current value is non-nil, it is set to nil.
If the current value is nil, it is set to the last non-nil value.

\(fn)" t nil)

(defalias 'hlt-paste-props 'hlt-yank-props)

(autoload 'hlt-yank-props "highlight" "\
Yank (paste) copied text properties over the active region.
Interactively, do nothing if there is no nonempty active region.
By default, yank only the copied properties defined by
 `hlt-default-copy/yank-props'.
With a plain or non-negative prefix arg, yank all copied properties.
With a negative prefix arg, you are prompted for the copied properties
 to yank.  To finish entering properties, hit `RET RET' (i.e., twice).

NOTE: If the list of copied text properties is empty, then yanking
      REMOVES ALL PROPERTIES from the text in the region.  This
      provides an easy way to UNpropertize text.

\(fn START END &optional ARG MSGP)" t nil)

(autoload 'hlt-mouse-copy-props "highlight" "\
Same as `hlt-copy-props', but copy at mouse pointer, not at point.

\(fn &optional EVENT ARG MSGP)" t nil)

(autoload 'hlt-copy-props "highlight" "\
Copy text properties at point for use by `hlt-yank-props'.
Properties are copied to `hlt-copied-props'.
By default, copy the properties defined by
 `hlt-default-copy/yank-props'.
With a plain or non-negative prefix arg, copy all properties.
With a negative prefix arg, you are prompted for the properties to
 copy.  To finish entering properties, hit `RET RET' (i.e., twice).

\(fn &optional POSITION ARG MSGP)" t nil)

(autoload 'hlt-highlight-isearch-matches "highlight" "\
Highlight matches of the current Isearch search pattern using FACE.
If the region is active then it limits highlighting.  If inactive then
highlight matches throughout the buffer, or the list of BUFFERS.  If
this is accessed from a `multi-search' command then the BUFFERS are
the buffers being searched.

With no prefix arg:
 * If `hlt-auto-faces-flag' is nil then use the last highlighting face
   used or chosen with command `hlt-choose-default-face'.
 * If non-nil then use the next highlighting face.

With a non-negative prefix arg, prompt for the face to use.
With a non-positive prefix arg, use `mouse-face' instead of `face'.

To use a prefix argument you must set either `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

\(fn &optional FACE MSGP MOUSEP BUFFERS STRING)" t nil)

(autoload 'hlt-unhighlight-isearch-matches "highlight" "\
Unhighlight matches of the current Isearch search pattern.
With no prefix arg, unhighlight all faces.
With a non-negative prefix arg, prompt for the face to unhighlight.
With a non-positive prefix arg, use `mouse-face' instead of `face'.
With any other prefix arg, unhighlight the last highlighting face used
 or chosen with command `hlt-choose-default-face'.
 (`hlt-auto-faces-flag' has no effect.)

To use a prefix argument you must set either `isearch-allow-scroll' or
`isearch-allow-prefix' (if available) to non-nil.  Otherwise, a prefix
arg during Isearch exits Isearch.

Non-interactively, FACE = nil means unhighlight all faces.

\(fn &optional FACE MSGP MOUSEP BUFFERS STRING)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; highlight-autoloads.el ends here
