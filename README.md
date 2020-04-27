editor
======

Vim style editor written in Racket

## Roadmap

These are the big Vim features, put generally in the order in which we plan to implement them.

| Status             | Command                |
| ------------------ | ---------------------- |
| [x] | Normal Mode            |
| [x] | Insert Mode            |
| [x] | Visual Mode            |
| [x] | Visual Line Mode       |
| [x] | Number Prefixes        |
| [x] | . Operator             |
| [x] | Searching with / and ? |
| [x] | Undo/Redo              |
| [x] | Marks                  |
| [x] | Text Objects           |
| [x] | Visual Block Mode      |
| [x] | Replace Mode           |
| [x] | Macros                 |
| [ ] | Buffer/Window/Tab      |
| [ ] | Command Remapping      |

Now follows an exhaustive list of every known Vim command that we could find.

## Left-right motions

| Status             | Command        | Description                                                                    |
| ------------------ | -------------- | ------------------------------------------------------------------------------ |
| [x] | :1234: h       | left (also: CTRL-H, BS, or Left key)                                           |
| [x] | :1234: l       | right (also: Space or Right key)                                               |
| [x] | 0              | to first character in the line (also: Home key)                                |
| [x] | ^              | to first non-blank character in the line                                       |
| [x] | :1234: \$      | to the last character in the line (N-1 lines lower) (also: End key)            |
| [x] | :1234: \|      | to column N (default: 1)                                                       |
| [x] | :1234: f{char} | to the Nth occurrence of {char} to the right                                   |
| [x] | :1234: F{char} | to the Nth occurrence of {char} to the left                                    |
| [x] | :1234: t{char} | till before the Nth occurrence of {char} to the right                          |
| [x] | :1234: T{char} | till before the Nth occurrence of {char} to the left                           |
| [x] | :1234: ;       | repeat the last "f", "F", "t", or "T" N times                                  |
| [x] | :1234: ,       | repeat the last "f", "F", "t", or "T" N times in opposite direction            |
| [ ] | g0             | to first character in screen line (differs from "0" when lines wrap)           |
| [ ] | g^             | to first non-blank character in screen line (differs from "^" when lines wrap) |
| [ ] | :1234: g\$     | to last character in screen line (differs from "\$" when lines wrap)           |
| [ ] | gm             | to middle of the screen line                                                   |

## Up-down motions

| Status             | Command   | Description                                                                               |
| ------------------ | --------- | ----------------------------------------------------------------------------------------- |
| [x] | :1234: k  | up N lines (also: CTRL-P and Up)                                                          |
| [x] | :1234: j  | down N lines (also: CTRL-J, CTRL-N, NL, and Down)                                         |
| [x] | :1234: G  | goto line N (default: last line), on the first non-blank character                        |
| [x] | :1234: gg | goto line N (default: first line), on the first non-blank character                       |
| [x] | :1234: %  | goto line N percentage down in the file; N must be given, otherwise it is the `%` command |
| [ ] | :1234: gk | up N screen lines (differs from "k" when line wraps)                                      |
| [ ] | :1234: gj | down N screen lines (differs from "j" when line wraps)                                    |
| [ ] | :1234: -  | up N lines, on the first non-blank character                                              |
| [ ] | :1234: +  | down N lines, on the first non-blank character (also: CTRL-M and CR)                      |
| [ ] | :1234: \_ | down N-1 lines, on the first non-blank character                                          |

## Text object motions

| Status             | Command    | Description                                                 |
| ------------------ | ---------- | ----------------------------------------------------------- |
| [x] | :1234: w   | N words forward                                             |
| [x] | :1234: W   | N blank-separated WORDs forward                             |
| [x] | :1234: e   | N words forward to the end of the Nth word                  |
| [x] | :1234: E   | N words forward to the end of the Nth blank-separated WORD  |
| [x] | :1234: b   | N words backward                                            |
| [x] | :1234: B   | N blank-separated WORDs backward                            |
| [ ] | :1234: ge  | N words backward to the end of the Nth word                 |
| [ ] | :1234: gE  | N words backward to the end of the Nth blank-separated WORD |
| [ ] | :1234: )   | N sentences forward                                         |
| [ ] | :1234: (   | N sentences backward                                        |
| [ ] | :1234: }   | N paragraphs forward                                        |
| [ ] | :1234: {   | N paragraphs backward                                       |
| [ ] | :1234: ]]  | N sections forward, at start of section                     |
| [ ] | :1234: [[  | N sections backward, at start of section                    |
| [ ] | :1234: ][  | N sections forward, at end of section                       |
| [ ] | :1234: []  | N sections backward, at end of section                      |
| [ ] | :1234: [(  | N times back to unclosed '('                                |
| [ ] | :1234: [{  | N times back to unclosed '{'                                |
| [ ] | :1234: ])  | N times forward to unclosed ')'                             |
| [ ] | :1234: ]}  | N times forward to unclosed '}'                             |

## Pattern searches

| Status                    | Command                            | Description                                            | Note                                                                            |
| ------------------------- | ---------------------------------- | ------------------------------------------------------ | ------------------------------------------------------------------------------- |
| [x] | :1234: `/{pattern}[/[offset]]<CR>` | search forward for the Nth occurrence of {pattern}     | Offset not supported. |
| [x] | :1234: `?{pattern}[?[offset]]<CR>` | search backward for the Nth occurrence of {pattern}    | As above. |
| [x] | :1234: `/<CR>`                     | repeat last search, in the forward direction           |
| [x] | :1234: `?<CR>`                     | repeat last search, in the backward direction          | 
| [x] | :1234: n                           | repeat last search                                     |
| [x] | :1234: N                           | repeat last search, in opposite direction              |
| [x] | :1234: \*                          | search forward for the identifier under the cursor     |
| [x] | :1234: #                           | search backward for the identifier under the cursor    |
| [ ] | :1234: g\*                         | like "\*", but also find partial matches               |
| [ ] | :1234: g#                          | like "#", but also find partial matches                |
| [ ] | gd                                 | goto local declaration of identifier under the cursor  |
| [ ] | gD                                 | goto global declaration of identifier under the cursor |

## Marks and motions

| Status             | Command                                                     | Description                                            |
| ------------------ | ----------------------------------------------------------- | ------------------------------------------------------ |
| [x] | m{a-zA-Z}                                                   | mark current position with mark {a-zA-Z}               |
| [x] | `{a-z} | go to mark {a-z} within current file               |
| [x] | `{A-Z} | go to mark {A-Z} in any file                       |
| [ ] | `{0-9} | go to the position where Vim was previously exited |
| [ ] | `` | go to the position before the last jump                |
| [ ] | `" | go to the position when last editing this file         |
| [ ] | `[ | go to the start of the previously operated or put text |
| [ ] | '[                                                          | go to the start of the previously operated or put text |
| [ ] | `] | go to the end of the previously operated or put text   |
| [ ] | ']                                                          | go to the end of the previously operated or put text   |
| [x] | `< | go to the start of the (previous) Visual area          |
| [x] | `> | go to the end of the (previous) Visual area            |
| [ ] | `. | go to the position of the last change in this file     |
| [ ] | '.                                                          | go to the position of the last change in this file     |
| [x] | '{a-zA-Z0-9[]'"<>.}                                         | same as `, but on the first non-blank in the line      |
| [ ] | :marks                                                      | print the active marks                                 |
| [ ] | :1234: CTRL-O                                               | go to Nth older position in jump list                  |
| [ ] | :1234: CTRL-I                                               | go to Nth newer position in jump list                  |
| [ ] | :ju[mps]                                                    | print the jump list                                    |

## Various motions

| Status             | Command             | Description                                                                                        |
| ------------------ | ------------------- | -------------------------------------------------------------------------------------------------- |
| [x] | %                   | find the next brace, bracket and go to its match |
| [ ] | :1234: H            | go to the Nth line in the window, on the first non-blank                                           |
| [ ] | M                   | go to the middle line in the window, on the first non-blank                                        |
| [ ] | :1234: L            | go to the Nth line from the bottom, on the first non-blank                                         |
| [ ] | :1234: go           | go to Nth byte in the buffer                                                                       |
| [ ] | :[range]go[to][off] | go to [off] byte in the buffer                                                                     |

## Scrolling

| Status             | Command       | Description                                    |
| ------------------ | ------------- | ---------------------------------------------- |
| [ ] | :1234: CTRL-E | window N lines downwards (default: 1)          |
| [ ] | :1234: CTRL-D | window N lines Downwards (default: 1/2 window) |
| [ ] | :1234: CTRL-F | window N pages Forwards (downwards)            |
| [ ] | :1234: CTRL-Y | window N lines upwards (default: 1)            |
| [ ] | :1234: CTRL-U | window N lines Upwards (default: 1/2 window)   |
| [ ] | :1234: CTRL-B | window N pages Backwards (upwards)             |
| [ ] | z CR or zt    | redraw, current line at top of window          |
| [ ] | z. or zz      | redraw, current line at center of window       |
| [ ] | z- or zb      | redraw, current line at bottom of window       |

These only work when 'wrap' is off:

| Status                    | Command   | Description                                   |
| ------------------------- | --------- | --------------------------------------------- | 
| [ ] | :1234: zh | scroll screen N characters to the right       |
| [ ] | :1234: zl | scroll screen N characters to the left        |
| [ ] | :1234: zH | scroll screen half a screenwidth to the right |
| [ ] | :1234: zL | scroll screen half a screenwidth to the left  |

## Inserting text

| Status             | Command   | Description                                                   |
| ------------------ | --------- | ------------------------------------------------------------- |
| [x] | :1234: a  | append text after the cursor (N times)                        |
| [x] | :1234: A  | append text at the end of the line (N times)                  |
| [x] | :1234: i  | insert text before the cursor (N times) (also: Insert)        |
| [x] | :1234: I  | insert text before the first non-blank in the line (N times)  |
| [ ] | :1234: gI | insert text in column 1 (N times)                             |
| [ ] | gi        | insert at the end of the last change                          |
| [x] | :1234: o  | open a new line below the current line, append text (N times) |
| [x] | :1234: O  | open a new line above the current line, append text (N times) |

in Visual block mode:

| Status             | Command | Description                                             |
| ------------------ | ------- | ------------------------------------------------------- |
| [x] | I       | insert the same text in front of all the selected lines |
| [x] | A       | append the same text after all the selected lines       |

## Insert mode keys

leaving Insert mode:

| Status             | Command          | Description                                 |
| ------------------ | ---------------- | ------------------------------------------- |
| [x] | Esc              | end Insert mode, back to Normal mode        |
| [ ] | CTRL-C           | like Esc, but do not use an abbreviation    |
| [ ] | CTRL-O {command} | execute {command} and return to Insert mode |

moving around:

| Status             | Command          | Description                             |
| ------------------ | ---------------- | --------------------------------------- |
| [x] | cursor keys      | move cursor left/right/up/down          |
| [ ] | shift-left/right | one word left/right                     |
| [ ] | shift-up/down    | one screenful backward/forward          |
| [ ] | End              | cursor after last character in the line |
| [ ] | Home             | cursor to first character in the line   |

## Special keys in Insert mode

| Status                    | Command                      | Description                                                        |
| ------------------------- | ---------------------------- | ------------------------------------------------------------------ |
| [ ] | CTRL-V {char}..              | insert character literally, or enter decimal byte value            |
| [ ] | NL or CR or CTRL-M or CTRL-J | begin new line                                                     |
| [ ] | CTRL-E                       | insert the character from below the cursor                         |
| [ ] | CTRL-Y                       | insert the character from above the cursor                         |
| [ ] | CTRL-A                       | insert previously inserted text                                    |
| [ ] | CTRL-@                       | insert previously inserted text and stop Insert mode               |
| [ ] | CTRL-R {0-9a-z%#:.-="}       | insert the contents of a register                                  |
| [ ] | CTRL-N                       | insert next match of identifier before the cursor                  |
| [ ] | CTRL-P                       | insert previous match of identifier before the cursor              |
| [ ] | CTRL-X ...                   | complete the word before the cursor in various ways                |
| [ ] | BS or CTRL-H                 | delete the character before the cursor                             |
| [ ] | Del                          | delete the character under the cursor                              |
| [ ] | CTRL-W                       | delete word before the cursor                                      |
| [ ] | CTRL-U                       | delete all entered characters in the current line                  |
| [ ] | CTRL-T                       | insert one shiftwidth of indent in front of the current line       |
| [ ] | CTRL-D                       | delete one shiftwidth of indent in front of the current line       |
| [ ] | 0 CTRL-D                     | delete all indent in the current line                              |
| [ ] | ^ CTRL-D                     | delete all indent in the current line, restore indent in next line |

## Special inserts

| Status    | Command       | Description                                              |
| --------- | ------------- | -------------------------------------------------------- |
| [ ] | :r [file]     | insert the contents of [file] below the cursor           |
| [ ] | :r! {command} | insert the standard output of {command} below the cursor |

## Deleting text

| Status             | Command          | Description                                        |
| ------------------ | ---------------- | -------------------------------------------------- |
| [x] | :1234: x         | delete N characters under and after the cursor     |
| [ ] | :1234: Del       | delete N characters under and after the cursor     |
| [x] | :1234: X         | delete N characters before the cursor              |
| [x] | :1234: d{motion} | delete the text that is moved over with {motion}   |
| [x] | {visual}d        | delete the highlighted text                        |
| [x] | :1234: dd        | delete N lines                                     |
| [ ] | :1234: D         | delete to the end of the line (and N-1 more lines) |
| [ ] | :1234: J         | join N-1 lines (delete EOLs)                       |
| [ ] | {visual}J        | join the highlighted lines                         |
| [ ] | :1234: gJ        | like "J", but without inserting spaces             |
| [ ] | {visual}gJ       | like "{visual}J", but without inserting spaces     |
| [ ] | :[range]d [x]    | delete [range] lines [into register x]             |

## Copying and moving text

| Status             | Command          | Description                                            |
| ------------------ | ---------------- | ------------------------------------------------------ |
| [ ] | "{char}          | use register {char} for the next delete, yank, or put  |
| [ ] | "\*              | use register `*` to access system clipboard            |
| [ ] | :reg             | show the contents of all registers                     |
| [ ] | :reg {arg}       | show the contents of registers mentioned in {arg}      |
| [x] | :1234: y{motion} | yank the text moved over with {motion} into a register |
| [x] | {visual}y        | yank the highlighted text into a register              |
| [x] | :1234: yy        | yank N lines into a register                           |
| [ ] | :1234: Y         | yank N lines into a register                           |
| [x] | :1234: p         | put a register after the cursor position (N times)     |
| [x] | :1234: P         | put a register before the cursor position (N times)    |
| [ ] | :1234: ]p        | like p, but adjust indent to current line              |
| [ ] | :1234: [p        | like P, but adjust indent to current line              |
| [ ] | :1234: gp        | like p, but leave cursor after the new text            |
| [ ] | :1234: gP        | like P, but leave cursor after the new text            |

## Changing text

| Status                    | Command         | Description |
| ------------------------- | --------------- | ----------------------------------------------------------------------------|
| [x] | :1234: r{char}  | replace N characters with {char}                                                                  |
| [ ] | :1234: gr{char} | replace N characters without affecting layout                                                     |
| [x] | :1234: R        | enter Replace mode (repeat the entered text N times)                                              |
| [ ] | :1234: gR       | enter virtual Replace mode: Like Replace mode but without affecting layout                        |
| [x] | {visual}r{char} | in Visual block, visual, or visual line modes: Replace each char of the selected text with {char} |

(change = delete text and enter Insert mode)

| Status             | Command                 | Description                                                                                     |
| ------------------ | ----------------------- | ----------------------------------------------------------------------------------------------- |
| [x] | :1234: c{motion}        | change the text that is moved over with {motion}                                                |
| [x] | {visual}c               | change the highlighted text                                                                     |
| [x] | :1234: cc               | change N lines                                                                                  |
| [x] | :1234: S                | change N lines                                                                                  |
| [x] | :1234: C                | change to the end of the line (and N-1 more lines)                                              |
| [x] | :1234: s                | change N characters                                                                             |
| [x] | {visual}c               | in Visual block mode: Change each of the selected lines with the entered text                   |
| [ ] | {visual}C               | in Visual block mode: Change each of the selected lines until end-of-line with the entered text |
| [x] | {visual}~               | switch case for highlighted text                                                                |
| [x] | {visual}u               | make highlighted text lowercase                                                                 |
| [x] | {visual}U               | make highlighted text uppercase                                                                 |
| [x] | g~{motion}              | switch case for the text that is moved over with {motion}                                       |
| [x] | gu{motion}              | make the text that is moved over with {motion} lowercase                                        |
| [x] | gU{motion}              | make the text that is moved over with {motion} uppercase                                        |
| [ ] | {visual}g?              | perform rot13 encoding on highlighted text                                                      |
| [ ] | g?{motion}              | perform rot13 encoding on the text that is moved over with {motion}                             |
| [ ] | :1234: CTRL-A           | add N to the number at or after the cursor                                                      |
| [ ] | :1234: CTRL-X           | subtract N from the number at or after the cursor                                               |
| [x] | :1234: <{motion}        | move the lines that are moved over with {motion} one shiftwidth left                            |
| [x] | :1234: <<               | move N lines one shiftwidth left                                                                |
| [x] | :1234: >{motion}        | move the lines that are moved over with {motion} one shiftwidth right                           |
| [x] | :1234: >>               | move N lines one shiftwidth right                                                               |
| [ ] | :1234: gq{motion}       | format the lines that are moved over with {motion} to 'textwidth' length                        |
| [ ]       | :[range]ce[nter][width] | center the lines in [range]                                                                     |
| [ ]       | :[range]le[ft][indent]  | left-align the lines in [range] (with [indent])                                                 |
| [ ]       | :[ranee]ri[ght][width]  | right-align the lines in [range]                                                                |

## Complex changes

| Status                                 | Command                                        | Description |
| -------------------------------------- | ---------------------------------------------- | ----------- |
| [ ] | :1234: `!{motion}{command}<CR>`  | filter the lines that are moved over through {command} |
| [ ] | :1234: `!!{command}<CR>`         | filter N lines through {command}                       |
| [ ] | `{visual}!{command}<CR>`         | filter the highlighted lines through {command}         |
| [ ] | `:[range]! {command}<CR>`        | filter [range] lines through {command}                 |
| [ ] | :1234: ={motion}                 | filter the lines that are moved over through 'equalprg' |
| [x] | :1234: ==                        | filter N lines through 'equalprg'                      |
| [x] | {visual}=                        | filter the highlighted lines through 'equalprg'        |
| [x] | :[range]s[ubstitute]/{pattern}/{string}/[g][c] | substitute {pattern} by {string} in [range] lines; with [g], replace all occurrences of {pattern}; with [c], confirm each replacement |
| [ ] | :[range]s[ubstitute][g][c]            | repeat previous ":s" with new range and options |
| [ ] | &                                     | Repeat previous ":s" on current line without options |
| [ ] | :[range]ret[ab][!] [tabstop]          | set 'tabstop' to new value and adjust white space accordingly   |

## Visual mode

| Status             | Command | Description                                         |
| ------------------ | ------- | --------------------------------------------------- |
| [x] | v       | start highlighting characters or stop highlighting  |
| [x] | V       | start highlighting linewise or stop highlighting    |
| [x] | CTRL-V  | start highlighting blockwise or stop highlighting   |
| [x] | o       | exchange cursor position with start of highlighting |
| [ ] | gv      | start highlighting on previous visual area          |

## Text objects (only in Visual mode or after an operator)

| Status             | Command                                           | Description                                                 |
| ------------------ | ------------------------------------------------- | ----------------------------------------------------------- |
| [x] | :1234: aw                                         | Select "a word"                                             |
| [x] | :1234: iw                                         | Select "inner word"                                         |
| [x] | :1234: aW                                         | Select "a WORD"                                             |
| [x] | :1234: iW                                         | Select "inner WORD"                                         |
| [ ] | :1234: as                                         | Select "a sentence"                                         |
| [ ] | :1234: is                                         | Select "inner sentence"                                     |
| [ ] | :1234: ap                                         | Select "a paragraph"                                        |
| [ ] | :1234: ip                                         | Select "inner paragraph"                                    |
| [x] | :1234: a], a[                                     | select '[' ']' blocks                                       |
| [x] | :1234: i], i[                                     | select inner '[' ']' blocks                                 |
| [x] | :1234: ab, a(, a)                                 | Select "a block" (from "[(" to "])")                        |
| [x] | :1234: ib, i), i(                                 | Select "inner block" (from "[(" to "])")                    |
| [x] | :1234: a>, a<                                     | Select "a &lt;&gt; block"                                   |
| [x] | :1234: i>, i<                                     | Select "inner <> block"                                     |
| [x] | :1234: aB, a{, a}                                 | Select "a Block" (from "[{" to "]}")                        |
| [x] | :1234: iB, i{, i}                                 | Select "inner Block" (from "[{" to "]}")                    |
| [ ] | :1234: at                                         | Select "a tag block" (from &lt;aaa&gt; to &lt;/aaa&gt;)     |
| [ ] | :1234: it                                         | Select "inner tag block" (from &lt;aaa&gt; to &lt;/aaa&gt;) |
| [ ] | :1234: a'                                         | Select "a single quoted string"                             |
| [ ] | :1234: i'                                         | Select "inner single quoted string"                         |
| [ ] | :1234: a"                                         | Select "a double quoted string"                             |
| [ ] | :1234: i"                                         | Select "inner double quoted string"                         |
| [ ] | :1234: a` | Select "a backward quoted string"     |
| [ ] | :1234: i` | Select "inner backward quoted string" |

## Repeating commands

| Status                    | Command                           | Description    |
| ------------------------- | --------------------------------- | -------------- |
| [x] | :1234: . | repeat last change (with count replaced with N)                                                    | Content changes that don't happen under cursor can not be repeated. |
| [x] | q{a-z}                            | record typed characters into register {a-z}                                                        |
| [ ] | q{A-Z}                            | record typed characters, appended to register {a-z}                                                |
| [x] | q                                 | stop recording                                                                                     |
| [x] | :1234: @{a-z}                     | execute the contents of register {a-z} (N times)                                                   |
| [x] | :1234: @@                         | repeat previous @{a-z} (N times)                                                                   |
| [ ] | :@{a-z}                           | execute the contents of register {a-z} as an Ex command                                            |
| [ ] | :@@                               | repeat previous :@{a-z}                                                                            |
| [ ] | :[range]g[lobal]/{pattern}/[cmd]  | execute Ex command [cmd](default: ':p') on the lines within [range] where {pattern} matches        |
| [ ] | :[range]g[lobal]!/{pattern}/[cmd] | execute Ex command [cmd](default: ':p') on the lines within [range] where {pattern} does NOT match |
| [ ] | :so[urce] {file}                  | read Ex commands from {file}                                                                       |
| [ ] | :so[urce]! {file}                 | read Vim commands from {file}                                                                      |
| [ ] | :sl[eep][sec]                     | don't do anything for [sec] seconds                                                                |
| [ ] | :1234: gs                         | goto Sleep for N seconds                                                                           |

## Undo/Redo commands

| Status             | Command       | Description                | Note                                                       |
| ------------------ | ------------- | -------------------------- | ---------------------------------------------------------- |
| [x] | :1234: u      | undo last N changes        | Current implementation may not cover every case perfectly. |
| [x] | :1234: CTRL-R | redo last N undone changes | As above.                                                  |
| [ ] | U             | restore last changed line  |

## Ex ranges

| Status                    | Command       | Description                                                                  |
| ------------------------- | ------------- | ---------------------------------------------------------------------------- |
| [ ] | ,             | separates two line numbers                                                   |
| [ ] | ;             | idem, set cursor to the first line number before interpreting the second one |
| [ ] | {number}      | an absolute line number                                                      |
| [ ] | .             | the current line                                                             |
| [ ] | \$            | the last line in the file                                                    |
| [ ] | %             | equal to 1,\$ (the entire file)                                              |
| [ ] | \*            | equal to '<,'> (visual area)                                                 |
| [ ] | 't            | position of mark t                                                           |
| [ ] | /{pattern}[/] | the next line where {pattern} matches                                        |
| [ ] | ?{pattern}[?] | the previous line where {pattern} matches                                    |
| [ ] | +[num]        | add [num] to the preceding line number (default: 1)                          |
| [ ] | -[num]        | subtract [num] from the preceding line number (default: 1)                   |

## Editing a file

| Status                    | Command        | Description  | 
| ------------------------- | -------------- | ------------ | 
| [ ] | :e[dit] {file} | Edit {file}. |

## Multi-window commands

| Status                    | Command           | Description                                                             |
| ------------------------- | ----------------- | ----------------------------------------------------------------------- |
| [ ] | :e[dit] {file}    | Edit {file}.                                                            |
| [ ] | &lt;ctrl-w&gt; hl | Switching between windows.                                              |
| [ ] | :sp {file}        | Split current window in two.                                            |              
| [ ] | :vsp {file}       | Split vertically current window in two.                                 |                    
| [ ] | &lt;ctrl-w&gt; s  | Split current window in two.                                            |            
| [ ] | &lt;ctrl-w&gt; v  | Split vertically current window in two.                                 |                  
| [ ] | &lt;ctrl-w&gt; o  | Close other editor groups.                                              |                 
| [ ] | :new              | Create a new window horizontally and start editing an empty file in it. |         
| [ ] | :vne[w]           | Create a new window vertically and start editing an empty file in it.   |               
