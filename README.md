editor
======

Vim style editor written in Racket

Motions

* `h`: Move left
* `j`: Move down
* `k`: Move up
* `l`: Move right
* `0`: Move to the start of the line (before leading whitespace)
* `^`: Move to the start of the line (after leading whitespace)
* `$`: Move to the end of the line
* `w`: Move to the next word
* `W`: Move to the next WORD
* `e`: Move to the next word ending
* `E`: Move to the next WORD ending
* `b`: Move back to the previous word
* `B`: Move back to the previous WORD
* `gg`: Move to the first line of the file (or a specific line, if a count is given)
* `G`: Move to the last line of the file (or a specific line, if a count is given)
* [todo] `+` or Return: Move to the start of the next line (after leading whitespace)
* [todo] `-`: Move to the start of the previous line (after leading whitespace)

Document search

* `/`: Search forwards
* `?`: Search backwards
* `n`: Go to the next match for the latest search
* `N`: Go to the previous match for the latest search

Marks

* `m`: Create a mark
* `` ` ``: Move to a given mark
* `'`: Move to the line containing a given mark

Insertion

* `a`: Append text after the cursor
* `A`: Append text at the end of the line
* `i`: Insert text before the cursor
* `I`: Insert text at the start of the line (after leading whitespace)
* [todo] `gI`: Insert text at the start of the line (before leading whitespace)
* `o`: Open the next line
* `O`: Open the previous line
* `s`: Substitute characters under the cursor
* `S`: Substitute to the end of the line

Operators

* `c`, `cc` and `C`: Change text
* `d`, `dd` and `D`: Delete text
* Operators work with all of the motions and document search commands listed above, and the following text objects:

* `i` or `a` followed by `b`, `(` or `)`: Inside or around parenthesis
* `i` or `a` followed by `B`, `{` or `}`: Inside or around braces
* `i` or `a` followed by `[` or `]`: Inside or around square brackets

Line search

* `f`: Find the next occurrence of a character
* `F`: Find the previous occurrence of a character
* `t`: Find the character before the next occurrence of a character
* `T`: Find the character after the previous ocurrence of a character
* `;`: Repeat the last line search
* `,`: Repeat the last line search, reversing the direction

Edits

* `r`: Replace the character under the cursor
* `x`: Delete the character under the cursor
* `X`: Delete the character before the cursor
* `.`: Repeat the last edit or operator

Yank and put

* `yy` or `Y`: Yank the current line
* `p`: Put the most recently yanked line after the current line
* `P`: Put the most recently yanked line before the current line
