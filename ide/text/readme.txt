  This is still a beta version of the IDE

This file is just a log of important changes
starting 1999/10/29


2000/01/28:
   + Partial Syntax released:
     this allows to open highlighted files faster.
     The highlighting is only computed up to the current editor position
     and is continued in the Idle loop as a background process
     (it not a real separate process).

2000/01/10:
   + working register window

1999/11/10:
   + Grouped action started for Undo.
     Undo of Copy/Cut/Paste or Clear should work.

1999/10/29:
  Undo/Redo stuff added to normal compilation
  this is still buggy !!!
  Any use of Copy/Cut/Paste or Clear will generate wrong Undo
