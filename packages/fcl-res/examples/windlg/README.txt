Windows Dialog Resource Demo
============================

This test shows how to use fpcres for compiling DIALOG and DIALOGEX
resources into binary format that Windows can display.

You need to compile fpcres with DIALOG/DIALOGEX support for this test to work.

Then:
1. Compile the .rc file into a .res file using fpcres:

   fpcres -of res -o testdlg.res testdlg.rc

   (fpcres must be in your path)

2. Compile the test program:

   fpc windlgtest.lpr

   The compiler will automatically link in testdlg.res because of the {$R testdlg.res} directive in the source.

2. Run windlgtest.exe

   If all went well, it will show two dialogs in sequence:

   - Dialog 100: A DIALOG with labels, text fields, checkbox, groupbox
                 and OK/Cancel buttons
   - Dialog 200: A DIALOGEX with centered text and a Close button

   Each dialog closes when you press OK/Cancel/Close.

Style values used:

The STYLE value 0x10C80080 is:
  WS_VISIBLE     (0x10000000)
  WS_CAPTION     (0x00C00000)  = WS_BORDER | WS_DLGFRAME
  WS_SYSMENU     (0x00080000)
  DS_SETFONT     (0x00000040)
  DS_MODALFRAME  (0x00000080)

EDITTEXT 105 style 0x500100A0 is:
  WS_CHILD       (0x40000000)
  WS_VISIBLE     (0x10000000)
  WS_TABSTOP     (0x00010000)
  ES_PASSWORD    (0x00000020)
  ES_AUTOHSCROLL (0x00000080)
