Unit nCrt;
{---------------------------------------------------------------------------
                                 CncWare
                           (c) Copyright 1999
                   Portions copyright the FreePascal Team
 ---------------------------------------------------------------------------
  Filename..: ncrt.pp
  Programmer: Ken J. Wright, ken@cncware.com
  Date......: 03/01/99

  Purpose - A crt replacement using ncurses.

-------------------------------<< REVISIONS >>--------------------------------
  Ver  |   Date   | Prog| Description
-------+----------+-----+-----------------------------------------------------
  1.00 | 03/01/99 | kjw | Initial Release.
------------------------------------------------------------------------------
  2.00 | 12/13/99 | kjw | nCrt is now a drop-in replacement for the standard
                        | FPC crt unit. All the previous OOP features have
                        | been moved to a new unit, oCrt (object crt).
                        | See oCrt.pp for a complete revision history.
  2.02 | 12/15/99 | kjw | See ncrt.inc.
  2.03 | 12/16/99 | kjw | See ncrt.inc
  2.04 | 01/04/00 | kjw | See ncrt.inc
  2.05 | 01/06/00 | kjw | See ncrt.inc, ocrt.pp
  2.06 | 01/11/00 | kjw | See ncrt.inc.
  2.07 | 01/31/00 | kjw | See ncrt.inc, ocrt.pp
------------------------------------------------------------------------------
}
Interface

Uses linux,ncurses;

{$i ncrt.inc}

Begin
   { load the color pairs array with color pair indices (0..63) }
   For bg := 0 to 7 Do For fg := 0 to 7 do cp[bg,fg] := (bg*8)+fg;
   { initialize ncurses }
   If Not StartCurses(ActiveWn) Then
      Halt;

   SubWn := nil;
   TextMode(LastMode);

   { Redirect the standard output }
   assigncrt(Output);
   Rewrite(Output);
   TextRec(Output).Handle:=StdOutputHandle;
   { Redirect the standard input }
   assigncrt(Input);
   Reset(Input);
   TextRec(Input).Handle:=StdInputHandle;

   { set the unit exit procedure }
   ExitSave := ExitProc;
   ExitProc := @nExit;

End. { of Unit nCrt }
