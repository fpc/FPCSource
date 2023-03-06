{$IFNDEF FPC_DOTTEDUNITS}
Unit nCrt;
{$ENDIF FPC_DOTTEDUNITS}
{---------------------------------------------------------------------------
                                 CncWare
                         (c) Copyright 1999-2000
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
                        | See ocrt.pp & ncrt.inc for a complete revision
                        | history.
------------------------------------------------------------------------------
}
Interface

{$IFDEF FPC_DOTTEDUNITS}
Uses
{$ifdef Unix}
    UnixApi.Base,
    UnixApi.TermIO,
{$endif}
  Api.Ncurses,
  TP.DOS;  {TP.DOS needed for TextRec}
{$ELSE FPC_DOTTEDUNITS}
Uses
{$ifdef Unix}
    baseunix,
    termio,
{$endif}
  ncurses,
  dos;  {dos needed for TextRec}
{$ENDIF FPC_DOTTEDUNITS}

{$i ncrt.inc}

Begin
   { initialize ncurses }
   If StartCurses(ActiveWn) Then
      { defaults, crtassign, etc. }
      nInit
   Else
      CursesFailed;
End. { of Unit nCrt }
