Unit nCrt;
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

Uses
{$ifdef Unix}
  {$ifdef ver1_0}
    linux,
  {$else}
    unix,
  {$endif}
{$endif}
  ncurses;

{$i ncrt.inc}

Begin
   { initialize ncurses }
   If StartCurses(ActiveWn) Then
      { defaults, crtassign, etc. }
      nInit
   Else
      CursesFailed;
End. { of Unit nCrt }
{
  $Log$
  Revision 1.3  2002-09-07 15:43:01  peter
    * old logs removed and tabs fixed

  Revision 1.2  2002/05/31 11:54:33  marco
  * Renamefest for 1.0, many 1.1.x spots patched also.

  Revision 1.1  2002/01/29 17:55:17  peter
    * splitted to base and extra

}
