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
  Revision 1.2  2002-05-31 11:54:33  marco
  * Renamefest for 1.0, many 1.1.x spots patched also.

  Revision 1.1  2002/01/29 17:55:17  peter
    * splitted to base and extra

  Revision 1.6  2001/04/19 12:40:56  marco
   * Fixed freebsd thingy

  Revision 1.5  2001/04/08 12:27:55  peter
    * made it compilable with both 1.0.x and 1.1

  Revision 1.4  2001/01/21 21:38:52  marco
   * renamefest in packages

  Revision 1.3  2000/08/29 05:51:09  michael
  + Merged changes and additions from fixbranch

  Revision 1.2  2000/07/13 11:33:27  michael
  + removed logs

}
