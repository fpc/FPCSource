{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Printer unit for BP7 compatible RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit printer;
interface

var
  lst : text;

implementation

var
  old_exit : pointer;

procedure printer_exit;
begin
  close(lst);
  exitproc:=old_exit;
end;


begin
  assign(lst,'PRN');
  rewrite(lst);
  old_exit:=exitproc;
  exitproc:=@printer_exit;
end.
{
  $Log$
  Revision 1.3  2000-01-07 16:41:50  daniel
    * copyright 2000

  Revision 1.2  2000/01/07 16:32:33  daniel
    * copyright 2000 added

  Revision 1.1  1999/05/18 13:19:03  hajny
    + directly copied from GO32v2, should work w/o changes

  Revision 1.1  1998/12/21 13:07:03  peter
    * use -FE

  Revision 1.2  1998/05/22 00:39:26  peter
    * go32v1, go32v2 recompiles with the new objects
    * remake3 works again with go32v2
    - removed some "optimizes" from daniel which were wrong

}
