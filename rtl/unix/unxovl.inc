{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort

    Some generic overloads for stringfunctions in the unix unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function StatFS(Path:Pathstr;Var Info:Tstatfs):cint;

{
  Get all information on a fileSystem, and return it in Info.
  Path is the name of a file/directory on the fileSystem you wish to
  investigate.
}
 
begin
  path:=path+#0;
  statfs:=statfs(pchar(@path[1]),info);
end;

Function fpReadLink(Name:pathstr):pathstr;
{
  Read a link (where it points to)
}
var
  LinkName : pathstr;
  i : cint;
begin
  Name:=Name+#0;
  i:=fpReadLink(@Name[1],@LinkName[1],high(linkname));
  if i>0 then
   begin
     linkname[0]:=chr(i);
     fpReadLink:=LinkName;
   end
  else
   fpReadLink:='';
end;

{
 $Log$
 Revision 1.2  2004-01-01 16:10:23  marco
  * fpreadlink(pathstr) moved to unxovl (since not platform specific),
 	small fixes for "make all OPT='-dFPC_USE_LIBC'

 Revision 1.1  2003/12/30 12:27:44  marco
  * FPC_USE_LIBC



}

