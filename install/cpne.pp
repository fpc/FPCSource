{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Pierre Muller,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program copy_if_not_empty;

uses dos;

var in,out : file;

begin
if paramcount>0 then
  begin
     assign(in,paramstr(1));
     if filerec(in).size=0 then
       begin
          erase(in);
          exit;
       end;
  end;
if paramcount>1 then
     rename(paramstr(1),paramstr(2));
end.

{
  $Log$
  Revision 1.1  1998-05-12 10:55:23  peter
    * moved cpne.pp to install/

}
