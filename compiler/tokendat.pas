{
    $Id$
    Copyright (c) 1998-2000 by Daniel Mantione, Peter Vreman
    Members of the Free Pascal development team

    This little program generates a file of tokendata

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
program tokendat;

{$ifdef FPC}
  {$FATAL Use tp 7 to compile, FPC can't be used because the records are written different.}
{$else}
  {$ifndef TP}
    -- You need to define -dTP and -dI386
  {$endif}
  {$ifndef I386}
    -- You need to define -dTP and -dI386
  {$endif}
{$endif}

uses    tokens;

{Header is designed both to identify the file and to display a nice
 message when you use the type command on it.

Explanation:

#8      String length is also displayed. A backspace erases it.
#13#10  Needed to display dos prompt on next line.
#26     End of file. Causes type to stop reading the file.
}

const
  headerstr:string[length(tokheader)]=tokheader;
var
  f:file;
  a:longint;
begin
    new(tokenidx);
    create_tokenidx;
    assign(f,'tokens.dat');
    rewrite(f,1);
    {Write header...}
    blockwrite(f,headerstr,sizeof(headerstr));
    {Write size of tokeninfo.}
    a:=sizeof(arraytokeninfo);
    blockwrite(f,a,sizeof(a));
    {Write tokeninfo.}
    blockwrite(f,arraytokeninfo,sizeof(arraytokeninfo));
    {Write tokenindex.}
    blockwrite(f,tokenidx^,sizeof(tokenidx^));
    close(f);
    dispose(tokenidx);
end.
{
  $Log$
  Revision 1.1  2000-07-13 06:30:02  michael
  + Initial import

  Revision 1.5  2000/01/07 01:14:47  peter
    * updated copyright to 2000

  Revision 1.4  1999/09/08 16:02:03  peter
    * tokendat compiles for tp
    * tokens.dat supplied by default

}

