{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

    Generation of a .def file for needed for Os2/Win32

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
unit gendef;
interface
uses cobjects;

type
  pdeffile=^tdeffile;
  tdeffile=object
    fname       : string;
    exportlist,
    importlist  : tstringcontainer;
    constructor init(const fn:string);
    destructor  done;
    procedure addexport(const s:string);
    procedure addimport(const s:string);
    procedure writefile;
  end;
var
  deffile : tdeffile;


implementation

uses
  systems,globals;

{******************************************************************************
                               TDefFile
******************************************************************************}

constructor tdeffile.init(const fn:string);
begin
  fname:=fn;
  importlist.init;
  exportlist.init;
end;


destructor tdeffile.done;
begin
  importlist.done;
  exportlist.done;
end;



procedure tdeffile.addexport(const s:string);
begin
  exportlist.insert(s);
end;


procedure tdeffile.addimport(const s:string);
begin
  importlist.insert(s);
end;


procedure tdeffile.writefile;
var
  t : text;
begin
  assign(t,fname);
  {$I+}
   rewrite(t);
  {$I-}
  if ioresult<>0 then
   exit;
{$ifdef i386}

  case target_info.target of
   target_Os2 : begin

                  write(t,'NAME '+inputfile);
                  if usewindowapi then
                   write(t,' WINDOWAPI');
                  writeln(t,'');

                  writeln(t,'PROTMODE');
                  writeln(t,'DESCRIPTION '+''''+description+'''');
                  writeln(t,'DATA'#9'MULTIPLE');
                  writeln(t,'STACKSIZE'#9+tostr(stacksize));
                  writeln(t,'HEAPSIZE'#9+tostr(heapsize));
                end;

  end;
{$endif}

{write imports}

  if not importlist.empty then
   begin
     writeln(t,'');

     writeln(t,'IMPORTS');
     while not importlist.empty do
      writeln(t,#9+importlist.get);
   end;

{write exports}

  if not exportlist.empty then
   begin
     writeln(t,'');

     writeln(t,'EXPORTS');
     while not exportlist.empty do
      writeln(t,#9+exportlist.get);
   end;

  close(t);
end;

end.
{
  $Log$
  Revision 1.1  1998-06-04 23:51:39  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

}
  