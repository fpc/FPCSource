{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
    fname : string;
    constructor init(const fn:string);
    destructor  done;
    procedure addexport(const s:string);
    procedure addimport(const s:string);
    procedure writefile;
    function empty : boolean;
  private
    is_empty : boolean;
    WrittenOnDisk : boolean;
    exportlist,
    importlist   : tstringcontainer;
  end;
var
  deffile : tdeffile;


implementation

uses
  systems,cutils,globtype,globals;

{******************************************************************************
                               TDefFile
******************************************************************************}

constructor tdeffile.init(const fn:string);
begin
  fname:=fn;
  WrittenOnDisk:=false;
  is_empty:=true;
  importlist.init;
  exportlist.init;
end;


destructor tdeffile.done;
var
  f : file;
begin
  if WrittenOnDisk and
     not(cs_link_extern in aktglobalswitches) then
   begin
     assign(f,fname);
     {$I-}
      erase(f);
     {$I+}
     if ioresult<>0 then;
   end;
  importlist.done;
  exportlist.done;
end;



procedure tdeffile.addexport(const s:string);
begin
  exportlist.insert(s);
  is_empty:=false;
end;


procedure tdeffile.addimport(const s:string);
begin
  importlist.insert(s);
  is_empty:=false;
end;

function tdeffile.empty : boolean;
begin
  empty:=is_empty and (description='');
end;



procedure tdeffile.writefile;
var
  t : text;
begin
  If WrittenOnDisk then
    Exit;
{ open file }
  assign(t,fname);
  {$I+}
   rewrite(t);
  {$I-}
  if ioresult<>0 then
   exit;
{$ifdef i386}
  case target_info.target of
    target_i386_Os2 :
      begin
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
  target_i386_win32 :
    begin
      if description<>'' then
        writeln(t,'DESCRIPTION '+''''+description+'''');
      if dllversion<>'' then
        writeln(t,'VERSION '+dllversion);
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
  WrittenOnDisk:=true;
end;

end.
{
  $Log$
  Revision 1.3  2000-08-27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:41  michael
  + removed logs

}
