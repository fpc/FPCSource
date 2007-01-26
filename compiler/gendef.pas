{
    Copyright (c) 1998-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface
uses
  cclasses;

type
  tdeffile=class
    fname : string;
    constructor create(const fn:string);
    destructor  destroy;override;
    procedure addexport(const s:string);
    procedure addimport(const s:string);
    procedure writefile;
    function empty : boolean;
  private
    is_empty : boolean;
    WrittenOnDisk : boolean;
    exportlist,
    importlist   : TCmdStrList;
  end;

var
  deffile : tdeffile;


implementation

uses
  SysUtils,
  systems,cutils,globtype,globals;

{******************************************************************************
                               TDefFile
******************************************************************************}

constructor tdeffile.create(const fn:string);
begin
  fname:=fn;
  WrittenOnDisk:=false;
  is_empty:=true;
  importlist:=TCmdStrList.Create;
  exportlist:=TCmdStrList.Create;
end;


destructor tdeffile.destroy;
begin
  if WrittenOnDisk and
     not(cs_link_nolink in current_settings.globalswitches) then
    DeleteFile(FName);
  importlist.Free;
  exportlist.Free;
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
  empty:=is_empty or DescriptionSetExplicity;
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
  case target_info.system of
    system_i386_Os2, system_i386_emx:
      begin
        write(t,'NAME '+ChangeFileExt(inputfilename,''));
        if usewindowapi then
          write(t,' WINDOWAPI');
        writeln(t,'');
        writeln(t,'PROTMODE');
        writeln(t,'DESCRIPTION '+''''+description+'''');
        writeln(t,'DATA'#9'MULTIPLE');
        writeln(t,'STACKSIZE'#9+tostr(stacksize));
        writeln(t,'HEAPSIZE'#9+tostr(heapsize));
      end;
  system_i386_win32, system_i386_wdosx :
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
      writeln(t,#9+importlist.getfirst);
   end;

{write exports}
  if not exportlist.empty then
   begin
     writeln(t,'');
     writeln(t,'EXPORTS');
     while not exportlist.empty do
      writeln(t,#9+exportlist.getfirst);
   end;

  close(t);
  WrittenOnDisk:=true;
end;

end.
