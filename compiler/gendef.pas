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
    importlist   : tstringlist;
  end;

var
  deffile : tdeffile;


implementation

uses
  systems,cutils,globtype,globals;

{******************************************************************************
                               TDefFile
******************************************************************************}

constructor tdeffile.create(const fn:string);
begin
  fname:=fn;
  WrittenOnDisk:=false;
  is_empty:=true;
  importlist:=TStringList.Create;
  exportlist:=TStringList.Create;
end;


destructor tdeffile.destroy;
begin
  if WrittenOnDisk and
     not(cs_link_extern in aktglobalswitches) then
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
  target_i386_win32, target_i386_wdosx :
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
{
  $Log$
  Revision 1.9  2002-05-16 19:46:36  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.7  2002/04/04 18:36:46  carl
  + added wdosx support (patch from Pavel)

  Revision 1.6  2001/04/13 01:22:07  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.5  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.4  2000/09/24 15:06:16  peter
    * use defines.inc

  Revision 1.3  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:41  michael
  + removed logs

}
