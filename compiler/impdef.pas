{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit finds the export defs from PE files

    C source code of DEWIN Windows disassembler (written by A. Milukov) was
    partially used

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
unit impdef;

{$i defines.inc}

interface

function makedef(const binname,textname:string):longbool;

implementation
var
  f:file;
  t:text;
  TheWord:array[0..1]of char;
  PEoffset:cardinal;
  loaded:longint;
  FileCreated:longbool;

function DOSstubOK(var x:cardinal):longbool;
begin
  blockread(f,TheWord,2,loaded);
  if loaded<>2 then
   DOSstubOK:=false
  else
   begin
    DOSstubOK:=TheWord='MZ';
    seek(f,$3C);
    blockread(f,x,4,loaded);
    if(loaded<>4)or(x>filesize(f))then
     DOSstubOK:=false;
   end;
end;

function isPE(x:cardinal):longbool;
begin
  seek(f,x);
  blockread(f,TheWord,2,loaded);
  isPE:=(loaded=2)and(TheWord='PE');
end;

var
  cstring:array[0..127]of char;

function GetEdata(PE:cardinal):longbool;
type
  TObjInfo=packed record
   ObjName:array[0..7]of char;
   VirtSize,
   VirtAddr,
   RawSize,
   RawOffset,
   Reloc,
   LineNum:cardinal;
   RelCount,
   LineCount:word;
   flags:cardinal;
  end;
var
  i:cardinal;
  ObjOfs:cardinal;
  Obj:TObjInfo;
  APE_obj,APE_Optsize:word;
  ExportRVA:cardinal;
  delta:cardinal;

procedure ProcessEdata;
  var
   j:cardinal;
   ulongval:cardinal;
   ExpDir:packed record
    flag,
    stamp:cardinal;
    Major,
    Minor:word;
    Name,
    Base,
    NumFuncs,
    NumNames,
    AddrFuncs,
    AddrNames,
    AddrOrds:cardinal;
   end;
  begin
   with Obj do
    begin
     seek(f,RawOffset+delta);
     blockread(f,ExpDir,sizeof(ExpDir));
     seek(f,RawOffset-VirtAddr+ExpDir.Name);
     blockread(f,cstring,sizeof(cstring));
     for j:=0 to pred(ExpDir.NumNames)do
      begin
       seek(f,RawOffset-VirtAddr+ExpDir.AddrNames+j*4);
       blockread(f,ulongval,4);
       seek(f,RawOffset-VirtAddr+ulongval);
       blockread(f,cstring,sizeof(cstring));
       if not FileCreated then
        begin
         FileCreated:=true;
         rewrite(t);
         writeln(t,'EXPORTS');
        end;
       { do not use the implicit '_' }
       writeln(t,cstring,'=',cstring);
      end;
   end;
  end;
begin
  GetEdata:=false;
  FileCreated:=false;
  seek(f,PE+120);
  blockread(f,ExportRVA,4);
  seek(f,PE+6);
  blockread(f,APE_Obj,2);
  seek(f,PE+20);
  blockread(f,APE_OptSize,2);
  ObjOfs:=APE_OptSize+PEoffset+24;
  for i:=1 to APE_obj do
   begin
    seek(f,ObjOfs);
    blockread(f,Obj,sizeof(Obj));
    inc(ObjOfs,sizeof(Obj));
    with Obj do
     if(VirtAddr<=ExportRva)and(ExportRva<VirtAddr+VirtSize)then
      begin
       delta:=ExportRva-VirtAddr;
       ProcessEdata;
       GetEdata:=true;
      end;
   end;
end;


function makedef(const binname,textname:string):longbool;
var
  OldFileMode:longint;
begin
  FileCreated:=false;
  assign(f,binname);
  assign(t,textname);
  OldFileMode:=filemode;
  filemode:=0;
  reset(f,1);
  filemode:=OldFileMode;
  if not DOSstubOK(PEoffset)then
   makedef:=false
  else if not IsPE(PEoffset)then
   makedef:=false
  else
   makedef:=GetEdata(PEoffset);
  close(f);
  if FileCreated then
   close(t);
end;
{
  $Log$
  Revision 1.3  2000-09-24 15:06:17  peter
    * use defines.inc

  Revision 1.2  2000/07/13 11:32:43  michael
  + removed logs
}
