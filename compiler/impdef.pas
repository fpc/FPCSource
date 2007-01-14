{
    Copyright (c) 1998-2002 by Pavel

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

{$ifndef STANDALONE}
  {$i fpcdefs.inc}
{$endif}

interface

   uses
     SysUtils;

   var
     as_name,
     ar_name : string;

    function makedef(const binname,
{$IFDEF STANDALONE}
                           textname,
{$ENDIF}
                           libname:string):longbool;


implementation

{$IFDEF STANDALONE}
var
  __textname : string;
const
  kind : array[longbool] of pchar=('',' DATA');
{$ENDIF}

var
  f:file;
{$IFDEF STANDALONE}
  t:text;
  FileCreated:longbool;
{$ENDIF}
  lname:string;
  impname:string;
  TheWord:array[0..1]of char;
  PEoffset:cardinal;
  loaded:longint;

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


function isPE(x:longint):longbool;
begin
  seek(f,x);
  blockread(f,TheWord,2,loaded);
  isPE:=(loaded=2)and(TheWord='PE');
end;


var
  cstring : array[0..127]of char;
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
const
 IMAGE_SCN_CNT_CODE=$00000020;
 const
{$ifdef unix}
  DirSep = '/';
{$else}
  {$if defined(amiga) or defined(morphos)}
  DirSep = '/';
  {$else}
  DirSep = '\';
  {$endif}
{$endif}
var
 path:string;
 _d:dirstr;
 _n:namestr;
 _e:extstr;
 common_created:longbool;
procedure cleardir(const s,ext:string);
 var
  ff:file;
  dir:searchrec;
  attr:word;
 begin
  findfirst(s+dirsep+ext,anyfile,dir);
  while (doserror=0) do
   begin
     assign(ff,s+dirsep+dir.name);
     GetFattr(ff,attr);
     if not((DOSError<>0)or(Attr and Directory<>0))then
      Erase(ff);
     findnext(dir);
   end;
  findclose(dir);
 end;
procedure CreateTempDir(const s:string);
 var
  attr:word;
  ff:file;
 begin
  assign(ff,s);
  GetFattr(ff,attr);
  if DosError=0 then
   begin
    cleardir(s,'*.sw');
    cleardir(s,'*.swo');
   end
 else
  begin
    {$I-}
     mkdir(s);
    {$I+}
    if ioresult<>0 then;
  end;
 end;
procedure call_as(const name:string);
 begin
  FlushOutput;
  ExecuteProcess(as_name,'-o '+name+'o '+name);
 end;
procedure call_ar;
 var
  f:file;
  attr:word;
 begin
{$IFDEF STANDALONE}
  if impname='' then
   exit;
{$ENDIF}
  assign(f,impname);
  GetFAttr(f,attr);
  If DOSError=0 then
   erase(f);
  FlushOutput;
  ExecuteProcess(ar_name,'rs '+impname+' '+path+dirsep+'*.swo');
  cleardir(path,'*.sw');
  cleardir(path,'*.swo');
  {$i-}
  RmDir(path);
  {$i+}
  if ioresult<>0 then;
 end;
procedure makeasm(index:cardinal;name:pchar;isData:longbool);
 type
  tt=array[1..1]of pchar;
  pt=^tt;
 const
  fn_template:array[1..24]of pchar=(
   '.section .idata$2',
   '.rva        .L4',
   '.long       0,0',
   '.rva        ',
   '.rva        .L5',
   '.section .idata$4',
   '.L4:',
   '.rva        .L6',
   '.long       0',
   '.section .idata$5',
   '.L5:',
   '.text',
   '.globl      ',
   ':',
   'jmp *.L7',
   '.balign 4,144',
   '.section .idata$5',
   '.L7:',
   '.rva        .L6',
   '.long       0',
   '.section .idata$6',
   '.L6:',
   '.short      0',
   '.ascii      "\000"'
  );
  var_template:array[1..19]of pchar=(
   '.section .idata$2',
   '.rva        .L7',
   '.long       0,0',
   '.rva        ',
   '.rva        .L8',
   '.section .idata$4',
   '.L7:',
   '.rva        .L9',
   '.long       0',
   '.section .idata$5',
   '.L8:',
   '.globl      ',
   ':',
   '.rva        .L9',
   '.long       0',
   '.section .idata$6',
   '.L9:',
   '.short      0',
   '.ascii      "\000"'
  );
  __template:array[longbool]of pointer=(@fn_template,@var_template);
  common_part:array[1..5]of pchar=(
   '.balign 2,0',
   '.section .idata$7',
   '.globl      ',
   ':',
   '.ascii      "\000"'
  );
  posit:array[longbool,1..4]of longint=((4,13,14,24),(4,12,13,19));
 var
  template:array[longbool]of pt absolute __template;
  f:text;
  s:string;
  i:longint;
  n:string;
  common_name,asmout:string;
  __d:dirstr;
  __n:namestr;
  __x:extstr;
 begin
  if not common_created then
   begin
    common_name:='_$'+_n+'@common';
    asmout:=path+dirsep+'0.sw';
    assign(f,asmout);
    rewrite(f);
    for i:=1 to 5 do
     begin
      s:=StrPas(Common_part[i]);
      case i of
       3:
        s:=s+common_name;
       4:
        s:=common_name+s;
       5:
        begin
         fsplit(lname,__d,__n,__x);
         insert(__n+__x,s,9);
        end;
      end;
      writeln(f,s);
     end;
    close(f);
    call_as(asmout);
    common_created:=true;
   end;
  n:=strpas(name);
  str(succ(index):0,s);
  asmout:=path+dirsep+s+'.sw';
  assign(f,asmout);
  rewrite(f);
  for i:=1 to posit[isData,4]do
   begin
    s:=StrPas(template[isData]^[i]);
    if i=posit[isData,1]then
     s:=s+common_name
    else if i=posit[isData,2]then
     s:=s+n
    else if i=posit[isData,3]then
     s:=n+s
    else if i=posit[isData,4]then
     insert(n,s,9);
    writeln(f,s);
   end;
  close(f);
  call_as(asmout);
 end;
procedure ProcessEdata;
  type
   a8=array[0..7]of char;
  function GetSectionName(rva:cardinal;var Flags:cardinal):a8;
   var
    i:cardinal;
    LocObjOfs:cardinal;
    LocObj:TObjInfo;
   begin
    GetSectionName:='';
    Flags:=0;
    LocObjOfs:=APE_OptSize+PEoffset+24;
    for i:=1 to APE_obj do
     begin
      seek(f,LocObjOfs);
      blockread(f,LocObj,sizeof(LocObj));
      if(rva>=LocObj.VirtAddr)and(rva<=LocObj.VirtAddr+LocObj.RawSize)then
       begin
        GetSectionName:=a8(LocObj.ObjName);
        Flags:=LocObj.flags;
       end;
     end;
   end;
  var
   j,Fl:cardinal;
   ulongval,procEntry:cardinal;
   Ordinal:word;
   isData:longbool;
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
     fsplit(impname,_d,_n,_e);
     path:=_d+_n+'.ils';
{$IFDEF STANDALONE}
     if impname<>'' then
{$ENDIF}
     CreateTempDir(path);
     Common_created:=false;
     for j:=0 to pred(ExpDir.NumNames)do
      begin
       seek(f,RawOffset-VirtAddr+ExpDir.AddrOrds+j*2);
       blockread(f,Ordinal,2);
       seek(f,RawOffset-VirtAddr+ExpDir.AddrFuncs+Cardinal(Ordinal*4));
       blockread(f,ProcEntry,4);
       seek(f,RawOffset-VirtAddr+ExpDir.AddrNames+j*4);
       blockread(f,ulongval,4);
       seek(f,RawOffset-VirtAddr+ulongval);
       blockread(f,cstring,sizeof(cstring));
{$IFDEF STANDALONE}
       if not FileCreated then
        begin
         FileCreated:=true;
         if(__textname<>'')or(impname='')then
          begin
           rewrite(t);
           writeln(t,'EXPORTS');
          end;
        end;
{$ENDIF}
       isData:=GetSectionName(procentry,Fl)='';
       if not isData then
        isData:=Fl and IMAGE_SCN_CNT_CODE<>IMAGE_SCN_CNT_CODE;
{$IFDEF STANDALONE}
       if(__textname<>'')or(impname='')then
        writeln(t,cstring,' @',succ(ordinal):0,' ',kind[isData]);
       if impname<>''then
{$ENDIF}
       makeasm(j,cstring,isData);
      end;
     call_ar;
   end;
  end;

begin
  GetEdata:=false;
{$IFDEF STANDALONE}
  FileCreated:=false;
{$ENDIF}
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


function makedef(const binname,
{$IFDEF STANDALONE}
                       textname,
{$ENDIF}
                       libname:string):longbool;
var
  OldFileMode:longint;
begin
  assign(f,binname);
{$IFDEF STANDALONE}
  FileCreated:=false;
  assign(t,textname);
  __textname:=textname;
{$ENDIF}
  impname:=libname;
  lname:=binname;
  OldFileMode:=filemode;
  {$I-}
   filemode:=0;
   reset(f,1);
   filemode:=OldFileMode;
  {$I+}
  if IOResult<>0 then
   begin
     makedef:=false;
     exit;
   end;
  if not DOSstubOK(PEoffset)then
   makedef:=false
  else if not IsPE(PEoffset)then
   makedef:=false
  else
   makedef:=GetEdata(PEoffset);
  close(f);
{$IFDEF STANDALONE}
  if FileCreated then
   if(textname<>'')or(impname='')then
    close(t);
{$ENDIF}
end;

end.
