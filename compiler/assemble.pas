{
    $Id$
    Copyright (c) 1998 by the FPC development team

    This unit handles the assemblerfile write and assembler calls of FPC

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

 ****************************************************************************}

unit assemble;

interface

uses
  dos,cobjects,globals,aasm;

const
{$ifdef tp}
  AsmOutSize=1024;
{$else}
  AsmOutSize=32768;
{$endif}

type
  PAsmList=^TAsmList;
  TAsmList=object
  {filenames}
    path     : pathstr;
    name     : namestr;
    asmfile,             { current .s and .o file }
    objfile,
    as_bin   : string;
  {outfile}
    AsmSize,
    outcnt   : longint;
    outbuf   : array[0..AsmOutSize-1] of char;
    outfile  : file;
    Constructor Init;
    Destructor Done;
    Function  FindAssembler:string;
    Function  CallAssembler(const command,para:string):Boolean;
    Function  DoAssemble:boolean;
    Procedure RemoveAsm;
    procedure NextSmartName;
    Procedure AsmFlush;
    Procedure AsmWrite(const s:string);
    Procedure AsmWritePChar(p:pchar);
    Procedure AsmWriteLn(const s:string);
    Procedure AsmLn;
    procedure AsmCreate;
    procedure AsmClose;
    procedure WriteTree(p:paasmoutput);virtual;
    procedure WriteAsmList;virtual;
  end;

Procedure GenerateAsm;
Procedure OnlyAsm;

var
  SmartLinkFilesCnt : longint;

Implementation

uses
  script,files,systems,verbose
{$ifdef linux}
  ,linux
{$endif}
  ,strings
{$ifdef i386}
  {$ifndef NoAg386Att}
    ,ag386att
  {$endif NoAg386Att}
  {$ifndef NoAg386Nsm}
    ,ag386nsm
  {$endif NoAg386Nsm}
  {$ifndef NoAg386Int}
    ,ag386int
  {$endif NoAg386Int}
{$endif}
{$ifdef m68k}
  {$ifndef NoAg68kGas}
    ,ag68kgas
  {$endif NoAg68kGas}
  {$ifndef NoAg68kMot}
    ,ag68kmot
  {$endif NoAg68kMot}
  {$ifndef NoAg68kMit}
    ,ag68kmit
  {$endif NoAg68kMit}
{$endif}
  ;


{*****************************************************************************
                                  TAsmList
*****************************************************************************}

Function DoPipe:boolean;
begin
  DoPipe:=(cs_asm_pipe in aktglobalswitches) and
          not(cs_asm_leave in aktglobalswitches) and
          (aktoutputformat=as_o);
end;


const
  lastas  : byte=255;
var
  LastASBin : string;
Function TAsmList.FindAssembler:string;
var
  asfound : boolean;
begin
  if lastas<>ord(target_asm.id) then
   begin
     lastas:=ord(target_asm.id);
     LastASBin:=FindExe(target_asm.asmbin,asfound);
     if (not asfound) and not(cs_asm_extern in aktglobalswitches) then
      begin
        Message1(exec_w_assembler_not_found,LastASBin);
        aktglobalswitches:=aktglobalswitches+[cs_asm_extern];
      end;
     if asfound then
      Message1(exec_u_using_assembler,LastASBin);
   end;
  FindAssembler:=LastASBin;
end;


Function TAsmList.CallAssembler(const command,para:string):Boolean;
begin
  callassembler:=true;
  if not(cs_asm_extern in aktglobalswitches) then
   begin
     swapvectors;
     exec(command,para);
     swapvectors;
     if (doserror<>0) then
      begin
        Message(exec_w_cant_call_assembler);
        callassembler:=false;
      end
     else
      if (dosexitcode<>0) then
       begin
        Message(exec_w_error_while_assembling);
        aktglobalswitches:=aktglobalswitches+[cs_asm_extern];
        callassembler:=false;
       end;
   end;
  if cs_asm_extern in aktglobalswitches then
   AsmRes.AddAsmCommand(command,para,name);
end;


procedure TAsmList.RemoveAsm;
var
  g : file;
  i : word;
begin
  if cs_asm_leave in aktglobalswitches then
   exit;
  if cs_asm_extern in aktglobalswitches then
   AsmRes.AddDeleteCommand(AsmFile)
  else
   begin
     assign(g,AsmFile);
     {$I-}
      erase(g);
     {$I+}
     i:=ioresult;
   end;
end;


Function TAsmList.DoAssemble:boolean;
var
  s : string;
begin
  DoAssemble:=true;
  if DoPipe then
   exit;
  if (SmartLinkFilesCnt<=1) and not(cs_asm_extern in aktglobalswitches) then
   Message1(exec_i_assembling,name);
  s:=target_asm.asmcmd;
  Replace(s,'$ASM',AsmFile);
  Replace(s,'$OBJ',ObjFile);
  if CallAssembler(FindAssembler,s) then
   RemoveAsm;
end;


procedure TAsmList.NextSmartName;
begin
  inc(SmartLinkFilesCnt);
  if SmartLinkFilesCnt>999999 then
   Message(assem_f_too_many_asm_files);
  AsmFile:=Path+FixFileName(current_module^.asmprefix^+tostr(SmartLinkFilesCnt)+target_info.asmext);
  ObjFile:=Path+FixFileName(current_module^.asmprefix^+tostr(SmartLinkFilesCnt)+target_info.objext);
end;


{*****************************************************************************
                       TAsmList AsmFile Writing
*****************************************************************************}

Procedure TAsmList.AsmFlush;
begin
  if outcnt>0 then
   begin
     BlockWrite(outfile,outbuf,outcnt);
     outcnt:=0;
   end;
end;


Procedure TAsmList.AsmWrite(const s:string);
begin
  if OutCnt+length(s)>=AsmOutSize then
   AsmFlush;
  Move(s[1],OutBuf[OutCnt],length(s));
  inc(OutCnt,length(s));
  inc(AsmSize,length(s));
end;


Procedure TAsmList.AsmWriteLn(const s:string);
begin
  AsmWrite(s);
  AsmLn;
end;


Procedure TAsmList.AsmWritePChar(p:pchar);
var
  i,j : longint;
begin
  i:=StrLen(p);
  j:=i;
  while j>0 do
   begin
     i:=min(j,AsmOutSize);
     if OutCnt+i>=AsmOutSize then
      AsmFlush;
     Move(p[0],OutBuf[OutCnt],i);
     inc(OutCnt,i);
     inc(AsmSize,i);
     dec(j,i);
     p:=pchar(@p[i]);
   end;
end;


Procedure TAsmList.AsmLn;
begin
  if OutCnt>=AsmOutSize-2 then
   AsmFlush;
  OutBuf[OutCnt]:=target_os.newline[1];
  inc(OutCnt);
  inc(AsmSize);
  if length(target_os.newline)>1 then
   begin
     OutBuf[OutCnt]:=target_os.newline[2];
     inc(OutCnt);
     inc(AsmSize);
   end;
end;


procedure TAsmList.AsmCreate;
begin
  if (cs_smartlink in aktmoduleswitches) then
   NextSmartName;
{$ifdef linux}
  if DoPipe then
   begin
     Message1(exec_i_assembling_pipe,asmfile);
     POpen(outfile,'as -o '+objfile,'W');
   end
  else
{$endif}
   begin
     Assign(outfile,asmfile);
     {$I-}
      Rewrite(outfile,1);
     {$I+}
     if ioresult<>0 then
      Message1(exec_d_cant_create_asmfile,asmfile);
   end;
  outcnt:=0;
  AsmSize:=0;
end;


procedure TAsmList.AsmClose;
var
  f : file;
  l : longint;
begin
  AsmFlush;
{$ifdef linux}
  if DoPipe then
   Close(outfile)
  else
{$endif}
   begin
   {Touch Assembler time to ppu time is there is a ppufilename}
     if Assigned(current_module^.ppufilename) then
      begin
        Assign(f,current_module^.ppufilename^);
        {$I-}
         reset(f,1);
        {$I+}
        if ioresult=0 then
         begin
           getftime(f,l);
           close(f);
           reset(outfile,1);
           setftime(outfile,l);
         end;
      end;
     close(outfile);
   end;
end;


procedure TAsmList.WriteTree(p:paasmoutput);
begin
end;


procedure TAsmList.WriteAsmList;
begin
end;


Constructor TAsmList.Init;
var
  i : word;
begin
{ load start values }

  asmfile:=current_module^.asmfilename^;
  objfile:=current_module^.objfilename^;
  name:=FixFileName(current_module^.modulename^);
  OutCnt:=0;
  SmartLinkFilesCnt:=0;
{ Which path will be used ? }
  if (cs_smartlink in aktmoduleswitches) then
   begin
     path:=current_module^.path^+FixFileName(current_module^.modulename^)+target_info.smartext;
     {$I-}
      mkdir(path);
     {$I+}
     i:=ioresult;
     path:=FixPath(path);
   end
  else
   path:=current_module^.path^;
end;


Destructor TAsmList.Done;
begin
end;


{*****************************************************************************
                     Generate Assembler Files Main Procedure
*****************************************************************************}

Procedure GenerateAsm;
var
  a : PAsmList;
begin
  case aktoutputformat of
{$ifdef i386}
  {$ifndef NoAg386Att}
        as_o,as_o_aout,as_asw : a:=new(pi386attasmlist,Init);
  {$endif NoAg386Att}
  {$ifndef NoAg386Nsm}
 as_nasmcoff,
  as_nasmelf,
  as_nasmobj : a:=new(pi386nasmasmlist,Init);
  {$endif NoAg386Nsm}
  {$ifndef NoAg386Int}
     as_tasm : a:=new(pi386intasmlist,Init);
  {$endif NoAg386Int}
{$endif}
{$ifdef m68k}
  {$ifndef NoAg68kGas}
     as_o,
   as_gas : a:=new(pm68kgasasmlist,Init);
  {$endif NoAg86KGas}
  {$ifndef NoAg68kMot}
   as_mot : a:=new(pm68kmotasmlist,Init);
  {$endif NoAg86kMot}
  {$ifndef NoAg68kMit}
   as_mit : a:=new(pm68kmitasmlist,Init);
  {$endif NoAg86KMot}
{$endif}
  else
   Message(assem_f_assembler_output_not_supported);
  end;
  a^.AsmCreate;
  a^.WriteAsmList;
  a^.AsmClose;
  a^.DoAssemble;
  dispose(a,Done);
end;


Procedure OnlyAsm;
var
  a : PAsmList;
begin
  a:=new(pasmlist,Init);
  a^.DoAssemble;
  dispose(a,Done);
end;


end.
{
  $Log$
  Revision 1.18  1998-08-21 14:08:39  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.17  1998/08/17 09:17:43  peter
    * static/shared linking updates

  Revision 1.16  1998/08/14 21:56:30  peter
    * setting the outputfile using -o works now to create static libs

  Revision 1.15  1998/08/14 18:16:09  peter
    * return after a failed call will now add it to ppas

  Revision 1.14  1998/08/10 14:49:41  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.13  1998/07/14 21:46:40  peter
    * updated messages file

  Revision 1.12  1998/07/08 14:58:34  daniel
  * First check if call to assembler is succesfull, then check it's exit code.
  This is more logical than first checking the exit code. For some mysterious
  reason this did not give problems on DOS & Linux. On OS/2 it did.

  Revision 1.11  1998/06/08 22:59:43  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.10  1998/06/04 23:51:33  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.9  1998/05/23 01:21:01  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.8  1998/05/11 13:07:53  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.7  1998/05/07 00:17:00  peter
    * smartlinking for sets
    + consts labels are now concated/generated in hcodegen
    * moved some cpu code to cga and some none cpu depended code from cga
      to tree and hcodegen and cleanup of hcodegen
    * assembling .. output reduced for smartlinking ;)

  Revision 1.6  1998/05/04 17:54:24  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.5  1998/04/29 10:33:44  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.4  1998/04/27 23:10:27  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.3  1998/04/10 14:41:43  peter
    * removed some Hints
    * small speed optimization for AsmLn

  Revision 1.2  1998/04/08 11:34:18  peter
    * nasm works (linux only tested)
}
