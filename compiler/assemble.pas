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
  AsmOutSize=10000;
{$endif}
  SmartExt='.sl';

{$ifdef i386}
{ tof = (of_none,of_o,of_obj,of_masm,of_att,of_nasm,of_win32) }
  AsBin : array[tof] of string[8]=('','as','nasm','tasm','as','nasm','asw');
  { I hope that all I386 assembler recongnize this as
    a comment begin (PM) }
  As_comment : string[2] = '# ';
{$endif}
{$ifdef m68k}
{ tof = (of_none,of_o,of_gas,of_mot,of_mit)  }
  AsBin : array[tof] of string[8]=('','','as68k','','');
  { I hope that all M68K assembler recongnize this as
    a comment begin (PM) }
  As_comment : string[2] = '| ';
{$endif}


type
  PAsmList=^TAsmList;
  TAsmList=object
  {filenames}
    path     : dirstr;
    name     : namestr;
    asmfile,
    objfile,
    srcfile,
    as_bin   : string;
    smartcnt : longint;
  {outfile}
    outcnt   : longint;
    outbuf   : array[0..AsmOutSize-1] of char;
    outfile  : file;
    Constructor Init(const fn:string);
    Destructor Done;
    Function  FindAssembler(curr_of:tof):string;
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

Procedure GenerateAsm(const fn:string);
Procedure OnlyAsm(const fn:string);


Implementation

uses
  script,files,systems,verbose
{$ifdef linux}
  ,linux
{$endif}
  ,strings
{$ifdef i386}
  ,ag386att,ag386int
{$endif}
{$ifdef m68k}
  ,ag68kmot,ag68kgas,ag68kmit
{$endif}
  ;


Function DoPipe:boolean;
begin
  DoPipe:=use_pipe and (not WriteAsmFile) and (current_module^.output_format=of_o);
end;


{*****************************************************************************
                       TAsmList Calling and Name
*****************************************************************************}

const
  last_of  : tof=of_none;
var
  LastASBin : string;
Function TAsmList.FindAssembler(curr_of:tof):string;
var
  asfound : boolean;
begin
  if last_of<>curr_of then
   begin
     last_of:=curr_of;
     LastASBin:=FindExe(asbin[curr_of],asfound);
     if (not asfound) and (not externasm) then
      begin
        Message1(exec_w_assembler_not_found,LastASBin);
        externasm:=true;
      end;
     if asfound then
      Message1(exec_u_using_assembler,LastASBin);
   end;
  FindAssembler:=LastASBin;
end;


Function TAsmList.CallAssembler(const command,para:string):Boolean;
begin
  if not externasm then
   begin
     swapvectors;
     exec(command,para);
     swapvectors;
     if (dosexitcode<>0) then
      begin
        Message(exec_w_error_while_assembling);
        callassembler:=false;
        exit;
      end
     else
      if (doserror<>0) then
       begin
         Message(exec_w_cant_call_assembler);
         externasm:=true;
       end;
   end;
  if externasm then
   AsmRes.AddAsmCommand(command,para,asmfile);
  callassembler:=true;
end;


procedure TAsmList.RemoveAsm;
var
  g : file;
  i : word;
begin
  if writeasmfile then
   exit;
  if ExternAsm then
   AsmRes.AddDeleteCommand(asmfile)
  else
   begin
     assign(g,asmfile);
     {$I-}
      erase(g);
     {$I+}
     i:=ioresult;
   end;
end;


Function TAsmList.DoAssemble:boolean;
begin
  if DoPipe then
   exit;
  if not externasm then
   Message1(exec_i_assembling,asmfile);
  case current_module^.output_format of
{$ifdef i386}
   of_att : begin
              externasm:=true; {Force Extern Asm}
              if CallAssembler(FindAssembler(of_att),' -D -o '+objfile+' '+AsmFile) then
               RemoveAsm;
            end;
     of_o : begin
              if CallAssembler(FindAssembler(of_o),'-D -o '+objfile+' '+AsmFile) then
               RemoveAsm;
            end;
 of_win32 : begin
              if CallAssembler(FindAssembler(of_win32),'-D -o '+objfile+' '+AsmFile) then
               RemoveAsm;
            end;
  of_nasm : begin
            {$ifdef linux}
              if CallAssembler(FindAssembler(of_nasm),' -f elf -o '+objfile+' '+AsmFile) then
               RemoveAsm;
            {$else}
              if CallAssembler(FindAssembler(of_nasm),' -f coff -o '+objfile+' '+AsmFile) then
               RemoveAsm;
            {$endif}
            end;
   of_obj : begin
              if CallAssembler(FindAssembler(of_nasm),' -f obj -o '+objfile+' '+AsmFile) then
               RemoveAsm;
            end;
  of_masm : begin
            { !! Nothing yet !! }
            end;
{$endif}
{$ifdef m68k}
   of_mot,
   of_mit : begin
            { !! Nothing yet !! }
            end;
   of_o,of_gas : begin
              if CallAssembler(FindAssembler(of_gas),'  --register-prefix-optional'+
              ' -o '+objfile+' '+asmfile) then
               RemoveAsm;
            end;
{$endif}
  else
   internalerror(30000);
  end;
  DoAssemble:=true;
end;


procedure TAsmList.NextSmartName;
begin
  inc(smartcnt);
  if smartcnt>999999 then
   Comment(V_Fatal,'Too many assembler files');
  AsmFile:=Path+FixFileName('as'+tostr(smartcnt)+target_info.asmext);
  ObjFile:=Path+FixFileName('as'+tostr(smartcnt)+target_info.objext);
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
     dec(j,i);
     p:=pchar(@p[i]);
   end;
end;


Procedure TAsmList.AsmLn;
begin
  if OutCnt>=AsmOutSize-2 then
   AsmFlush;
  OutBuf[OutCnt]:=target_info.newline[1];
  inc(OutCnt);
  if length(target_info.newline)>1 then
   begin
     OutBuf[OutCnt]:=target_info.newline[2];
     inc(OutCnt);
   end;
end;


procedure TAsmList.AsmCreate;
begin
  if SmartLink then
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


Constructor TAsmList.Init(const fn:string);
var
  ext : extstr;
  i   : word;
begin
{Create filenames for easier access}
  fsplit(fn,path,name,ext);
  srcfile:=fn;
  asmfile:=path+name+target_info.asmext;
  objfile:=path+name+target_info.objext;
  OutCnt:=0;
{Smartlinking}
  smartcnt:=0;
  if smartlink then
   begin
     path:=FixPath(path)+FixFileName(name+smartext);
     {$I-}
      mkdir(path);
     {$I+}
     i:=ioresult;
   end;
  path:=FixPath(path);
end;


Destructor TAsmList.Done;
begin
end;


{*****************************************************************************
                     Generate Assembler Files Main Procedure
*****************************************************************************}

Procedure GenerateAsm(const fn:string);
var
  a : PAsmList;
begin
  case current_module^.output_format of
{$ifdef i386}
     of_o,
 of_win32,
   of_att : a:=new(pi386attasmlist,Init(fn));
   of_obj,
  of_masm,
  of_nasm : a:=new(pi386intasmlist,Init(fn));
{$endif}
{$ifdef m68k}
     of_o,
   of_gas : a:=new(pm68kgasasmlist,Init(fn));
   of_mot : a:=new(pm68kmotasmlist,Init(fn));
   of_mit : a:=new(pm68kmitasmlist,Init(fn));
{$endif}
  else
   internalerror(30000);
  end;
  a^.AsmCreate;
  a^.WriteAsmList;
  a^.AsmClose;
  a^.DoAssemble;
  dispose(a,Done);
end;


Procedure OnlyAsm(const fn:string);
var
  a : PAsmList;
begin
  case current_module^.output_format of
{$ifdef i386}
     of_o,
 of_win32,
   of_att : a:=new(pi386attasmlist,Init(fn));
   of_obj,
  of_masm,
  of_nasm : a:=new(pi386intasmlist,Init(fn));
{$endif}
{$ifdef m68k}
     of_o,
   of_gas : a:=new(pm68kgasasmlist,Init(fn));
   of_mot : a:=new(pm68kmotasmlist,Init(fn));
   of_mit : a:=new(pm68kmitasmlist,Init(fn));
{$endif}
  else
   internalerror(30000);
  end;
  a^.DoAssemble;
  dispose(a,Done);
end;

end.
{
  $Log$
  Revision 1.5  1998-04-29 10:33:44  pierre
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

  Revision 1.1.1.1  1998/03/25 11:18:16  root
  * Restored version

  Revision 1.17  1998/03/10 13:23:00  florian
    * small win32 problems fixed

  Revision 1.16  1998/03/10 01:17:14  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.15  1998/03/09 10:37:41  peter
    * fixed very long pchar writing (> outbufsize)

  Revision 1.14  1998/03/05 22:43:45  florian
    * some win32 support stuff added

  Revision 1.13  1998/03/04 14:18:58  michael
  * modified messaging system

  Revision 1.12  1998/03/04 01:34:51  peter
    * messages for unit-handling and assembler/linker
    * the compiler compiles without -dGDB, but doesn't work yet
    + -vh for Hint

  Revision 1.11  1998/03/02 01:48:05  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.10  1998/02/26 11:57:00  daniel
  * New assembler optimizations commented out, because of bugs.
  * Use of dir-/name- and extstr.

  Revision 1.9  1998/02/24 10:29:12  peter
    * -a works again

  Revision 1.8  1998/02/21 03:31:40  carl
    + mit68k asm support.

  Revision 1.7  1998/02/18 14:18:16  michael
  + added log at end of file (retroactively)

  revision 1.6
  date: 1998/02/18 13:43:11;  author: michael;  state: Exp;  lines: +3 -19
  + Implemented an OS independent AsmRes object.
  ----------------------------
  revision 1.5
  date: 1998/02/17 21:20:28;  author: peter;  state: Exp;  lines: +60 -54
    + Script unit
    + __EXIT is called again to exit a program
    - target_info.link/assembler calls
    * linking works again for dos
    * optimized a few filehandling functions
    * fixed stabs generation for procedures
  ----------------------------
  revision 1.4
  date: 1998/02/16 12:51:27;  author: michael;  state: Exp;  lines: +2 -2
  + Implemented linker object
  ----------------------------
  revision 1.3
  date: 1998/02/15 21:15:58;  author: peter;  state: Exp;  lines: +8 -9
    * all assembler outputs supported by assemblerobject
    * cleanup with assembleroutputs, better .ascii generation
    * help_constructor/destructor are now added to the externals
    - generation of asmresponse is not outputformat depended
  ----------------------------
  revision 1.2
  date: 1998/02/14 01:45:04;  author: peter;  state: Exp;  lines: +3 -14
    * more fixes
    - pmode target is removed
    - search_as_ld is removed, this is done in the link.pas/assemble.pas
    + findexe() to search for an executable (linker,assembler,binder)
  ----------------------------
  revision 1.1
  date: 1998/02/13 22:28:16;  author: peter;  state: Exp;
    + Initial implementation
}
