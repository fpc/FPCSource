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
  dos,cobjects,globtype,globals,aasm;

{$ifdef Ag386Bin}
  {$define NoAg386Att}
  {$define NoAg386Int}
  {$define NoAg386Nsm}
{$endif}

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
    IsEndFile : boolean;  { special 'end' file for import dir ? }
  {outfile}
    AsmSize,
    AsmStartSize,
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
    Procedure AsmClear;
    Procedure AsmWrite(const s:string);
    Procedure AsmWritePChar(p:pchar);
    Procedure AsmWriteLn(const s:string);
    Procedure AsmLn;
    procedure AsmCreate;
    procedure AsmClose;
    procedure Synchronize;
    procedure WriteTree(p:paasmoutput);virtual;
    procedure WriteAsmList;virtual;
  end;

Procedure GenerateAsm;
Procedure OnlyAsm;

var
  SmartLinkFilesCnt : longint;

Implementation

uses
  script,files,systems,verbose,comphook
{$ifdef linux}
  ,linux
{$endif}
  ,strings
{$ifdef i386}
  {$ifdef Ag386Bin}
    ,ag386bin
  {$endif}
  {$ifndef NoAg386Att}
    ,ag386att
  {$endif NoAg386Att}
  {$ifndef NoAg386Nsm}
    ,ag386nsm
  {$endif NoAg386Nsm}
  {$ifndef NoAg386Int}
    ,ag386int
  {$endif NoAg386Int}
  {$ifdef Ag386Cof}
    ,ag386cof
  {$endif Ag386Cof}
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
  {$ifndef NoAg68kMpw}
    ,ag68kmpw
  {$endif NoAg68kMpw}
{$endif}
  ;


{*****************************************************************************
                                  TAsmList
*****************************************************************************}

Function DoPipe:boolean;
begin
  DoPipe:=(cs_asm_pipe in aktglobalswitches) and
          not(cs_asm_leave in aktglobalswitches)
{$ifdef i386}
          and (aktoutputformat=as_i386_o)
{$endif i386}
{$ifdef m68k}
          and (aktoutputformat=as_m68k_o);
{$endif m68k}
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
     { is an assembler passed ? }
     if utilsdirectory<>'' then
         begin
            LastASBin:=Search(target_asm.asmbin+source_os.exeext,
              utilsdirectory,asfound)+target_asm.asmbin+source_os.exeext;
         end
       else
         LastASBin:=FindExe(target_asm.asmbin,asfound);
     if (not asfound) and not(cs_asm_extern in aktglobalswitches) then
      begin
        Message1(exec_w_assembler_not_found,LastASBin);
        aktglobalswitches:=aktglobalswitches+[cs_asm_extern];
      end;
     if asfound then
      Message1(exec_t_using_assembler,LastASBin);
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
        Message1(exec_w_cant_call_assembler,tostr(doserror));
        aktglobalswitches:=aktglobalswitches+[cs_asm_extern];
        callassembler:=false;
      end
     else
      if (dosexitcode<>0) then
       begin
        Message1(exec_w_error_while_assembling,tostr(dosexitcode));
        callassembler:=false;
       end;
   end
  else
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
   RemoveAsm
  else
   begin
      DoAssemble:=false;
      inc(status.errorcount);
   end;
end;


procedure TAsmList.NextSmartName;
var
  s : string;
begin
  inc(SmartLinkFilesCnt);
  if SmartLinkFilesCnt>999999 then
   Message(assem_f_too_many_asm_files);
  if IsEndFile then
   begin
     s:=current_module^.asmprefix^+'e';
     IsEndFile:=false;
   end
  else
   s:=current_module^.asmprefix^;
  AsmFile:=Path+FixFileName(s+tostr(SmartLinkFilesCnt)+target_info.asmext);
  ObjFile:=Path+FixFileName(s+tostr(SmartLinkFilesCnt)+target_info.objext);
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


Procedure TAsmList.AsmClear;
begin
  outcnt:=0;
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
  AsmStartSize:=0;
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


{Touch Assembler and object time to ppu time is there is a ppufilename}
procedure TAsmList.Synchronize;
var
  f : file;
  l : longint;
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
        assign(f,asmfile);
        {$I-}
         reset(f,1);
        {$I+}
        if ioresult=0 then
         begin
           setftime(f,l);
           close(f);
         end;
        if not(cs_asm_extern in aktglobalswitches) then
         begin
           assign(f,objfile);
           {$I-}
            reset(f,1);
           {$I+}
           if ioresult=0 then
            begin
              setftime(f,l);
              close(f);
            end;
         end;
      end;
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
  IsEndFile:=false;
{ Which path will be used ? }
  if (cs_smartlink in aktmoduleswitches) then
   begin
     path:=current_module^.path^+FixFileName(current_module^.modulename^)+target_info.smartext;
     {$I-}
      mkdir(path);
     {$I+}
     i:=ioresult;
     path:=FixPath(path,false);
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
{$ifdef i386}
  {$ifdef Ag386Bin}
    b : Pi386binasmlist;
  {$endif}
{$endif}
begin
  case aktoutputformat of
{$ifdef i386}
  {$ifdef Ag386Bin}
     as_i386_dbg,
     as_i386_coff,
     as_i386_pecoff :
       begin
         case aktoutputformat of
           as_i386_dbg :
             b:=new(pi386binasmlist,Init(og_dbg));
           as_i386_coff :
             b:=new(pi386binasmlist,Init(og_coff));
           as_i386_pecoff :
             b:=new(pi386binasmlist,Init(og_pecoff));
         end;
         b^.WriteBin;
         dispose(b,done);
         exit;
       end;
  {$endif Ag386Bin}
  {$ifndef NoAg386Att}
     as_i386_o,
     as_i386_o_aout,
     as_i386_asw :
       a:=new(pi386attasmlist,Init);
  {$endif NoAg386Att}
  {$ifndef NoAg386Nsm}
     as_i386_nasmcoff,
     as_i386_nasmelf,
     as_i386_nasmobj :
       a:=new(pi386nasmasmlist,Init);
  {$endif NoAg386Nsm}
  {$ifndef NoAg386Int}
     as_i386_tasm :
       a:=new(pi386intasmlist,Init);
  {$endif NoAg386Int}
{$endif}
{$ifdef m68k}
  {$ifndef NoAg68kGas}
     as_m68k_o,
     as_m68k_gas :
       a:=new(pm68kgasasmlist,Init);
  {$endif NoAg86KGas}
  {$ifndef NoAg68kMot}
     as_m68k_mot :
       a:=new(pm68kmotasmlist,Init);
  {$endif NoAg86kMot}
  {$ifndef NoAg68kMit}
     as_m68k_mit :
       a:=new(pm68kmitasmlist,Init);
  {$endif NoAg86KMot}
  {$ifndef NoAg68kMpw}
     as_m68k_mpw :
       a:=new(pm68kmpwasmlist,Init);
  {$endif NoAg68kMpw}
{$endif}
  else
    Message(assem_f_assembler_output_not_supported);
  end;
  a^.AsmCreate;
  a^.WriteAsmList;
  a^.AsmClose;
  a^.DoAssemble;
  a^.synchronize;
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
  Revision 1.37  1999-02-24 00:59:11  peter
    * small updates for ag386bin

  Revision 1.36  1999/02/22 02:15:01  peter
    * updates for ag386bin

  Revision 1.35  1999/02/17 10:16:26  peter
    * small fixes for the binary writer

  Revision 1.34  1999/01/10 15:37:52  peter
    * moved some tables from ra386*.pas -> i386.pas
    + start of coff writer
    * renamed asmutils unit to rautils

  Revision 1.33  1998/12/11 00:02:45  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.32  1998/11/06 09:46:46  pierre
   * assemble failure increments status errorcount again !!

  Revision 1.31  1998/10/26 22:23:28  peter
    + fixpath() has an extra option to allow a ./ as path

  Revision 1.30  1998/10/16 13:37:14  florian
    + switch -FD added to specify the path for utilities

  Revision 1.29  1998/10/15 16:19:42  peter
    * fixed asmsynchronize

  Revision 1.28  1998/10/14 15:56:43  pierre
    * all references to comp suppressed for m68k

  Revision 1.27  1998/10/13 16:50:01  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.26  1998/10/13 13:10:11  peter
    * new style for m68k/i386 infos and enums

  Revision 1.25  1998/10/13 08:19:24  pierre
    + source_os is now set correctly for cross-processor compilers
      (tos contains all target_infos and
       we use CPU86 and CPU68 conditionnals to
       get the source operating system
       this only works if you do not undefine
       the source target  !!)
    * several cg68k memory leaks fixed
    + started to change the code so that it should be possible to have
      a complete compiler (both for m68k and i386 !!)

  Revision 1.24  1998/10/08 23:28:50  peter
    * -vu shows unit info, -vt shows tried/used files

  Revision 1.23  1998/10/07 04:27:37  carl
    + MPW support

  Revision 1.22  1998/09/16 16:41:39  peter
    * merged fixes

  Revision 1.21.2.1  1998/09/16 16:11:38  peter
    * missing isendfile reset in .init

  Revision 1.21  1998/09/07 18:33:32  peter
    + smartlinking for win95 imports

  Revision 1.20  1998/09/04 17:34:20  pierre
    * bug with datalabel corrected
    + assembler errors better commented
    * one nested record crash removed

  Revision 1.19  1998/08/26 10:06:34  peter
    * reduce amount of asmfiles generated
    * no stabs are written in writefilelineinfo when debuginfo is off

  Revision 1.18  1998/08/21 14:08:39  pierre
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
