{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

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

 ****************************************************************************
}
unit assemble;

{$i defines.inc}

interface

uses
{$ifdef Delphi}
  sysutils,
  dmisc,
{$else Delphi}
  strings,
  dos,
{$endif Delphi}
  globtype,globals,aasm;

const
  AsmOutSize=32768;

type
  TAssembler=class
  public
  {filenames}
    path     : pathstr;
    name     : namestr;
    asmfile,         { current .s and .o file }
    objfile  : string;
    SmartAsm : boolean;
    SmartFilesCount,
    SmartHeaderCount : longint;
    Constructor Create(smart:boolean);
    Destructor Destroy;override;
    procedure WriteTree(p:TAAsmoutput);virtual;
    procedure WriteAsmList;virtual;
    procedure NextSmartName(place:tcutplace);
  end;

  TExternalAssembler=class(TAssembler)
  private
    procedure CreateSmartLinkPath(const s:string);
  protected
  {outfile}
    AsmSize,
    AsmStartSize,
    outcnt   : longint;
    outbuf   : array[0..AsmOutSize-1] of char;
    outfile  : file;
  public
    Function  FindAssembler:string;
    Function  CallAssembler(const command,para:string):Boolean;
    Function  DoAssemble:boolean;
    Procedure RemoveAsm;
    Procedure AsmFlush;
    Procedure AsmClear;
    Procedure AsmWrite(const s:string);
    Procedure AsmWritePChar(p:pchar);
    Procedure AsmWriteLn(const s:string);
    Procedure AsmLn;
    procedure AsmCreate(Aplace:tcutplace);
    procedure AsmClose;
    procedure Synchronize;
  public
    Constructor Create(smart:boolean);
  end;


Procedure GenerateAsm(smart:boolean);
Procedure OnlyAsm;


Implementation

uses
  cutils,script,fmodule,systems,verbose
{$ifdef unix}
  {$ifdef ver1_0}
    ,linux
  {$else}
    ,unix
  {$endif}
{$endif}
{$ifdef i386}
  {$ifndef NoAg386Bin}
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
                                   TAssembler
*****************************************************************************}

Constructor TAssembler.Create(smart:boolean);
begin
{ load start values }
  asmfile:=current_module.asmfilename^;
  objfile:=current_module.objfilename^;
  name:=Lower(current_module.modulename^);
  path:=current_module.outputpath^;
  SmartAsm:=smart;
  SmartFilesCount:=0;
  SmartHeaderCount:=0;
  SmartLinkOFiles.Clear;
end;


Destructor TAssembler.Destroy;
begin
end;


procedure TAssembler.WriteTree(p:TAAsmoutput);
begin
end;


procedure TAssembler.WriteAsmList;
begin
end;


procedure TAssembler.NextSmartName(place:tcutplace);
var
  s : string;
begin
  inc(SmartFilesCount);
  if SmartFilesCount>999999 then
   Message(asmw_f_too_many_asm_files);
  case place of
    cut_begin :
      begin
        inc(SmartHeaderCount);
        s:=current_module.asmprefix^+tostr(SmartHeaderCount)+'h';
      end;
    cut_normal :
      s:=current_module.asmprefix^+tostr(SmartHeaderCount)+'s';
    cut_end :
      s:=current_module.asmprefix^+tostr(SmartHeaderCount)+'t';
  end;
  AsmFile:=Path+FixFileName(s+tostr(SmartFilesCount)+target_info.asmext);
  ObjFile:=Path+FixFileName(s+tostr(SmartFilesCount)+target_info.objext);
  { insert in container so it can be cleared after the linking }
  SmartLinkOFiles.Insert(Objfile);
end;


{*****************************************************************************
                                  TExternalAssembler
*****************************************************************************}

Function DoPipe:boolean;
begin
  DoPipe:=(cs_asm_pipe in aktglobalswitches) and
          not(cs_asm_leave in aktglobalswitches)
{$ifdef i386}
          and (aktoutputformat=as_i386_as)
{$endif i386}
{$ifdef m68k}
          and (aktoutputformat=as_m68k_as);
{$endif m68k}
end;


Constructor TExternalAssembler.Create(smart:boolean);
begin
  inherited Create(smart);
  if SmartAsm then
   begin
     path:=FixPath(current_module.outputpath^+FixFileName(current_module.modulename^)+target_info.smartext,false);
     CreateSmartLinkPath(path);
   end;
  Outcnt:=0;
end;


procedure TExternalAssembler.CreateSmartLinkPath(const s:string);
var
  dir : searchrec;
  hs  : string;
begin
  if PathExists(s) then
   begin
     { the path exists, now we clean only all the .o and .s files }
     { .o files }
     findfirst(s+dirsep+'*'+target_info.objext,anyfile,dir);
     while (doserror=0) do
      begin
        RemoveFile(s+dirsep+dir.name);
        findnext(dir);
      end;
     findclose(dir);
     { .s files }
     findfirst(s+dirsep+'*'+target_info.asmext,anyfile,dir);
     while (doserror=0) do
      begin
        RemoveFile(s+dirsep+dir.name);
        findnext(dir);
      end;
     findclose(dir);
   end
  else
   begin
     hs:=s;
     if hs[length(hs)] in ['/','\'] then
      delete(hs,length(hs),1);
     {$I-}
      mkdir(hs);
     {$I+}
     if ioresult<>0 then;
   end;
end;


const
  lastas  : byte=255;
var
  LastASBin : pathstr;
Function TExternalAssembler.FindAssembler:string;
var
  asfound : boolean;
  UtilExe  : string;
begin
  asfound:=false;
  UtilExe:=AddExtension(target_asm.asmbin,source_os.exeext);
  if lastas<>ord(target_asm.id) then
   begin
     lastas:=ord(target_asm.id);
     { is an assembler passed ? }
     if utilsdirectory<>'' then
       asfound:=FindFile(UtilExe,utilsdirectory,LastASBin);
     if not AsFound then
       asfound:=FindExe(UtilExe,LastASBin);
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


Function TExternalAssembler.CallAssembler(const command,para:string):Boolean;
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


procedure TExternalAssembler.RemoveAsm;
var
  g : file;
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
     if ioresult<>0 then;
   end;
end;


Function TExternalAssembler.DoAssemble:boolean;
var
  s : string;
begin
  DoAssemble:=true;
  if DoPipe then
   exit;
  if not(cs_asm_extern in aktglobalswitches) then
   begin
     if SmartAsm then
      begin
        if (SmartFilesCount<=1) then
         Message1(exec_i_assembling_smart,name);
      end
     else
     Message1(exec_i_assembling,name);
   end;
  s:=target_asm.asmcmd;
  Replace(s,'$ASM',AsmFile);
  Replace(s,'$OBJ',ObjFile);
  if CallAssembler(FindAssembler,s) then
   RemoveAsm
  else
   begin
      DoAssemble:=false;
      GenerateError;
   end;
end;


Procedure TExternalAssembler.AsmFlush;
begin
  if outcnt>0 then
   begin
     BlockWrite(outfile,outbuf,outcnt);
     outcnt:=0;
   end;
end;


Procedure TExternalAssembler.AsmClear;
begin
  outcnt:=0;
end;


Procedure TExternalAssembler.AsmWrite(const s:string);
begin
  if OutCnt+length(s)>=AsmOutSize then
   AsmFlush;
  Move(s[1],OutBuf[OutCnt],length(s));
  inc(OutCnt,length(s));
  inc(AsmSize,length(s));
end;


Procedure TExternalAssembler.AsmWriteLn(const s:string);
begin
  AsmWrite(s);
  AsmLn;
end;


Procedure TExternalAssembler.AsmWritePChar(p:pchar);
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


Procedure TExternalAssembler.AsmLn;
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


procedure TExternalAssembler.AsmCreate(Aplace:tcutplace);
begin
  if SmartAsm then
   NextSmartName(Aplace);
{$ifdef unix}
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


procedure TExternalAssembler.AsmClose;
var
  f : file;
  l : longint;
begin
  AsmFlush;
{$ifdef unix}
  if DoPipe then
   PClose(outfile)
  else
{$endif}
   begin
   {Touch Assembler time to ppu time is there is a ppufilename}
     if Assigned(current_module.ppufilename) then
      begin
        Assign(f,current_module.ppufilename^);
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
procedure TExternalAssembler.Synchronize;
begin
{Touch Assembler time to ppu time is there is a ppufilename}
  if Assigned(current_module.ppufilename) then
   begin
     SynchronizeFileTime(current_module.ppufilename^,asmfile);
     if not(cs_asm_extern in aktglobalswitches) then
       SynchronizeFileTime(current_module.ppufilename^,objfile);
   end;
end;


{*****************************************************************************
                     Generate Assembler Files Main Procedure
*****************************************************************************}

Procedure GenerateAsm(smart:boolean);
var
  a : TExternalAssembler;
{$ifdef i386}
  {$ifndef NoAg386Bin}
    b : TInternalAssembler;
  {$endif}
{$endif}
begin
  case aktoutputformat of
     as_none : ;
{$ifdef i386}
  {$ifndef NoAg386Bin}
     as_i386_dbg,
     as_i386_coff,
     as_i386_pecoff,
     as_i386_elf :
       begin
         case aktoutputformat of
           as_i386_dbg :
             b:=TInternalAssembler.Create(og_dbg,smart);
           as_i386_coff :
             b:=TInternalAssembler.Create(og_coff,smart);
           as_i386_pecoff :
             b:=TInternalAssembler.Create(og_pecoff,smart);
           as_i386_elf :
             b:=TInternalAssembler.Create(og_elf,smart);
         end;
         b.WriteBin;
         b.Free;
         if assigned(current_module.ppufilename) then
          begin
            if smart then
              SynchronizeFileTime(current_module.ppufilename^,current_module.staticlibfilename^)
            else
              SynchronizeFileTime(current_module.ppufilename^,current_module.objfilename^);
          end;
         exit;
       end;
  {$endif NoAg386Bin}
  {$ifndef NoAg386Att}
     as_i386_as,
     as_i386_as_aout,
     as_i386_asw :
       a:=T386ATTAssembler.create(smart);
  {$endif NoAg386Att}
  {$ifndef NoAg386Nsm}
     as_i386_nasmcoff,
     as_i386_nasmwin32,
     as_i386_nasmelf,
     as_i386_nasmobj :
       a:=T386NasmAssembler.Create(smart);
  {$endif NoAg386Nsm}
  {$ifndef NoAg386Int}
     as_i386_masm,
     as_i386_tasm :
       a:=T386IntelAssembler.Create(smart);
  {$endif NoAg386Int}
{$endif}
{$ifdef m68k}
  {$ifndef NoAg68kGas}
     as_m68k_as,
     as_m68k_gas :
       a:=new(pm68kgasasmlist,Init(smart));
  {$endif NoAg86KGas}
  {$ifndef NoAg68kMot}
     as_m68k_mot :
       a:=new(pm68kmoTExternalAssembler,Init(smart));
  {$endif NoAg86kMot}
  {$ifndef NoAg68kMit}
     as_m68k_mit :
       a:=new(pm68kmiTExternalAssembler,Init(smart));
  {$endif NoAg86KMot}
  {$ifndef NoAg68kMpw}
     as_m68k_mpw :
       a:=new(pm68kmpwasmlist,Init(smart));
  {$endif NoAg68kMpw}
{$endif}
  else
    Message(asmw_f_assembler_output_not_supported);
  end;
  a.AsmCreate(cut_normal);
  a.WriteAsmList;
  a.AsmClose;
  a.DoAssemble;
  a.synchronize;
  a.Free;
end;


Procedure OnlyAsm;
var
  a : TExternalAssembler;
begin
  a:=TExternalAssembler.Create(false);
  a.DoAssemble;
  a.Free;
end;

end.
{
  $Log$
  Revision 1.17  2001-04-13 01:22:06  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.16  2001/03/13 18:42:39  peter
    * don't create temporary smartlink dir for internalassembler

  Revision 1.15  2001/03/05 21:39:11  peter
    * changed to class with common TAssembler also for internal assembler

  Revision 1.14  2001/02/26 08:08:16  michael
  * bug correction: pipes must be closed by pclose (not close);
    There was too many not closed processes under Linux before patch.
    Test this by making a compiler under Linux with command
      OPT="-P" make
    and check a list of processes in another shell with
      ps -xa

  Revision 1.13  2001/02/20 21:36:39  peter
    * tasm/masm fixes merged

  Revision 1.12  2001/02/09 23:06:17  peter
    * fixed uninited var

  Revision 1.11  2001/02/05 20:46:59  peter
    * support linux unit for ver1_0 compilers

  Revision 1.10  2001/01/21 20:32:45  marco
   * Renamefest. Compiler part. Not that hard.

  Revision 1.9  2001/01/12 19:19:44  peter
    * fixed searching for utils

  Revision 1.8  2000/12/25 00:07:25  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.7  2000/11/13 15:26:12  marco
   * Renamefest

  Revision 1.6  2000/10/01 19:48:23  peter
    * lot of compile updates for cg11

  Revision 1.5  2000/09/24 15:06:11  peter
    * use defines.inc

  Revision 1.4  2000/08/27 16:11:49  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.3  2000/07/13 12:08:24  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:32  michael
  + removed logs

}
