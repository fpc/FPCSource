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

 ****************************************************************************}

unit assemble;

interface

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  cobjects,globtype,globals,aasm;

const
{$ifdef tp}
  AsmOutSize=1024;
{$else}
  AsmOutSize=32768;
{$endif}

type
  PAsmList=^TAsmList;
  TAsmList=object
  private
    procedure CreateSmartLinkPath(const s:string);
  public
  {filenames}
    path     : pathstr;
    name     : namestr;
    asmfile,         { current .s and .o file }
    objfile,
    as_bin   : string;
    SmartAsm : boolean;
    smarthcount : longint;
    place    : TCutPlace; { special 'end' file for import dir ? }
  {outfile}
    AsmSize,
    AsmStartSize,
    outcnt   : longint;
    outbuf   : array[0..AsmOutSize-1] of char;
    outfile  : file;
    Constructor Init(smart:boolean);
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
    procedure AsmCreate(Aplace:tcutplace);
    procedure AsmClose;
    procedure Synchronize;
    procedure WriteTree(p:paasmoutput);virtual;
    procedure WriteAsmList;virtual;
  end;

var
  SmartLinkFilesCnt : longint;

Procedure GenerateAsm(smart:boolean);
Procedure OnlyAsm;


Implementation

uses
  script,files,systems,verbose
{$ifdef linux}
  ,linux
{$endif}
  ,strings
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
                                  TAsmList
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


const
  lastas  : byte=255;
var
  LastASBin : pathstr;
Function TAsmList.FindAssembler:string;
var
  asfound : boolean;
begin
  if lastas<>ord(target_asm.id) then
   begin
     lastas:=ord(target_asm.id);
     { is an assembler passed ? }
     if utilsdirectory<>'' then
       LastASBin:=FindFile(target_asm.asmbin+source_os.exeext,utilsdirectory,asfound)+
         target_asm.asmbin+source_os.exeext;
     if LastASBin='' then
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


Function TAsmList.DoAssemble:boolean;
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
        if (SmartLinkFilesCnt<=1) then
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


procedure TAsmList.NextSmartName;
var
  s : string;
begin
  inc(SmartLinkFilesCnt);
  if SmartLinkFilesCnt>999999 then
   Message(asmw_f_too_many_asm_files);
  case place of
    cut_begin :
      begin
        inc(smarthcount);
        s:=current_module^.asmprefix^+tostr(smarthcount)+'h';
      end;
    cut_normal :
      s:=current_module^.asmprefix^+tostr(smarthcount)+'s';
    cut_end :
      s:=current_module^.asmprefix^+tostr(smarthcount)+'t';
  end;
  AsmFile:=Path+FixFileName(s+tostr(SmartLinkFilesCnt)+target_info.asmext);
  ObjFile:=Path+FixFileName(s+tostr(SmartLinkFilesCnt)+target_info.objext);
  { insert in container so it can be cleared after the linking }
  SmartLinkOFiles.Insert(Objfile);
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


procedure TAsmList.AsmCreate(Aplace:tcutplace);
begin
  place:=Aplace;
  if SmartAsm then
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
begin
{Touch Assembler time to ppu time is there is a ppufilename}
  if Assigned(current_module^.ppufilename) then
   begin
     SynchronizeFileTime(current_module^.ppufilename^,asmfile);
     if not(cs_asm_extern in aktglobalswitches) then
       SynchronizeFileTime(current_module^.ppufilename^,objfile);
   end;
end;


procedure TAsmList.WriteTree(p:paasmoutput);
begin
end;


procedure TAsmList.WriteAsmList;
begin
end;


procedure TAsmList.CreateSmartLinkPath(const s:string);
var
  dir : searchrec;
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
{$ifdef fpc}
     findclose(dir);
{$endif}
     { .s files }
     findfirst(s+dirsep+'*'+target_info.asmext,anyfile,dir);
     while (doserror=0) do
      begin
        RemoveFile(s+dirsep+dir.name);
        findnext(dir);
      end;
{$ifdef fpc}
     findclose(dir);
{$endif}
   end
  else
   begin
     {$I-}
      mkdir(s);
     {$I+}
     if ioresult<>0 then;
   end;
end;


Constructor TAsmList.Init(smart:boolean);
begin
{ load start values }
  asmfile:=current_module^.asmfilename^;
  objfile:=current_module^.objfilename^;
  name:=FixFileName(current_module^.modulename^);
  OutCnt:=0;
  SmartLinkFilesCnt:=0;
  SmartLinkOFiles.Clear;
  place:=cut_normal;
  SmartAsm:=smart;
  SmartHCount:=0;
{ Which path will be used ? }
  if SmartAsm then
   begin
     path:=current_module^.outputpath^+FixFileName(current_module^.modulename^)+target_info.smartext;
     CreateSmartLinkPath(path);
     path:=FixPath(path,false);
   end
  else
   path:=current_module^.outputpath^;
end;


Destructor TAsmList.Done;
begin
end;


{*****************************************************************************
                     Generate Assembler Files Main Procedure
*****************************************************************************}

Procedure GenerateAsm(smart:boolean);
var
  a : PAsmList;
{$ifdef i386}
  {$ifndef NoAg386Bin}
    b : Pi386binasmlist;
  {$endif}
{$endif}
begin
  case aktoutputformat of
     as_none : ;
{$ifdef i386}
  {$ifndef NoAg386Bin}
     as_i386_dbg,
     as_i386_coff,
     as_i386_pecoff :
       begin
         case aktoutputformat of
           as_i386_dbg :
             b:=new(pi386binasmlist,Init(og_dbg,smart));
           as_i386_coff :
             b:=new(pi386binasmlist,Init(og_coff,smart));
           as_i386_pecoff :
             b:=new(pi386binasmlist,Init(og_pecoff,smart));
         end;
         b^.WriteBin;
         dispose(b,done);
         if assigned(current_module^.ppufilename) then
          begin
            if smart then
              SynchronizeFileTime(current_module^.ppufilename^,current_module^.staticlibfilename^)
            else
              SynchronizeFileTime(current_module^.ppufilename^,current_module^.objfilename^);
          end;
         exit;
       end;
  {$endif NoAg386Bin}
  {$ifndef NoAg386Att}
     as_i386_as,
     as_i386_as_aout,
     as_i386_asw :
       a:=new(pi386attasmlist,Init(smart));
  {$endif NoAg386Att}
  {$ifndef NoAg386Nsm}
     as_i386_nasmcoff,
     as_i386_nasmwin32,
     as_i386_nasmelf,
     as_i386_nasmobj :
       a:=new(pi386nasmasmlist,Init(smart));
  {$endif NoAg386Nsm}
  {$ifndef NoAg386Int}
     as_i386_tasm :
       a:=new(pi386intasmlist,Init(smart));
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
       a:=new(pm68kmotasmlist,Init(smart));
  {$endif NoAg86kMot}
  {$ifndef NoAg68kMit}
     as_m68k_mit :
       a:=new(pm68kmitasmlist,Init(smart));
  {$endif NoAg86KMot}
  {$ifndef NoAg68kMpw}
     as_m68k_mpw :
       a:=new(pm68kmpwasmlist,Init(smart));
  {$endif NoAg68kMpw}
{$endif}
  else
{$ifdef TP}
    exit;
{$else}
    Message(asmw_f_assembler_output_not_supported);
{$endif}
  end;
  a^.AsmCreate(cut_normal);
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
  a:=new(pasmlist,Init(false));
  a^.DoAssemble;
  dispose(a,Done);
end;

end.
{
  $Log$
  Revision 1.2  2000-07-13 11:32:32  michael
  + removed logs

}
