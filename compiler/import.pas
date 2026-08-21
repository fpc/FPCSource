{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements an uniform import object

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
unit import;

{$i fpcdefs.inc}

interface

uses
  globtype,systemstypes,systems,aasmdata,compilerbase;

type
   timportlib=class
   private
      FCompiler: TCompilerBase;
      notsupmsg : boolean;
      procedure NotSupported;
   protected
      property Compiler: TCompilerBase read FCompiler;
   public
      constructor Create(ACompiler: TCompilerBase);virtual;
      destructor Destroy;override;
      procedure generatelib(AsmData: TAsmData);virtual;
   end;

   TDLLScanner=class
   private
     FCompiler: TCompilerBase;
   protected
     function FindDLL(const s:TCmdStr;var founddll:TCmdStr):boolean;
     property Compiler: TCompilerBase read FCompiler;
   public
     constructor Create(ACompiler: TCompilerBase);virtual;
     function Scan(const binname:string):boolean;virtual;abstract;
   end;

   TImportLibClass=class of TImportLib;
   TDLLScannerClass=class of TDLLScanner;

procedure RegisterImport(t:tsystem;c:TImportLibClass);
procedure RegisterDLLScanner(t:tsystem;c:TDLLScannerClass);
function CreateImport(ACompiler: TCompilerBase): TImportLib;
function SystemHasDLLScanner(t:tsystem): Boolean;
function CreateDLLScanner(t:tsystem;ACompiler: TCompilerBase): TDLLScanner;


implementation

uses
  sysutils,cfileutl,
  verbose,globals,compiler;

var
  CImportLib  : array[tsystem] of TImportLibClass;
  CDLLScanner : array[tsystem] of TDLLScannerClass;

{****************************************************************************
                              TImportLib
****************************************************************************}

constructor timportlib.Create(ACompiler: TCompilerBase);
begin
  FCompiler:=ACompiler;
  notsupmsg:=false;
end;


destructor timportlib.Destroy;
begin
end;


procedure timportlib.NotSupported;
begin
  { show the message only once }
  if not notsupmsg then
   begin
     compiler.verbose.Message(exec_e_dll_not_supported);
     notsupmsg:=true;
   end;
end;


procedure timportlib.generatelib(AsmData: TAsmData);
begin
  NotSupported;
end;

{****************************************************************************
                              TDLLScanner
****************************************************************************}

constructor TDLLScanner.Create(ACompiler: TCompilerBase);
begin
  inherited Create;
  FCompiler:=ACompiler;
end;


{ searches a (windows) DLL file }
function TDLLScanner.FindDLL(const s:TCmdStr;var founddll:TCmdStr):boolean;
var
  sysdir : TCmdStr;
  Found : boolean;
begin
  Found:=false;
  { Look for DLL in:
    1. Current dir
    2. Library Path
    3. windir,windir/system,windir/system32 }
  Found:=compiler.CFileUtl.FindFile(s,'.'+source_info.DirSep,false,founddll);
  if (not found) then
   Found:=compiler.globals.librarysearchpath.FindFile(s,false,founddll);

  { when cross compiling, it is pretty useless to search windir etc. for dlls }
  if (not found) and (source_info.system=compiler.target.info.system) then
   begin
     sysdir:=compiler.CFileUtl.FixPath(GetEnvironmentVariable('windir'),false);
     Found:=compiler.CFileUtl.FindFile(s,sysdir+';'+sysdir+'system'+source_info.DirSep+';'+sysdir+'system32'+source_info.DirSep,false,founddll);
   end;
  if (not found) then
   begin
     compiler.verbose.Message1(exec_w_libfile_not_found,s);
     FoundDll:=s;
   end;
  FindDll:=Found;
end;

{*****************************************************************************
                                 Init/Done
*****************************************************************************}

procedure RegisterImport(t:tsystem;c:TImportLibClass);
begin
  CImportLib[t]:=c;
end;


procedure RegisterDLLScanner(t:tsystem;c:TDLLScannerClass);
begin
  CDLLScanner[t]:=c;
end;


function CreateImport(ACompiler: TCompilerBase): TImportLib;
begin
  if assigned(CImportLib[ACompiler.target.info.system]) then
    result:=CImportLib[ACompiler.target.info.system].Create(ACompiler)
  else
    result:=TImportLib.Create(ACompiler);
end;

function SystemHasDLLScanner(t:tsystem): Boolean;
begin
  result:=assigned(CDLLScanner[t]);
end;

function CreateDLLScanner(t:tsystem;ACompiler: TCompilerBase): TDLLScanner;
begin
  result:=CDLLScanner[t].Create(ACompiler);
end;

end.
