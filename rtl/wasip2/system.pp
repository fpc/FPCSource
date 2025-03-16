{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2020,2021 by the Free Pascal development team.

    System unit for The WebAssembly System Interface (WASI).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit system;

interface


{$IFNDEF FPC_DISABLE_MONITOR}
{$DEFINE SYSTEM_HAS_FEATURE_MONITOR}
{$ENDIF}

{$define FPC_IS_SYSTEM}

{$ifdef FPC_WASM_THREADS}
  {$define DISABLE_NO_THREAD_MANAGER}
{$else FPC_WASM_THREADS}
  {$define USE_NOTHREADMANAGER}
{$endif FPC_WASM_THREADS}

{$I systemh.inc}

const
  LineEnding = #10;
  LFNSupport = true;
  DirectorySeparator = '/';
  DriveSeparator = '';
  ExtensionSeparator = '.';
  PathSeparator = ':';
  AllowDirectorySeparators : set of AnsiChar = ['\','/'];
  AllowDriveSeparators : set of AnsiChar = [];
{  FileNameCaseSensitive and FileNameCasePreserving are defined below! }
  maxExitCode = 65535;
  MaxPathLen = 4096;
  AllFilesMask = '*';

const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = true;
  FileNameCasePreserving: boolean = true;
  CtrlZMarksEOF: boolean = false; (* #26 not considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

var
  argc: longint;
  argv: PPAnsiChar;
{  envp: PPAnsiChar;}

implementation

var
  StkLen: SizeUInt; external name '__stklen';

{$I system.inc}

{var
  argv_size,
  argv_buf_size: __wasi_size_t;
  argv_buf: Pointer;
  environc,environ_buf_size,envp_size: __wasi_size_t;
  environ_buf: Pointer;}

function GetProcessID: SizeUInt;
begin
end;

Procedure Randomize;
Begin
End;

procedure System_exit;
begin
End;

procedure Setup_Environment;
begin
end;

procedure setup_arguments;
begin
end;

Function ParamCount: Longint;
Begin
  paramcount := 0;
End;

function paramstr(l: longint) : shortstring;
begin
  paramstr:='';
end;

procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
  result := stklen;
end;

begin
  StackLength:=CheckInitialStkLen(stklen);
  StackBottom:=Pointer(PtrUInt(InitialHeapBlockStart)-PtrUInt(StackLength));
  { To be set if this is a GUI or console application }
  IsConsole := TRUE;
{$ifdef FPC_HAS_FEATURE_DYNLIBS}
  { If dynlibs feature is disabled,
    IsLibrary is a constant, which can thus not be set to a value }
  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;
{$endif def FPC_HAS_FEATURE_DYNLIBS}
  { Setup heap }
  InitInitialHeapBlock;
  InitHeap;
  SysInitExceptions;
  initunicodestringmanager;
  { Reset IO Error }
  InOutRes:=0;
{$ifdef FPC_HAS_FEATURE_THREADING}
  InitSystemThreads;
{$ifdef FPC_WASM_THREADS}
  InitThreadVars(@WasiRelocateThreadVar);
{$endif}
{$endif}
  { Setup stdin, stdout and stderr }
  SysInitStdIO;
  Setup_Environment;
//  Setup_PreopenedDirs;
{$ifdef FPC_WASM_THREADS}
  TLSInfoBlock:=Nil;
{$endif}
end.
