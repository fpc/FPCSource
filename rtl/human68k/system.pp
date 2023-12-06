{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by Karoly Balogh

    System unit for the Human 68k (Sharp X68000)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;

interface

{$define FPC_IS_SYSTEM}
{$define FPC_STDOUT_TRUE_ALIAS}
{$define FPC_ANSI_TEXTFILEREC}
{$define FPC_SYSTEM_EXIT_NO_RETURN}
{$.define FPC_HUMAN68K_USE_TINYHEAP}

{$ifdef FPC_HUMAN68K_USE_TINYHEAP}
{$define HAS_MEMORYMANAGER}
{$endif FPC_HUMAN68K_USE_TINYHEAP}

{$i systemh.inc}
{$ifdef FPC_HUMAN68K_USE_TINYHEAP}
{$i tnyheaph.inc}
{$endif FPC_HUMAN68K_USE_TINYHEAP}

{Platform specific information}
const
    LineEnding = #13#10;
    LFNSupport = false;
    CtrlZMarksEOF: boolean = false; (* #26 not considered as end of file *)
    DirectorySeparator = '\';
    DriveSeparator = ':';
    ExtensionSeparator = '.';
    PathSeparator = ';';
    AllowDirectorySeparators : set of char = ['\','/'];
    AllowDriveSeparators : set of char = [':'];
    FileNameCaseSensitive = false;
    FileNameCasePreserving = false;
    maxExitCode = 255;
    MaxPathLen = 255;
    AllFilesMask = '*.*';

    sLineBreak = LineEnding;
    DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

const
    UnusedHandle    = -1;
    StdInputHandle: longint = 0;
    StdOutputHandle: longint = 1;
    StdErrorHandle: longint = 2;

var
    args: PChar;
    argc: LongInt;
    argv: PPChar;
    envp: PPChar;

    human68k_vernum: word;

    {$if defined(FPUSOFT)}

    {$define fpc_softfpu_interface}
    {$i softfpu.pp}
    {$undef fpc_softfpu_interface}

    {$endif defined(FPUSOFT)}


  implementation

    {$if defined(FPUSOFT)}

    {$define fpc_softfpu_implementation}
    {$define softfpu_compiler_mul32to64}
    {$define softfpu_inline}
    {$i softfpu.pp}
    {$undef fpc_softfpu_implementation}

    { we get these functions and types from the softfpu code }
    {$define FPC_SYSTEM_HAS_float64}
    {$define FPC_SYSTEM_HAS_float32}
    {$define FPC_SYSTEM_HAS_flag}
    {$define FPC_SYSTEM_HAS_extractFloat64Frac0}
    {$define FPC_SYSTEM_HAS_extractFloat64Frac1}
    {$define FPC_SYSTEM_HAS_extractFloat64Exp}
    {$define FPC_SYSTEM_HAS_extractFloat64Sign}
    {$define FPC_SYSTEM_HAS_ExtractFloat32Frac}
    {$define FPC_SYSTEM_HAS_extractFloat32Exp}
    {$define FPC_SYSTEM_HAS_extractFloat32Sign}

    {$endif defined(FPUSOFT)}

    {$i system.inc}
    {$ifdef FPC_HUMAN68K_USE_TINYHEAP}
    {$i tinyheap.inc}
    {$endif FPC_HUMAN68K_USE_TINYHEAP}


  function GetProcessID:SizeUInt;
  begin
    GetProcessID := 1;
  end;

var
  h68k_startup: Th68kdos_startup; external name '_h68k_startup';
  h68k_psp: Ph68kdos_psp; external name '_h68k_psp';

{$ifdef FPC_HUMAN68K_USE_TINYHEAP}
  initial_heap_start: pointer; public name '__initial_heap_start';
  initial_heap_end: pointer; public name '__initial_heap_end';
{$endif FPC_HUMAN68K_USE_TINYHEAP}


{*****************************************************************************
                             ParamStr
*****************************************************************************}

{ number of args }
function ParamCount: LongInt;
begin
  ParamCount:=argc;
end;

{ argument number l }
function ParamStr(l: LongInt): shortstring;
begin
  if assigned(argv) and (l >= 0) and (l <= argc) then
    ParamStr:=argv[l]
  else
    ParamStr:='';
end;

procedure GenerateArgs;
var
  argcc: longint;
  argl,pathlen,namelen: longint;
  p: pchar;
  argsp: pchar;
  inquotes: boolean;
  inarg: boolean;
  i: longint;
begin
  inquotes:=false;
  inarg:=false;

  if not assigned(h68k_startup.comm) then
    exit;

  p:=@h68k_startup.comm^.buffer;
  argl:=h68k_startup.comm^.len;

  args:=GetMem(argl+1);
  if not assigned(args) then
    exit;
  fillchar(args^,argl+1,#0);
  argsp:=args;

  for i:=0 to argl-1 do
    begin
      case p[i] of
        ' ':
          begin
            if not inquotes then
              begin
                if inarg then
                  begin
                    inc(argc);
                    inarg:=false;
                  end;
                argsp^:=#0;
              end
            else
              argsp^:=p[i];
            inc(argsp);
          end;
        '"':
          begin
            inquotes:=not inquotes;
          end;
        else
          begin
            inarg:=true;
            argsp^:=p[i];
            inc(argsp);
          end;
      end;
    end;
  if inarg then
    inc(argc);

  argv:=GetMem(argc+1);
  if not assigned(argv) then
    exit;
  argsp:=args;
  argcc:=0;
  inarg:=false;
  while (argsp < (args + argl)) and (argcc < argc) do
    begin
      if argsp^ = #0 then
        inarg:=false
      else
        if not inarg then
          begin
            inarg:=true;
            argv[argcc+1]:=argsp;
            inc(argcc);
          end;
      inc(argsp);
    end;

  pathlen:=strlen(h68k_psp^.exe_path);
  namelen:=strlen(h68k_psp^.exe_name);
  argl:=pathlen+namelen;
  argv[0]:=GetMem(argl+1);
  if not assigned(argv[0]) then
    exit;
  Move(h68k_psp^.exe_path[0],argv[0][0],pathlen);
  Move(h68k_psp^.exe_name[0],argv[0][pathlen],namelen);
  argv[0][argl]:=#0;
end;

procedure SysInitParamsAndEnv;
begin
  GenerateArgs;
end;


  procedure randomize;
  begin
    {$WARNING: randseed is uninitialized}
    randseed:=0;
  end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
procedure haltproc(e:longint); noreturn; external name '_haltproc';

Procedure system_exit; noreturn;
begin
  haltproc(ExitCode);
end;


{*****************************************************************************
                         System Unit Initialization
*****************************************************************************}

{$ifdef FPC_HUMAN68K_USE_TINYHEAP}
procedure InitHeap;
var
  aligned_heap_start: pointer;
begin
  aligned_heap_start:=align(initial_heap_start,sizeof(ttinyheapblock));
  RegisterTinyHeapBlock_Simple_Prealigned(aligned_heap_start, ptruint(initial_heap_end - aligned_heap_start));
end;
{$endif FPC_HUMAN68K_USE_TINYHEAP}


procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
{$ifndef FPC_STDOUT_TRUE_ALIAS}
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{$endif FPC_STDOUT_TRUE_ALIAS}
end;

function CheckInitialStkLen (StkLen: SizeUInt): SizeUInt;
begin
  CheckInitialStkLen := StkLen;
end;


begin
  human68k_vernum:=word(h68kdos_vernum);

  StackLength := CheckInitialStkLen (InitialStkLen);
{ Initialize ExitProc }
  ExitProc:=Nil;
{ Setup heap }
  InitHeap;
  SysInitExceptions;
{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
  InitUnicodeStringManager;
{$endif FPC_HAS_FEATURE_UNICODESTRINGS}
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
{ Setup command line arguments }
  SysInitParamsAndEnv;
{$ifdef FPC_HAS_FEATURE_THREADING}
  InitSystemThreads;
{$endif FPC_HAS_FEATURE_THREADING}
end.
