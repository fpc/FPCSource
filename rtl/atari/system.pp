{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 the Free Pascal development team

    Portions based on the Atari RTL for FPC 1.x
    Copyright (c) 1999-2000 by Carl Eric Codere
    member of the Free Pascal development team

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
{$define FPC_SYSTEM_NO_VERBOSE_UNICODEERROR}

{.$define FPC_ATARI_USE_TINYHEAP}
{$ifdef FPC_ATARI_USE_TINYHEAP}
{$define HAS_MEMORYMANAGER}
{$endif FPC_ATARI_USE_TINYHEAP}

{$i systemh.inc}
{$ifdef FPC_ATARI_USE_TINYHEAP}
{$i tnyheaph.inc}
{$endif FPC_ATARI_USE_TINYHEAP}

{Platform specific information}
const
    LineEnding = #13#10;
    LFNSupport = false;
    CtrlZMarksEOF: boolean = false; (* #26 not considered as end of file *)
    DirectorySeparator = '\';
    DriveSeparator = ':';
    ExtensionSeparator = '.';
    PathSeparator = ';';
    AllowDirectorySeparators : set of AnsiChar = ['\','/'];
    AllowDriveSeparators : set of AnsiChar = [':'];
    FileNameCaseSensitive = false;
    FileNameCasePreserving = false;
    maxExitCode = 255;
    MaxPathLen = 255;
    AllFilesMask = '*.*';

    sLineBreak = LineEnding;
    DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

const
    UnusedHandle    = $ffff;
    StdInputHandle  = 0;
    StdOutputHandle = 1;
    StdErrorHandle  = 2;

var
    args: PAnsiChar;
    argc: LongInt;
    argv: PPAnsiChar;
    envp: PPAnsiChar;
    AppFlag: Boolean;			{ Application or Accessory				}


    {$if defined(FPUSOFT)}

    {$define fpc_softfpu_interface}
    {$i softfpu.pp}
    {$undef fpc_softfpu_interface}

    {$endif defined(FPUSOFT)}

  implementation

    {$define FPC_SYSTEM_HAS_STACKTOP}
    {$define FPC_SYSTEM_HAS_BACKTRACESTR}

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
    {$ifdef FPC_ATARI_USE_TINYHEAP}
    {$i tinyheap.inc}
    {$endif FPC_ATARI_USE_TINYHEAP}
    {$i syspara.inc}



  function GetProcessID:SizeUInt;
  begin
    {$WARNING To be checked by platform maintainer}
    GetProcessID := 1;
  end;


  procedure SysInitParamsAndEnv;
  begin
    // [0] index contains the args length...
    args:=@basepage^.p_cmdlin[0];
    GenerateArgs;
  end;


  procedure randomize;
  begin
    {$WARNING: randseed initial value is 24bit}
    randseed:=xbios_random;
  end;

function fpGetEnv(const envvar : ShortString): RawByteString; public name '_fpc_atari_getenv';
  var
    hp : PAnsiChar;
    i : longint;
    upperenv, str : RawByteString;
begin
   fpGetEnv:='';
   hp:=basepage^.p_env;
   if (hp=nil) then
      exit;
   upperenv:=upcase(envvar);
   while hp^<>#0 do
     begin
        str:=hp;
        i:=pos('=',str);
        if upcase(copy(str,1,i-1))=upperenv then
          begin
             fpGetEnv:=copy(str,i+1,length(str)-i);
             break;
          end;
        { next string entry}
        hp:=hp+strlen(hp)+1;
     end;
end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
procedure haltproc(e:longint); cdecl; external name 'haltproc'; noreturn;

Procedure system_exit; noreturn;
begin
  haltproc(ExitCode);
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

Procedure ConsoleRead(var t:TextRec);
var
  dosResult: longint;
Begin
  dosResult:=gemdos_fread(t.Handle,t.BufSize,t.Bufptr);
  t.BufPos:=0;
  { Reading from console on TOS does not include the terminating CR/LF }
  if (dosResult >= 0) then
    begin
      t.BufEnd := dosResult;
      if (dosResult>=1) and (t.Bufptr^[dosResult-1] = #10) then
        begin end
      else
      if (t.BufEnd < t.BufSize) then
        begin
          t.BufPtr^[t.BufEnd] := #13;
          inc(t.BufEnd);
        end;
      if (t.BufEnd < t.BufSize) then
        begin
          t.BufPtr^[t.BufEnd] := #10;
          inc(t.BufEnd);
        end;
    end
  else
    Error2InOutRes(dosResult);
End;

procedure myOpenStdIO(var f:text;mode:longint;hdl:thandle);
begin
  OpenStdIO(f, mode, hdl);
  if (InOutRes=0) and (Mode=fmInput) and Do_Isdevice(hdl) then
  begin
    TextRec(f).InOutFunc:=@ConsoleRead;
  end;
end;

procedure SysInitStdIO;
begin
  myOpenStdIO(Input,fmInput,StdInputHandle);
  myOpenStdIO(Output,fmOutput,StdOutputHandle);
  myOpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
{$ifndef FPC_STDOUT_TRUE_ALIAS}
  myOpenStdIO(StdOut,fmOutput,StdOutputHandle);
  myOpenStdIO(StdErr,fmOutput,StdErrorHandle);
{$endif FPC_STDOUT_TRUE_ALIAS}
end;

function CheckInitialStkLen (StkLen: SizeUInt): SizeUInt;
begin
  CheckInitialStkLen := StkLen;
end;


begin
  StackLength := CheckInitialStkLen (InitialStkLen);
  StackBottom := StackTop - StackLength;
  StackMargin := min(align(StackLength div 20,2),STACK_MARGIN_MAX);
{ Initialize ExitProc }
  ExitProc:=Nil;
{$ifndef FPC_ATARI_USE_TINYHEAP}
{ Setup heap }
  InitHeap;
{$endif FPC_ATARI_USE_TINYHEAP}
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
