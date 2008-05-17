{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Francesco Lombardi.

    System unit for Gameboy Advance

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit System;

interface
{$define __ARM__}

{$define FPC_IS_SYSTEM}
{$define FPC_HAS_FEATURE_THREADING}
{$define FPC_HAS_FEATURE_CONSOLEIO}
{$define FPC_HAS_FEATURE_COMMANDARGS}
{$define FPC_HAS_FEATURE_TEXTIO}
{$define FPC_HAS_FEATURE_FILEIO}

{$i systemh.inc}
{$i gbabiosh.inc}

{$define fpc_softfpu_interface}
  {$i softfpu.pp}
{$undef fpc_softfpu_interface}


const
 LineEnding = #10;
 LFNSupport = true;
 CtrlZMarksEOF: boolean = false;
 DirectorySeparator = '/';
 DriveSeparator = ':';
 ExtensionSeparator = '.';
 PathSeparator = ';';
 AllowDirectorySeparators : set of char = ['\','/'];
 AllowDriveSeparators : set of char = [':'];
 FileNameCaseSensitive = false;
 maxExitCode = 255;
 MaxPathLen = 255;
 AllFilesMask = '*';


 sLineBreak : string[1] = LineEnding;
 DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

const
  UnusedHandle    = $ffff;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = $ffff;


var
  argc: LongInt = 0;
  argv: PPChar;
  envp: PPChar;
  errno: integer;
  fake_heap_end: ^byte; cvar; external;

procedure randomize(value: integer);

implementation

{$define fpc_softfpu_implementation}
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
{$define FPC_SYSTEM_HAS_extractFloat32Frac}
{$define FPC_SYSTEM_HAS_extractFloat32Exp}
{$define FPC_SYSTEM_HAS_extractFloat32Sign}

{$i system.inc}
{$i gbabios.inc}

{$ifdef FPC_HAS_FEATURE_PROCESSES}
function GetProcessID: SizeUInt;
begin
end;
{$endif}


{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}
procedure System_exit;
begin
end;



{*****************************************************************************
                             ParamStr/Randomize
*****************************************************************************}
const
  QRAN_SHIFT  = 15;
  QRAN_MASK   = ((1 shl QRAN_SHIFT) - 1);
  QRAN_MAX    = QRAN_MASK;
  QRAN_A      = 1664525;
  QRAN_C      = 1013904223;

{ set randseed to a new pseudo random value }
procedure randomize();
begin
  RandSeed := 63458; 
end;

procedure randomize(value: integer);
begin
  RandSeed := value; 
end;

function random(): integer; 
begin	
	RandSeed := QRAN_A * RandSeed + QRAN_C;
	random := (RandSeed shr 16) and QRAN_MAX;
end;

function random(value: integer): integer; 
var
  a: integer;
begin	
	RandSeed := QRAN_A * RandSeed + QRAN_C;
	a := (RandSeed shr 16) and QRAN_MAX;
  random := (a * value) shr 15;
end;


{$ifdef FPC_HAS_FEATURE_COMMANDARGS}  
{ number of args }
function paramcount : longint;
begin
  paramcount:=0;
end;

{ argument number l }
function paramstr(l : longint) : string;
begin
  paramstr:='';
end;
{$endif}

{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
end;
{$endif}


function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
  result := stklen;
end;


begin
  StackLength := CheckInitialStkLen(InitialStkLen);
  StackBottom := StackTop - StackLength;
{ OS specific startup }

{ Setup heap }
  InitHeap;
  SysInitExceptions;
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  { Setup stdin, stdout and stderr }
  SysInitStdIO;
{$endif FPC_HAS_FEATURE_CONSOLEIO}
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  { Reset IO Error }
  InOutRes:=0;
{$endif FPC_HAS_FEATURE_CONSOLEIO}
{$ifdef FPC_HAS_FEATURE_THREADING}
  { threading }
  InitSystemThreads;
{$endif FPC_HAS_FEATURE_THREADING}
  initvariantmanager;
end.
