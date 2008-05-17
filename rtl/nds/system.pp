{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Francesco Lombardi.

    System unit for Nintendo DS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit System;

interface

{$define FPC_IS_SYSTEM}
{$define FPC_HAS_FEATURE_THREADING}
{$define FPC_HAS_FEATURE_CONSOLEIO}
{$define FPC_HAS_FEATURE_COMMANDARGS}
{$define FPC_HAS_FEATURE_TEXTIO}
{$define FPC_HAS_FEATURE_FILEIO}


{$i ndsbiosh.inc}
{$i systemh.inc}

{$define fpc_softfpu_interface}
{$i softfpu.pp}
{$undef fpc_softfpu_interface}

function IsARM9(): boolean;

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

  sLineBreak: string[1] = LineEnding;
  DefaultTextLineBreakStyle: TTextLineBreakStyle = tlbsCRLF;

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
  irq_vector: integer; external name '__irq_vector';
  


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
{$define FPC_SYSTEM_HAS_ExtractFloat32Frac}
{$define FPC_SYSTEM_HAS_extractFloat32Exp}
{$define FPC_SYSTEM_HAS_extractFloat32Sign}

{$i system.inc}
{$i ndsbios.inc}


{
  NDS CPU detecting function
  --------------------------
   ARM946E-S processor can handle dsp extensions, but ARM7TDMI does not. FPC can 
   detect dsp by catching a SIGILL that fires when ARM7 cpu tries to use a dsp 
   command. Unfortunately, NDS' rtl does not have any error catching mechanism.
   This function takes care to check if the code is running on an ARM9 or on an 
   ARM7 CPU, by checking the IRQ vector address ($0B003FFC for ARM9, 0380fff8 
   for ARM7), declared in the linker script. This function is cleaner than the
   older one, because does not raise any memory writing error.  
   It works on Nintendo DS only, I guess :)
}
function IsARM9(): boolean;
begin
  IsARM9 := integer(@irq_vector) = $0B003FFC;
end;

{$ifdef FPC_HAS_FEATURE_PROCESSES}
function GetProcessID: SizeUInt;
begin
  GetProcessID := 0;
end;
{$endif}


{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}
procedure System_exit;
begin
  // Boo!
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
procedure randomize;
var
  IPC_Timer: array [0..2] of byte absolute $27FF01B;
begin
  RandSeed := (IPC_Timer[0]  * 3600) + (IPC_Timer[1] * 60) + IPC_Timer[2]; 
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
  paramcount := 0;
end;

{ argument number l }
function paramstr(l : longint) : string;
begin
  paramstr := '';
end;
{$endif FPC_HAS_FEATURE_COMMANDARGS}

{$ifdef FPC_HAS_FEATURE_TEXTIO}
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

{ Set up signals handlers }
  if IsARM9 then
    fpc_cpucodeinit;

{ Setup heap }
  InitHeap;
  SysInitExceptions;
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  { Setup stdin, stdout and stderr }
  SysInitStdIO;
  { Reset IO Error }
  InOutRes:=0;
{$endif FPC_HAS_FEATURE_CONSOLEIO}
{ Arguments }
{$ifdef FPC_HAS_FEATURE_THREADING}
  { threading }
  InitSystemThreads;
{$endif FPC_HAS_FEATURE_THREADING}
  initvariantmanager;
end.
