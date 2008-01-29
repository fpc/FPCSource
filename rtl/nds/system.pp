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
  fake_heap_end: ^byte; cvar;

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
  NDS CPU detecting function (thanks to 21o6):
  --------------------------------------------
   "You see, the ARM7 can't write to bank A of VRAM, but it doesn't give any
    error ... it just doesn't write there... so it's easily determinable what
    CPU is running the code"

   ARM946E-S processor can handle dsp extensions extensions, but ARM7TDMI does
   not. FPC can't retrieve the CPU target at compiling time, so this small
   function takes care to check if the code is running on an ARM9 or on an ARM7
   CPU. It works on Nintendo DS only, I guess :)
}
function IsARM9(): boolean;
var
  Dummy : pword absolute $06800000;
  tmp: word;
begin
  tmp := Dummy^;
  Dummy^ := $C0DE;
  IsARM9 := Dummy^ = $C0DE;
  Dummy^ := tmp;
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

{ set randseed to a new pseudo random value }
procedure randomize;
begin
  // Boo!
end;

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
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
{ Arguments }
  InitSystemThreads;
  initvariantmanager;
end.
