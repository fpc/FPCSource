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

{$define __ARM__} (* For future usage! *)
{$define FPC_IS_SYSTEM}
{$define USE_NOTHREADMANAGER}

{$i gbabiosh.inc}

{$I systemh.inc}

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
  fake_heap_start: pchar; cvar;
  fake_heap_end: pchar; cvar;

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

{$I system.inc}

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

{ set randseed to a new pseudo random value }
procedure randomize;
begin
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


procedure InitHeap;
begin
  FillChar(freelists_fixed,sizeof(tfreelists),0);
  FillChar(freelists_free_chunk,sizeof(freelists_free_chunk),0);

  freelist_var:=nil;
  {The GBA has no operating system from which we ask memory, so we
   initialize the heap with a single block of memory.}
  freeoslistcount:=1;
  freeoslist:=pointer($2040000);
  fillchar(freeoslist^,sizeof(freeoslist^),0);
  freeoslist^.size:=$40000; {GBA heap is $40000 bytes.}
  fillchar(internal_status,sizeof(internal_status),0);
end;




begin
  StackLength := CheckInitialStkLen(InitialStkLen);
  ///StackBottom := Sptr - StackLength;
  StackBottom := StackTop - StackLength;
{ OS specific startup }
  fake_heap_start := pchar(0);
  fake_heap_end := pchar(0);
{ Set up signals handlers }

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
