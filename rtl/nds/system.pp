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
{ $define USE_NOTHREADMANAGER}
{ $define HAS_MEMORYMANAGER}
{ $undef FPC_HAS_FEATURE_TEXTIO}

{$i ndsbiosh.inc}

{$I systemh.inc}

{$define fpc_softfpu_interface}
{$i softfpu.pp}
{$undef fpc_softfpu_interface}

function IsARM9(): boolean; 
procedure InitHeapThread;

const
 LineEnding = #10;
 LFNSupport = true;
 CtrlZMarksEOF: boolean = false;
 DirectorySeparator = '/';
 DriveSeparator = ':';
 PathSeparator = ';';
 FileNameCaseSensitive = false;
 maxExitCode = 255;
 MaxPathLen = 255;


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
//  fake_heap_start: ^byte; cvar;
  fake_heap_end: ^byte; cvar; 
//  heap_start: longint; external name 'end';
//  heap_end: longint; external name '__eheap_end';
  heap_start: longint; external name 'end';
  heap_end: longint; external name 'fake_heap_end';
//  __eheap_start: longint; cvar; external;
//  fake_heap_end: longint; cvar; external;





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

{$i ndsbios.inc}

{ NDS CPU detecting function (thanks to 21o6): 
  --------------------------------------------
   "You see, the ARM7 can't write to bank A of VRAM, but it doesn't give any 
    error ... it just doesn't write there... so it's easily determinable what 
    CPU is running the code"}
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


(*
procedure InitHeap;
begin
  FillChar(freelists_fixed,sizeof(tfreelists),0);
  FillChar(freelists_free_chunk,sizeof(freelists_free_chunk),0);

  freelist_var:=nil;
  {The GBA has no operating system from which we ask memory, so we
   initialize the heap with a single block of memory.}
  freeoslistcount:=1;
  //freeoslist:=pointer($023FF000);
  freeoslist:=pointer(heap_start);
  fillchar(freeoslist^,sizeof(freeoslist^),0);
  //freeoslist^.size:=$00040000;//$003FF000;
  freeoslist^.size:=heap_end-heap_start;
  fillchar(internal_status,sizeof(internal_status),0);
end;
*)

procedure InitHeap;
var
  loc_freelists: pfreelists;
begin
  { we cannot initialize the locks here yet, thread support is
    not loaded yet }
  loc_freelists := @freelists;

  // PROVA -->
  loc_freelists^.varlist := nil;
  loc_freelists^.oscount := 1;
  loc_freelists := pointer(heap_start);

  fillchar(loc_freelists^, sizeof(tfreelists), 0);
  fillchar(orphaned_freelists, sizeof(orphaned_freelists), 0);

  loc_freelists^.oslist^.size := heap_end - heap_start;
  fillchar(loc_freelists^.internal_status, sizeof(TFPCHeapStatus), 0);  
  // <-- PROVA

end;


procedure InitHeapThread;
var
  loc_freelists: pfreelists;
begin
  loc_freelists := @freelists;
  fillchar(loc_freelists^,sizeof(tfreelists),0);
{$ifdef DUMP_MEM_USAGE}
  fillchar(sizeusage,sizeof(sizeusage),0);
  fillchar(maxsizeusage,sizeof(sizeusage),0);
{$endif}
end;

begin
  StackLength := CheckInitialStkLen(InitialStkLen);
  ///StackBottom := Sptr - StackLength;
  StackBottom := StackTop - StackLength;
{ OS specific startup }
//  fake_heap_start := pchar(0);
//  fake_heap_end := pchar(0);
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
