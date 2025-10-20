unit system;

interface

{$define FPC_IS_SYSTEM}

{ The heap for Oric is implemented
  in tinyheap.inc include file,
  but it uses default SysGetMem names }

{$define HAS_MEMORYMANAGER}

{ Use AnsiChar for files }
{$define FPC_ANSI_TEXTFILEREC}
{$define FPC_STDOUT_TRUE_ALIAS}
{$define FPC_STDERR_IS_ALIAS_FOR_STDOUT}

{$I systemh.inc}
{$I tnyheaph.inc}

{$ifndef FPUNONE}
{$ifdef FPC_HAS_FEATURE_SOFTFPU}

{$define fpc_softfpu_interface}
{$i softfpu.pp}
{$undef fpc_softfpu_interface}

{$endif FPC_HAS_FEATURE_SOFTFPU}

{$endif FPUNONE}

var
{ Mem[] support }
  mem  : array[0..$7fff-1] of byte absolute $0;
{  memw : array[0..($7fff div sizeof(word))-1] of word absolute $0:$0;
  meml : array[0..($7fff div sizeof(longint))-1] of longint absolute $0:$0;}

implementation

const
  LineEnding = #13;
  { LFNSupport is a variable here, defined below!!! }
  DirectorySeparator = '\';
  DriveSeparator = ':';
  ExtensionSeparator = '.';
  PathSeparator = ';';
  AllowDirectorySeparators : set of AnsiChar = ['\','/'];
  AllowDriveSeparators : set of AnsiChar = [':'];
  { FileNameCaseSensitive and FileNameCasePreserving are defined separately below!!! }
  maxExitCode = 255;
  MaxPathLen = 256;

{ Default filehandles }
  UnusedHandle    = $ffff;{ instead of -1, as it is a word value}
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = false;
  FileNameCasePreserving: boolean = false;
  CtrlZMarksEOF: boolean = true; (* #26 is considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCR;

var
  fpc_stackarea_start: word; external name '__fpc_stackarea_start';
  fpc_stackarea_end: word; external name '__fpc_stackarea_end';
  __heapsize: Word;external name '__heapsize';
  __fpc_initialheap: array[0..0] of byte;external name '__fpc_initialheap';

{$I system.inc}
{$I tinyheap.inc}

{$ifndef FPUNONE}
{$ifdef FPC_HAS_FEATURE_SOFTFPU}

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
{$define FPC_SYSTEM_HAS_extractFloat64Frac}
{$define FPC_SYSTEM_HAS_extractFloat64Sign}
{$define FPC_SYSTEM_HAS_ExtractFloat32Frac}
{$define FPC_SYSTEM_HAS_extractFloat32Exp}
{$define FPC_SYSTEM_HAS_extractFloat32Sign}

{$endif FPC_HAS_FEATURE_SOFTFPU}
{$endif FPUNONE}

procedure randomize;
begin
end;

function GetProcessID: SizeUInt;
begin
  GetProcessID:=0;
end;

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
  result := stklen;
end;

procedure system_exit;
begin
  repeat
  until false;
end;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

procedure InitZXHeap;
begin
//  RegisterTinyHeapBlock_Simple_Prealigned(@__fpc_initialheap,__heapsize);
end;

procedure SysInitStdIO;
begin
(*  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
{$ifndef FPC_STDERR_IS_ALIAS_FOR_STDOUT}
  OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
{$endif FPC_STDERR_IS_ALIAS_FOR_STDOUT}
{$ifndef FPC_STDOUT_TRUE_ALIAS}
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{$endif FPC_STDOUT_TRUE_ALIAS}*)
end;

begin
(*  StackBottom:=@fpc_stackarea_start;
  StackLength:=(@fpc_stackarea_end-@fpc_stackarea_start)+1;
  { To be set if this is a GUI or console application }
  IsConsole := TRUE;
{$ifdef FPC_HAS_FEATURE_DYNLIBS}
  { If dynlibs feature is disabled,
    IsLibrary is a constant, which can thus not be set to a value }
  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;
{$endif def FPC_HAS_FEATURE_DYNLIBS}
{ Setup heap }
  InitZXHeap;
  SysInitExceptions;
{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
  initunicodestringmanager;
{$endif def FPC_HAS_FEATURE_UNICODESTRINGS}
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
{$ifdef FPC_HAS_FEATURE_THREADING}
  InitSystemThreads;
{$endif}*)
end.
