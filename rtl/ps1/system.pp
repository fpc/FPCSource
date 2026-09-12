unit system;
interface

{$define FPC_IS_SYSTEM}
{$define FPCRTL_FILESYSTEM_SINGLE_BYTE_API}


{$I systemh.inc}

//const
{$ifdef FPC_HAS_FEATURE_EXITCODE}
  maxExitCode = 255;
{$endif}

{$ifdef FPC_HAS_FEATURE_FILEIO}
  AllowDirectorySeparators : set of AnsiChar = ['\','/'];
  AllowDriveSeparators : set of AnsiChar = [':'];
  DirectorySeparator = '/';
  DriveSeparator = ':';
  ExtensionSeparator = '.';
  PathSeparator = ':';
  MaxPathLen = 255;
  LFNSupport = true;
  FileNameCaseSensitive = true;
  FileNameCasePreserving = true;
  AllFilesMask = '*';
{$endif}

{$if defined(FPC_HAS_FEATURE_TEXTIO) or defined(FPC_HAS_FEATURE_FILEIO)}
  UnusedHandle    = $ffff;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;
{$endif}

{$ifdef FPC_HAS_FEATURE_TEXTIO}
  CtrlZMarksEOF: boolean = true;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;
  LineEnding = #10;
  sLineBreak = #10;
{$endif}

{$ifdef FPC_HAS_FEATURE_COMMANDARGS}
var
  argc: LongInt = 0;
  argv: PPAnsiChar = nil;
  envp: PPAnsiChar = nil;
{$endif}

procedure gte_Command(const AValue: DWord); [internproc:fpc_in_gtecommand_x];

{$ifdef FPC_HAS_FEATURE_SOFTFPU}
  {$define fpc_softfpu_interface}
  {$i softfpu.pp}
  {$undef fpc_softfpu_interface}
{$endif}

var
  PS1RandSeed: LongWord = $12345678;
procedure Randomize;
function Random(l: LongInt): LongInt;

implementation

var
  StkLen : SizeUInt; external name '__stklen';
  bss_end: record end; external name '__bss_end__';

  
{$ifdef FPC_HAS_FEATURE_TEXTIO}
function pcsxPresent: Boolean;
begin
  Result := PDWord($1f802080)^ = $58534350;
end;

procedure _putchar(ch: Char);
begin
  if pcsxPresent then
    PByte($1f802080)^ := Byte(ch);
end;
{$endif}


{$if defined(FPC_HAS_FEATURE_CLASSES) and not defined(FPC_HAS_FEATURE_EXCEPTIONS)}

function RaiseList: PExceptObject;
begin
  Result := nil;
end;

function AcquireExceptionObject: Pointer;
begin
  Result := nil;
end;

procedure ReleaseExceptionObject;
begin
end;

{$endif}


{$ifdef FPC_HAS_FEATURE_SOFTFPU}

  {$define fpc_softfpu_implementation}
  {$i softfpu.pp}
  {$undef fpc_softfpu_implementation}

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

{$define HAS_MEMORYMANAGER}

{$IMPLICITEXCEPTIONS OFF}

{$I system.inc}


procedure Randomize;
begin
  PS1RandSeed :=
    PS1RandSeed xor
    LongWord(PtrUInt(@PS1RandSeed)) xor
    $A5A5A5A5;

  if PS1RandSeed = 0 then
    PS1RandSeed := $12345678;
end;


function Random(l: LongInt): LongInt;
var
  r: LongWord;
begin
  r := PS1RandSeed;

  r := r xor (r shl 13);
  r := r xor (r shr 17);
  r := r xor (r shl 5);

  PS1RandSeed := r;

  if l > 0 then
    Result := LongInt(r mod LongWord(l))
  else
    Result := 0;
end;


{$ifdef FPC_HAS_FEATURE_PROCESSES}
function GetProcessID: LongWord;
begin
  Result := 0;
end;
{$endif}

{$ifdef FPC_HAS_FEATURE_COMMANDARGS}
function ParamCount: LongInt;
begin
  Result := 0;
end;

function ParamStr(l: LongInt): ShortString;
begin
  Result := '';
end;
{$endif}

{$ifdef FPC_HAS_FEATURE_TEXTIO}
procedure SysInitStdIO;
begin
  OpenStdIO(Input,     fmInput,  StdInputHandle);
  OpenStdIO(Output,    fmOutput, StdOutputHandle);
  OpenStdIO(ErrOutput, fmOutput, StdErrorHandle);
  OpenStdIO(StdOut,    fmOutput, StdOutputHandle);
  OpenStdIO(StdErr,    fmOutput, StdErrorHandle);
end;
{$endif}

function CheckInitialStkLen(stklen: SizeUInt): SizeUInt;
const
  MinHeap = 1024;
var
  MaxStack: SizeInt;
begin
  MaxStack := SizeInt(PtrUInt($801ffff0) - PtrUInt(@bss_end)) - MinHeap;
  if MaxStack < 0 then
    MaxStack := 0;

  if stklen < SizeUInt(MaxStack) then
    Result := stklen
  else
    Result := SizeUInt(MaxStack);
end;

procedure system_exit;
begin
  repeat
  until false;
end;

begin
  StackLength := CheckInitialStkLen(StkLen);
  StackBottom := Pointer(PtrUInt($801ffff0) - PtrUInt(StackLength));

{$ifdef FPC_HAS_FEATURE_TEXTIO}
  IsConsole := True;
{$else}
  IsConsole := False;
{$endif}

{$ifdef FPC_HAS_FEATURE_HEAP}
  _InitHeap(PtrUInt(@bss_end), PtrUInt(StackBottom));
  InitHeap;
{$endif}

{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
  SysInitExceptions;
{$endif}

{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
  InitUnicodeStringManager;
{$endif}

{$ifdef FPC_HAS_FEATURE_TEXTIO}
  SysInitStdIO;
  InOutRes := 0;
{$endif}

{$ifdef FPC_HAS_FEATURE_THREADING}
  InitSystemThreads;
{$endif}
end.
