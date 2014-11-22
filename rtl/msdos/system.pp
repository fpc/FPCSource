unit system;

interface

{$DEFINE FPC_NO_DEFAULT_HEAP}

{$DEFINE FPC_INCLUDE_SOFTWARE_MUL}
{$DEFINE FPC_INCLUDE_SOFTWARE_MOD_DIV}

{$DEFINE FPC_USE_SMALL_DEFAULTSTACKSIZE}
{ To avoid warnings in thread.inc code,
  but value must be really given after
  systemh.inc is included otherwise the
  $mode switch is not effective }

{$I systemh.inc}
{$I tnyheaph.inc}

const
  LineEnding = #13#10;
  { LFNSupport is a variable here, defined below!!! }
  DirectorySeparator = '\';
  DriveSeparator = ':';
  ExtensionSeparator = '.';
  PathSeparator = ';';
  AllowDirectorySeparators : set of char = ['\','/'];
  AllowDriveSeparators : set of char = [':'];
  { FileNameCaseSensitive and FileNameCasePreserving are defined separately below!!! }
  maxExitCode = 255;
  MaxPathLen = 256;

const
{ Default filehandles }
  UnusedHandle    = $ffff;{ instead of -1, as it is a word value}
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = false;
  FileNameCasePreserving: boolean = false;
  CtrlZMarksEOF: boolean = true; (* #26 is considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

{ Default memory segments (Tp7 compatibility) }
  seg0040: Word = $0040;
  segA000: Word = $A000;
  segB000: Word = $B000;
  segB800: Word = $B800;
{ The value that needs to be added to the segment to move the pointer by
  64K bytes (BP7 compatibility) }
  SelectorInc: Word = $1000;

var
{ Mem[] support }
  mem  : array[0..$7fff-1] of byte absolute $0:$0;
  memw : array[0..($7fff div sizeof(word))-1] of word absolute $0:$0;
  meml : array[0..($7fff div sizeof(longint))-1] of longint absolute $0:$0;
{ C-compatible arguments and environment }
  argc:longint; //!! public name 'operatingsystem_parameter_argc';
  argv:PPchar; //!! public name 'operatingsystem_parameter_argv';
  envp:PPchar; //!! public name 'operatingsystem_parameter_envp';
  dos_argv0 : pchar; //!! public name 'dos_argv0';

{ The DOS Program Segment Prefix segment (TP7 compatibility) }
  PrefixSeg:Word;public name '__fpc_PrefixSeg';

  SaveInt00: FarPointer;public name '__SaveInt00';

  AllFilesMask: string [3];
{$ifndef RTLLITE}
{ System info }
  LFNSupport : boolean;
{$ELSE RTLLITE}
const
  LFNSupport = false;
{$endif RTLLITE}

implementation

procedure DebugWrite(const S: string); forward;
procedure DebugWriteLn(const S: string); forward;

const
  fCarry = 1;

  { used for an offset fixup for accessing the proc parameters in asm routines
    that use nostackframe. We can't use the parameter name directly, because
    i8086 doesn't support sp relative addressing. }
{$ifdef FPC_X86_CODE_FAR}
  extra_param_offset = 2;
{$else FPC_X86_CODE_FAR}
  extra_param_offset = 0;
{$endif FPC_X86_CODE_FAR}
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  extra_data_offset = 2;
{$else}
  extra_data_offset = 0;
{$endif}

type
  PFarByte = ^Byte;far;
  PFarChar = ^Char;far;
  PFarWord = ^Word;far;

var
  __stktop : pointer;public name '__stktop';
  __stkbottom : pointer;public name '__stkbottom';
  __nearheap_start: pointer;public name '__nearheap_start';
  __nearheap_end: pointer;public name '__nearheap_end';
  dos_version:Word;public name 'dos_version';

{$I registers.inc}

procedure Intr(IntNo: Byte; var Regs: Registers); external name 'FPC_INTR';
procedure MsDos(var Regs: Registers); external name 'FPC_MSDOS';

{ invokes int 21h with the carry flag set on entry; used for the LFN functions
  to ensure that the carry flag is set on exit on older DOS versions which don't
  support them }
procedure MsDos_Carry(var Regs: Registers); external name 'FPC_MSDOS_CARRY';

procedure InstallInterruptHandlers; external name 'FPC_INSTALL_INTERRUPT_HANDLERS';
procedure RestoreInterruptHandlers; external name 'FPC_RESTORE_INTERRUPT_HANDLERS';

function CheckNullArea: Boolean; external name 'FPC_CHECK_NULLAREA';

{$I system.inc}

{$I tinyheap.inc}

procedure DebugWrite(const S: string);
begin
  asm
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
    push ds
	lds si, S
{$else}
    mov si, S
{$endif}
{$ifdef FPC_ENABLED_CLD}
    cld
{$endif FPC_ENABLED_CLD}
    lodsb
    mov cl, al
    xor ch, ch
    jcxz @@zero_length
    mov ah, 2

@@1:
    lodsb
    mov dl, al
    int 21h
    loop @@1
@@zero_length:
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
    pop ds
{$endif}
  end ['ax','bx','cx','dx','si','di'];
end;

procedure DebugWriteLn(const S: string);
begin
  DebugWrite(S);
  DebugWrite(#13#10);
end;

{*****************************************************************************
                              ParamStr/Randomize
*****************************************************************************}

function GetProgramName: string;
var
  dos_env_seg: Word;
  ofs: Word;
  Ch, Ch2: Char;
begin
  if dos_version < $300 then
    begin
      GetProgramName := '';
      exit;
    end;
  dos_env_seg := PFarWord(Ptr(PrefixSeg, $2C))^;
  ofs := 1;
  repeat
    Ch := PFarChar(Ptr(dos_env_seg,ofs - 1))^;
    Ch2 := PFarChar(Ptr(dos_env_seg,ofs))^;
    if (Ch = #0) and (Ch2 = #0) then
      begin
        Inc(ofs, 3);
        GetProgramName := '';
        repeat
          Ch := PFarChar(Ptr(dos_env_seg,ofs))^;
          if Ch <> #0 then
            GetProgramName := GetProgramName + Ch;
          Inc(ofs);
          if ofs = 0 then
            begin
              GetProgramName := '';
              exit;
            end;
        until Ch = #0;
        exit;
      end;
    Inc(ofs);
    if ofs = 0 then
      begin
        GetProgramName := '';
        exit;
      end;
  until false;
end;


function GetCommandLine: string;
var
  len, I: Integer;
begin
  len := PFarByte(Ptr(PrefixSeg, $80))^;
  SetLength(GetCommandLine, len);
  for I := 1 to len do
    GetCommandLine[I] := PFarChar(Ptr(PrefixSeg, $80 + I))^;
end;


function GetArg(ArgNo: Integer; out ArgResult: string): Integer;
var
  cmdln: string;
  I: Integer;
  InArg: Boolean;
begin
  cmdln := GetCommandLine;
  ArgResult := '';
  I := 1;
  InArg := False;
  GetArg := 0;
  for I := 1 to Length(cmdln) do
    begin
      if not InArg and (cmdln[I] <> ' ') then
        begin
          InArg := True;
          Inc(GetArg);
        end;
      if InArg and (cmdln[I] = ' ') then
        InArg := False;
      if InArg and (GetArg = ArgNo) then
        ArgResult := ArgResult + cmdln[I];
    end;
end;


function paramcount : longint;
var
  tmpstr: string;
begin
  paramcount := GetArg(-1, tmpstr);
end;


function paramstr(l : longint) : string;
begin
  if l = 0 then
    paramstr := GetProgramName
  else
    GetArg(l, paramstr);
end;

procedure randomize;
var
  hl   : longint;
  regs : Registers;
begin
  regs.AH:=$2C;
  MsDos(regs);
  hl:=regs.DX;
  randseed:=hl*$10000+ regs.CX;
end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}

procedure system_exit;
var
  h : byte;
begin
  RestoreInterruptHandlers;
  for h:=0 to max_files-1 do
    if openfiles[h] then
      begin
{$ifdef SYSTEMDEBUG}
         writeln(stderr,'file ',opennames[h],' not closed at exit');
{$endif SYSTEMDEBUG}
         if h>=5 then
           do_close(h);
      end;
{$ifndef FPC_MM_TINY}
  if not CheckNullArea then
    writeln(stderr, 'Nil pointer assignment');
{$endif FPC_MM_TINY}
  asm
    mov al, byte [exitcode]
    mov ah, 4Ch
    int 21h
  end;
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

procedure InitDosHeap;
type
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  TPointerArithmeticType = HugePointer;
{$else}
  TPointerArithmeticType = Pointer;
{$endif}
begin
  SetMemoryManager(TinyHeapMemoryManager);
  RegisterTinyHeapBlock_Simple_Prealigned(__nearheap_start, TPointerArithmeticType(__nearheap_end) - TPointerArithmeticType(__nearheap_start));
end;

function CheckLFN:boolean;
var
  regs     : Registers;
  RootName : pchar;
  buf      : array [0..31] of char;
begin
{ Check LFN API on drive c:\ }
  RootName:='C:\';
{ Call 'Get Volume Information' ($71A0) }
  regs.AX:=$71a0;
  regs.ES:=Seg(buf);
  regs.DI:=Ofs(buf);
  regs.CX:=32;
  regs.DS:=Seg(RootName^);
  regs.DX:=Ofs(RootName^);
  MsDos_Carry(regs);
{ If carryflag=0 and LFN API bit in ebx is set then use Long file names }
  CheckLFN:=(regs.Flags and fCarry=0) and (regs.BX and $4000=$4000);
end;

procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;

function GetProcessID: SizeUInt;
begin
  GetProcessID := PrefixSeg;
end;

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
  result := stklen;
end;

begin
  StackBottom := __stkbottom;
  StackLength := __stktop - __stkbottom;
  InstallInterruptHandlers;
  DetectFPU;
  if Test8087>0 then
    SysInitFPU;
  { To be set if this is a GUI or console application }
  IsConsole := TRUE;
  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;
{ Setup heap }
  InitDosHeap;
  SysInitExceptions;
  initunicodestringmanager;
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Use LFNSupport LFN }
  LFNSupport:=CheckLFN;
  if LFNSupport then
   begin
    FileNameCasePreserving:=true;
    AllFilesMask := '*';
   end
  else
   AllFilesMask := '*.*';
{ Reset IO Error }
  InOutRes:=0;
end.
