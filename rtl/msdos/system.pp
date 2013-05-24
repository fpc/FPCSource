unit system;

{$ASMMODE intel}

interface

{$DEFINE FPC_INCLUDE_SOFTWARE_MUL}
{$DEFINE FPC_INCLUDE_SOFTWARE_MOD_DIV}

{$DEFINE FPC_USE_SMALL_DEFAULTSTACKSIZE}
{ To avoid warnings in thread.inc code,
  but value must be really given after
  systemh.inc is included otherwise the
  $mode switch is not effective }

{$I systemh.inc}

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
  seg0040 = $0040;
  segA000 = $A000;
  segB000 = $B000;
  segB800 = $B800;

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

  dos_psp:Word;public name 'dos_psp';
  __stkbottom : pointer;public name '__stkbottom';
  __nearheap_start: pointer;public name '__nearheap_start';
  __nearheap_end: pointer;public name '__nearheap_end';

  AllFilesMask: string [3];
{$ifndef RTLLITE}
{ System info }
  LFNSupport : boolean;
{$ELSE RTLLITE}
const
  LFNSupport = false;
{$endif RTLLITE}

procedure DebugWrite(const S: string);
procedure DebugWriteLn(const S: string);

implementation

const
  fCarry = 1;

var
  dos_version:Word;public name 'dos_version';

{$I registers.inc}

procedure Intr(IntNo: Byte; var Regs: Registers); external name 'FPC_INTR';
procedure MsDos(var Regs: Registers); external name 'FPC_MSDOS';

{ invokes int 21h with the carry flag set on entry; used for the LFN functions
  to ensure that the carry flag is set on exit on older DOS versions which don't
  support them }
procedure MsDos_Carry(var Regs: Registers); external name 'FPC_MSDOS_CARRY';

{$I system.inc}

{$I tinyheap.inc}

procedure DebugWrite(const S: string);
begin
  asm
    mov si, S
    lodsb
    mov cl, al
    xor ch, ch
    mov ah, 2

@@1:
    lodsb
    mov dl, al
    int 21h
    loop @@1
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
type
  PFarByte = ^Byte;far;
  PFarWord = ^Word;far;
var
  dos_env_seg: Word;
  ofs: Word;
  Ch, Ch2: Char;
begin
  dos_env_seg := PFarWord(Ptr(dos_psp, $2C))^;
  ofs := 1;
  repeat
    Ch := Chr(PFarByte(Ptr(dos_env_seg,ofs - 1))^);
    Ch2 := Chr(PFarByte(Ptr(dos_env_seg,ofs))^);
    if (Ch = #0) and (Ch2 = #0) then
      begin
        Inc(ofs, 3);
        GetProgramName := '';
        repeat
          Ch := Chr(PFarByte(Ptr(dos_env_seg,ofs))^);
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

function paramcount : longint;
begin
  paramcount := 0;
end;


function paramstr(l : longint) : string;
begin
  if l = 0 then
    paramstr := GetProgramName
  else
    paramstr := '';
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
  for h:=0 to max_files-1 do
    if openfiles[h] then
      begin
{$ifdef SYSTEMDEBUG}
         writeln(stderr,'file ',opennames[h],' not closed at exit');
{$endif SYSTEMDEBUG}
         if h>=5 then
           do_close(h);
      end;
  asm
    mov al, byte [exitcode]
    mov ah, 4Ch
    int 21h
  end;
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

procedure InitNearHeap;
begin
  SetMemoryManager(TinyHeapMemoryManager);
  RegisterTinyHeapBlock(__nearheap_start, ptruint(__nearheap_end) - ptruint(__nearheap_start));
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
  GetProcessID := dos_psp;
end;

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
  result := stklen;
end;

begin
  StackLength := CheckInitialStkLen(InitialStkLen);
  StackBottom := __stkbottom;
  if DetectFPU then
    SysInitFPU;
  { To be set if this is a GUI or console application }
  IsConsole := TRUE;
  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;
{ Setup heap }
  InitNearHeap;
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
  initvariantmanager;
  Writeln(dos_version);
end.
