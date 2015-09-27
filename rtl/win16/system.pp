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

{$DEFINE HAS_CMDLINE}

{$I systemh.inc}
{$IFDEF FPC_X86_DATA_NEAR}
{$I locheaph.inc}
{$ELSE FPC_X86_DATA_NEAR}
{$I glbheaph.inc}
{$ENDIF FPC_X86_DATA_NEAR}

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
{  seg0040: Word = $0040;
  segA000: Word = $A000;
  segB000: Word = $B000;
  segB800: Word = $B800;}

type
  LPSTR = ^Char;far;
  PFarChar = ^Char;far;
  PHugeChar = ^Char;huge;

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

{ BP7 compatible windows variables }
{ In C, these are the parameters to WinMain }
  CmdLine: LPSTR;public name '__fpc_CmdLine';
  CmdShow: SmallInt;public name '__fpc_CmdShow';
  HInstance: Word{HINST};public name '__fpc_HInstance';
  HPrevInst: Word{HINST};public name '__fpc_HPrevInst';
{ The value that needs to be added to the segment to move the pointer by
  64K bytes (BP7 compatibility) }
  SelectorInc: Word;public name '__fpc_SelectorInc';

{  SaveInt00: FarPointer;public name '__SaveInt00';}

  AllFilesMask: string [3];
{$ifndef RTLLITE}
{ System info }
  LFNSupport : boolean;
{$ELSE RTLLITE}
const
  LFNSupport = false;
{$endif RTLLITE}


implementation

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
  PFarWord = ^Word;far;

  { structure, located at DS:0, initialized by InitTask }
  PAutoDataSegHeader = ^TAutoDataSegHeader;
  TAutoDataSegHeader = record
    null: Word;
    oOldSP: Word;
    hOldSS: Word;
    pLocalHeap: Word;
    pAtomTable: Word;
    pStackTop: Word;
    pStackMin: Word;
    pStackBot: Word;
  end;

{$I registers.inc}

procedure MsDos(var Regs: Registers); external name 'FPC_MSDOS';

{ invokes int 21h with the carry flag set on entry; used for the LFN functions
  to ensure that the carry flag is set on exit on older DOS versions which don't
  support them }
procedure MsDos_Carry(var Regs: Registers); external name 'FPC_MSDOS_CARRY';

{$define SYSTEMUNIT}
{$I wintypes.inc}
{$I winprocsh.inc}
{$I winprocs.inc}

{$I system.inc}

{$IFDEF FPC_X86_DATA_NEAR}
{$I locheap.inc}
{$ELSE FPC_X86_DATA_NEAR}
{$I glbheap.inc}
{$ENDIF FPC_X86_DATA_NEAR}


{*****************************************************************************
                              ParamStr/Randomize
*****************************************************************************}

{function GetProgramName: string;
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
end;}


function GetArg(ArgNo: Integer; out ArgResult: string): Integer;
var
  I: Integer;
  InArg: Boolean;
begin
  ArgResult := '';
  I := 0;
  InArg := False;
  GetArg := 0;
  while CmdLine[I]<>#0 do
    begin
      if not InArg and (CmdLine[I] <> ' ') then
        begin
          InArg := True;
          Inc(GetArg);
        end;
      if InArg and (CmdLine[I] = ' ') then
        InArg := False;
      if InArg and (GetArg = ArgNo) then
        ArgResult := ArgResult + CmdLine[I];
      Inc(I);
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
    paramstr := ''{GetProgramName}
  else
    GetArg(l, paramstr);
end;

procedure randomize;
{var
  hl   : longint;
  regs : Registers;}
begin
{  regs.AH:=$2C;
  MsDos(regs);
  hl:=regs.DX;
  randseed:=hl*$10000+ regs.CX;}
end;

{****************************************************************************
                    Error Message writing using messageboxes
****************************************************************************}

const
  ErrorBufferLength = 1024;
  ErrorMessageBoxFlags = MB_OK or MB_ICONHAND or MB_TASKMODAL;
var
  ErrorBuf : array[0..ErrorBufferLength] of char;
  ErrorLen : SizeInt;

procedure ErrorWrite(Var F: TextRec);
{
  An error message should always end with #13#10#13#10
}
var
  i : SizeInt;
Begin
  while F.BufPos>0 do
    begin
      begin
        if F.BufPos+ErrorLen>ErrorBufferLength then
          i:=ErrorBufferLength-ErrorLen
        else
          i:=F.BufPos;
        Move(F.BufPtr^,ErrorBuf[ErrorLen],i);
        inc(ErrorLen,i);
        ErrorBuf[ErrorLen]:=#0;
      end;
      if ErrorLen=ErrorBufferLength then
        begin
          if not NoErrMsg then
{$IFDEF FPC_X86_DATA_NEAR}
            MessageBox(0,Ptr(Seg(ErrorBuf),Ofs(ErrorBuf)),nil,ErrorMessageBoxFlags);
{$ELSE FPC_X86_DATA_NEAR}
            MessageBox(0,@ErrorBuf,nil,ErrorMessageBoxFlags);
{$ENDIF FPC_X86_DATA_NEAR}
          ErrorLen:=0;
        end;
      Dec(F.BufPos,i);
    end;
End;


procedure ErrorClose(Var F: TextRec);
begin
  if ErrorLen>0 then
    begin
{$IFDEF FPC_X86_DATA_NEAR}
      MessageBox(0,Ptr(Seg(ErrorBuf),Ofs(ErrorBuf)),nil,ErrorMessageBoxFlags);
{$ELSE FPC_X86_DATA_NEAR}
      MessageBox(0,@ErrorBuf,nil,ErrorMessageBoxFlags);
{$ENDIF FPC_X86_DATA_NEAR}
      ErrorLen:=0;
    end;
end;


procedure ErrorOpen(Var F: TextRec);
Begin
  TextRec(F).InOutFunc:=@ErrorWrite;
  TextRec(F).FlushFunc:=@ErrorWrite;
  TextRec(F).CloseFunc:=@ErrorClose;
  ErrorLen:=0;
End;


procedure AssignError(Var T: Text);
begin
  Assign(T,'');
  TextRec(T).OpenFunc:=@ErrorOpen;
  Rewrite(T);
end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}

procedure system_exit;
{var
  h : byte;}
begin
(*  RestoreInterruptHandlers;
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
{$endif FPC_MM_TINY}*)
  Close(stderr);
  Close(stdout);
  Close(erroutput);
  Close(Input);
  Close(Output);
  asm
    mov al, byte [exitcode]
    mov ah, 4Ch
    int 21h
  end;
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

procedure InitWin16Heap;
begin
{$ifdef FPC_X86_DATA_NEAR}
  SetMemoryManager(LocalHeapMemoryManager);
{$else FPC_X86_DATA_NEAR}
  SetMemoryManager(GlobalHeapMemoryManager);
{$endif FPC_X86_DATA_NEAR}
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
  FillChar(regs,SizeOf(regs),0);
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
  AssignError(stderr);
  AssignError(StdOut);
  Assign(Output,'');
  Assign(Input,'');
  Assign(ErrOutput,'');
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
{$if defined(FPC_X86_DATA_FAR) or defined(FPC_X86_DATA_HUGE)}
  with PAutoDataSegHeader(Ptr(DSeg,0))^ do
    begin
      StackBottom := Ptr(SSeg,pStackTop);
      StackLength := pStackBot-pStackTop;
    end;
{$else}
  with PAutoDataSegHeader(0)^ do
    begin
      StackBottom := NearPointer(pStackTop);
      StackLength := pStackBot-pStackTop;
    end;
{$endif}
  { To be set if this is a GUI or console application }
  IsConsole := FALSE;
  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;
{ Setup heap }
  InitWin16Heap;
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
