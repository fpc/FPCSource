unit system;

{$ASMMODE intel}

interface

{$ifdef FULL_RTL}

{$DEFINE FPC_INCLUDE_SOFTWARE_MUL}

{$I systemh.inc}
{$endif FULL_RTL}

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
  UnusedHandle    = -1;
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
  AllFilesMask: string [3];
{$ifndef RTLLITE}
{ System info }
  LFNSupport : boolean;
{$ELSE RTLLITE}
const
  LFNSupport = false;
{$endif RTLLITE}

{$ifndef FULL_RTL}
type
  DWord = LongWord;
  Cardinal = LongWord;
  Integer = SmallInt;
  UInt64 = QWord;

  HRESULT = LongInt;
{$endif FULL_RTL}

procedure DebugWrite(const S: string);
procedure DebugWriteLn(const S: string);

implementation

{$ifdef FULL_RTL}
{$I system.inc}
{$endif FULL_RTL}

procedure fpc_Initialize_Units;[public,alias:'FPC_INITIALIZEUNITS']; compilerproc;
begin
end;

procedure do_exit;[Public,Alias:'FPC_DO_EXIT'];
begin
  asm
    mov ax, 4c00h
    int 21h
  end;
end;

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
  end;
end;

procedure DebugWriteLn(const S: string);
begin
  DebugWrite(S);
  DebugWrite(#13#10);
end;

end.
