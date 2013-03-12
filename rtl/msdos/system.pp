unit system;

{$ASMMODE intel}

interface

{$ifdef FULL_RTL}

{$DEFINE FPC_INCLUDE_SOFTWARE_MUL}

{$I systemh.inc}
{$endif FULL_RTL}

const
  maxExitCode = 255;

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
