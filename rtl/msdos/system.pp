unit system;

{$ASMMODE intel}

interface

type
  HRESULT = LongInt;

procedure DebugWrite(const S: string);
procedure DebugWriteLn(const S: string);

implementation

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
