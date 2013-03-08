unit system;

{$ASMMODE intel}

interface

type
  HRESULT = LongInt;

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

end.
