{ %cpu=x86_64 }

{$ifdef FPC}
  {$PIC OFF}
{$endif FPC}

program asmclass;
{$mode delphiunicode}
{$asmmode intel}
type
  TMyClass = class
  public
    Data: int64;
    function AsmToClass: int64; ms_abi_default;
  end;

function TMyClass.AsmToClass: int64; ms_abi_default;
asm
   mov RAX, Self.Data // Error: Can't access fields directly for parameters
end;

function AsmToClassProc(Inst: TMyClass): int64; ms_abi_default;
asm
   mov RAX, Inst.Data // Error: Can't access fields directly for parameters
end;

function AsmToClassProc2(Inst: TMyClass): pointer; ms_abi_default;
asm
   mov RAX, qword ptr [Inst]
end;

var
  C: TMyClass;
begin
  C := TMyClass.Create;
  c.data:=$123456789AB;
  if C.AsmToClass<>$123456789AB then
    halt(1);
  if AsmToClassProc(C)<>$123456789AB then
    halt(2);

  if AsmToClassProc2(C)<>ppointer(c)^ then
    halt(3);
  C.Free;
  writeln('ok');
end.
