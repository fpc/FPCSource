{ %cpu=x86_64 }

program asmclass;
{$mode delphiunicode}
{$asmmode intel}
type
  TMyClass = class
  public
    Data: longint;
    function AsmToClass: longint; ms_abi_default;
  end;

function TMyClass.AsmToClass: longint; ms_abi_default;
asm
   mov RAX, Self.Data // Error: Can't access fields directly for parameters
end;

function AsmToClassProc(Inst: TMyClass): longint; ms_abi_default;
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
  c.data:=123548;
  if C.AsmToClass<>123548 then
    halt(1);
  if AsmToClassProc(C)<>123548 then
    halt(2);

  if AsmToClassProc2(C)<>ppointer(c)^ then
    halt(3);
  C.Free;
end.
