{ %FAIL }
program tw24588;
{$mode objfpc}
{$asmmode intel}
type
generic TFoo<T>=class
procedure CrashMe(_val: T);
end;

procedure TFoo.CrashMe(_val: T);
begin
asm
mov edi,edi
end;
end;

begin
end.

