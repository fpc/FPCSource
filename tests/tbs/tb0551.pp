{ %cpu=x86_64 }
{ %norun }

{$asmmode intel}
begin
  asm
    dq 123456789101213
  end;
end.

