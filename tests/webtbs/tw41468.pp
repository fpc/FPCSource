{ %cpu=aarch64 }
procedure Foo;assembler;
asm
  shrn v3.8b,v2.8h,#4 //Invalid arrangement specifier ".8h"
end;

begin
end.