type
   pbyte = ^byte;

begin
   if pbyte(typeinfo(longint))^<>1 then
     halt(1);
end.
