{ %OPT=-O3 }
program LoopUnrollTest;
procedure Test;
var
   I: Integer;
   procedure Test2;
   begin
     if I<>1 then
       halt(1)
     else
       begin
         writeln('ok');
         halt(0);
       end;
   end;
begin
   for I := 1 to 10 do
     Test2;
end;
begin
   Test;
end.
