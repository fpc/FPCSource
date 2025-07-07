{ %CPU=X86_64 }
{ %OPT=-O2 }
program tw41317;

{$MODE OBJFPC}

function Test(A: DWord; B: Int64): Int64; noinline;
begin
   while (True) do
   begin
      if (B > 0) then
      begin
         // B is -1, so this line does not run
         B := A;
      end;
      Result := B;
      break;
   end;
end;

var
   V: Int64;
begin
   V := Test(0, -1);
   Write('Testing 32-to-64-bit CMOV result: ', V, ' (should be -1)...');
   if V <> -1 then
   begin
     WriteLn(' FAIL!');
     Halt(1);
   end;

   WriteLn(#10'ok');
end.
