Program Example15;
uses Crt;

{ Program to demonstrate the Delay function. }
var
  i : longint;
begin
  WriteLn('Counting Down');
  for i:=10 downto 1 do
   begin
     WriteLn(i);
     Delay(1000); {Wait one second}
   end;
  WriteLn('BOOM!!!');
end.
