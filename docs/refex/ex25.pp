Program Example25;

{ Program to demonstrate the FillChar function. }

Var S : String[10];
    I : Byte;
begin
  For i:=10 downto 0 do
    begin
    { Fill S with i spaces }
    FillChar (S,SizeOf(S),' ');
    { Set Length }
    SetLength(S,I);
    Writeln (s,'*');
    end;
end.
