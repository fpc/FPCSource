Program Example103;

{ Program to demonstrate the FillByte function. }

Var S : String[10];
    I : Byte;

begin
  For i:=10 downto 0 do
    begin
    { Fill S with i bytes }
    FillChar (S,SizeOf(S),32);
    { Set Length }
    SetLength(S,I);
    Writeln (s,'*');
    end;
end.
