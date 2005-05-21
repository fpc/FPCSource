program dumpfile;

Var
  F : File of Byte;
  Col : Integer;
  B : Byte;

begin
  Assign(F,Paramstr(1));
  Reset(F);
  Col:=1;
  while not eof(f) do
    begin
    Read(F,B);
    write(HexStr(B,2),' ');
    Col:=Col+3;
    If Col>72 then
      begin
      Writeln;
      Col:=1;
      end;
    end;
  Close(f);
  If Col<>1 then
    Writeln;
end.
    
