Program Example19;

{ Program to demonstrate the fdOpen, fdwrite and fdCLose functions. }

Uses BaseUnix;

Const Line : String[80] = 'This is easy writing !';

Var FD : Cint;

begin
  FD:=fpOpen ('Test.dat',O_WrOnly or O_Creat);
  if FD>0 then
    begin
    if length(Line)<>fpwrite (FD,Line[1],Length(Line)) then
      Writeln ('Error when writing to file !');
    fpClose(FD);
    end;
end.
