Program Example19;

{ Program to demonstrate the fdOpen, fdwrite and fdCLose functions. }

Uses oldlinux;

Const Line : String[80] = 'This is easy writing !';

Var FD : Longint;

begin
  FD:=fdOpen ('Test.dat',Open_WrOnly or Open_Creat);
  if FD>0 then
    begin
    if length(Line)<>fdwrite (FD,Line[1],Length(Line)) then
      Writeln ('Error when writing to file !');
    fdClose(FD);
    end;
end.
