Program Example20;

{ Program to demonstrate the fdRead and fdTruncate functions. }

Uses oldlinux;

Const Data : string[10] = '12345687890';

Var FD : Longint;
    l : longint;

begin
  FD:=fdOpen('test.dat',open_wronly or open_creat,octal(666));
  if fd>0 then
    begin
    { Fill file with data }
    for l:=1 to 10 do
      if fdWrite (FD,Data[1],10)<>10 then
        begin
        writeln ('Error when writing !');
        halt(1);
        end;
    fdClose(FD);
    FD:=fdOpen('test.dat',open_rdonly);
    { Read data again }
    If FD>0 then
      begin
      For l:=1 to 5 do
        if fdRead (FD,Data[1],10)<>10 then
          begin
          Writeln ('Error when Reading !');
          Halt(2);
          end;
      fdCLose(FD);
      { Truncating file at 60 bytes }
      { For truncating, file must be open or write }
      FD:=fdOpen('test.dat',open_wronly,octal(666));
      if FD>0 then
        begin
        if not fdTruncate(FD,60) then
           Writeln('Error when truncating !');
        fdClose (FD);
        end;
      end;
    end;
end.
