Program Example20;

{ Program to demonstrate the fdRead and fdTruncate functions. }

Uses BaseUnix;

Const Data : string[10] = '1234567890';

Var FD : cint;
    l : longint;

begin
  FD:=fpOpen('test.dat',o_wronly or o_creat,&666);
  if fd>0 then
    begin
    { Fill file with data }
    for l:=1 to 10 do
      if fpWrite (FD,Data[1],10)<>10 then
        begin
        writeln ('Error when writing !');
        halt(1);
        end;
    fpClose(FD);
    FD:=fpOpen('test.dat',o_rdonly);
    { Read data again }
    If FD>0 then
      begin
      For l:=1 to 5 do
        if fpRead (FD,Data[1],10)<>10 then
          begin
          Writeln ('Error when Reading !');
          Halt(2);
          end;
      fpClose(FD);
      { Truncating file at 60 bytes }
      { For truncating, file must be open or write }
      FD:=fpOpen('test.dat',o_wronly,&666);
      if FD>0 then
        begin
        if fpfTruncate(FD,60)<>0 then
           Writeln('Error when truncating !');
        fpClose (FD);
        end;
      end;
    end;
end.
