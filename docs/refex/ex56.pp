Program Example56;

{ Program to demonstrate the Seek function. }

Var
  F : File;
  I,j : longint;

begin
  { Create a file and fill it with data }
  Assign (F,'test.tmp');
  Rewrite(F); { Create file }
  Close(f);
  FileMode:=2;
  ReSet (F,Sizeof(i)); { Opened read/write }
  For I:=0 to 10 do
    BlockWrite (F,I,1);
  { Go Back to the begining of the file }
  Seek(F,0);
  For I:=0 to 10 do
    begin
    BlockRead (F,J,1);
    If J<>I then
      Writeln ('Error: expected ' ,i,', got ',j);
    end;
  Close (f);
end.
