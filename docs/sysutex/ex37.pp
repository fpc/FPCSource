Program Example37;

{ This program demonstrates the FileCreate function }

Uses sysutils;

Var I,J,F : Longint;

Begin
  F:=FileCreate ('test.dat');
  If F=-1 then
    Halt(1);
  For I:=0 to 100 do
    FileWrite(F,I,SizeOf(i));
  FileClose(f);
  F:=FileOpen ('test.dat',fmOpenRead);
  For I:=0 to 100 do
    begin
    FileRead (F,J,SizeOF(J));
    If J<>I then
      Writeln ('Mismatch at file position ',I)
    end;
  FileSeek(F,0,fsFromBeginning);
  Randomize;
  Repeat
    FileSeek(F,Random(100)*4,fsFromBeginning);
    FileRead (F,J,SizeOf(J));
    Writeln ('Random read : ',j);
  Until J>80;
  FileClose(F);
  F:=FileOpen('test.dat',fmOpenWrite);
  I:=50*SizeOf(Longint);
  If FileTruncate(F,I) then
    Writeln('SuccessFully truncated file to ',I,' bytes.');
  FileClose(F);
End.