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
  FileSeek(F,0,0);
  Randomize;
  Repeat
    FileSeek(F,Random(100)*4,0);
    FileRead (F,J,SizeOf(J));
    Writeln ('Random read : ',j);
  Until J>80;
  FileClose(F);
End.