Program Example31;

{ This program demonstrates the DeleteFile function }

Uses sysutils;

Var
  Line : String;
  F,I : Longint;

Begin
  F:=FileCreate('test.txt');
  Line:='Some string line.'#10;
  For I:=1 to 10 do
    FileWrite (F,Line[1],Length(Line));
  FileClose(F);
  DeleteFile('test.txt');
End.