Program Example44;

{ This program demonstrates the RenameFile function }

Uses sysutils;

Var F : Longint;
    S : String;

Begin
  S:='Some short file.';
  F:=FileCreate ('test.dap');
  FileWrite(F,S[1],Length(S));
  FileClose(F);
  If RenameFile ('test.dap','test.dat') then
    Writeln ('Successfully renamed files.');
End.