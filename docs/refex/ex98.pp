Program Example98;

{ Program to demonstrate the exitproc function. }

Var
  OldExitProc : Pointer;

Procedure MyExit;

begin
  Writeln('My Exitproc was called. Exitcode = ',ExitCode);
  { restore old exit procedure }
  ExitProc:=OldExitProc;
end;

begin
  OldExitProc:=ExitProc;
  ExitProc:=@MyExit;
  If ParamCount>0 Then
    Halt(66);
end.
