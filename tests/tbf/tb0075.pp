{ %FAIL }
{ Old file: tbf0352.pp }
{  }

{$ifdef fpc}{$MODE OBJFPC}{$endif}

Procedure Proc1(args:array of const);
begin
end;

Procedure Proc2(args:array of longint);
Begin
  { this should give an error }
  Proc1(args);
End;

Begin
  Proc1([0,1]);
End.
