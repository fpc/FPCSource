{ Old file: tbs0116.pp }
{ when local variable size is > $ffff, enter can't be used to create the stack frame, but it is with -Og }

Procedure test;
{compile with -Og to show bug}

Var a: Array[1..4000000] of longint;
Begin
End;

Begin
End.
