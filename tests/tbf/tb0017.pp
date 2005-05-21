{ %FAIL }
{ Old file: tbf0101.pp }
{ no type checking for routines in interfance and       OK 0.99.1 (CEC) }

Unit tbs0101;

Interface

 Procedure MyProc(V: Integer);


Implementation

 Procedure MyProc(Y: Integer);
 Begin
 end;


end.
