{ %FAIL }
{ Old file: tbf0167.pp }
{ crash when declaring a procedure with same name as object OK 0.99.9 (PFV) }

type ObjTest = Object
     End;

Procedure ObjTest;
Begin
end;

Begin
end.
