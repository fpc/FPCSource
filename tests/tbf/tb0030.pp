{ %FAIL }
{ Old file: tbf0161.pp }
{ internal error when trying to create a set with another OK 0.99.9 (PFV) }

Program tbs0161;

{the following program should give a syntax error, but causes an internal error}

const s = [1,2,3,4,5];

var b: Byte;

Begin
  If b in [s] then;
End.
