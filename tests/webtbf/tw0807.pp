{ %FAIL }
{$mode objfpc}

Program test;

uses crt;

type
  TMatrix = class
              Constructor Create;
            private
              Elements : array [1..10,1..10] of real;
            end;

Constructor TMatrix.Create;

begin
end;

OPERATOR :=(r:Real):TMatrix;
  BEGIN
    WITH RESULT DO
      BEGIN
{ Do something }
      END;
    writeln ('Call to overloaded operator :=, real operand');
  END;
operator :=(m : TMatrix):TMatrix;
  BEGIN
    WITH RESULT DO
      BEGIN
{ Do something }
      END;
    writeln ('Call to overloaded operator :=, matrix operand');
  END;

var
  m : TMatrix;
  m2 : TMatrix;

begin
  clrscr;
  writeln ('Performing calculations...');
  m:=TMatrix.Create;
  m2:=TMatrix.Create;
  writeln ('Assigning real to matrix...');
{ This one works }
  m:=1;
  writeln ('Assigning matrix to matrix...');
{ This one does not work }
  m:=m2;
  writeln ('Done.');
end.
