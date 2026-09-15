{ %FAIL }

{ a "not" behind an operand must be followed by "in" }
program tnotin3;

{$mode objfpc}{$H+}
{$modeswitch delphislang}

var
  A, B: boolean;
begin
  A:=true;
  B:=A not B;
end.
