{%KNOWNCOMPILEERROR=CodeWarrior Pascal is sometimes similar to and sometimes different from TP/Delphi regarding calling procvars }
{$mode macpas}

program tmacfunret;

var
   called:boolean;

  function B(function x: integer): integer;

  begin
    b:=x;
  end;

  function A: Integer;

  begin
    if not called then
      begin
        called:=true;
        A:=B(A);
      end
    else
      A:=42;
  end;

var
  i: Integer;

begin
  called:=false;
  i:= A;
  Writeln(i);
  if i <> 42 then
    halt(1);
end.
