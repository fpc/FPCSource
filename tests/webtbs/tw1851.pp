{ %opt=-Sew -vw }

{$mode objfpc}{$H+}

function A: boolean;

  procedure CheckResult;
  begin
    if not Result then writeln('Oha');
  end;

begin
  Result:=false;
  CheckResult;
end;

begin
  A;
end.
