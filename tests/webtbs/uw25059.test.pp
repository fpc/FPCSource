unit uw25059.test;

{$mode delphi}

interface

function ExecuteTest(): Integer;

implementation

uses
  uw25059,
  uw25059.withdot;

function ExecuteTest(): Integer;
begin
  Result := uw25059.GetValue();
end;

end.
