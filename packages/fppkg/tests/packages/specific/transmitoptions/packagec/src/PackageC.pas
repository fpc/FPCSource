Program PackageC;

{$mode objfpc}{$H+}

{$ifndef PackageA}
{$fatal PackageA is not defined - The transmit-options are not in order}
{$endif}

var
  B: Boolean;

begin
  B := PackageAUnitAFunctionA;
end.
