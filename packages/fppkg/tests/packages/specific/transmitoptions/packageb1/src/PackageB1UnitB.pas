Unit PackageB1UnitB;

{$mode objfpc}{$H+}

{$ifndef PackageA}
{$fatal PackageA is not defined - The transmit-options are not in order}
{$endif}

interface

uses 
  PackageAUnitA;

function PackageB1UnitBFunctionB: Boolean;

implementation

function PackageB1UnitBFunctionB: Boolean;
begin
  Result := PackageAUnitAFunctionA;
end;

end.
