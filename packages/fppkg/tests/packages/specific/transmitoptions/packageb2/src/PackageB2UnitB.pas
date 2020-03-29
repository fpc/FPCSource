Unit PackageB2UnitB;

{$mode objfpc}{$H+}

interface

{$ifndef PackageA}
{$fatal PackageA is not defined - The transmit-options are not in order}
{$endif}

uses
  PackageAUnitA;

function PackageB2UnitBFunctionB: Boolean;

implementation

function PackageB2UnitBFunctionB: Boolean;
begin
  Result := PackageAUnitAFunctionA;
end;

end.
