Unit PackageBUnitB;

{$mode objfpc}{$H+}

interface

uses 
  PackageAUnitA;

function PackageBUnitBFunctionB: Boolean;

implementation

function PackageBUnitBFunctionB: Boolean;
begin
  Result := PackageAUnitAFunctionA;
end;

end.
