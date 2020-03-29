{$mode objfpc}
program Project1;

uses variants;

  function CreateVarArray( const Bounds: array of SizeInt;
                           const VarType: integer ): variant;
  var
    Dim01: array [0..1] of SizeInt absolute Bounds;
  begin
    Result := VarArrayCreate( Dim01, varType );
  end;

begin
end.
