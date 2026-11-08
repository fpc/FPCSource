{ Inside a generic, a type nested in a specialization of another generic is the same type wherever
  the specialization is named: through the ancestor and through TBase<T>.TTable, as in Delphi }
program tgenericnested1;

{$mode delphi}

type
  TBase<T> = class
  public type
    THandler = function(aParser: TBase<T>): Boolean;
    TTable = array[0..1] of THandler;
    PTable = ^TTable;
  public class var
    cTables: array[0..1] of TTable;
  end;

  TDerived<T> = class(TBase<T>)
    function Build: Boolean;
  end;

function Accept(aParser: TBase<Integer>): Boolean;
begin
  Result := Assigned(aParser);
end;

function TDerived<T>.Build: Boolean;
var
  lTbl: TBase<T>.TTable;
  lPtr: TBase<T>.PTable;
begin
  lTbl[0] := nil;
  lTbl[1] := nil;
  cTables[0] := lTbl;
  lPtr := @cTables[0];
  lTbl := lPtr^;
  Result := not Assigned(lTbl[1]);
end;

var
  lD: TDerived<Integer>;
  lTable: TBase<Integer>.TTable;
begin
  lD := TDerived<Integer>.Create;
  if not lD.Build then
    Halt(1);
  lTable[0] := @Accept;
  lTable[1] := nil;
  TDerived<Integer>.cTables[1] := lTable;
  lTable := TBase<Integer>.cTables[1];
  if not lTable[0](lD) then
    Halt(2);
  lD.Free;
  Writeln('ok');
end.
