{ %fail }

{$ifdef fpc}{$MODE delphi}{$endif}

type
  TA = class(TInterfacedObject)
  public
    constructor Create;
  end;

  TB = class(TA)
    function Clone: TB;
  end;


constructor TA.Create;
begin
  inherited Create;
end;

function TB.Clone: TB;
var
  b: TB;
begin
  b := Create;
  writeln(integer(b));
  writeln(integer(self));
  Result := b;
end;

var
  aa,aa2: TB;
begin
  aa := TB.Create;
  aa2 := aa.Clone;
end.
