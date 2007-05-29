{$MODE DELPHI}

type
TDADataTable =class(TObject)
public
function GetAsCurrency(Index: integer): Currency;safecall;
end;

function TDADataTable.GetAsCurrency(Index: integer): Currency;
begin
Result:=0;
end;

begin
end.
