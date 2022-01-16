{ %FAIL }

unit tw28753;

{$mode delphi}

interface

type
  TList<T> = class(TObject);

implementation

procedure ObjectListToJson(Value: TList);
begin // Error: Internal error 200301231
end;

end.
