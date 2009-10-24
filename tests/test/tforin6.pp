program Project18;

{$APPTYPE CONSOLE}

type
  { TMyListEnumerator }

  TMyListEnumerator = object
  private
    FCurrent: Integer;
  public
    constructor Create;
    destructor Destroy;
    function MoveNext: Boolean;
    property Current: Integer read FCurrent;
  end;

  TMyList = class
  end;

{ TMyListEnumerator }

constructor TMyListEnumerator.Create;
begin
  WriteLn('create');
  FCurrent := 0;
end;

destructor TMyListEnumerator.Destroy;
begin
  WriteLn('destroy');
  inherited;
end;

function TMyListEnumerator.MoveNext: Boolean;
begin
  inc(FCurrent);
  Result := FCurrent <= 3;
end;

operator enumerator (AList: TMyList): TMyListEnumerator;
begin
  Result.Create;
end;

var
  List: TMyList;
  i: integer;
begin
  List := TMyList.Create;
  for i in List do
    WriteLn(i);
  List.Free;
end.

