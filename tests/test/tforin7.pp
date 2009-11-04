program tforin7;

// test 'enumerator MoveNext' and 'enumrator Current' directives

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

type
  { TMyListEnumerator }

  TMyListEnumerator = object
  private
    FCurrent: Integer;
  public
    constructor Create;
    destructor Destroy;
    function StepNext: Boolean; enumerator MoveNext;
    property Value: Integer read FCurrent; enumerator Current;
  end;

  TMyList = class
  end;

{ TMyListEnumerator }

constructor TMyListEnumerator.Create;
begin
  FCurrent := 0;
end;

destructor TMyListEnumerator.Destroy;
begin
  inherited;
end;

function TMyListEnumerator.StepNext: Boolean;
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

