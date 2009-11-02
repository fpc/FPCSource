program tforin5;

// test operator Enumerator support for classes

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

type
  TMyList = class
  end;

  { TMyListEnumerator }

  TMyListEnumerator = class
  private
    FCurrent: Integer;
  public
    constructor Create;
    function MoveNext: Boolean;
    property Current: Integer read FCurrent;
  end;

{ TMyListEnumerator }

constructor TMyListEnumerator.Create;
begin
  FCurrent := 0;
end;

function TMyListEnumerator.MoveNext: Boolean;
begin
  inc(FCurrent);
  Result := FCurrent <= 3;
end;

operator enumerator (AList: TMyList): TMyListEnumerator;
begin
  Result := TMyListEnumerator.Create;
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

