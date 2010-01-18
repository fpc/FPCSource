program tforin3;

// check for-in loop if GetEnumerator returns nil

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

type

  { TListEnumerator }

  TListEnumerator = class
  private
    FCurrent: String;
  public
    function MoveNext: Boolean;
    property Current: String read FCurrent;
  end;

  TMyList = class
  public
    function GetEnumerator: TListEnumerator;
  end;

{ TListEnumerator }

function TListEnumerator.MoveNext: Boolean;
begin
  Result := True;
end;

{ TMyList }

function TMyList.GetEnumerator: TListEnumerator;
begin
  Result := nil;
end;

var
  S: String;
  L: TMyList;
begin
  L := TMyList.Create;
  for S in L do
    ;
  L.Free;
end.

