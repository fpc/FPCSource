program tformin8;

// Test IEnumerable and IEnumerator in the for-in loop

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

type
  { TMyListEnumerator }

  TMyListEnumerator = class(TInterfacedObject, IEnumerator)
  private
    FCurrent: Pointer;
    function GetCurrent: TObject;
  public
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TObject read GetCurrent;
  end;

  TMyList = class(TInterfacedObject, IEnumerable)
  public
    function GetEnumerator: IEnumerator;
  end;

{ TMyListEnumerator }


function TMyListEnumerator.GetCurrent: TObject;
begin
  Result := TObject(FCurrent);
end;

function TMyListEnumerator.MoveNext: Boolean;
begin
  inc(PByte(FCurrent));
  Result := FCurrent <= Pointer(3);
end;

procedure TMyListEnumerator.Reset;
begin
  FCurrent := nil;
end;

{ TMyList }

function TMyList.GetEnumerator: IEnumerator;
begin
  Result := TMyListEnumerator.Create;
end;

var
  List: IEnumerable;
  i: TObject;
begin
  List := TMyList.Create;
  for i in List do
    WriteLn(Integer(i));
  List := nil;
end.

