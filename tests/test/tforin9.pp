program tforin9;

// Test interface with 'enumerator MoveNext', 'enumerator Current' directives

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

type

  { IMyIterator }

  IMyIterator = interface
    function GetValue: TObject;
    function StepForward: Boolean; enumerator MoveNext;
    property Value: TObject read GetValue; enumerator Current;
  end;
  { TMyListEnumerator }

  TMyListEnumerator = class(TInterfacedObject, IMyIterator)
  private
    FValue: Pointer;
    function GetValue: TObject;
  public
    function StepForward: Boolean;
    property Value: TObject read GetValue;
  end;

  TMyList = class
  public
    function GetIterator: IMyIterator;
  end;

operator Enumerator(AList: TMyList): IMyIterator;
begin
  Result := AList.GetIterator;
end;

{ TMyListEnumerator }


function TMyListEnumerator.GetValue: TObject;
begin
  Result := TObject(FValue);
end;

function TMyListEnumerator.StepForward: Boolean;
begin
  inc(PByte(FValue));
  Result := FValue <= Pointer(3);
end;

{ TMyList }

function TMyList.GetIterator: IMyIterator;
begin
  Result := TMyListEnumerator.Create;
end;

var
  List: TMyList;
  i: TObject;
begin
  List := TMyList.Create;
  for i in List do
    WriteLn(PtrInt(i));
  List.Free;
end.

