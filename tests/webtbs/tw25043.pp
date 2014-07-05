program tw25043;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

const
  TestValue = 30061978;

type
  TDescendant = class(TObject)
  public
    function    GetValue(): Integer;
  end;

  TCollection<E: class> = class(TObject)
  protected
    Value: E;

    function    GetElements(Index: Integer): E;

  public
    property    Element: E read Value;
    property    Elements[Index: Integer]: E read GetElements; default;
  end;

  TDescendantCollection<E: TDescendant> = class(TCollection<E>)
  public
    procedure   TestValues();
  end;

{ TDescendant }

function TDescendant.GetValue(): Integer;
begin
  Result := TestValue;
end;

{ TDescendantCollection<E> }

procedure TDescendantCollection<E>.TestValues();
begin
  if Value.GetValue() <> TestValue then
    Halt(1);

  if Element.GetValue() <> TestValue then
    Halt(1);

  if GetElements(0).GetValue() <> TestValue then
    Halt(1);

  if Elements[0].GetValue() <> TestValue then
    Halt(1);

  if Self[0].GetValue() <> TestValue then
    Halt(1);
end;

{ TCollection<E> }

function TCollection<E>.GetElements(Index: Integer): E;
begin
  Result := Value;
end;

var
  Collection: TDescendantCollection<TDescendant>;
  Descendant: TDescendant;

begin
  Descendant := TDescendant.Create();

  Collection := TDescendantCollection<TDescendant>.Create();
  Collection.Value := Descendant;
  Collection.TestValues();
  Collection.Free();

  Descendant.Free();
end.
