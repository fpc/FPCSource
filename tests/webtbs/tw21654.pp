program test;

{$mode objfpc}

Type

{ TMyGeneric }

 Generic TMyGeneric<T> = Class
  Private
    bValue: Integer; Static;
    Function GetValue: Integer;
  Public
    Property Value: Integer Read GetValue;
    Constructor Create(Const aValue: Integer);
End;

{ TMyGeneric }

Function TMyGeneric.GetValue: Integer;
Begin
  Result := bValue;
end;

Constructor TMyGeneric.Create(Const aValue: Integer);
Begin
  bValue := aValue;
End;

Type TMyClass = Specialize TMyGeneric<TObject>;

begin
end.
