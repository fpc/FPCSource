unit tw18688;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type

  IValueHolder<T> = interface
    function GetT:T;
    procedure SetT(NewValue:T);
  end;

  TValueHolder <T> = class (TInterfacedObject, IValueHolder<T>)
  private
    FValue: T;
  public
    constructor Create;
    destructor Destroy; override;
    function GetT:T;
    procedure SetT(NewValue:T);
  end;

  RValueHolder<T> = record
  private
    type
      _IValueHolder = IValueHolder<T>;
      _TValueHolder = TValueHolder<T>;
    var
    Ptr: _IValueHolder;

    function GetImpl: _IValueHolder;
  public

    class operator Implicit (a:RValueHolder<T>):T; inline;
    class operator Implicit (a:T):RValueHolder<T>; inline;

    function GetValue:T; inline;
    property V:T read GetValue;
  end;

  //TObjectValue = TValueHolder<TObject> ;    // works if not commented
  TObjectValue2 = RValueHolder<TObject> ;


implementation

constructor TValueHolder <T>.Create;
begin
  FValue := nil
end;

destructor TValueHolder <T>.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;


function TValueHolder <T>.GetT:T;
begin
  Result := FValue;
end;

procedure TValueHolder <T>.SetT(NewValue:T);
begin
  if FValue <> NewValue then
  begin
    FreeAndNil(FValue);
    FValue := NewValue;
  end;
end;

function RValueHolder<T>.GetImpl: _IValueHolder;
begin
  if Pointer(Ptr) = nil then
  begin
    Ptr := _TValueHolder.Create;
  end;
  Result := Ptr;
end;

class operator RValueHolder<T>.Implicit (a:RValueHolder<T>):T;
begin
  Result:=a.V;
end;

class operator RValueHolder<T>.Implicit (a:T):RValueHolder<T>;
begin
  Result.GetImpl.SetT(a);
end;

function RValueHolder<T>.GetValue:T;
begin
  Result := GetImpl.GetT;
end;

end.

