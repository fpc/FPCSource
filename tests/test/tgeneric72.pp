{ %NORUN }

{ This tests that the two specializations of TUsedGeneric (once with 
  TGeneric<LongInt>.TSubType and once with TGeneric<Pointer>.TSubType)  inside
  TGeneric are unique }

program tgeneric72;

{$mode objfpc}

type
  generic TUsedGeneric<T> = class
  type
    PT = ^T;
  var
    f: PT;
  end;

  generic TGeneric<T> = class
  type
    TSubType = record
      Field: T;
    end;
    PSubType = ^TSubType;
    TMyUsedGeneric = specialize TUsedGeneric<TSubType>;
  private
    f: PSubType;
  public
    function GetUsedGeneric: TMyUsedGeneric;
  end;

function TGeneric.GetUsedGeneric: TMyUsedGeneric;
begin
  Result := TMyUsedGeneric.Create;
  Result.f := f;
end;

type
  TGenericLongInt = specialize TGeneric<LongInt>;
  TGenericPointer = specialize TGeneric<Pointer>;

begin

end.

