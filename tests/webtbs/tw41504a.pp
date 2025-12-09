{ %NORUN }

program tw41504a;
{$mode objFPC}

type
  TClassMain = class
    procedure method; virtual; abstract;
  end;

  generic TGenClass<T> = class
  type
    TType = T;
    TSelf = TGenClass;
  end;

  LType = type specialize TGenClass<TClassMain>.TSelf.TType;

begin
end.

