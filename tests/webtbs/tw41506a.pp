{ %NORUN }

program tw41506a;
{$mode objFPC}

type
  generic TGenClass<T> = class
  end;
  TCommon = tw41506a.specialize TGenClass<byte>;

begin
end.

