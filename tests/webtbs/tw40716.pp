{ %NORUN }

program tw40716;
{$mode objfpc}{$H+}
{uses
  gmap,gutil;}
type
  generic GMyMap<TKey, TValue, TCompare>=class//(specialize TMap<TKey, TValue, TCompare>)
    type
      TSelf=specialize GMyMap<TKey, TValue, TCompare>;
      TMyMapHelper = class helper for TSelf
        function test:integer;
      end;
    procedure Foo;
  end;
function GMyMap.TMyMapHelper.test:integer;
begin
  result:=42;//FSet.Size;
end;
procedure GMyMap.Foo;
begin
  Self.Test;
end;
begin
end.

