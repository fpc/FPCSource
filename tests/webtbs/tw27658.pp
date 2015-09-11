{$MODE OBJFPC}
program Project1;

uses gmap;

type

generic TMyMap <TKey, TValue, TCompare> = class(specialize TMap<TKey, TValue, TCompare>)
end;
generic TMyCounter <TKey, TCompare> = class(specialize TMyMap<TKey, SizeUInt, TCompare>)
end;

begin
end.
