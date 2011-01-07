{$mode objfpc}

type
  generic TMyArray<T> = array[0..10] of longint;

var
  MyArr: specialize TMyArray<String>;
begin
  MyArr[0] := 1;
end.
