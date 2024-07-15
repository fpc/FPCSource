{ %NORUN }

program tw40708;

{$mode objfpc}

type generic TMatrix4<T> = array[0..3] of array [0..3] of T; // works
type generic TAnotherMatrix4<T> = array[0..3,0..3] of T; // "Identifier not found T"

type TMatrix4LongInt = specialize TMatrix4<LongInt>;
type TAnotherMatrix4LongInt = specialize TAnotherMatrix4<LongInt>;
begin
end.

