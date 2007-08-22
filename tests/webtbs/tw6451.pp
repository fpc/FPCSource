{ %fail }

{$codepage utf8}

{ can't convert widechar to char, because don't know what }
{ encoding to use at compile-time                         }
const abc : array [1..4] of char = ('a','é','b','c');
begin
end.
