{ %fail }
{%OPT=-Cr}

type
   smallrange = 5..287;
const
   c1 : byte = succ(256);
   c2 : byte = pred(0);
   c3 : byte = succ(high(smallrange));
   c4 : byte = pred(low(smallrange));
   c5 = succ(high(smallrange));
   c6 = pred(low(smallrange));
var
 b: byte;
Begin
 b:=succ(256);
 b:=pred(0);
end.
