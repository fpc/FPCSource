{ %fail }

var
   l : longint;
   s : single;

begin
   { THis should fail with an invalid type cast }
   l:=longint(single(s+1))+12;
end.

