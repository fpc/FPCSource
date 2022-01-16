{ %norun }
const
   l = high(ptrint);	//   2000  <-->  2100
type
   t = array[ 1..l ]of int8;	//   1.95  <-->  2.05  GiBy
var
   p: ^t;
begin
   new(p);
   writeln( sizeof(p^) );
   p^[l]:=0;  writeln(p^[l])
end .
