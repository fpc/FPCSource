const
  nl=#10;
type
  cs=set of char;

function p(c:cs):boolean;
begin
  p:=(#10 in c);
end;

begin
  if p([#1..#255]-[nl]) then
   halt(1);
end.
