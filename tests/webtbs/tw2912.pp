{ Source provided for Free Pascal Bug Report 2912 }
{ Submitted by "Bill Pearce" on  2004-01-19 }
{ e-mail: pearceg@post.queensu.ca }
procedure SetRowCount(n,m : integer);
var
  tmp : array of array of string;
begin
  SetLength(tmp, n, m);
end;

begin
  SetRowCount(10,2);
end.
