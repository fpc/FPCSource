{ Source provided for Free Pascal Bug Report 2525 }
{ Submitted by "Pavel V. Ozerski" on  2003-06-05 }
{ e-mail: ozerski@list.ru }
procedure MyProc(x:array of longint);
 begin
   writeln(high(x));
   if high(x)<>2 then
     halt(1);
 end;
type
 tMyEnum=(My1,My2,My3);
var
 ar:array[tMyEnum]of longint;
begin
 MyProc(ar);
end.
