{ Source provided for Free Pascal Bug Report 2028 }
{ Submitted by "Han Wentao" on  2002-07-04 }
{ e-mail: hanwentao@cnnb.net }
{$INLINE ON}
function Max(a,b:Byte):Byte; inline;
begin
  if a>b then
    Max:=a
  else
    Max:=b;
end;

var
 l1,  l2 : longint;
begin
  l1:=Max(1,2);
  l2:=Max(2,1);
  if l1 <> 2 then
   begin
     WriteLn('Error!');
     halt(1);
   end;
  if l2 <> 2 then
   begin
     WriteLn('Error!');
     halt(1);
   end;
end.
