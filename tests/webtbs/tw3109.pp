
var
  l : smallint;
  q : cardinal;
begin
  l:=1;
{$if sizeof(l)=2}
  l:=2;
{$endif}
  if l<>2 then
   begin
     writeln('Error!');
     halt(1);
   end;
end.
