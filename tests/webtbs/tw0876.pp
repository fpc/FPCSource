{ %target=go32v2,linux,freebsd,darwin }
{ %note=This test needs C libraries }
{ %OPT=-pg }

program test1;
var
  i,j:longint;
  l : longint;
  a,b:double;

procedure test;
begin
 b:=1.0;
 i:=2;
 a:=b+3;
 j:=i div 2;
end;

procedure test2;
begin
  test;
  Writeln('i=',i,' l=',l);
end;

begin
  for l:=1 to 10000 do
    begin
      test;
      test2;
      test;
    end;
end.
