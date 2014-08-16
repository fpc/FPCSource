procedure trashstack;
var
  a: array[0..high(word)] of byte;
begin
  fillchar(a,sizeof(a),$ff);
end;

procedure test;
var
  s1,s2,s3,s4: ansistring;
begin
  s2:='';
  s3:='';
  s4:='';
  s1:=s2+s3+s4;
  if s1<>'' then
    halt(1);
end;

begin
  trashstack;
  test;
end.
