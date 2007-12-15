program comparechar0bug;

var str1 : pchar = 'test';
    str2 : pchar = 'test';
    str3 : pchar = 'testa';
    str4 : pchar = 'asdf';
    res : longint;
begin
  res:=CompareChar0(str1[0],str2[0],maxint);
  if res<>0 then
    halt(1);
  res:=CompareChar0(str1[0],str3[0],maxint);
  if res>=0 then
    halt(1);
  res:=CompareChar0(str4[0],str1[0],maxint);
  if res>=0 then
    halt(1);
  res:=CompareChar0(str1[0],str4[0],maxint);
  if res<=0 then
    halt(1);

  writeln('ok');
end.
