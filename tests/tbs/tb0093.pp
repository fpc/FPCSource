{ Old file: tbs0111.pp }
{ blockread(typedfile,...) is not allowed in TP7 }

var
  ft : text;
  f : file of word;
  i : word;
  buf : string;
begin
  assign(ft,'tbs0111.tmp');
  rewrite(ft);
  for i:=1 to 40 do
    Writeln(ft,'Dummy text to test bug 111');
  close(ft);
  assign(f,'tbs0111.tmp');
  reset(f);
  blockread(f,buf[1],127,i);    { This is not allowed in BP7 }
  buf[0]:=chr(i*2);
  close(f);
  writeln(i);
  writeln(buf);
  erase(f);
end.
