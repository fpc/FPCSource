var
  f : file of word;
  i : word;
  buf : string;
begin
  assign(f,'tbs0111.pp');
  reset(f);
  blockread(f,buf[1],sizeof(buf),i);    { This is not allowed in BP7 }
  buf[0]:=chr(i);
  close(f);
  writeln(i);
  writeln(buf);
end.