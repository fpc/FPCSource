program bug;

begin
  {$I-}
  mkdir('test895');
  InOutRes:=0;
  {$I+}
  writeln('This is a test');
  {$I-}
  mkdir('test895');
  InOutRes:=0;
  {$I+}
  writeln('This is a test');
  {$I-}
  rmdir('test895');
  InOutRes:=0;
  {$I+}
  writeln('This is a test');
end.
