{ Old file: tbs0001.pp }
{  tests a bugs in the .ascii output (#0 and too long)  OK 0.9.2 }

program smalltest;
  const
      teststr : string = ' '#9#255#0;
begin
      writeln(teststr);
      teststr := 'gaga';
      writeln(teststr);
      if teststr<>'gaga' then halt(1);
end.
