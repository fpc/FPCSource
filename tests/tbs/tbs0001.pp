program smalltest;
  const
      teststr : string = ' '#9#255#0;
begin
      writeln(teststr);
      teststr := 'gaga';
      writeln(teststr);
      if teststr<>'gaga' then halt(1);
end.
