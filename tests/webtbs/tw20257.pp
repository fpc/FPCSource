program bool_compare_bug;

var test_for_0:integer;
      expect:bytebool;
begin // test 1 -- passed
      test_for_0:=1;
      expect:=false;
      if (test_for_0=0)=expect then writeln('> pass')else halt(1);
      // test 2 -- FAILED! [bug]
      test_for_0:=0;
      expect:=true;
      if (test_for_0=0)=expect then writeln('> pass')else halt(2);
      //
end.
