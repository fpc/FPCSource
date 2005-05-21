
{ this program shows a possible problem
  of name mangling in FPC  (PM) }
  procedure test;

    function a : longint;
      begin
         a:=1;
      end;

    begin
       writeln('a = ',a);
    end;

  procedure test(b : byte);

    function a : longint;
      begin
         a:=2;
      end;

    begin
       writeln('b = ',b);
       writeln('a = ',a);
    end;

  type a = word;

  function test_(b : a) : longint;
    begin
      test_:=b;
    end;

begin
   test(1);
   test;
   test(4);
end.
