{ %FILES=cpptcl2.o }
{ Test the C++ class renaming }
program tcppcl2;

{$mode objfpc}
{$L cpptcl2.o}

type
  TestClass = cppclass external
  public
    class procedure TestProc;
  end;

  TestClass2 = cppclass external name 'testclass'
  public
    class procedure TestProc;
  end;

begin
  try
    TestClass.TestProc;
    TestClass2.TestProc;
  except
    ExitCode := 1;
    writeln('error');
  end;
end.

