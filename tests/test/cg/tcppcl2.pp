{ Test the C++ class renaming }
program tcppcl2;

{$mode objfpc}
{$L cpptcl2.o}

type
  TestClass = cppclass
  public
    class procedure TestProc;
  end; external;

  TestClass2 = cppclass
  public
    class procedure TestProc;
  end; external name 'testclass';

begin
  try
    TestClass.TestProc;
    TestClass2.TestProc;
  except
    ExitCode := 1;
    writeln('error');
  end;
end.

