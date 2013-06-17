program tgeneric93;

uses
  ugeneric93a,
  ugeneric93b;

const
  // should be False
{$if declared(NotDeclared<>)}
  TestNotDeclared = True;
{$else}
  TestNotDeclared = False;
{$endif}

  // should be False
{$if declared(TTestDelphi)}
  TestTTestDelphi = True;
{$else}
  TestTTestDelphi = False;
{$endif}

  // should be True
{$if declared(TTestDelphi<>)}
  TestTTestDelphi1 = True;
{$else}
  TestTTestDelphi1 = False;
{$endif}

  // should be False
{$if declared(TTestDelphi<,>)}
  TestTTestDelphi2 = True;
{$else}
  TestTTestDelphi2 = False;
{$endif}

  // should be True
{$if declared(TTestDelphi<,,>)}
  TestTTestDelphi3 = True;
{$else}
  TestTTestDelphi3 = False;
{$endif}

// should be True
{$if declared(TTest2Delphi)}
  TestTTest2Delphi = True;
{$else}
  TestTTest2Delphi = False;
{$endif}

// should be False
{$if declared(TTest2Delphi<>)}
  TestTTest2Delphi1 = True;
{$else}
  TestTTest2Delphi1 = False;
{$endif}

// should be True
{$if declared(TTest2Delphi<,>)}
  TestTTest2Delphi2 = True;
{$else}
  TestTTest2Delphi2 = False;
{$endif}

  // should be False
{$if declared(TTestFPC)}
  TestTTestFPC = True;
{$else}
  TestTTestFPC = False;
{$endif}

  // should be False
{$if declared(TTestFPC<>)}
  TestTTestFPC1 = True;
{$else}
  TestTTestFPC1 = False;
{$endif}

  // should be True
{$if declared(TTestFPC<,>)}
  TestTTestFPC2 = True;
{$else}
  TestTTestFPC2 = False;
{$endif}

begin
  if TestNotDeclared then
    Halt(1);
  if TestTTestDelphi then
    Halt(2);
  if not TestTTestDelphi1 then
    Halt(3);
  if TestTTestDelphi2 then
    Halt(4);
  if not TestTTestDelphi3 then
    Halt(5);
  if not TestTTest2Delphi then
    Halt(6);
  if TestTTest2Delphi1 then
    Halt(7);
  if not TestTTest2Delphi2 then
    Halt(8);
  if TestTTestFPC then
    Halt(9);
  if TestTTestFPC1 then
    Halt(10);
  if not TestTTestFPC2 then
    Halt(11);
  Writeln('OK');
end.
