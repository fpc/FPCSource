{ Source provided for Free Pascal Bug Report 2421 }
{ Submitted by "N. Hug" on  2003-03-16 }
{ e-mail: hug__@t-online.de }
PROGRAM Bug;

{$ifdef fpc}{$MODE DELPHI}{$endif}

TYPE  TTestEvent =PROCEDURE OF OBJECT;

TYPE  TTest      =CLASS
                    FOnTest   :TTestEvent;
                    PROPERTY OnTest:TTestEvent READ FOnTest WRITE FOnTest;
                  END;


TYPE  THost      =CLASS
                  PRIVATE
                    FTest :TTest;
                    PROCEDURE DoTest;

                  PUBLIC
                    CONSTRUCTOR Create;
                  END;


PROCEDURE THost.DoTest;
BEGIN
  // Accessing instance data results in a crash.
  // It shows that SELF is not properly set.
  IF FTest = NIL THEN begin
  END;
END;


CONSTRUCTOR THost.Create;
BEGIN
  INHERITED;

  FTest := TTest.Create;

  // Buggy code:
  FTest.OnTest := DoTest;

  // Non buggy alternatives:
  // FTest.OnTest  := SELF.DoTest;
  // FTest.FOnTest := DoTest;

  // Now call it.
  FTest.OnTest;
END;



VAR  M     :THost;

BEGIN
  M := THost.Create;
  M.Free;
END.
