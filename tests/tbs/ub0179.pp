{ Old file: tbs0213a.pp }
{  }

{ different tests for the problem of local
  functions having the same name }

unit ub0179;

interface

PROCEDURE Testsomething(VAR A:LONGINT);

PROCEDURE Testsomething(VAR A:WORD);

implementation


PROCEDURE Testsomething(VAR A:LONGINT);

FUNCTION Internaltest(L:LONGINT):LONGINT;

BEGIN
 InternalTest:=L+10;
END;

BEGIN
 A:=Internaltest(20)+5;
END;

PROCEDURE Testsomething(VAR A:WORD);

FUNCTION Internaltest(L:LONGINT):WORD;

BEGIN
 InternalTest:=L+15;
END;

BEGIN
 A:=Internaltest(20)+5;
END;

PROCEDURE Testsomething2(VAR A:LONGINT);

FUNCTION Internaltest(L:LONGINT):LONGINT;

BEGIN
 InternalTest:=L+10;
END;

BEGIN
 A:=Internaltest(20)+5;
END;

PROCEDURE Testsomething2(VAR A:WORD);

FUNCTION Internaltest(L:LONGINT):WORD;

BEGIN
 InternalTest:=L+15;
END;

BEGIN
 A:=Internaltest(20)+5;
END;

PROCEDURE Testsomething3(VAR A:WORD);forward;

PROCEDURE Testsomething3(VAR A:LONGINT);

FUNCTION Internaltest(L:LONGINT):LONGINT;

BEGIN
 InternalTest:=L+10;
END;

BEGIN
 A:=Internaltest(20)+5;
END;

PROCEDURE Testsomething3(VAR A:WORD);

FUNCTION Internaltest(L:LONGINT):WORD;

BEGIN
 InternalTest:=L+15;
END;

BEGIN
 A:=Internaltest(20)+5;
END;

VAR O  : LONGINT;
    O2 : WORD;

BEGIN
 TestSomething(O);
 TestSomething(O2);
END.
