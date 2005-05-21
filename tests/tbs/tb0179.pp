{ Old file: tbs0213.pp }
{ name mangling problem with nested procedures in overloaded }

uses
  ub0179;

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

VAR O  : LONGINT;
    O2 : WORD;

BEGIN
 TestSomething(O);
 TestSomething(O2);
END.
