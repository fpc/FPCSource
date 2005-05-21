{ %RESULT=217 }
{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondtryfinally()                               }
{               secondraise()                                    }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondtypeconv()                               }
{                 secondtryexcept()                              }
{                 secondcalln()                                  }
{                 secondadd()                                    }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{****************************************************************}
program ttryfin3;

{$ifdef fpc}
{$mode objfpc}
{$endif}

Type
  TAObject = class(TObject)
    a : longint;
    end;
  TBObject = Class(TObject)
    b : longint;
    end;


{ The test cases were taken from the SAL internal architecture manual }

    procedure fail;
    begin
      WriteLn('Failure.');
      halt(1);
    end;

var
 global_counter : integer;

Procedure raiseanexception;

Var A : TAObject;

begin
{  Writeln ('Creating exception object');}
  A:=TAObject.Create;
{  Writeln ('Raising with this object');}
  raise A;
  { this should never happen, if it does there is a problem! }
  RunError(255);
end;


procedure IncrementCounter(x: integer);
begin
  Inc(global_counter);
end;

procedure DecrementCounter(x: integer);
begin
  Dec(global_counter);
end;


{  }
Procedure DoTryFinallyOne;
var
 failed : boolean;
begin
  Write('Try..Finally with exception rise in finally block...');
  global_counter:=0;
  failed:=true;
  Try
    IncrementCounter(global_counter);
    DecrementCounter(global_counter);
  finally
    RaiseAnException;
  end;
end;



Begin
  DoTryFinallyOne;
end.
