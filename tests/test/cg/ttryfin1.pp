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
program ttryfin1;

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


{ Will the finally clause of a try block be called if the try block exited normally? }
Function DoTryFinallyOne: boolean;
var
 failed : boolean;
begin
  Write('Try..Finally clause...');
  global_counter:=0;
  failed:=true;
  DoTryFinallyOne := failed;
  Try
    IncrementCounter(global_counter);
    DecrementCounter(global_counter);
  finally
    if global_counter = 0 then
      failed :=false;
    DoTryFinallyOne := failed;
  end;
end;


{
  Will the finally clause of a try block be called if the try block
  is inside a sub-block and the try block is exited with the break
  statement?
}
Function DoTryFinallyTwo : boolean;
var
 failed : boolean;
begin
  Write('Try..Finally with break statement...');
  global_counter:=0;
  failed:=true;
  DoTryFinallyTwo := failed;
  while (failed) do
    begin
      Try
       IncrementCounter(global_counter);
       DecrementCounter(global_counter);
       break;
      finally
        if global_counter = 0 then
          failed :=false;
        DoTryFinallyTwo := failed;
     end;
  end;
end;


{
  Will the finally clause of a try block be called if the try block
  is inside a sub-block and the try block is exited with the continue
  statement?
}
Function DoTryFinallyThree : boolean;
var
 failed : boolean;
begin
  Write('Try..Finally with continue statement...');
  global_counter:=0;
  failed:=true;
  DoTryFinallyThree := failed;
  while (failed) do
    begin
      Try
       IncrementCounter(global_counter);
       DecrementCounter(global_counter);
       continue;
      finally
        if global_counter = 0 then
           failed :=false;
        DoTryFinallyThree := failed;
     end;
  end;
end;


{
  Will the finally clause of a try block be called if the try block
  is inside a sub-block and the try block is exited with the exit
  statement?
}
Function DoTryFinallyFour: boolean;
var
 failed : boolean;
begin
  Write('Try..Finally with exit statement...');
  global_counter:=0;
  failed:=true;
  DoTryFinallyFour := failed;
  while (failed) do
    begin
      Try
       IncrementCounter(global_counter);
       DecrementCounter(global_counter);
       exit;
      finally
        if global_counter = 0 then
           failed :=false;
        DoTryFinallyFour := failed;
     end;
  end;
end;


(*
{ Will the finally clause of a try block be called if the try block raises an exception? }
Procedure DoTryFinallyThree;
var
 failed : boolean;
begin
  Write('Try..Finally with exception rise...');
  global_counter:=0;
  failed:=true;
  Try
    IncrementCounter(global_counter);
    RaiseAnException;
    DecrementCounter(global_counter);
  finally
    if global_counter = 1 then
      failed :=false;
    if failed then
      fail
    else
      WriteLn('Success!');
  end;
end;
*)


{ Will the finally clause of all nested try blocks be called if the try blocks exited normally? }
Function DoTryFinallyFive: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Finally nested clauses (three-level nesting)...');
  global_counter:=0;
  failed:=true;
  DoTryFinallyFive := failed;
  x:=0;
  Try
    IncrementCounter(global_counter);
    Try
        DecrementCounter(global_counter);
        IncrementCounter(global_counter);
        Try
           DecrementCounter(global_counter);
        finally
          Inc(x);
        end;
    finally
      Inc(x);
    End;
  finally
    if (global_counter = 0) and (x = 2) then
      failed :=false;
    DoTryFinallyFive := failed;
  end;
end;


{
   Will the finally clauses of all try blocks be called if they are
   nested within each other and all are nested within a sub-block
   and a break statement is encountered in the innermost try
   block?
}
Function DoTryFinallySix : boolean;
var
 failed : boolean;
 x: integer;
begin
  Write('Try..Finally nested clauses with break statement...');
  global_counter:=0;
  x:=0;
  failed:=true;
  DoTryFinallySix := failed;
  while (failed) do
  begin
      Try
        IncrementCounter(global_counter);
        Try
          DecrementCounter(global_counter);
          IncrementCounter(global_counter);
          Try
             DecrementCounter(global_counter);
             break;
          finally
            Inc(x);
          end;
        finally
            Inc(x);
        End;
     finally
        if (global_counter = 0) and (x = 2) then
          failed :=false;
        DoTryFinallySix := failed;
     end;
  end;
end;


{
   Will the finally clauses of all try blocks be called if they are
   nested within each other and all are nested within a sub-block
   and a continue statement is encountered in the innermost try
   block?
}
Function DoTryFinallySeven : boolean;
var
 failed : boolean;
 x: integer;
begin
  Write('Try..Finally nested clauses with continue statement...');
  global_counter:=0;
  x:=0;
  failed:=true;
  DoTryFinallySeven := failed;
  while (failed) do
  begin
      Try
        IncrementCounter(global_counter);
        Try
          DecrementCounter(global_counter);
          IncrementCounter(global_counter);
          Try
             DecrementCounter(global_counter);
             continue;
          finally
            Inc(x);
          end;
        finally
            Inc(x);
        End;
     finally
        if (global_counter = 0) and (x = 2) then
          failed :=false;
        DoTryFinallySeven := failed;
     end;
  end;
end;

{
   Will the finally clauses of all try blocks be called if they are
   nested within each other and all are nested within a sub-block
   and an exit statement is encountered in the innermost try
   block?
}
Function DoTryFinallyEight : boolean;
var
 failed : boolean;
 x: integer;
begin
  Write('Try..Finally nested clauses with exit statement...');
  global_counter:=0;
  x:=0;
  failed:=true;
  DoTryFinallyEight := failed;
  while (failed) do
  begin
      Try
        IncrementCounter(global_counter);
        Try
          DecrementCounter(global_counter);
          IncrementCounter(global_counter);
          Try
             DecrementCounter(global_counter);
             exit;
          finally
            Inc(x);
          end;
        finally
            Inc(x);
        End;
     finally
        if (global_counter = 0) and (x = 2) then
          failed :=false;
        DoTryFinallyEight := failed;
     end;
  end;
end;

(*
------------------
*)
{
  If several try blocks are nested within a sub-block, and that sub-block is
  nested in a try block within another try block, and the innermost try
  blocks are exited due to a break, will all finally clauses be called?
}
Function DoTryFinallyNine : boolean;
var
 failed : boolean;
 x: integer;
begin
  Write('Try..Finally nested clauses with break statement in other try-block...');
  global_counter:=0;
  x:=0;
  failed:=true;
  DoTryFinallyNine := failed;
  Try
    while (failed) do
    begin
        Try
          IncrementCounter(global_counter);
          Try
            DecrementCounter(global_counter);
            IncrementCounter(global_counter);
            Try
               DecrementCounter(global_counter);
               break;
            finally
              Inc(x);
            end;
          finally
              Inc(x);
          End;
       finally
          if (global_counter = 0) and (x = 2) then
            failed :=false;
          DoTryFinallyNine := failed;
       end;
    end; {end while }
  finally
    { normally this should execute! }
    DoTryFinallyNine := failed;
  end;
end;


{
  If several try blocks are nested within a sub-block, and that sub-block is
  nested in a try block within another try block, and the innermost try
  blocks are exited due to an exit, will all finally clauses be called?
}
Function DoTryFinallyTen : boolean;
var
 failed : boolean;
 x: integer;
begin
  Write('Try..Finally nested clauses with exit statement in other try-block...');
  global_counter:=0;
  x:=0;
  failed:=true;
  DoTryFinallyTen := failed;
  Try
    while (failed) do
    begin
        Try
          IncrementCounter(global_counter);
          Try
            DecrementCounter(global_counter);
            IncrementCounter(global_counter);
            Try
               DecrementCounter(global_counter);
               exit;
            finally
              Inc(x);
            end;
          finally
              Inc(x);
          End;
       finally
          x:=1;
       end;
    end; {end while }
  finally
    { normally this should execute! }
    if (global_counter = 0) and (x = 1) then
       failed :=false;
    DoTryFinallyTen := failed;
  end;
end;


var
 failed: boolean;
begin
  failed := DoTryFinallyOne;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryFinallyTwo;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryFinallyThree;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryFinallyFour;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryFinallyFive;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryFinallySix;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryFinallySeven;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryFinallyEight;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryFinallyNine;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryFinallyTen;
  if failed then
   fail
  else
   WriteLn('Success!');
end.
