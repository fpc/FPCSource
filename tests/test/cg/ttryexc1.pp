{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondtryexcept()                                }
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
{ REMARKS : Tested with Delphi 3 as reference implementation     }
{****************************************************************}
program ttryexc1;

{$ifdef fpc}
{$mode objfpc}
{$endif}

Type
  TAObject = class(TObject)
    a : longint;
    end;
  TBObject = Class(TObject)
    b : longint;
      constructor create(c: longint);
    end;


{ The test cases were taken from the SAL internal architecture manual }

    procedure fail;
    begin
      WriteLn('Failure.');
      halt(1);
    end;

var
 global_counter : integer;


 constructor tbobject.create(c:longint);
  begin
    inherited create;
    b:=c;
  end;


Procedure raiseanexception;

Var A : TAObject;
var B : TAobject;

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


Function DoTryExceptOne: boolean;
var
 failed : boolean;
begin
  Write('Try..Except clause...');
  global_counter:=0;
  failed:=true;
  DoTryExceptOne := failed;
  Try
    IncrementCounter(global_counter);
    DecrementCounter(global_counter);
  except
  end;
  if global_counter = 0 then
      failed :=false;
  DoTryExceptOne := failed;
end;


Function DoTryExceptTwo : boolean;
var
 failed : boolean;
begin
  Write('Try..Except with break statement...');
  global_counter:=0;
  failed:=true;
  DoTryExceptTwo := failed;
  while (failed) do
    begin
      Try
       IncrementCounter(global_counter);
       DecrementCounter(global_counter);
       break;
      except
      end;
  end;
  if global_counter = 0 then
    failed :=false;
  DoTryExceptTwo := failed;
end;




Function DoTryExceptFour: boolean;
var
 failed : boolean;
begin
  Write('Try..Except with exit statement...');
  global_counter:=0;
  failed:=true;
  DoTryExceptFour := failed;
  while (failed) do
    begin
      Try
       IncrementCounter(global_counter);
       DecrementCounter(global_counter);
       DoTryExceptFour := false;
       exit;
      except
      end;
  end;
end;


Function DoTryExceptFive: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Except nested clauses (three-level nesting)...');
  global_counter:=0;
  failed:=true;
  DoTryExceptFive := failed;
  x:=0;
  Try
    IncrementCounter(global_counter);
    Try
        DecrementCounter(global_counter);
        IncrementCounter(global_counter);
        Try
           DecrementCounter(global_counter);
        except
          Inc(x);
        end;
    except
      Inc(x);
    End;
  except
  end;
  if (global_counter = 0) then
   failed :=false;
  DoTryExceptFive := failed;
end;


Function DoTryExceptSix : boolean;
var
 failed : boolean;
 x: integer;
begin
  Write('Try..Except nested clauses with break statement...');
  global_counter:=0;
  x:=0;
  failed:=true;
  DoTryExceptSix := failed;
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
          except
            Inc(x);
          end;
        except
            Inc(x);
        End;
     except
     end;
 end;
 if (global_counter = 0) then
   failed :=false;
 DoTryExceptSix := failed;
end;


Function DoTryExceptEight : boolean;
var
 failed : boolean;
 x: integer;
begin
  Write('Try..Except nested clauses with exit statement...');
  global_counter:=0;
  x:=0;
  failed:=true;
  DoTryExceptEight := failed;
  while (failed) do
  begin
      Try
        IncrementCounter(global_counter);
        Try
          DecrementCounter(global_counter);
          IncrementCounter(global_counter);
          Try
             DecrementCounter(global_counter);
             DoTryExceptEight := false;
             exit;
          except
            Inc(x);
          end;
        except
            Inc(x);
        End;
     except
     end;
  end;
end;


Function DoTryExceptNine : boolean;
var
 failed : boolean;
 x: integer;
begin
  Write('Try..Except nested clauses with break statement in other try-block...');
  global_counter:=0;
  x:=0;
  failed:=true;
  DoTryExceptNine := failed;
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
            except
              Inc(x);
            end;
          except
              Inc(x);
          End;
       except
       end;
    end; {end while }
  except
    { normally this should execute! }
    DoTryExceptNine := failed;
  end;
  if (global_counter = 0) and (x = 0) then
    failed :=false;
  DoTryExceptNine := failed;
end;


{****************************************************************************}

{***************************************************************************}
{                          Exception is thrown                              }
{***************************************************************************}
Function DoTryExceptTen: boolean;
var
 failed : boolean;
begin
  Write('Try..Except clause with raise...');
  global_counter:=0;
  failed:=true;
  DoTryExceptTen := failed;
  Try
    IncrementCounter(global_counter);
    RaiseAnException;
    DecrementCounter(global_counter);
  except
      if global_counter = 1 then
          failed :=false;
      DoTryExceptTen := failed;
  end;
end;

Function DoTryExceptEleven : boolean;
var
 failed : boolean;
begin
  Write('Try..Except with raise and break statement...');
  global_counter:=0;
  failed:=true;
  DoTryExceptEleven := failed;
  while (failed) do
    begin
      Try
       IncrementCounter(global_counter);
       DecrementCounter(global_counter);
       RaiseAnException;
       break;
      except
       if global_counter = 0 then
         failed :=false;
       DoTryExceptEleven := failed;
      end;
  end;
end;

Function DoTryExceptTwelve: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Except nested clauses (three-level nesting)...');
  global_counter:=0;
  failed:=true;
  DoTryExceptTwelve := failed;
  x:=0;
  Try
    IncrementCounter(global_counter);
    Try
        DecrementCounter(global_counter);
        IncrementCounter(global_counter);
        Try
           DecrementCounter(global_counter);
           RaiseAnException;
        except
          if (global_counter = 0) then
            failed :=false;
          DoTryExceptTwelve := failed;
        end;
    except
      DoTryExceptTwelve := true;
    End;
  except
      DoTryExceptTwelve := true;
  end;
end;


Function DoTryExceptThirteen: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Except nested clauses (three-level nesting)...');
  global_counter:=0;
  failed:=true;
  DoTryExceptThirteen := failed;
  x:=0;
  Try
    IncrementCounter(global_counter);
    Try
        DecrementCounter(global_counter);
        IncrementCounter(global_counter);
        RaiseAnException;
        Try
           DecrementCounter(global_counter);
        except
          DoTryExceptThirteen := true;
        end;
    except
      if (global_counter = 1) then
        failed :=false;
      DoTryExceptThirteen := failed;
    End;
  except
      DoTryExceptThirteen := true;
  end;
end;

{***************************************************************************}
{                   Exception is thrown in except block                     }
{***************************************************************************}
Function DoTryExceptFourteen: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Except nested clauses with single re-raise...');
  global_counter:=0;
  failed:=true;
  DoTryExceptFourteen := failed;
  x:=0;
  Try
    IncrementCounter(global_counter);
    Try
        DecrementCounter(global_counter);
        IncrementCounter(global_counter);
        Try
           DecrementCounter(global_counter);
           RaiseAnException;
        except
          { raise to next block }
          Raise;
        end;
    except
      if (global_counter = 0) then
        failed :=false;
      DoTryExceptFourteen := failed;
    End;
  except
      DoTryExceptFourteen := true;
  end;
end;



Function DoTryExceptFifteen: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Except nested clauses with re-reraises (1)...');
  global_counter:=0;
  failed:=true;
  DoTryExceptFifteen := failed;
  x:=0;
  Try
    IncrementCounter(global_counter);
    Try
        DecrementCounter(global_counter);
        IncrementCounter(global_counter);
        Try
           DecrementCounter(global_counter);
           RaiseAnException;
        except
          { raise to next block }
          Raise;
        end;
    except
       { re-raise to next block }
       Raise;
    End;
  except
      if (global_counter = 0) then
        failed :=false;
      DoTryExceptFifteen := failed;
  end;
end;

procedure nestedtryblock(var global_counter: integer);
begin
    IncrementCounter(global_counter);
    Try
        DecrementCounter(global_counter);
        IncrementCounter(global_counter);
        Try
           DecrementCounter(global_counter);
           RaiseAnException;
        except
          { raise to next block }
          Raise;
        end;
    except
       { re-raise to next block }
       Raise;
    End;
end;


Function DoTryExceptSixteen: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Except nested clauses with re-reraises (2)...');
  global_counter:=0;
  failed:=true;
  DoTryExceptSixteen := failed;
  x:=0;
  Try
    NestedTryBlock(global_counter);
  except
      if (global_counter = 0) then
        failed :=false;
      DoTryExceptSixteen := failed;
  end;
end;


Function DoTryExceptSeventeen: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Except nested clauses with raises...');
  global_counter:=0;
  failed:=true;
  DoTryExceptSeventeen := failed;
  x:=0;
  Try
    IncrementCounter(global_counter);
    Try
        DecrementCounter(global_counter);
        IncrementCounter(global_counter);
        Try
           DecrementCounter(global_counter);
           RaiseAnException;
        except
          { raise to next block }
          raise TAObject.Create;
        end;
    except
       { re-raise to next block }
       raise TBObject.Create(1234);
    End;
  except
      if (global_counter = 0) then
        failed :=false;
      DoTryExceptSeventeen := failed;
  end;
end;

{***************************************************************************}
{                  Exception flow control in except block                   }
{***************************************************************************}
Function DoTryExceptEighteen: boolean;
var
 failed : boolean;
begin
  Write('Try..Except clause with raise with break in except block...');
  global_counter:=0;
  failed:=true;
  DoTryExceptEighteen := failed;
  while (failed) do
    begin
        Try
          IncrementCounter(global_counter);
          RaiseAnException;
          DecrementCounter(global_counter);
        except
            if global_counter = 1 then
                failed :=false;
            DoTryExceptEighteen := failed;
            break;
        end;
    end;
end;


Function DoTryExceptNineteen: boolean;
var
 failed : boolean;
begin
  Write('Try..Except clause with raise with exit in except block...');
  global_counter:=0;
  failed:=true;
  DoTryExceptNineteen := failed;
  while (failed) do
    begin
        Try
          IncrementCounter(global_counter);
          RaiseAnException;
          DecrementCounter(global_counter);
        except
            if global_counter = 1 then
                failed :=false;
            DoTryExceptNineteen := failed;
            exit;
        end;
    end;
end;


Function DoTryExceptTwenty: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Except nested clauses with raises with break in inner try...');
  global_counter:=0;
  failed:=true;
  DoTryExceptTwenty := failed;
  x:=0;
  Try
    IncrementCounter(global_counter);
    Try
        while (x = 0) do
          begin
            DecrementCounter(global_counter);
            IncrementCounter(global_counter);
            Try
               DecrementCounter(global_counter);
               RaiseAnException;
            except
              { raise to next block }
              raise TAObject.Create;
              break;
            end;
          end;
    except
       { re-raise to next block }
       raise TBObject.Create(1234);
    End;
  except
      if (global_counter = 0) then
        failed :=false;
      DoTryExceptTwenty := failed;
  end;
end;


Function DoTryExceptTwentyOne: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Except nested clauses with raises with continue in inner try...');
  global_counter:=0;
  failed:=true;
  DoTryExceptTwentyOne := failed;
  x:=0;
  Try
    IncrementCounter(global_counter);
    Try
        while (x = 0) do
          begin
            DecrementCounter(global_counter);
            IncrementCounter(global_counter);
            Try
               DecrementCounter(global_counter);
               RaiseAnException;
            except
              { raise to next block }
              raise TAObject.Create;
              x:=1;
              continue;
            end;
          end;
    except
       { re-raise to next block }
       raise TBObject.Create(1234);
    End;
  except
      if (global_counter = 0) then
        failed :=false;
      DoTryExceptTwentyOne := failed;
  end;
end;


Function DoTryExceptTwentyTwo: boolean;
var
 failed : boolean;
 x : integer;
begin
  Write('Try..Except nested clauses with raises with exit in inner try...');
  global_counter:=0;
  failed:=true;
  DoTryExceptTwentyTwo := failed;
  x:=0;
  Try
    IncrementCounter(global_counter);
    Try
        while (x = 0) do
          begin
            DecrementCounter(global_counter);
            IncrementCounter(global_counter);
            Try
               DecrementCounter(global_counter);
               RaiseAnException;
            except
              { raise to next block }
              raise TAObject.Create;
              exit;
            end;
          end;
    except
       { re-raise to next block }
       raise TBObject.Create(1234);
    End;
  except
      if (global_counter = 0) then
        failed :=false;
      DoTryExceptTwentyTwo := failed;
  end;
end;


var
 failed: boolean;
begin
  failed := DoTryExceptOne;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptTwo;
  if failed then
   fail
  else
   WriteLn('Success!');
{  failed := DoTryExceptThree;
  if failed then
   fail
  else
   WriteLn('Success!');}
  failed := DoTryExceptFour;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptFive;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptSix;
  if failed then
   fail
  else
   WriteLn('Success!');
{  failed := DoTryExceptSeven;
  if failed then
   fail
  else
   WriteLn('Success!');}
  failed := DoTryExceptEight;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptNine;
  if failed then
   fail
  else
   WriteLn('Success!');
  (************************ Exceptions are created from here ****************************)
  failed := DoTryExceptTen;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptEleven;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptTwelve;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptThirteen;
  if failed then
   fail
  else
   WriteLn('Success!');
  (************************ Exceptions in except block       ****************************)
  failed := DoTryExceptFourteen;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptFifteen;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptSixteen;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptSeventeen;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptEighteen;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptNineteen;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptTwenty;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptTwentyOne;
  if failed then
   fail
  else
   WriteLn('Success!');
  failed := DoTryExceptTwentyTwo;
  if failed then
   fail
  else
   WriteLn('Success!');
end.
