{ %RESULT=217 }
{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondraise()                                    }
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
program traise5;

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


 constructor tbobject.create(c:longint);
  begin
    inherited create;
    b:=c;
  end;


  procedure MyRoutine;
   Begin
     WriteLn('hello world!');
   end;

var
 bobj: TBobject;
 i: integer;
Begin
  i:=$7f;
{$ifdef ver1_0}
  raise TBobject.create(i) at longint(@MyRoutine);
{$else}
  raise TBobject.create(i) at @MyRoutine;
{$endif}
end.
