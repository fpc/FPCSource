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
program traise2;

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


Begin
  raise TAobject.create;
end.

{
  $Log$
  Revision 1.2  2002-09-07 15:40:56  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/10 08:27:44  carl
    + mre tests for cg testuit

}
