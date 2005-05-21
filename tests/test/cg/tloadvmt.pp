{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondloadvmt()                                  }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS : Tested with Delphi 3 as reference implementation     }
{****************************************************************}
program tloadvmt;

{$ifdef fpc}
{$mode objfpc}
{$endif}

const
  RESULT_STRING = 'Hello world';

Type
  TAObject = class(TObject)
    a : longint;
    end;
  TBObject = Class(TAObject)
    b : longint;
    s : shortstring;
      constructor create(c: longint);
      function getstring : shortstring;
    end;


    procedure fail;
    begin
      WriteLn('Failure.');
      halt(1);
    end;


 constructor tbobject.create(c:longint);
  begin
    taobject.create;
    b:=c;
    s:=RESULT_STRING;
  end;

 function tbobject.getstring : shortstring;
  begin
    getstring := s;
  end;


var
 bobj: TBobject;
 i: integer;
 l : longint;
Begin
  i:=$7f;
  Write('Secondloadvmt test...');
  bobj:=TBobject.create(i);
  if bobj.getstring <> RESULT_STRING then
    fail
  else
    WriteLn('Success!');
end.
