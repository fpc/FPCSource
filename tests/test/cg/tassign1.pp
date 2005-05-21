{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondassign()                                   }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS : Tested with Delphi 3 as reference implementation     }
{           Tests the sortstring assignment.                     }
{****************************************************************}
program tassign1;

{$ifdef fpc}
{$mode objfpc}
{$endif}

const
  RESULT_STRING = 'Hello world';



    procedure fail;
    begin
      WriteLn('Failure.');
      halt(1);
    end;

    function getc : char;
     begin
      getc := 'a';
     end;


var
 failed : boolean;
 s: shortstring;
 c: char;
Begin
  Write('secondassign shortstring node testing...');
  failed := false;

  { constant string }
  s:=RESULT_STRING;
  if s<>RESULT_STRING then
    failed := true;
  { empty constant string, small optim. }
  s:='';
  if s<>'' then
    failed := true;
  { constant character }
  s:='a';
  if s<>'a' then
    failed := true;
  { non-constant character }
  c:='a';
  s:=c;
  if s<>'a' then
     failed := true;

  s:=getc;
  if s<>'a' then
     failed := true;

  if failed then
    fail
  else
    WriteLn('Success!');
end.
