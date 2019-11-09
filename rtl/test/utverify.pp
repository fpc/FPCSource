{******************************************}
{  Used to check the DOS unit              }
{------------------------------------------}
{  SetVerify / GetVerify routine testing   }
{******************************************}
{$mode objfpc}
unit utverify;

interface

uses punit, utrtl;

implementation

uses utdos, dos;

{$IFDEF GO32V2}
{$DEFINE SUPPORTS_VERIFY}
{$ENDIF}


Function TestVerify : TTestString;

Var
 B: Boolean;
 s: string;
Begin
  Result:='';
  B:=False;
  if ShowDebugOutput then
    begin
    WriteLn('----------------------------------------------------------------------');
    WriteLn('                       GETVERIFY/SETVERIFY                            ');
    WriteLn('----------------------------------------------------------------------');
    end;
  if not CheckDosError('Initial value',0) then exit;
  s:='Testing GetVerify...';
  SetVerify(TRUE);
  if not CheckDosError(S,0) then exit;
  GetVerify(b);
  if not CheckDosError(S,0) then exit;
  if not AssertEquals(S+' return value',true,B) then exit;
  s:='Testing SetVerify...';
  SetVerify(FALSE);
  if not CheckDosError(S,0) then exit;
  GetVerify(b);
  if not CheckDosError(S,0) then exit;
  { verify actually only works under dos       }
  { and always returns TRUE on other platforms }
  { not anymore (JM)                           }
  if not AssertEquals(S+' test 2',False, B) then exit;
end;


initialization
  AddTest('TestVerify',@testverify,EnsureSuite('Dos'));
end.
