{******************************************}
{  Used to check the DOS unit              }
{------------------------------------------}
{  TestEncCount routine testing            }
{******************************************}
{$mode objfpc}
unit utenv;

interface

uses punit, utrtl;

implementation

uses dos, utdos;

Function TestEnvCount : TTestString;

Var
 I: Integer;
 E,S : string;
Begin
  Result:='';
  if ShowDebugOutput then
    begin
    WriteLn('----------------------------------------------------------------------');
    WriteLn('                       ENVCOUNT/ENVSTR                                ');
    WriteLn('----------------------------------------------------------------------');
    WriteLn(' Note: Environment variables should be of the form VAR=VALUE          ');
    WriteLn(' Note: Non valid indexes should return empty strings.                 ');
    WriteLn(' Note: Index 0 points to an empty string                              ');
    WriteLn('----------------------------------------------------------------------');
    end;
  if not CheckDosError('Initial value',0) then exit;
  {*------------------------- NOTE -------------------------------------*}
  {* Variables should be of the form VAR=VALUE                          *}
  {*--------------------------------------------------------------------*}
  if not AssertTrue('Have environment',EnvCount>0) then exit;
  if ShowDebugOutput then
    begin
    WriteLn('Number of environment variables : ',EnvCount);
    WriteLn('CURRENT ENVIRONMENT');
    end;
  For I:=1 to EnvCount do
    begin
    Str(I,S);
    E:=EnvStr(i);
    if not CheckDosError('After getting valid environment variable '+S,0) then exit;
    if not AssertTrue('Environment var '+S+' is not empty',E<>'') then exit;
    if ShowDebugOutput then
      WriteLn(E);
    end;
  if ShowDebugOutput then
    begin
    WriteLn('----------------------------------------------------------------------');
    WriteLn(' Note: The next few lines should be empty strings, as they are        ');
    WriteLn('       invalid environment indexes.                                   ');
    WriteLn('----------------------------------------------------------------------');
    end;
 For i:=-5 to 0 do
   begin
   Str(I,S);
   E:=EnvStr(i);
   if not CheckDosError('After getting valid environment variable '+S,0) then exit;
   if not AssertTrue('Invalid environment var '+S+' is empty',E='') then exit;
   if ShowDebugOutput then
     WriteLn(E);
   end;
 For i:=EnvCount+10 to EnvCount+20 do
   begin
   Str(I,S);
   E:=EnvStr(i);
   if not CheckDosError('After getting valid environment variable '+S,0) then exit;
   if not AssertTrue('Invalid environment var '+S+' is empty',E='') then exit;
   if ShowDebugOutput then
     WriteLn(E);
   end;
end;

Begin
  AddTest('TestEnvCount',@TestEnvCount,EnsureSuite('Dos'));
end.
