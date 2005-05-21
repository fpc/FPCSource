{******************************************}
{  Used to check the DOS unit              }
{------------------------------------------}
{  TestEncCount routine testing            }
{******************************************}
Program tenv;

uses dos;


const
  has_errors : boolean = false;


{ verifies that the DOSError variable is equal to }
{ the value requested.                            }
Procedure CheckDosError(err: Integer);
 var
  x : integer;
  s :string;
 Begin
  Write('Verifying value of DOS Error...');
  x := DosError;
  case x of
  0 : s := '(0): No Error.';
  2 : s := '(2): File not found.';
  3 : s := '(3): Path not found.';
  5 : s := '(5): Access Denied.';
  6 : s := '(6): Invalid File Handle.';
  8 : s := '(8): Not enough memory.';
  10 : s := '(10) : Invalid Environment.';
  11 : s := '(11) : Invalid format.';
  18 : s := '(18) : No more files.';
  else
    s := 'INVALID DOSERROR';
  end;
  if err <> x then
    Begin
      WriteLn('FAILURE. (Value should be ',err,' '+s+')');
      has_errors:=true;
    end
  else
    WriteLn('Success.');
 end;


Procedure TestEnvCount;
Var
 I: Integer;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                       ENVCOUNT/ENVSTR                                ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Environment variables should be of the form VAR=VALUE          ');
 WriteLn(' Note: Non valid indexes should return empty strings.                 ');
 WriteLn(' Note: Index 0 points to an empty string                              ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 {*------------------------- NOTE -------------------------------------*}
 {* Variables should be of the form VAR=VALUE                          *}
 {*--------------------------------------------------------------------*}
 WriteLn('Number of environment variables : ',EnvCount);
 WriteLn('CURRENT ENVIRONMENT');
 For I:=1 to EnvCount do
  WriteLn(EnvStr(i));
 CheckDosError(0);
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: The next few lines should be empty strings, as they are        ');
 WriteLn('       invalid environment indexes.                                   ');
 WriteLn('----------------------------------------------------------------------');
 For i:=-5 to 0 do
  WriteLn(EnvStr(i));
 CheckDosError(0);
 For i:=20000 to 20002 do
  WriteLn(EnvStr(i));
 CheckDosError(0);
end;

Begin
  TestEnvCount;
  if has_errors then
    Halt(1);
end.
