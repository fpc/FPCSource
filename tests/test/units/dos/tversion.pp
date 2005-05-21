{ %INTERACTIVE }

{******************************************}
{  Used to check the DOS unit              }
{------------------------------------------}
{  DosVersion routine testing              }
{******************************************}
program tversion;

uses dos;

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
    end
  else
    WriteLn('Success.');
 end;



Procedure TestDosVersion;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                          DOSVERSION                                  ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Number should be major version followed by minor version.      ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 {*------------------------- NOTE -------------------------------------*}
 {* This is OS specific. LO -> Major revision, HI -> Minor Revision    *}
 {*--------------------------------------------------------------------*}
 WriteLn('Operating system Version : ',Lo(DosVersion),'.',Hi(DosVersion));
 CheckDosError(0);
end;

Begin
  TestDosVersion;
end.
