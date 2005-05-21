{ %INTERACTIVE }
{ this one is interactive because
  on removable drives it will generate
  alert boxes on some OS like windows }

{******************************************}
{  Used to check the DOS unit              }
{------------------------------------------}
{  DiskFree / DiskSize   routine testing   }
{******************************************}
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


Procedure TestdiskSize;
Var
 i : Integer;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                          DISKSIZE/DISKFREE                           ');
 WriteLn('----------------------------------------------------------------------');
 WriteLn(' Note: Should return -1 on both functions if device is not ready.     ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
{ Check Disksize / DiskFree routines }
 for I:=0 to 20 do
 Begin
   Write('Disk unit ',i:2,' free size : ',DiskFree(i):10, ' Total Size: ',DiskSize(i):10);
   WriteLn(' bytes.');
 end;
 CheckDosError(0);
end;

Begin
  TestDiskSize;
  if has_errors then
    Halt(1);
end.
