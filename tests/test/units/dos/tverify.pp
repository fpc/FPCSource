{******************************************}
{  Used to check the DOS unit              }
{------------------------------------------}
{  SetVerify / GetVerify routine testing   }
{******************************************}
Program tverify;

uses dos;

{$IFDEF GO32V2}
{$DEFINE SUPPORTS_VERIFY}
{$ENDIF}

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
      Halt(1);
    end
  else
    WriteLn('Success.');
 end;

Procedure TestVerify;
Var
 B: Boolean;
 s: string;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                       GETVERIFY/SETVERIFY                            ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 s:='Testing GetVerify...';
 SetVerify(TRUE);
 CheckDosError(0);
 GetVerify(b);
 CheckDosError(0);
 if b then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE.');
    halt(1);
  end;
 s:='Testing GetVerify...';
 SetVerify(FALSE);
 CheckDosError(0);
 GetVerify(b);
 CheckDosError(0);
{ verify actually only works under dos       }
{ and always returns TRUE on other platforms }
{$ifdef supports_verify}
 if NOT b then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE.');
    halt(1);
  end;
{$else}
 if b then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE.');
    halt(1);
  end;
{$endif}
end;


Begin
  testverify;
end.
{
  $Log$
  Revision 1.1  2002-11-08 21:01:18  carl
    * separated some tests
    * make tfexpand more portable

}  
