{******************************************}
{  Used to check the DOS unit              }
{------------------------------------------}
{  SetCBreak / GetCBreak routine testing   }
{******************************************}
Program tbreak;

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


Procedure TestCBreak;
Var
 B: Boolean;
 s: string;
Begin
 WriteLn('----------------------------------------------------------------------');
 WriteLn('                       GETCBREAK/SETCBREAK                            ');
 WriteLn('----------------------------------------------------------------------');
 CheckDosError(0);
 s:='Testing GetCBreak...';
 SetCBreak(TRUE);
 CheckDosError(0);
 GetCBreak(b);
 CheckDosError(0);
 if b then
   WriteLn(s+'Success.')
 else
  Begin
    has_errors:=true;
    WriteLn(s+'FAILURE.');
  end;
{ actually setting Ctrl-C only works under DOS }
{$ifdef go32v2}
 s:='Testing GetCBreak...';
 SetCBreak(FALSE);
 CheckDosError(0);
 GetCBreak(b);
 CheckDosError(0);
 if NOT b then
   WriteLn(s+'Success.')
 else
  Begin
    has_errors:=true;
    WriteLn(s+'FAILURE.');
  end;
{$endif}
end;

Begin
  testcbreak;
  if has_errors then
    Halt(1);
end.
{
  $Log$
  Revision 1.3  2002-11-18 09:49:49  pierre
   * tried to make as many as possible tests non interactive

  Revision 1.2  2002/11/09 23:08:07  carl
    * fix compilation problems

  Revision 1.1  2002/11/08 21:01:18  carl
    * separated some tests
    * make tfexpand more portable

}
