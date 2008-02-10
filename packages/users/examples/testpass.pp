Program TestPass;
{$mode delphi}
{Test the user's password}
{$DEFINE DEBUG}

// The funcs in shadow are linux only.
uses {$ifdef linux}shadow, {$endif} pwd  ,crypth ;

Var
  strUserName, Password : String;
  sEntry : PPasswordFileEntry;
  pEntry : PPasswd;

Const
  Err_NoErr = 0;
  Err_NoUser = 1;
  Err_WrongPass = 2;
  NoUser = '*NO USER*';

Function UserEncPass(User: String): String;
Var
  A : Array[0..255] of char;
Begin
  A := strUserName;
  {$IFDEF DEBUG}
  Writeln('User name is ',A);
  {$ENDIF}
  sEntry := getspnam(A);
  If sEntry = nil then
  Begin
    {$IFDEF DEBUG}
    Writeln('No shadow entry');
    {$ENDIF}
    pEntry := fpgetpwnam(A);
    If pEntry = nil then
    Begin
      {$IFDEF DEBUG}
      Writeln('No passwd entry');
      {$ENDIF}
      UserEncPass := NoUser
    End
  End;
  if sEntry <> nil then UserEncPass := sEntry^.sp_pwdp;
  if pEntry <> nil then UserEncPass := pEntry^.pw_passwd
End;

Function CheckPass(User, Pass: String): Integer;
Var
  EncPass, ResultPass, SSalt : String;
  PCPass, PCSalt, PCResult : Array[0..255] of Char;
  I : Integer;
Begin
  EncPass := UserEncPass(User);
  {$IFDEF DEBUG}
  Writeln('Encrypted PW is : ',EncPass);
  {$ENDIF}
  If EncPass = NoUser then
  Begin
    CheckPass := Err_NoUser;
    {$IFDEF DEBUG}
    Writeln('No user named ',User,'.');
    {$ENDIF}
    Exit
  End;
  PCPass := Pass;
  If Copy(EncPass,1,3) = '$1$' then
  Begin
    I := 4;
    SSalt := '$1$';
    Repeat
      SSalt := SSalt + EncPass[I];
      Inc(I)
    Until EncPass[I] = '$';
    PCSalt := SSalt
  End
  else
    PCSalt := Copy(EncPass,1,2);
  {$IFDEF DEBUG}
  Writeln('Salt is : ',PCSalt);
  {$ENDIF}
  PCResult := crypt(PCPass, PCSalt);
  ResultPass := PCResult;
  {$IFDEF DEBUG}
  Writeln('From passwd/shadow : ',EncPass);
  Writeln('From crypt : ',ResultPass);
  {$ENDIF}
  If ResultPass = EncPass then
    CheckPass := Err_NoErr
  else
    CheckPass := Err_WrongPass
End;

Begin
  Write('User name : ');
  Readln(strUserName);
  Write('Password : ');
  Readln(Password);
  If CheckPass(strUserName, Password) = Err_NoErr then
    Writeln('User authentication succeeded')
  else
    Writeln('User autentication failed.');
End.
