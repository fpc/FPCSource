Program tw40595;

{$mode OBJFPC}{$H+}{$M+}
{$Modeswitch advancedrecords}
{$RTTI EXPLICIT PROPERTIES([vcPrivate,vcProtected,vcPublic,vcPublished])}
{$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
{$RTTI EXPLICIT METHODS([vcPrivate,vcProtected,vcPublic,vcPublished])}

Uses
  SysUtils,
  TypInfo;

Type
  { TUser }

  TUser = Class
  Private
    FName: string;
    FID: Integer;
  Public
    Procedure PrintID;
    Constructor Create;
    Procedure PrintName;
  Published
    Property ID: Integer read FID write FID;
    Property Name: string read FName write FName;
  End;

  { TUser }

  Constructor TUser.Create;
  Begin
    FName := 'TUser.Name: ' + WChar($20A1) + ' ' + WChar($2211);
  End;

  Procedure TUser.PrintID;
  Begin
    WriteLn('User ID   : ', ID);
  End;

  Procedure TUser.PrintName;
  Begin
    WriteLn('User Name : ', Name);
  End;

const
  MethodNames: array of String = (
    'PrintID',
    'Create',
    'PrintName'
  );
Var
  I: Integer;
  Count: LongInt;
  Method: PVmtMethodExEntry;
  Methods: PExtendedMethodInfoTable;
Begin
  Count := GetMethodList(TUser, Methods, []);
  If Methods <> nil Then
  Begin
    if Count <> Length(MethodNames) then
      Halt(2);
    For I := 0 To Pred(Count) Do
    Begin
      Method := Methods^[I];
      if Method^.Name <> MethodNames[i] then
        Halt(3 + i);
      WriteLn(Format('%-10s - %d', [Method^.Name, Method^.Kind]));
    End;
    Freemem(Methods);
  End else
    Halt(1);
End.

