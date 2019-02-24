{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST bridge : HTTP authorization

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sqldbrestauthini;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldbrestauth, inifiles;

Type
  TBasicAuthIniOption = (baoClearOnRead,      // Clear values first
                         baoSkipPassword,     // Do not save/load password
                         baoSkipMaskPassword, // do not mask the password
                         baoUserNameAsMask    // use the username as mask for password
                         );
  TBasicAuthIniOptions = Set of TBasicAuthIniOption;

  TSQLDBRestBasicAuthHelper = class helper for TRestBasicAuthenticator
  Private
    Procedure ClearValues;
  Public
    Procedure LoadFromIni(Const aIni: TCustomIniFile; aOptions : TBasicAuthIniOptions = []); overload;
    Procedure LoadFromIni(Const aIni: TCustomIniFile; ASection : String; aOptions : TBasicAuthIniOptions); overload;
    Procedure LoadFromFile(Const aFileName : String; aOptions : TBasicAuthIniOptions = []); overload;
    Procedure LoadFromFile(Const aFileName : String; Const ASection : String; aOptions : TBasicAuthIniOptions); overload;
    Procedure SaveToFile(Const aFileName : String; aOptions : TBasicAuthIniOptions = []);overload;
    Procedure SaveToFile(Const aFileName : String; Const ASection : String; aOptions : TBasicAuthIniOptions = []);overload;
    Procedure SaveToIni(Const aIni: TCustomIniFile; aOptions : TBasicAuthIniOptions = []); overload;
    Procedure SaveToIni(Const aIni: TCustomIniFile; ASection : String; aOptions : TBasicAuthIniOptions); overload;
  end;

Var
  DefaultBasicAuthSection : String = 'BasicAuth';
  TrivialEncryptKey : String = 'SQLDBAuth';

Function BasicAuthIniOptionsToStr(Options: TBasicAuthIniOptions): String;
Function StrToBasicAuthIniOptions(S : String) : TBasicAuthIniOptions;

implementation

uses typinfo,strutils;

Function BasicAuthIniOptionsToStr(Options: TBasicAuthIniOptions): String;

begin
  Result:=SetToString(PTypeInfo(TypeInfo(TBasicAuthIniOptions)),Integer(Options),false);
end;

Function StrToBasicAuthIniOptions(S : String) : TBasicAuthIniOptions;

var
  i : integer;
begin
  I:=StringToSet(PTypeInfo(TypeInfo(TBasicAuthIniOptions)),S);
  Result:=TBasicAuthIniOptions(I);
end;


{ TSQLDBRestBasicAuthHelper }

Const
  KeyUserID = 'UserID';
  KeyUserName = 'UserName';
  KeyPassword = 'Password';
  KeyRealm = 'Realm';
  KeySQL = 'SQL';



procedure TSQLDBRestBasicAuthHelper.ClearValues;
begin
  DefaultUserID:='';
  DefaultUserName:='';
  DefaultPassword:='';
  AuthenticateUserSQL.Clear;
  AuthenticationRealm:='';
end;

procedure TSQLDBRestBasicAuthHelper.LoadFromIni(const aIni: TCustomIniFile; ASection: String; aOptions: TBasicAuthIniOptions);

Var
  M,P : String;
begin
  With aIni do
    begin
    if (baoClearOnRead in aOptions) then
       ClearValues;
    DefaultUserName:=ReadString(ASection,KeyUserName,DefaultUserName);
    DefaultUserID:=ReadString(ASection,KeyUserID,DefaultUserID);
    AuthenticationRealm:=ReadString(ASection,KeyRealm,AuthenticationRealm);
    AuthenticateUserSQL.StrictDelimiter:=True;
    AuthenticateUserSQL.Delimiter:='&';
    AuthenticateUserSQL.DelimitedText:=ReadString(ASection,KeySQL,AuthenticateUserSQL.DelimitedText);
    // optional parts
    if not (baoSkipPassword in aOptions) then
      begin
      if baoSkipMaskPassword in aOptions then
        P:=ReadString(ASection,KeyPassword,DefaultPassword)
      else
        begin
        P:=ReadString(ASection,KeyPassword,'');
        if (P<>'') then
          begin
          if baoUserNameAsMask in aOptions then
            M:=DefaultUserName
          else
            M:=TrivialEncryptKey;
          P:=XorDecode(M,P);
          end;
        end;
      DefaultPassword:=P;
      end;
    end;
end;

procedure TSQLDBRestBasicAuthHelper.LoadFromIni(const aIni: TCustomIniFile; aOptions: TBasicAuthIniOptions);
begin
  LoadFromIni(aIni,DefaultBasicAuthSection,aOptions);
end;

procedure TSQLDBRestBasicAuthHelper.LoadFromFile(const aFileName: String; aOptions: TBasicAuthIniOptions);


begin
  Loadfromfile(aFileName,DefaultBasicAuthSection,aOptions);
end;

procedure TSQLDBRestBasicAuthHelper.LoadFromFile(const aFileName: String; const ASection: String; aOptions: TBasicAuthIniOptions);

Var
  Ini : TCustomIniFile;

begin
  Ini:=TMeminiFile.Create(aFileName);
  try
    LoadFromIni(Ini,aSection,aOptions);
  finally
    Ini.Free;
  end;
end;

procedure TSQLDBRestBasicAuthHelper.SaveToFile(const aFileName: String; aOptions: TBasicAuthIniOptions);
begin
  SaveToFile(aFileName,DefaultBasicAuthSection,aOptions);
end;

procedure TSQLDBRestBasicAuthHelper.SaveToFile(const aFileName: String; const ASection: String; aOptions: TBasicAuthIniOptions);
Var
  Ini : TCustomIniFile;

begin
  Ini:=TMeminiFile.Create(aFileName);
  try
    SaveToini(Ini,aSection,aOptions);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TSQLDBRestBasicAuthHelper.SaveToIni(const aIni: TCustomIniFile; aOptions: TBasicAuthIniOptions);
begin
  SaveToIni(aIni,DefaultBasicAuthSection,aOptions);
end;

procedure TSQLDBRestBasicAuthHelper.SaveToIni(const aIni: TCustomIniFile; ASection: String; aOptions: TBasicAuthIniOptions);

Var
  M,P : String;

begin
  With aIni do
    begin
    WriteString(ASection,KeyUserName,DefaultUserName);
    WriteString(ASection,KeyUserID,DefaultUserID);
    WriteString(ASection,KeyRealm,AuthenticationRealm);
    AuthenticateUserSQL.StrictDelimiter:=True;
    AuthenticateUserSQL.Delimiter:='&';
    WriteString(ASection,KeySQL,AuthenticateUserSQL.DelimitedText);
    if not (baoSkipPassword in aOptions) then
      begin
      P:=DefaultPassword;
      if Not (baoSkipMaskPassword in aOptions) then
        begin
        if baoUserNameAsMask in aOptions then
          M:=DefaultUserName
        else
          M:=TrivialEncryptKey;
        P:=XorEncode(M,P);
        end;
      WriteString(ASection,KeyPassword,P);
      end;
    end;
end;

end.

