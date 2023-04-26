unit sqldbini;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canney and other members of the
    Free Pascal development team

    SQLDB ini file support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, sqldb, inifiles, strutils;

Type
  TSQLDBIniOption = (sioClearOnRead,      // Clear values first
                     sioSkipPassword,     // Do not save/load password
                     sioSkipMaskPassword, // do not mask the password
                     sioUserNameAsMask,   // use the username as mask for password
                     sioSkipParams        // Do not read/write params.
                     );
  TSQLDBIniOptions = set of TSQLDBIniOption;

  { TSQLDBIniHelper }

  TSQLDBIniHelper = class helper for TSQLConnection
  Private
    Procedure ClearValues;
  Public
    Procedure LoadFromIni(Const aIni: TCustomIniFile; aOptions : TSQLDBIniOptions = []); overload;
    Procedure LoadFromIni(Const aIni: TCustomIniFile; const ASection : String; aOptions : TSQLDBIniOptions); overload;
    Procedure LoadFromFile(Const aFileName : String; aOptions : TSQLDBIniOptions = []); overload;
    Procedure LoadFromFile(Const aFileName : String; Const ASection : String; aOptions : TSQLDBIniOptions); overload;
    Procedure SaveToFile(Const aFileName : String; aOptions : TSQLDBIniOptions = []);overload;
    Procedure SaveToFile(Const aFileName : String; Const ASection : String; aOptions : TSQLDBIniOptions = []);overload;
    Procedure SaveToIni(Const aIni: TCustomIniFile; aOptions : TSQLDBIniOptions = []); overload;
    Procedure SaveToIni(Const aIni: TCustomIniFile; const ASection : String; aOptions : TSQLDBIniOptions); overload;
  end;

Var
  TrivialEncryptKey : String = 'SQLDB';
  DefaultSection : String = 'Connection';

implementation

{ TSQLDBIniHelper }

procedure TSQLDBIniHelper.ClearValues;
begin
  HostName:='';
  DatabaseName:='';
  UserName:='';
  Password:='';
  CharSet:='';
  Params.Clear;
  Port:=0;
end;


Const
  KeyHost = 'Host';
  KeyDatabaseName = 'DatabaseName';
  KeyUserName = 'UserName';
  KeyPassword = 'Password';
  KeyPort = 'Port';
  keyParams = 'Params';
  KeyCharset = 'Charset';
  KeyRole = 'Role';

Const
  ForbiddenParamKeys : Array[1..8] of unicodestring
                     = (keyHost,KeyDatabaseName,KeyUserName,KeyPassword,KeyPort,keyParams,keyCharSet,keyRole);
  ParamSeps = [',',';',' '];

procedure TSQLDBIniHelper.LoadFromIni(const aIni: TCustomIniFile; const ASection: String; aOptions: TSQLDBIniOptions);

Var
  M,N,P : String;
  I : integer;

begin
  With aIni do
    begin
    if (sioClearOnRead in aOptions) then
       ClearValues;
    HostName:=ReadString(ASection,KeyHost,HostName);
    DatabaseName:=ReadString(ASection,KeyDatabaseName,DatabaseName);
    UserName:=ReadString(ASection,KeyUserName,UserName);
    CharSet:=ReadString(ASection,KeyCharset,CharSet);
    Role:=ReadString(ASection,KeyRole,Role);
    Port:=ReadInteger(ASection,KeyPort,Port);
    // optional parts
    if not (sioSkipPassword in aOptions) then
      begin
      if sioSkipMaskPassword in aOptions then
        P:=ReadString(ASection,KeyPassword,Password)
      else
        begin
        P:=ReadString(ASection,KeyPassword,'');
        if (P<>'') then
          begin
          if sioUserNameAsMask in aOptions then
            M:=UserName
          else
            M:=TrivialEncryptKey;
          P:=XorDecode(M,P);
          end;
        end;
      Password:=P;
      end;
    if not (sioSkipParams in aOptions) then
      begin
      M:=ReadString(ASection,keyParams,'');
      For I:=1 to WordCount(M,ParamSeps) do
        begin
        N:=ExtractWord(I,M,ParamSeps);
        if IndexStr(Utf8Decode(N),ForbiddenParamKeys)=-1 then
          begin
          P:=ReadString(ASection,N,'');
          Params.Values[N]:=P;
          end;
        end;
      end;
    end;
end;

procedure TSQLDBIniHelper.LoadFromIni(const aIni: TCustomIniFile; aOptions: TSQLDBIniOptions);
begin
  LoadFromIni(aIni,Defaultsection,aOptions);
end;

procedure TSQLDBIniHelper.LoadFromFile(const aFileName: String; aOptions: TSQLDBIniOptions);


begin
  Loadfromfile(aFileName,DefaultSection,aOptions);
end;

procedure TSQLDBIniHelper.LoadFromFile(const aFileName: String; const ASection: String; aOptions: TSQLDBIniOptions);

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

procedure TSQLDBIniHelper.SaveToFile(const aFileName: String; aOptions: TSQLDBIniOptions);
begin
  SaveToFile(aFileName,DefaultSection,aOptions);
end;

procedure TSQLDBIniHelper.SaveToFile(const aFileName: String; const ASection: String; aOptions: TSQLDBIniOptions);
Var
  Ini : TCustomIniFile;

begin
  Ini:=TMeminiFile.Create(aFileName);
  try
    SaveToini(Ini,aSection,aOptions);
  finally
    Ini.Free;
  end;
end;

procedure TSQLDBIniHelper.SaveToIni(const aIni: TCustomIniFile; aOptions: TSQLDBIniOptions);
begin
  SaveToIni(aIni,DefaultSection,aOptions);
end;

procedure TSQLDBIniHelper.SaveToIni(const aIni: TCustomIniFile; const ASection: String; aOptions: TSQLDBIniOptions);
Var
  M,N,P : String;
  I : integer;

begin
  With aIni do
    begin
    WriteString(ASection,KeyHost,HostName);
    WriteString(ASection,KeyDatabaseName,DatabaseName);
    WriteString(ASection,KeyUserName,UserName);
    WriteString(ASection,KeyCharset,CharSet);
    WriteString(ASection,KeyRole,Role);
    WriteInteger(ASection,KeyPort,Port);
    if not (sioSkipPassword in aOptions) then
      begin
      P:=Password;
      if Not (sioSkipMaskPassword in aOptions) then
        begin
        if sioUserNameAsMask in aOptions then
          M:=UserName
        else
          M:=TrivialEncryptKey;
        P:=XorEncode(M,P);
        end;
      WriteString(ASection,KeyPassword,P);
      end;
    if not (sioSkipParams in aOptions) then
      begin
      M:='';
      for I:=0 to Params.Count-1 do
        begin
        Params.GetNameValue(I,N,P);
        if (N<>'') and (IndexStr(Utf8Decode(N),ForbiddenParamKeys)=-1) then
          begin
          WriteString(ASection,N,P);
          if (M<>'') then
            M:=M+',';
          M:=M+N;
          end;
        end;
      WriteString(ASection,KeyParams,M);
      end;
    end;
end;

end.

