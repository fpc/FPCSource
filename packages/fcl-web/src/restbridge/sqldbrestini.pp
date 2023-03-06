{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST Dispatcher .ini file load/save support.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit sqldbrestini;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpWeb.RestBridge.IO, FpWeb.RestBridge.Auth, FpWeb.RestBridge.Bridge, FpWeb.RestBridge.Schema, System.IniFiles;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, sqldbrestio, sqldbrestauth, sqldbrestbridge, sqldbrestschema, inifiles;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TConnectionIniOption = (scoClearOnRead,      // Clear values first
                          scoSkipPassword,     // Do not save/load password
                          scoSkipMaskPassword, // do not mask the password
                          scoUserNameAsMask,   // use the username as mask for password
                          scoSkipParams        // Do not read/write params.
                         );
  TConnectionIniOptions = Set of TConnectionIniOption;

  TSQLDBRestConnectionHelper = class helper for TSQLDBRestConnection
  Private
    Procedure ClearValues;
  Public
    Procedure LoadFromIni(Const aIni: TCustomIniFile; aOptions : TConnectionIniOptions = []); overload;
    Procedure LoadFromIni(Const aIni: TCustomIniFile; const ASection : String; aOptions : TConnectionIniOptions); overload;
    Procedure LoadFromIniFile(Const aFileName : String; aOptions : TConnectionIniOptions = []); overload;
    Procedure LoadFromIniFile(Const aFileName : String; Const ASection : String; aOptions : TConnectionIniOptions); overload;
    Procedure SaveToIniFile(Const aFileName : String; aOptions : TConnectionIniOptions = []);overload;
    Procedure SaveToIniFile(Const aFileName : String; Const ASection : String; aOptions : TConnectionIniOptions = []);overload;
    Procedure SaveToIni(Const aIni: TCustomIniFile; aOptions : TConnectionIniOptions = []); overload;
    Procedure SaveToIni(Const aIni: TCustomIniFile; const ASection : String; aOptions : TConnectionIniOptions); overload;
  end;
  TDispatcherIniOption = (dioSkipReadConnections,   // Do not Read connection definitions
                          dioSkipExposeConnections, // Do not Expose connections defined in .ini file
                          dioSkipReadSchemas,       // Do not Read schema definitions
                          dioDisableSchemas,        // Do not enable schemas
                          dioSkipWriteConnections,  // Do not write connection definitions
                          dioSkipWriteSchemas,      // Do not Read schema definitions
                          dioSkipBasicAuth,         // Do not read/write basic auth data.
                          dioSkipStringConfig       // Do not read strings config
                          );
  TDispatcherIniOptions = set of TDispatcherIniOption;

  { TSQLDBRestDispatcherHelper }

  TSQLDBRestDispatcherHelper = class helper for TSQLDBRestDispatcher
  private
  Public
    procedure ReadSchemas(const aIni: TCustomIniFile; const ASection: String; aOptions: TDispatcherIniOptions);
    procedure ReadConnections(const aIni: TCustomIniFile; const ASection: String);
    procedure WriteConnections(const aIni: TCustomIniFile; const ASection: String; aOptions : TConnectionIniOptions);
    procedure WriteSchemas(const aIni: TCustomIniFile; const ASection: String; const SchemaFileDir : String);
    Procedure LoadFromIni(Const aIni: TCustomIniFile; aOptions : TDispatcherIniOptions = []); overload;
    Procedure LoadFromIni(Const aIni: TCustomIniFile; const ASection : String; aOptions : TDispatcherIniOptions); overload;
    Procedure LoadFromFile(Const aFileName : String; aOptions : TDispatcherIniOptions = []); overload;
    Procedure LoadFromFile(Const aFileName : String; Const ASection : String; aOptions : TDispatcherIniOptions); overload;
    Procedure SaveToFile(Const aFileName : String; aOptions : TDispatcherIniOptions = []);overload;
    Procedure SaveToFile(Const aFileName : String; Const ASection : String; aOptions : TDispatcherIniOptions = []);overload;
    Procedure SaveToIni(Const aIni: TCustomIniFile; aOptions : TDispatcherIniOptions = []); overload;
    Procedure SaveToIni(Const aIni: TCustomIniFile; const ASection : String; aOptions : TDispatcherIniOptions); overload;
  end;

  { TRestStringsConfigHelper }

  TRestStringsConfigHelper = class helper for TRestStringsConfig
  Public
    Procedure LoadFromIni(Const aIni: TCustomIniFile); overload;
    Procedure LoadFromIni(Const aIni: TCustomIniFile; const ASection : String); overload;
    Procedure LoadFromFile(Const aFileName : String); overload;
    Procedure LoadFromFile(Const aFileName : String; Const ASection : String); overload;
    Procedure SaveToFile(Const aFileName : String);overload;
    Procedure SaveToFile(Const aFileName : String; Const ASection : String);overload;
    Procedure SaveToIni(Const aIni: TCustomIniFile); overload;
    Procedure SaveToIni(Const aIni: TCustomIniFile; const ASection : String); overload;
  end;


Function StrToOutputOptions(const S : String) : TRestOutputOptions;
Function StrToDispatcherOptions(const S : String) : TRestDispatcherOptions;
Function StrToConnectionIniOptions(const S : String) : TConnectionIniOptions;
Function OutputOptionsToStr(Options : TRestOutputOptions): String;
Function DispatcherOptionsToStr(Options: TRestDispatcherOptions) : String;
Function ConnectionIniOptionsToStr(Options: TConnectionIniOptions): String;

Var
  TrivialEncryptKey : String = 'SQLDB';
  DefaultConnectionSection : String = 'Connection';
  DefaultDispatcherSection : String = 'Dispatcher';
  DefaultStringsConfigSection : String = 'Dispatcher_strings';

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.TypInfo, System.StrUtils, FpWeb.RestBridge.Authini;
{$ELSE FPC_DOTTEDUNITS}
uses typinfo,strutils, sqldbrestauthini;
{$ENDIF FPC_DOTTEDUNITS}

Const
  KeyHost = 'Host';
  KeyDatabaseName = 'DatabaseName';
  KeyUserName = 'UserName';
  KeyPassword = 'Password';
  KeyPort = 'Port';
  keyParams = 'Params';
  KeyCharset = 'Charset';
  KeyRole = 'Role';
  KeyType = 'Type';
  KeyConnections = 'Connections';
  KeySchemas = 'Schemas';
  keyDispatcherOptions = 'DispatcherOptions';
  keyOutputOptions = 'OutputOptions';
  KeyBasePath = 'BasePath';
  KeyDefaultConnection = 'DefaultConnection';
  KeyEnforceLimit = 'EnforceLimit';
  KeyCORSAllowedOrigins = 'CORSAllowedOrigins';
  KeyLoadOptions = 'LoadOptions';
  KeyMinFieldOptions = 'MinFieldOptions';
  KeyFileName = 'File';
  KeyEnabled = 'Enabled';
  KeyBasicAuth = 'BasicAuth';

Function StrToOutputOptions(const S : String) : TRestOutputOptions;

var
  i : integer;

begin
  I:=StringToSet(PTypeInfo(TypeInfo(TRestOutputOptions)),S);
  Result:=TRestOutputOptions(I);
end;

Function StrToDispatcherOptions(Const S : String) : TRestDispatcherOptions;

var
  i : integer;

begin
  I:=StringToSet(PTypeInfo(TypeInfo(TRestDispatcherOptions)),S);
  Result:=TRestDispatcherOptions(I);
end;

Function StrToConnectionIniOptions(const S : String) : TConnectionIniOptions;

var
  i : integer;

begin
  I:=StringToSet(PTypeInfo(TypeInfo(TConnectionIniOptions)),S);
  Result:=TConnectionIniOptions(I);
end;

Function StrToRestFieldOptions(const S : String) : TRestFieldOptions;

var
  i : integer;

begin
  I:=StringToSet(PTypeInfo(TypeInfo(TRestFieldOptions)),S);
  Result:=TRestFieldOptions(I);
end;

Function OutputOptionsToStr(Options  : TRestOutputOptions): String;

begin
  Result:=SetToString(PTypeInfo(TypeInfo(TRestOutputOptions)),Integer(Options),False);
end;

Function DispatcherOptionsToStr(Options : TRestDispatcherOptions) : String;

begin
  Result:=SetToString(PTypeInfo(TypeInfo(TRestDispatcherOptions)),Integer(Options),false);
end;

Function ConnectionIniOptionsToStr(Options : TConnectionIniOptions): String;

begin
  Result:=SetToString(PTypeInfo(TypeInfo(TConnectionIniOptions)),Integer(Options),false);
end;

{ TRestStringsConfigHelper }

procedure TRestStringsConfigHelper.LoadFromIni(const aIni: TCustomIniFile);
begin
  LoadFromIni(aIni,DefaultStringsConfigSection);
end;

procedure TRestStringsConfigHelper.LoadFromIni(const aIni: TCustomIniFile; const ASection: String);

Var
  T : TRestStringProperty;
  N : String;
  S : UTF8String;

begin
  For T in TRestStringProperty do
    begin
    Str(T,N);
    Delete(N,1,2);
    S:=aIni.ReadString(aSection, N, GetRestString(T));
    SetRestString(T,S);
    end;
end;

procedure TRestStringsConfigHelper.LoadFromFile(const aFileName: String);
begin
  LoadFromFile(aFileName,DefaultStringsConfigSection);
end;

procedure TRestStringsConfigHelper.LoadFromFile(const aFileName: String; const ASection: String);
Var
  Ini : TCustomIniFile;

begin
  Ini:=TMeminiFile.Create(aFileName);
  try
    LoadFromIni(Ini,aSection);
  finally
    Ini.Free;
  end;
end;

procedure TRestStringsConfigHelper.SaveToFile(const aFileName: String);
begin
  SaveToFile(aFileName,DefaultStringsConfigSection);
end;

procedure TRestStringsConfigHelper.SaveToFile(const aFileName: String; const ASection: String);
Var
  Ini : TCustomIniFile;

begin
  Ini:=TMeminiFile.Create(aFileName);
  try
    SaveToIni(Ini,aSection);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TRestStringsConfigHelper.SaveToIni(const aIni: TCustomIniFile);
begin
  SaveToini(aIni,DefaultStringsConfigSection);
end;

procedure TRestStringsConfigHelper.SaveToIni(const aIni: TCustomIniFile; const ASection: String);
Var
  T : TRestStringProperty;
  N : String;

begin
  For T in TRestStringProperty do
    begin
    Str(T,N);
    Delete(N,1,2);
    aIni.WriteString(aSection, N, GetRestString(T));
    end;
end;



{ TSQLDBRestDispatcherHelper }

procedure TSQLDBRestDispatcherHelper.LoadFromIni(const aIni: TCustomIniFile; aOptions: TDispatcherIniOptions);
begin
  LoadFromIni(aIni,DefaultDispatcherSection,aOptions);
end;

procedure TSQLDBRestDispatcherHelper.ReadConnections(const aIni: TCustomIniFile; const ASection: String);

Var
  S,L : String;
  I : Integer;
  C : TSQLDBRestConnection;
  CIO : TConnectionIniOptions;
begin
  // Read connections
  L:=aIni.ReadString(aSection,KeyConnections,'');
  For I:=1 to WordCount(L,[',']) do
    begin
    S:=ExtractWord(I,L,[',']);
    C:=Connections.AddConnection('','','','','');
    C.Name:=S;
    CIO:=StrToConnectionIniOptions(aIni.ReadString(S,KeyLoadOptions,''));
    C.LoadFromIni(aIni,S,CIO);
    end;
end;

procedure TSQLDBRestDispatcherHelper.WriteConnections(const aIni: TCustomIniFile; const ASection: String; aOptions: TConnectionIniOptions);

Var
  S,L : String;
  I : Integer;

begin
  L:='';
  for I:=0 to Connections.Count-1 do
    begin
    if (L<>'') then
      L:=L+',';
    L:=L+Connections[i].Name;
    end;
  aIni.WriteString(aSection,KeyConnections,L);
  for I:=0 to Connections.Count-1 do
    begin
    S:=Connections[i].Name;
    L:=ConnectionIniOptionsToStr(aOptions);
    Connections[i].SaveToIni(aIni,S,aOptions);
    aIni.WriteString(S,KeyLoadOptions,L);
    end;
end;

procedure TSQLDBRestDispatcherHelper.WriteSchemas(const aIni: TCustomIniFile; Const ASection: String; Const SchemaFileDir : String);

Var
  S,L,FN : String;
  I : Integer;
  Sch : TSQLDBRestSchema;


begin
  // Read Schemas
  L:='';
  for I:=0 to Schemas.Count-1 do
    if Assigned(Schemas[i].Schema) then
      begin
      if (L<>'') then
        L:=L+',';
      L:=L+Schemas[i].Schema.Name;
      end;
  aIni.WriteString(aSection,KeySchemas,L);
  for I:=0 to Schemas.Count-1 do
    if Assigned(Schemas[i].Schema) then
      begin
      S:=Schemas[i].Schema.Name;
      Sch:=Schemas[i].Schema;
      if (SchemaFileDir<>'') then
        FN:=IncludeTrailingPathDelimiter(SchemaFileDir)+S+'.json'
      else
        FN:='';
      aIni.WriteString(S,KeyFileName,FN);
      aIni.WriteBool(S,KeyEnabled,Schemas[i].Enabled);
      if (FN<>'') then
        Sch.SaveToFile(FN);
    end;
end;

procedure TSQLDBRestDispatcherHelper.ReadSchemas(const aIni: TCustomIniFile; const ASection: String; aOptions: TDispatcherIniOptions);

Var
  S,L,FN : String;
  I : Integer;
  Sch : TSQLDBRestSchema;
  SRef : TSQLDBRestSchemaRef;


begin
  // Read Schemas
  L:=aIni.ReadString(aSection,KeySchemas,'');
  For I:=1 to WordCount(L,[',']) do
    begin
    S:=ExtractWord(I,L,[',']);
    Sch:=TSQLDBRestSchema.Create(Self);
    Sch.Name:=S;
    SRef:=Schemas.AddSchema(Sch);
    SRef.Enabled:=aIni.ReadBool(S,KeyEnabled,True);
    if (dioDisableSchemas in aOptions) then
      SRef.Enabled:=False;
    FN:=aIni.ReadString(S,KeyFileName,'');
    if (FN<>'') then
      Sch.LoadFromFile(FN);
    end;
end;

procedure TSQLDBRestDispatcherHelper.LoadFromIni(const aIni: TCustomIniFile; const ASection: String; aOptions: TDispatcherIniOptions);

Var
  I : Integer;
  FO : TRestFieldOptions;
  BAN : String;
  BA : TRestBasicAuthenticator;
  BAO : TBasicAuthIniOptions;

begin
  DispatchOptions:=StrToDispatcherOptions(aIni.ReadString(aSection,keyDispatcherOptions,''));
  OutputOptions:=StrToOutputOptions(aIni.ReadString(aSection,keyOutputOptions,''));
  BasePath:=aIni.ReadString(aSection,KeyBasePath,'');
  DefaultConnection:=aIni.ReadString(aSection,KeyDefaultConnection,'');
  EnforceLimit:=aIni.ReadInteger(aSection,KeyEnforceLimit,0);
  CORSAllowedOrigins:=aIni.ReadString(aSection,KeyCORSAllowedOrigins,'');
  if Not (dioSkipReadConnections in aOptions) then
    ReadConnections(aIni,aSection);
  if Not (dioSkipReadSchemas in aOptions) then
    ReadSchemas(aIni,aSection,aOptions);
  // Expose connections
  if not (dioSkipExposeConnections in aOptions) then
    for I:=0 to Connections.Count-1 do
      if Connections[i].Enabled then
        begin
        FO:=StrToRestFieldOptions(aIni.ReadString(Connections[i].Name,KeyMinFieldOptions,''));
        ExposeConnection(Connections[i],Nil,FO);
        end;
  if not (dioSkipBasicAuth in aOptions) then
    begin
    BAN:=aIni.ReadString(aSection,KeyBasicAuth,'');
    if BAN<>'' then
      begin
      BAO:=StrToBasicAuthIniOptions(aIni.ReadString(BAN,keyLoadOptions,''));
      BA:=TRestBasicAuthenticator.Create(Self);
      BA.Name:=BAN;
      BA.LoadFromIni(aIni,BAN,BAO);
      Self.Authenticator:=BA;
      end;
    end;
  if not (dioSkipStringConfig in aOptions) then
    Strings.LoadFromIni(aIni,aSection+'_strings');
end;

procedure TSQLDBRestDispatcherHelper.LoadFromFile(const aFileName: String; aOptions: TDispatcherIniOptions);
begin
  Loadfromfile(aFileName,DefaultDispatcherSection,aOptions);
end;

procedure TSQLDBRestDispatcherHelper.LoadFromFile(const aFileName: String; const ASection: String; aOptions: TDispatcherIniOptions);

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

procedure TSQLDBRestDispatcherHelper.SaveToFile(const aFileName: String; aOptions: TDispatcherIniOptions);
begin
  SaveTofile(aFileName,DefaultDispatcherSection,aOptions);
end;

procedure TSQLDBRestDispatcherHelper.SaveToFile(const aFileName: String; const ASection: String; aOptions: TDispatcherIniOptions);
Var
  Ini : TCustomIniFile;

begin
  Ini:=TMeminiFile.Create(aFileName);
  try
    SaveToIni(Ini,aSection,aOptions);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TSQLDBRestDispatcherHelper.SaveToIni(const aIni: TCustomIniFile; aOptions: TDispatcherIniOptions);
begin
  SaveToIni(aIni,DefaultDispatcherSection,aOptions);
end;

procedure TSQLDBRestDispatcherHelper.SaveToIni(const aIni: TCustomIniFile; const ASection: String; aOptions: TDispatcherIniOptions);

Var
  BAN : String;

begin
  aIni.WriteString(aSection,keyDispatcherOptions,DispatcherOptionsToStr(DispatchOptions));
  aIni.WriteString(aSection,keyOutputOptions,OutputOptionsToStr(OutputOptions));
  aIni.WriteString(aSection,KeyBasePath,BasePath);
  aIni.WriteString(aSection,KeyDefaultConnection,DefaultConnection);
  aIni.WriteInteger(aSection,KeyEnforceLimit,EnforceLimit);
  aIni.WriteString(aSection,KeyCORSAllowedOrigins,CORSAllowedOrigins);
  if Not (dioSkipWriteConnections in aOptions) then
    WriteConnections(aIni,aSection,[]);
  if Not (dioSkipWriteSchemas in aOptions) then
    WriteSchemas(aIni,aSection,ExtractFilePath(ExpandFileName(aIni.FileName)));
  if not (dioSkipBasicAuth in aOptions) then
    if Assigned(Authenticator) and (Authenticator is TRestBasicAuthenticator) then
      begin
      BAN:=Authenticator.Name;
      if BAN='' then
        BAN:=Self.Name+'_basicauth';
      TRestBasicAuthenticator(Authenticator).SaveToIni(aIni,BAN,[]);
      aIni.WriteString(aSection,KeyBasicAuth,BAN);
      end;
  if not (dioSkipStringConfig in aOptions) then
    Strings.SaveToIni(aIni,aSection+'_strings');
end;

{ TSQLDBRestConnectionHelper }

procedure TSQLDBRestConnectionHelper.ClearValues;
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
  ForbiddenParamKeys : Array[1..8] of unicodestring
                     = (keyHost,KeyDatabaseName,KeyUserName,KeyPassword,KeyPort,keyParams,keyCharSet,keyRole);
  ParamSeps = [',',';',' '];

procedure TSQLDBRestConnectionHelper.LoadFromIni(const aIni: TCustomIniFile; const ASection: String; aOptions: TConnectionIniOptions);

Var
  M,N,P : String;
  I : integer;

begin
  With aIni do
    begin
    if (scoClearOnRead in aOptions) then
       ClearValues;
    ConnectionType:=ReadString(ASection,KeyType,ConnectionType);
    HostName:=ReadString(ASection,KeyHost,HostName);
    DatabaseName:=ReadString(ASection,KeyDatabaseName,DatabaseName);
    UserName:=ReadString(ASection,KeyUserName,UserName);
    CharSet:=ReadString(ASection,KeyCharSet,CharSet);
    Role:=ReadString(ASection,KeyRole,Role);
    Port:=ReadInteger(ASection,KeyPort,Port);
    Enabled:=ReadBool(ASection,KeyEnabled,True);
    // optional parts
    if not (scoSkipPassword in aOptions) then
      begin
      if scoSkipMaskPassword in aOptions then
        P:=ReadString(ASection,KeyPassword,Password)
      else
        begin
        P:=ReadString(ASection,KeyPassword,'');
        if (P<>'') then
          begin
          if scoUserNameAsMask in aOptions then
            M:=UserName
          else
            M:=TrivialEncryptKey;
          P:=XorDecode(M,P);
          end;
        end;
      Password:=P;
      end;
    if not (scoSkipParams in aOptions) then
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

procedure TSQLDBRestConnectionHelper.LoadFromIni(const aIni: TCustomIniFile; aOptions: TConnectionIniOptions);
begin
  LoadFromIni(aIni,DefaultConnectionSection,aOptions);
end;

procedure TSQLDBRestConnectionHelper.LoadFromIniFile(const aFileName: String; aOptions: TConnectionIniOptions);


begin
  LoadfromInifile(aFileName,DefaultConnectionSection,aOptions);
end;

procedure TSQLDBRestConnectionHelper.LoadFromIniFile(const aFileName: String; const ASection: String; aOptions: TConnectionIniOptions);

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

procedure TSQLDBRestConnectionHelper.SaveToIniFile(const aFileName: String; aOptions: TConnectionIniOptions);
begin
  SaveToIniFile(aFileName,DefaultConnectionSection,aOptions);
end;

procedure TSQLDBRestConnectionHelper.SaveToIniFile(const aFileName: String; const ASection: String; aOptions: TConnectionIniOptions);
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

procedure TSQLDBRestConnectionHelper.SaveToIni(const aIni: TCustomIniFile; aOptions: TConnectionIniOptions);
begin
  SaveToIni(aIni,DefaultConnectionSection,aOptions);
end;

procedure TSQLDBRestConnectionHelper.SaveToIni(const aIni: TCustomIniFile; const ASection: String; aOptions: TConnectionIniOptions);
Var
  M,N,P : String;
  I : integer;

begin
  With aIni do
    begin
    WriteString(ASection,KeyHost,HostName);
    WriteString(ASection,KeyDatabaseName,DatabaseName);
    WriteString(ASection,KeyUserName,UserName);
    WriteString(ASection,KeyCharSet,CharSet);
    WriteString(ASection,KeyType,ConnectionType);
    WriteString(ASection,KeyRole,Role);
    WriteInteger(ASection,KeyPort,Port);
    WriteBool(ASection,KeyEnabled,Enabled);
    if not (scoSkipPassword in aOptions) then
      begin
      P:=Password;
      if Not (scoSkipMaskPassword in aOptions) then
        begin
        if scoUserNameAsMask in aOptions then
          M:=UserName
        else
          M:=TrivialEncryptKey;
        P:=XorEncode(M,P);
        end;
      WriteString(ASection,KeyPassword,P);
      end;
    if not (scoSkipParams in aOptions) then
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
