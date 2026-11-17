{
    This file is part of the Free Pascal component  library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Connection Management

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Opf.Connections;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Sqldb,
{$ELSE}
  Classes, SysUtils, sqldb,
{$ENDIF}
  Opf.Types;

Type
  { TSQLDBConnectionDef }

  TSQLDBConnectionDef = Class(TCollectionItem)
  private
    FCharSet: UTF8String;
    FConnection: TSQLConnection;
    FConnectionType: String;
    FDatabaseName: UTF8String;
    FEnabled: Boolean;
    FHostName: UTF8String;
    FName: UTF8String;
    FParams: TStrings;
    FPassword: UTF8String;
    FPort: Word;
    FRole: UTF8String;
    FUserName: UTF8String;
    FNotifier : TComponent;
    function GetName: UTF8String;
    procedure SetConnection(AValue: TSQLConnection);
    procedure SetParams(AValue: TStrings);
  Protected
    Function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  Public
    // Creates a new connection definition in the given list
    constructor Create(ACollection: TCollection); override;
    // Frees the connection definition
    Destructor Destroy; override;
    // Copies the settings of another connection definition
    Procedure Assign(Source: TPersistent); override;
    // Applies these settings (host, database, user, ...) to the given connection
    Procedure ConfigConnection(aConn : TSQLConnection); virtual;
  Published
    // Always use this connection instance
    Property SingleConnection : TSQLConnection Read FConnection Write SetConnection;
    // Allow this connection to be used.
    Property Enabled : Boolean Read FEnabled Write FEnabled default true;
    // Kind of database, used as the connector type of a TSQLConnector
    property ConnectionType : String Read FConnectionType Write FConnectionType;
    // Name for this connection
    Property Name : UTF8String Read GetName Write FName;
    // Database user password
    property Password : UTF8String read FPassword write FPassword;
    // Database username
    property UserName : UTF8String read FUserName write FUserName;
    // Database character set
    property CharSet : UTF8String read FCharSet write FCharSet;
    // Database hostname
    property HostName : UTF8String Read FHostName Write FHostName;
    // Database role
    Property Role :  UTF8String read FRole write FRole;
    // Name of the database
    property DatabaseName : UTF8String Read FDatabaseName Write FDatabaseName;
    // Extra connection parameters, as name=value lines
    Property Params : TStrings Read FParams Write SetParams;
    // Port DB is listening on
    Property Port : Word Read FPort Write FPort;
  end;

  TSQLDBConnectionList = Class;
  TSQLDBConnectionListClass = Class of TSQLDBConnectionList;

  { TSQLDBConnectionList }

  TSQLDBConnectionList = Class(TCollection)
  private
    FCipher: IOPPasswordCipher;
    FDefaultCipher: TObject;
    function GetCipher: IOPPasswordCipher;
    procedure SetCipher(aValue: IOPPasswordCipher);
    function GetConn(aIndex : integer): TSQLDBConnectionDef;
    procedure SetConn(aIndex : integer; AValue: TSQLDBConnectionDef);
    procedure SaveToIniFile(const aFileName: UTF8String);
    procedure LoadFromIniFile(const aFileName: UTF8String);
    procedure EncryptPasswords;
    procedure DecryptPasswords;
    function CreateEncryptedCopy: TSQLDBConnectionList;
    procedure WriteJSON(const aStream: TStream);
  Public
    // Frees the connection list
    Destructor Destroy; override;
    // Index of connection by name (case insensitive)
    Function IndexOfConnection(const aName : UTF8string) : Integer;
    // Find connection by name (case insensitive), nil if none found
    Function FindConnection(const aName : UTF8string) :  TSQLDBConnectionDef;
    // Add new instance, setting basic properties. Return new instance
    Function AddConnection(Const AType,aHostName,aDatabaseName,aUserName,aPassword : UTF8String) : TSQLDBConnectionDef;
    // Save connection definitions to a JSON file (an INI file for extension .ini).
    // The file is replaced only when it was written completely. On Unix the
    // replacement is atomic, a symbolic link is followed (the target is
    // replaced) and the file keeps its permissions and, when allowed, its
    // owner; a new file is created with permissions 0600 (it holds passwords).
    Procedure SaveToFile(Const aFileName : UTF8String);
    // Save connection definitions  to JSON stream
    Procedure SaveToStream(Const aStream : TStream);
    // Load connection definitions from JSON file.
    Procedure LoadFromFile(Const aFileName : UTF8String);
    // Load connection definitions from JSON stream.
    Procedure LoadFromStream(Const aStream : TStream);
    // Indexed access to connection definitions
    Property Definitions [aIndex : integer] : TSQLDBConnectionDef Read GetConn Write SetConn;  default;
    // Cipher for encrypting/decrypting passwords during save/load. Default: TOPNoCipher.
    Property PasswordCipher : IOPPasswordCipher Read GetCipher Write SetCipher;
  end;

implementation

uses Opf.Json, Opf.Consts,
{$IFDEF FPC_DOTTEDUNITS}
  {$IFDEF UNIX}UnixApi.Base,{$ENDIF}
  System.IniFiles;
{$ELSE}
  {$IFDEF UNIX}baseunix,{$ENDIF}
  inifiles;
{$ENDIF}

const
  ParamsSectionSuffix = '.Params';

{ TConnectionFreeNotifier }

Type
  TConnectionFreeNotifier = Class(TComponent)
    FRef : TSQLDBConnectionDef;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

procedure TConnectionFreeNotifier.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and Assigned(FRef) and (Fref.SingleConnection=aComponent) then
    Fref.SingleConnection:=Nil;
end;

{ TSQLDBConnectionDef }

procedure TSQLDBConnectionDef.SetParams(AValue: TStrings);
begin
  if FParams=AValue then Exit;
  FParams.Assign(AValue);
end;

function TSQLDBConnectionDef.GetDisplayName: string;
begin
  Result:=Name;
end;

procedure TSQLDBConnectionDef.SetDisplayName(const Value: string);
begin
  FName:=Value;
  inherited SetDisplayName(Value);
end;

procedure TSQLDBConnectionDef.SetConnection(AValue: TSQLConnection);
begin
  if FConnection=AValue then Exit;
  if Assigned(FConnection) then
    FConnection.RemoveFreeNotification(FNotifier);
  FConnection:=AValue;
  if Assigned(FConnection) then
    FConnection.FreeNotification(FNotifier);
end;

function TSQLDBConnectionDef.GetName: UTF8String;
begin
  Result:=FName;
  if (Result='') and Assigned(SingleConnection) then
    Result:=SingleConnection.Name;
  if (Result='') then
    Result:='Connection'+IntToStr(ID);
end;

constructor TSQLDBConnectionDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParams:=TStringList.Create;
  FNotifier:=TConnectionFreeNotifier.Create(Nil);
  TConnectionFreeNotifier(FNotifier).FRef:=Self;
  FEnabled:=True;
end;

destructor TSQLDBConnectionDef.Destroy;
begin
  TConnectionFreeNotifier(FNotifier).FRef:=Nil;
  FreeAndNil(FNotifier);
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TSQLDBConnectionDef.Assign(Source: TPersistent);

Var
  C : TSQLDBConnectionDef;

begin
  if (Source is TSQLDBConnectionDef) then
    begin
    C:=Source as TSQLDBConnectionDef;
    Password:=C.Password;
    UserName:=C.UserName;
    CharSet :=C.CharSet;
    HostName:=C.HostName;
    Role:=C.Role;
    DatabaseName:=C.DatabaseName;
    ConnectionType:=C.ConnectionType;
    Port:=C.Port;
    Name:=C.Name;
    Enabled:=C.Enabled;
    Params.Assign(C.Params);
    end
  else
    inherited Assign(Source);
end;

procedure TSQLDBConnectionDef.ConfigConnection(aConn: TSQLConnection);
begin
  aConn.CharSet:=Self.CharSet;
  aConn.HostName:=Self.HostName;
  aConn.DatabaseName:=Self.DatabaseName;
  aConn.UserName:=Self.UserName;
  aConn.Password:=Self.Password;
  aConn.Params:=Self.Params;
  // After Params: sqldb keeps the port in Params (TSQLConnection.Port is not
  // public in every FPC version)
  if Self.Port<>0 then
    aConn.Params.Values['Port']:=IntToStr(Self.Port);
  if Self.Role<>'' then
    aConn.Role:=Self.Role;
  if aConn is TSQLConnector then
    TSQLConnector(aConn).ConnectorType:=Self.ConnectionType;
end;

{ TSQLDBConnectionList }

function TSQLDBConnectionList.GetConn(aIndex : integer): TSQLDBConnectionDef;
begin
  Result:=TSQLDBConnectionDef(Items[aIndex]);
end;

procedure TSQLDBConnectionList.SetConn(aIndex : integer; AValue: TSQLDBConnectionDef);
begin
  Items[aIndex]:=aValue;
end;

function TSQLDBConnectionList.IndexOfConnection(const aName: UTF8string
  ): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and not SameText(GetConn(Result).Name,aName) do
    Dec(Result);
end;

function TSQLDBConnectionList.FindConnection(const aName: UTF8string): TSQLDBConnectionDef;
Var
  Idx : Integer;

begin
  Idx:=IndexOfConnection(aName);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=GetConn(Idx);
end;

function TSQLDBConnectionList.AddConnection(const AType, aHostName, aDatabaseName, aUserName, aPassword: UTF8String): TSQLDBConnectionDef;

Var
  Idx : Integer;
  N : String;
begin
  Result:=Add as TSQLDBConnectionDef;
  IDX:=Result.ID;
  Repeat
    N:='Connection'+IntToStr(IDX);
    Inc(Idx);
  Until IndexOfConnection(N)=-1;
  Result.Name:=N;
  Result.ConnectionType:=aType;
  Result.HostName:=aHostName;
  Result.DatabaseName:=aDatabaseName;
  Result.UserName:=aUserName;
  Result.Password:=aPassword;
end;

destructor TSQLDBConnectionList.Destroy;
begin
  FreeAndNil(FDefaultCipher);
  inherited Destroy;
end;

function TSQLDBConnectionList.GetCipher: IOPPasswordCipher;
begin
  if FCipher=Nil then
    begin
    FDefaultCipher:=TOPNoCipher.Create;
    FCipher:=TOPNoCipher(FDefaultCipher);
    end;
  Result:=FCipher;
end;

procedure TSQLDBConnectionList.SetCipher(aValue: IOPPasswordCipher);
begin
  FCipher:=aValue;
  FreeAndNil(FDefaultCipher);
end;

procedure TSQLDBConnectionList.EncryptPasswords;
var
  I: Integer;
begin
  for I:=0 to Count-1 do
    GetConn(I).Password:=PasswordCipher.Encrypt(GetConn(I).Password);
end;

procedure TSQLDBConnectionList.DecryptPasswords;
var
  I: Integer;
begin
  for I:=0 to Count-1 do
    GetConn(I).Password:=PasswordCipher.Decrypt(GetConn(I).Password);
end;

{ A copy of the list with encrypted passwords: what is saved. The list
  itself is never changed, so a failure or another thread never sees
  encrypted passwords in it. Caller frees. }

function TSQLDBConnectionList.CreateEncryptedCopy: TSQLDBConnectionList;
var
  I: Integer;
begin
  // Same class and a copy through JSON: descendants keep their own
  // published properties (e.g. TOPConnection.IsDefault)
  Result:=TSQLDBConnectionListClass(ClassType).Create(ItemClass);
  try
    OPSerializer.Deserialize(OPSerializer.Serialize(Self), Result);
    for I:=0 to Result.Count-1 do
      Result.GetConn(I).Password:=PasswordCipher.Encrypt(Result.GetConn(I).Password);
  except
    Result.Free;
    raise;
  end;
end;

{ Params are kept in a section of their own, [<name>.Params], so a parameter
  can have the name of a connection setting (e.g. Password). }

procedure TSQLDBConnectionList.SaveToIniFile(const aFileName: UTF8String);
var
  Ini: TIniFile;
  I, J: Integer;
  C: TSQLDBConnectionDef;
  Sect: UTF8String;
begin
  Ini:=TIniFile.Create(aFileName);
  try
    for I:=0 to Count-1 do
    begin
      C:=GetConn(I);
      Sect:=C.Name;
      Ini.WriteString(Sect, 'Host', C.HostName);
      Ini.WriteString(Sect, 'DatabaseName', C.DatabaseName);
      Ini.WriteString(Sect, 'UserName', C.UserName);
      Ini.WriteString(Sect, 'Password', C.Password);
      Ini.WriteInteger(Sect, 'Port', C.Port);
      Ini.WriteString(Sect, 'Charset', C.CharSet);
      Ini.WriteString(Sect, 'Role', C.Role);
      Ini.WriteString(Sect, 'ConnectionType', C.ConnectionType);
      Ini.WriteBool(Sect, 'Enabled', C.Enabled);
      for J:=0 to C.Params.Count-1 do
        Ini.WriteString(Sect+ParamsSectionSuffix, C.Params.Names[J], C.Params.ValueFromIndex[J]);
    end;
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TSQLDBConnectionList.LoadFromIniFile(const aFileName: UTF8String);
var
  Ini: TIniFile;
  Sections, Values: TStringList;
  I, J: Integer;
  C: TSQLDBConnectionDef;
  Sect, ParamNames, PName: UTF8String;
  ParamList: TStringList;
begin
  Clear;
  Ini:=TIniFile.Create(aFileName);
  Sections:=TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for I:=0 to Sections.Count-1 do
    begin
      Sect:=Sections[I];
      if SameText(Copy(Sect, Length(Sect)-Length(ParamsSectionSuffix)+1, Length(ParamsSectionSuffix)), ParamsSectionSuffix) then
        Continue;
      C:=Add as TSQLDBConnectionDef;
      C.Name:=Sect;
      C.HostName:=Ini.ReadString(Sect, 'Host', '');
      C.DatabaseName:=Ini.ReadString(Sect, 'DatabaseName', '');
      C.UserName:=Ini.ReadString(Sect, 'UserName', '');
      C.Password:=Ini.ReadString(Sect, 'Password', '');
      C.Port:=Ini.ReadInteger(Sect, 'Port', 0);
      C.CharSet:=Ini.ReadString(Sect, 'Charset', '');
      C.Role:=Ini.ReadString(Sect, 'Role', '');
      C.ConnectionType:=Ini.ReadString(Sect, 'ConnectionType', '');
      C.Enabled:=Ini.ReadBool(Sect, 'Enabled', True);
      if Ini.SectionExists(Sect+ParamsSectionSuffix) then
      begin
        Values:=TStringList.Create;
        try
          Ini.ReadSectionValues(Sect+ParamsSectionSuffix, Values);
          for J:=0 to Values.Count-1 do
            C.Params.Add(Values[J]);
        finally
          Values.Free;
        end;
      end
      else
      begin
        // Older files: Params lists the names of keys in the same section
        ParamNames:=Ini.ReadString(Sect, 'Params', '');
        if ParamNames<>'' then
        begin
          ParamList:=TStringList.Create;
          try
            ParamList.Delimiter:=',';
            ParamList.StrictDelimiter:=True;
            ParamList.DelimitedText:=ParamNames;
            for J:=0 to ParamList.Count-1 do
            begin
              PName:=Trim(ParamList[J]);
              if PName<>'' then
                C.Params.Add(PName+'='+Ini.ReadString(Sect, PName, ''));
            end;
          finally
            ParamList.Free;
          end;
        end;
      end;
    end;
  finally
    Sections.Free;
    Ini.Free;
  end;
end;

{$IFDEF UNIX}
{ The file a symbolic link points to (also through several links), or
  aFileName itself }

function ResolveSymLinks(const aFileName: UTF8String): UTF8String;
var
  Info: Stat;
  Target: RawByteString;
  Depth: Integer;
begin
  Result:=aFileName;
  Depth:=0;
  while (fpLStat(Result, Info)=0) and fpS_ISLNK(Info.st_mode) and (Depth<32) do
  begin
    Target:=fpReadLink(Result);
    if Target='' then
      Break;
    if Target[1]<>'/' then
      Target:=ExtractFilePath(Result)+Target;
    Result:=Target;
    Inc(Depth);
  end;
end;
{$ENDIF}

{ Written to a temporary file that replaces aFileName when complete: a
  failure leaves the old file as it was. Sections and keys that are no
  longer in the list do not survive either. }

procedure TSQLDBConnectionList.SaveToFile(const aFileName: UTF8String);
var
  Ext, Target, TmpName: UTF8String;
  F: TFileStream;
  H: THandle;
  Copy_: TSQLDBConnectionList;
  {$IFDEF UNIX}
  Info: Stat;
  HasOld: Boolean;
  {$ENDIF}
begin
  {$IFDEF UNIX}
  // Replace the file a link points to, not the link
  Target:=ResolveSymLinks(aFileName);
  HasOld:=fpStat(Target, Info)=0;
  {$ELSE}
  Target:=aFileName;
  {$ENDIF}
  TmpName:=Target+'.tmp';
  Copy_:=CreateEncryptedCopy;
  try
    if FileExists(TmpName) then
      DeleteFile(TmpName);
    try
      // Created empty first, with the final permissions: the passwords are
      // never in a file that others can read
      H:=FileCreate(TmpName{$IFDEF UNIX}, &600{$ENDIF});
      if H=THandle(-1) then
        raise EOPF.CreateFmt(SErrCannotReplaceFile, [aFileName]);
      FileClose(H);
      {$IFDEF UNIX}
      if HasOld then
      begin
        // Owner first: changing it can clear the set-id bits of the mode
        fpChown(TmpName, Info.st_uid, Info.st_gid);
        fpChmod(TmpName, Info.st_mode and &7777);
      end
      else
        fpChmod(TmpName, &600);
      {$ENDIF}
      Ext:=LowerCase(ExtractFileExt(aFileName));
      if Ext='.ini' then
        Copy_.SaveToIniFile(TmpName)
      else
      begin
        // fmCreate keeps the permissions of the existing (empty) file
        F:=TFileStream.Create(TmpName, fmCreate);
        try
          Copy_.WriteJSON(F);
        finally
          F.Free;
        end;
      end;
    except
      DeleteFile(TmpName);
      raise;
    end;
  finally
    Copy_.Free;
  end;
  {$IFDEF UNIX}
  // rename() replaces the old file atomically: there is no moment without one
  if fpRename(TmpName, Target)<>0 then
  begin
    DeleteFile(TmpName);
    raise EOPF.CreateFmt(SErrCannotReplaceFile, [aFileName]);
  end;
  {$ELSE}
  if FileExists(Target) and not DeleteFile(Target) then
    raise EOPF.CreateFmt(SErrCannotReplaceFile, [aFileName]);
  if not RenameFile(TmpName, Target) then
    raise EOPF.CreateFmt(SErrCannotReplaceFile, [aFileName]);
  {$ENDIF}
end;

procedure TSQLDBConnectionList.WriteJSON(const aStream: TStream);
var
  S: UTF8String;
begin
  S := OPSerializer.Serialize(Self);
  if Length(S) > 0 then
    aStream.WriteBuffer(S[1], Length(S));
end;

procedure TSQLDBConnectionList.SaveToStream(const aStream: TStream);
var
  Copy_: TSQLDBConnectionList;
begin
  // Passwords encrypted, as in a file
  Copy_:=CreateEncryptedCopy;
  try
    Copy_.WriteJSON(aStream);
  finally
    Copy_.Free;
  end;
end;

procedure TSQLDBConnectionList.LoadFromFile(const aFileName: UTF8String);
var
  Ext: UTF8String;
  F: TFileStream;
begin
  Ext:=LowerCase(ExtractFileExt(aFileName));
  if Ext='.ini' then
  begin
    LoadFromIniFile(aFileName);
    DecryptPasswords;
  end
  else
  begin
    F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
    try
      // Decrypts the passwords
      LoadFromStream(F);
    finally
      F.Free;
    end;
  end;
end;

procedure TSQLDBConnectionList.LoadFromStream(const aStream: TStream);
var
  S: UTF8String;
  Remaining: Int64;
begin
  Remaining := aStream.Size - aStream.Position;
  SetLength(S, Remaining);
  if Remaining > 0 then
    aStream.ReadBuffer(S[1], Remaining);
  OPSerializer.Deserialize(S, Self);
  DecryptPasswords;
end;


end.
