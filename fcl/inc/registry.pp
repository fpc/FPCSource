unit Registry;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

type
  HKEY = Integer;
  PHKEY = ^HKEY;

  ERegistryException = class(Exception);

  TRegKeyInfo = record
    NumSubKeys: Integer;
    MaxSubKeyLen: Integer;
    NumValues: Integer;
    MaxValueLen: Integer;
    MaxDataLen: Integer;
    //FileTime: TFileTime;
  end;

  TRegDataType = (rdUnknown, rdString, rdExpandString, rdInteger, rdBinary);

  TRegDataInfo = record
    RegData: TRegDataType;
    DataSize: Integer;
  end;

  { TRegistry }
  {
    @abstract(Class to provide access to a registry.)
    Introduced by Curtis White
    Currently maintained by Curtis White
  }
  TRegistry = class(TObject)
  private
    fCurrentKey: HKEY;
    fRootKey: HKEY;
    fLazyWrite: Boolean;
    fCurrentPath: string;
    //fCloseRootKey: Boolean;

    procedure SetRootKey(Value: HKEY);
  protected
    function GetBaseKey(Relative: Boolean): HKey;
    function GetData(const Name: string; Buffer: Pointer;
                  BufSize: Integer; var RegData: TRegDataType): Integer;
    function GetKey(const Key: string): HKEY;

    procedure ChangeKey(Value: HKey; const Path: string);
    procedure PutData(const Name: string; Buffer: Pointer;
                  BufSize: Integer; RegData: TRegDataType);
    procedure SetCurrentKey(Value: HKEY);
  public
    constructor Create;
    destructor Destroy; override;

    function CreateKey(const Key: string): Boolean;
    function DeleteKey(const Key: string): Boolean;
    function DeleteValue(const Name: string): Boolean;
    function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
    function GetDataSize(const ValueName: string): Integer;
    function GetDataType(const ValueName: string): TRegDataType;
    function GetKeyInfo(var Value: TRegKeyInfo): Boolean;
    function HasSubKeys: Boolean;
    function KeyExists(const Key: string): Boolean;
    function LoadKey(const Key, FileName: string): Boolean;
    function OpenKey(const Key: string; CanCreate: Boolean): Boolean;
    //function ReadCurrency(const Name: string): Currency;
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;
    function ReadBool(const Name: string): Boolean;
    function ReadDate(const Name: string): TDateTime;
    function ReadDateTime(const Name: string): TDateTime;
    function ReadFloat(const Name: string): Double;
    function ReadInteger(const Name: string): Integer;
    function ReadString(const Name: string): string;
    function ReadTime(const Name: string): TDateTime;
    function RegistryConnect(const UNCName: string): Boolean;
    function ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
    function RestoreKey(const Key, FileName: string): Boolean;
    function SaveKey(const Key, FileName: string): Boolean;
    function UnLoadKey(const Key: string): Boolean;
    function ValueExists(const Name: string): Boolean;

    procedure CloseKey;
    procedure GetKeyNames(Strings: TStrings);
    procedure GetValueNames(Strings: TStrings);
    procedure MoveKey(const OldName, NewName: string; Delete: Boolean);
    procedure RenameValue(const OldName, NewName: string);
    //procedure WriteCurrency(const Name: string; Value: Currency);
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
    procedure WriteBool(const Name: string; Value: Boolean);
    procedure WriteDate(const Name: string; Value: TDateTime);
    procedure WriteDateTime(const Name: string; Value: TDateTime);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteInteger(const Name: string; Value: Integer);
    procedure WriteString(const Name, Value: string);
    procedure WriteExpandString(const Name, Value: string);
    procedure WriteTime(const Name: string; Value: TDateTime);

    property CurrentKey: HKEY read fCurrentKey;
    property CurrentPath: string read fCurrentPath;
    property LazyWrite: Boolean read fLazyWrite write fLazyWrite;
    property RootKey: HKEY read fRootKey write SetRootKey;
  end;

  { TRegIniFile }
  {
    @abstract(Class to provide access to a registry in an Ini file manner.)
    Introduced by Curtis White
    Currently maintained by Curtis White
  }
  TRegIniFile = class(TRegistry)
  private
    fFileName: String;
  public
    constructor Create(const FN: string);

    function ReadString(const Section, Ident, Default: string): string;
    function ReadInteger(const Section, Ident: string;
                Default: Longint): Longint;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;

    procedure WriteString(const Section, Ident, Value: String);
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSectionValues(const Section: string; Strings: TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);

    property FileName: String read fFileName;
  end;


implementation

{******************************************************************************
                                  TRegistry
 ******************************************************************************}
{------------------------------------------------------------------------------
  Method: TRegistry.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TRegistry.Create;
begin
  inherited Create;
end;

{------------------------------------------------------------------------------
  Method: TRegistry.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TRegistry.Destroy;
begin
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.CreateKey
  Params:   Key: String key to create
  Returns:  Boolean containing result of the create. True if it succeeded.

  Create a registry key.
 ------------------------------------------------------------------------------}
function TRegistry.CreateKey(const Key: String): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.DeleteKey
  Params:   Key: String key to create
  Returns:  Boolean containing result of the delete. True if it succeeded.

  Delete a registry key.
 ------------------------------------------------------------------------------}
function TRegistry.DeleteKey(const Key: String): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.DeleteValue
  Params:   Name: Name of key of which to delete its value
  Returns:  Boolean containing result of the function. True if it succeeded.

  Delete the value for a registry key.
 ------------------------------------------------------------------------------}
function TRegistry.DeleteValue(const Name: String): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.GetBaseKey
  Params:   Relative: Is the key relative or absolute. True if relative.
  Returns:  HKey containing the base key.

  Gets the base key.
 ------------------------------------------------------------------------------}
function TRegistry.GetBaseKey(Relative: Boolean): HKey;
begin
  Result := CurrentKey;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.GetData
  Params:   Name: name of the key
            Buffer:
            BufSize:
            RegData:
  Returns:  Integer containing output from the function.

  Gets data from the registry.
 ------------------------------------------------------------------------------}
function TRegistry.GetData(const Name: String; Buffer: Pointer;
          BufSize: Integer; var RegData: TRegDataType): Integer;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.GetDataInfo
  Params:   ValueName: name of the value to get info on
            Value:
  Returns:  Boolean containing result of the function. True if it succeeded.

  Get info on the data value.
 ------------------------------------------------------------------------------}
function TRegistry.GetDataInfo(const ValueName: String; var Value: TRegDataInfo): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.GetDataSize
  Params:   ValueName: name of the value to get info on
  Returns:  Integer containing the size of the value.

  Get the size of the data value.
 ------------------------------------------------------------------------------}
function TRegistry.GetDataSize(const ValueName: String): Integer;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.GetDataType
  Params:   ValueName: name of the value to get info on
  Returns:  TRegDataType containing type of the value.

  Get the type of the data value.
 ------------------------------------------------------------------------------}
function TRegistry.GetDataType(const ValueName: string): TRegDataType;
begin
  Result := rdUnknown;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.GetKey
  Params:   Key: key to get
  Returns:  HKey containing the key.

  Get a key from the registry.
 ------------------------------------------------------------------------------}
function TRegistry.GetKey(const Key: String): HKEY;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.GetKeyInfo
  Params:   Value: value of info to get key info on
  Returns:  Boolean containing result of the function. True if it succeeded.

  Get the info of a key.
 ------------------------------------------------------------------------------}
function TRegistry.GetKeyInfo(var Value: TRegKeyInfo): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.HasSubKeys
  Params:   None
  Returns:  Boolean containing result of the function. True if there are sub
            keys.

  See if the key has sub keys.
 ------------------------------------------------------------------------------}
function TRegistry.HasSubKeys: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.KeyExists
  Params:   Key: the name of the key
  Returns:  Boolean containing result of the function. True if the key exists.

  Check to see if a key exists.
 ------------------------------------------------------------------------------}
function TRegistry.KeyExists(const Key: string): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.LoadKey
  Params:   Key: the name of the key
            FileName: file containing the key to load
  Returns:  Boolean containing result of the function. True if it succeeded.

  Load a key from a file.
 ------------------------------------------------------------------------------}
function TRegistry.LoadKey(const Key, FileName: string): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.OpenKey
  Params:   Key: the name of the key
            CanCreate: create the key if it does not exist. True to create
  Returns:  Boolean containing result of the function. True if it succeeded.

  Open a key and optionally create it if is does not exist.
 ------------------------------------------------------------------------------}
function TRegistry.OpenKey(const Key: string; CanCreate: Boolean): Boolean;
begin
  Result := True;
end;






{------------------------------------------------------------------------------
  Function: TRegistry.ReadBinaryData
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.ReadBool
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.ReadBool(const Name: string): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.ReadCurrency
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
{function TRegistry.ReadCurrency(const Name: string): Currency;
begin
  Result := 0.0;
end;}

{------------------------------------------------------------------------------
  Function: TRegistry.ReadDate
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.ReadDate(const Name: string): TDateTime;
begin
  Result := now;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.ReadDateTime
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.ReadDateTime(const Name: string): TDateTime;
begin
  Result := now;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.ReadFloat
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.ReadFloat(const Name: string): Double;
begin
  Result := 0.0;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.ReadInteger
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.ReadInteger(const Name: string): Integer;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.ReadString
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.ReadString(const Name: string): string;
begin
  Result := '';
end;

{------------------------------------------------------------------------------
  Function: TRegistry.ReadTime
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.ReadTime(const Name: string): TDateTime;
begin
  Result := now;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.RegistryConnect
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.RegistryConnect(const UNCName: string): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.ReplaceKey
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.RestoreKey
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.RestoreKey(const Key, FileName: string): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.SaveKey
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.SaveKey(const Key, FileName: string): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.UnLoadKey
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.UnLoadKey(const Key: string): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TRegistry.ValueExists
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegistry.ValueExists(const Name: string): Boolean;
begin
  Result := True;
end;







{------------------------------------------------------------------------------
  Method:  TRegistry.CloseKey
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.CloseKey;
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.ChangeKey
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.ChangeKey(Value: HKey; const Path: String);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.GetKeyName
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.GetKeyNames(Strings: TStrings);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.GetValueNames
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.GetValueNames(Strings: TStrings);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.MoveKey
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.MoveKey(const OldName, NewName: string; Delete: Boolean);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.PutData
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.PutData(const Name: string; Buffer: Pointer;
  BufSize: Integer; RegData: TRegDataType);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.RenameValue
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.RenameValue(const OldName, NewName: string);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.SetCurrentKey
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.SetCurrentKey(Value: HKEY);
begin
  fCurrentKey := Value;
end;

{------------------------------------------------------------------------------
  Method:  TRegistry.SetRootKey
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.SetRootKey(Value: HKEY);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.WriteBinaryData
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.WriteBool
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.WriteBool(const Name: string; Value: Boolean);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.WriteCurrency
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
{procedure TRegistry.WriteCurrency(const Name: string; Value: Currency);
begin

end;}

{------------------------------------------------------------------------------
  Method:  TRegistry.WriteDate
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.WriteDate(const Name: string; Value: TDateTime);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.WriteDateTime
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.WriteDateTime(const Name: string; Value: TDateTime);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.WriteExpandString
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.WriteExpandString(const Name, Value: string);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.WriteFloat
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.WriteFloat(const Name: string; Value: Double);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.WriteInteger
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.WriteInteger(const Name: string; Value: Integer);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.WriteString
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.WriteString(const Name, Value: string);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegistry.WriteTime
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegistry.WriteTime(const Name: string; Value: TDateTime);
begin

end;


{******************************************************************************
                                TRegIniFile
 ******************************************************************************}
{------------------------------------------------------------------------------
  Method: TRegIniFile.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TRegIniFile.Create(const FN: String);
begin
  inherited Create;
  fFileName := FN;
end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.DeleteKey(const Section, Ident: String);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.EraseSection(const Section: string);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.ReadSection(const Section: string; Strings: TStrings);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.ReadSections(Strings: TStrings);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.WriteInteger(const Section, Ident: string; Value: LongInt);
begin

end;

{------------------------------------------------------------------------------
  Method:  TRegIniFile.MyMethod
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TRegIniFile.WriteString(const Section, Ident, Value: String);
begin

end;



{------------------------------------------------------------------------------
  Function: TRegIniFile.MyFunction
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
  Result := Default;
end;

{------------------------------------------------------------------------------
  Function: TRegIniFile.MyFunction
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegIniFile.ReadInteger(const Section, Ident: string; Default: LongInt): LongInt;
begin
  Result := Default;
end;

{------------------------------------------------------------------------------
  Function: TRegIniFile.MyFunction
  Params:   AOwner: the owner of the class
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TRegIniFile.ReadString(const Section, Ident, Default: String): String;
begin
  Result := Default;
end;


end.

{
  $Log$
  Revision 1.3  2002-09-07 15:15:25  peter
    * old logs removed and tabs fixed

}
