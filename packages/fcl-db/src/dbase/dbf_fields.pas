unit dbf_fields;

interface

{$I dbf_common.inc}

uses
  Classes,
  SysUtils,
  db,
  dbf_common,
  dbf_str;

type
  PDbfFieldDef = ^TDbfFieldDef;

  TDbfFieldDef = class(TCollectionItem)
  private
    FFieldName: string;
    FFieldType: TFieldType;
    FNativeFieldType: TDbfFieldType;
    FDefaultBuf: PChar;
    FMinBuf: PChar;
    FMaxBuf: PChar;
    FSize: Integer;
    FPrecision: Integer;
    FHasDefault: Boolean;
    FHasMin: Boolean;
    FHasMax: Boolean;
    FAllocSize: Integer;
    FCopyFrom: Integer;
    FOffset: Integer;
    FAutoInc: Cardinal;
    FRequired: Boolean;
    FIsLockField: Boolean;
    FNullPosition: integer;

    function  GetDbfVersion: TXBaseVersion;
    procedure SetNativeFieldType(lFieldType: TDbfFieldType);
    procedure SetFieldType(lFieldType: TFieldType);
    procedure SetSize(lSize: Integer);
    procedure SetPrecision(lPrecision: Integer);
    // Converts VCL/LCL field types to dbf native field type markers ('C' etc)
    procedure VCLToNative;
    // Converts dbf native field type markers ('C' etc) to VCL/LCL field types
    procedure NativeToVCL;
    procedure FreeBuffers;
  protected
    function  GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;

    property DbfVersion: TXBaseVersion read GetDbfVersion;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignDb(DbSource: TFieldDef);

    // Checks and adjusts field size & precision
    procedure CheckSizePrecision;
    procedure SetDefaultSize;
    procedure AllocBuffers;
    function  IsBlob: Boolean;

    property DefaultBuf: PChar read FDefaultBuf;
    property MinBuf: PChar read FMinBuf;
    property MaxBuf: PChar read FMaxBuf;
    property HasDefault: Boolean read FHasDefault write FHasDefault;
    property HasMin: Boolean read FHasMin write FHasMin;
    property HasMax: Boolean read FHasMax write FHasMax;
    property Offset: Integer read FOffset write FOffset;
    property AutoInc: Cardinal read FAutoInc write FAutoInc;
    property IsLockField: Boolean read FIsLockField write FIsLockField;
    property CopyFrom: Integer read FCopyFrom write FCopyFrom;
  published
    property FieldName: string     read FFieldName write FFieldName;
    // VCL/LCL field type mapped to this field
    property FieldType: TFieldType read FFieldType write SetFieldType;
    // Native dbf field type
    property NativeFieldType: TDbfFieldType read FNativeFieldType write SetNativeFieldType;
    // Size in physical dbase file.
    // Note: this often differs from the VCL field sizes
    property Size: Integer         read FSize write SetSize;
    property NullPosition: integer read FNullPosition write FNullPosition;
    property Precision: Integer    read FPrecision write SetPrecision;
    property Required: Boolean     read FRequired  write FRequired;
  end;

  TDbfFieldDefs = class(TCollection)
  private
    FOwner: TPersistent;
    FDbfVersion: TXBaseVersion;
    function GetItem(Idx: Integer): TDbfFieldDef;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);

{$ifdef SUPPORT_DEFAULT_PARAMS}
    procedure Add(const Name: string; DataType: TFieldType; Size: Integer = 0; Required: Boolean = False);
{$else}
    procedure Add(const Name: string; DataType: TFieldType; Size: Integer; Required: Boolean);
{$endif}
    function AddFieldDef: TDbfFieldDef;

    property Items[Idx: Integer]: TDbfFieldDef read GetItem;
    property DbfVersion: TXBaseVersion read FDbfVersion write FDbfVersion;
  end;

implementation

uses
  dbf_dbffile;      // for dbf header structures

{$I dbf_struct.inc}

const
(*
The theory for Delphi/FPC is:
    ftSmallint  16 bits = -32768 to 32767
                          123456 = 6 digit max theorically
                          DIGITS_SMALLINT = 6;
    ftInteger  32 bits = -2147483648 to 2147483647
                         12345678901 = 11 digits max
                         DIGITS_INTEGER = 11;
    ftLargeInt 64 bits = -9223372036854775808 to 9223372036854775807
                         12345678901234567890 = 20 digits max
                         DIGITS_LARGEINT = 20;

But in fact if I accept 6 digits into a ftSmallInt then tDbf will not
be able to handles fields with 999999 (6 digits).

So I oversize the field type in order to accept anything coming from the
database.
    ftSmallint  16 bits = -32768 to 32767
    ... dbf supports:       -999 to  9999
                           4 digits max in practice
        therefore         DIGITS_SMALLINT = 4;
    ftInteger  32 bits = -2147483648 to 2147483647
    ... dbf supports:      -99999999 to  999999999                                        12345678901 = 11 digits max
        therefore        DIGITS_INTEGER = 9;
    ftLargeInt 64 bits = -9223372036854775808 to 9223372036854775807
    ... dbf supports:      -99999999999999999 to  999999999999999999
        therefore        DIGITS_LARGEINT = 18;
 *)
  DIGITS_SMALLINT = 4;
  DIGITS_INTEGER = 9;
  DIGITS_LARGEINT = 18;

//====================================================================
// DbfFieldDefs
//====================================================================
function TDbfFieldDefs.GetItem(Idx: Integer): TDbfFieldDef;
begin
  Result := TDbfFieldDef(inherited GetItem(Idx));
end;

constructor TDbfFieldDefs.Create(Owner: TPersistent);
begin
  inherited Create(TDbfFieldDef);
  FOwner := Owner;
end;

function TDbfFieldDefs.AddFieldDef: TDbfFieldDef;
begin
  Result := TDbfFieldDef(inherited Add);
end;

function TDbfFieldDefs.GetOwner: TPersistent; {override;}
begin
  Result := FOwner;
end;

procedure TDbfFieldDefs.Add(const Name: string; DataType: TFieldType; Size: Integer; Required: Boolean);
var
  FieldDef: TDbfFieldDef;
begin
  FieldDef := AddFieldDef;
  FieldDef.FieldName := Name;
  FieldDef.FieldType := DataType;
  if Size <> 0 then
    FieldDef.Size := Size;
  FieldDef.Required := Required;
end;

//====================================================================
// DbfFieldDef
//====================================================================
constructor TDbfFieldDef.Create(ACollection: TCollection); {virtual}
begin
  inherited;

  FDefaultBuf := nil;
  FMinBuf := nil;
  FMaxBuf := nil;
  FAllocSize := 0;
  FCopyFrom := -1;
  FPrecision := 0;
  FHasDefault := false;
  FHasMin := false;
  FHasMax := false;
  FNullPosition := -1;
end;

destructor TDbfFieldDef.Destroy; {override}
begin
  FreeBuffers;
  inherited;
end;

procedure TDbfFieldDef.Assign(Source: TPersistent);
var
  DbfSource: TDbfFieldDef;
begin
  if Source is TDbfFieldDef then
  begin
    // copy from another TDbfFieldDef
    DbfSource := TDbfFieldDef(Source);
    FFieldName := DbfSource.FieldName;
    FFieldType := DbfSource.FieldType;
    FNativeFieldType := DbfSource.NativeFieldType;
    FSize := DbfSource.Size;
    FPrecision := DbfSource.Precision;
    FRequired := DbfSource.Required;
    FCopyFrom := DbfSource.Index;
    FIsLockField := DbfSource.IsLockField;
    FNullPosition := DbfSource.NullPosition;
    // copy default,min,max
    AllocBuffers;
    if DbfSource.DefaultBuf <> nil then
      Move(DbfSource.DefaultBuf^, FDefaultBuf^, FAllocSize*3);
    FHasDefault := DbfSource.HasDefault;
    FHasMin := DbfSource.HasMin;
    FHasMax := DbfSource.HasMax;
    // do we need offsets?
    FOffset := DbfSource.Offset;
    FAutoInc := DbfSource.AutoInc;
{$ifdef SUPPORT_FIELDDEF_TPERSISTENT}
  end else if Source is TFieldDef then begin
    AssignDb(TFieldDef(Source));
{$endif}
  end else
    inherited Assign(Source);
end;

procedure TDbfFieldDef.AssignDb(DbSource: TFieldDef);
begin
  // copy from Db.TFieldDef
  FFieldName := DbSource.Name;
  FFieldType := DbSource.DataType;
  // We do NOT copy over size if TFieldDef size is different from our native size
  if not(DBSource.DataType in [ftBCD,ftCurrency]) then
    FSize := DbSource.Size;
  FPrecision := DbSource.Precision;
  FRequired := DbSource.Required;
{$ifdef SUPPORT_FIELDDEF_INDEX}
  FCopyFrom := DbSource.Index;
{$endif}
  FIsLockField := false;
  // convert VCL fieldtypes to native DBF fieldtypes
  VCLToNative;
  // for integer / float fields try to fill in Size/precision
  if FSize = 0 then
    SetDefaultSize
  else
    CheckSizePrecision;
  // VCL does not have default value support
  AllocBuffers;
  FHasDefault := false;
  FHasMin := false;
  FHasMax := false;
  FOffset := 0;
  FAutoInc := 0;
end;

procedure TDbfFieldDef.AssignTo(Dest: TPersistent);
var
  DbDest: TFieldDef;
begin
{$ifdef SUPPORT_FIELDDEF_TPERSISTENT}
  // copy to VCL fielddef?
  if Dest is TFieldDef then
  begin
    DbDest := TFieldDef(Dest);
    // VCL TFieldDef does not know how to handle TDbfFieldDef!
    // what a shame :-)
{$ifdef SUPPORT_FIELDDEF_ATTRIBUTES}
    DbDest.Attributes := [];
    DbDest.ChildDefs.Clear;
    DbDest.DataType := FFieldType;
    DbDest.Required := FRequired;
    DbDest.Size := FSize;
    DbDest.Name := FFieldName;
{$endif}
  end else
{$endif}
    inherited AssignTo(Dest);
end;

function TDbfFieldDef.GetDbfVersion: TXBaseVersion;
begin
  Result := TDbfFieldDefs(Collection).DbfVersion;
end;

procedure TDbfFieldDef.SetFieldType(lFieldType: tFieldType);
begin
  FFieldType := lFieldType;
  VCLToNative;
  SetDefaultSize;
end;

procedure TDbfFieldDef.SetNativeFieldType(lFieldType: tDbfFieldType);
begin
  // convert lowercase to uppercase
  if (lFieldType >= 'a') and (lFieldType <= 'z') then
    lFieldType := Chr(Ord(lFieldType)-32);
  FNativeFieldType := lFieldType;
  NativeToVCL;
  CheckSizePrecision;
end;

procedure TDbfFieldDef.SetSize(lSize: Integer);
begin
  FSize := lSize;
  CheckSizePrecision;
end;

procedure TDbfFieldDef.SetPrecision(lPrecision: Integer);
begin
  FPrecision := lPrecision;
  CheckSizePrecision;
end;

procedure TDbfFieldDef.NativeToVCL;
begin
  case FNativeFieldType of
    '+' :
      if DbfVersion = xBaseVII then
        FFieldType := ftAutoInc;
    'I' : FFieldType := ftInteger;
    'O' : FFieldType := ftFloat;
    '@', 'T':
          FFieldType := ftDateTime;
    'C',
    #$91  {Russian 'C'}
        : FFieldType := ftString;
    'L' : FFieldType := ftBoolean;
    'F', 'N':
      begin
        if (FPrecision = 0) then
        begin
          if FSize <= DIGITS_SMALLINT then
            FFieldType := ftSmallInt
          else
          if FSize <= DIGITS_INTEGER then
            FFieldType := ftInteger
          else
{$ifdef SUPPORT_INT64}
            FFieldType := ftLargeInt;
{$else}
            FFieldType := ftFloat;
{$endif}
        end else begin
          FFieldType := ftFloat;
        end;
      end;
    'D' : FFieldType := ftDate;
    'M' : FFieldType := ftMemo;
    'B' : 
      if (DbfVersion = xFoxPro) or (DbfVersion=xVisualFoxPro) then
        FFieldType := ftFloat
      else
        FFieldType := ftBlob;
    'G' : FFieldType := ftDBaseOle;
    'Y' :
      if DbfGlobals.CurrencyAsBCD then
        FFieldType := ftBCD
      else
        FFieldType := ftCurrency;
    '0' : FFieldType := ftBytes; { Visual FoxPro ``_NullFlags'' }
    {
    To do: add support for Visual Foxpro types
    http://msdn.microsoft.com/en-US/library/ww305zh2%28v=vs.80%29.aspx
    P Picture (perhaps also in FoxPro)
    V Varchar/varchar binary (perhaps also in FoxPro) 1 byte up to 255 bytes (or perhaps 254)
    W Blob (perhaps also in FoxPro), 4 bytes in a table; stored in .fpt
    Q Varbinary (perhaps also in Foxpro)
    }
  else
    FNativeFieldType := #0;
    FFieldType := ftUnknown;
  end; //case
end;

procedure TDbfFieldDef.VCLToNative;
begin
  FNativeFieldType := #0;
  case FFieldType of
    ftAutoInc  : FNativeFieldType  := '+';
    ftDateTime :
      if DbfVersion = xBaseVII then
        FNativeFieldType := '@'
      else
      if (DbfVersion = xFoxPro) or (DbfVersion = xVisualFoxPro) then
        FNativeFieldType := 'T'
      else
        FNativeFieldType := 'D';
{$ifdef SUPPORT_FIELDTYPES_V4}
    ftFixedChar,
    ftWideString,
{$endif}
    ftString   : FNativeFieldType  := 'C';
    ftBoolean  : FNativeFieldType  := 'L';
    ftFloat, ftSmallInt, ftWord
{$ifdef SUPPORT_INT64}
      , ftLargeInt
{$endif}
               : FNativeFieldType := 'N';
    ftDate     : FNativeFieldType := 'D';
    ftMemo     : FNativeFieldType := 'M';
    ftBlob     : FNativeFieldType := 'B';
    ftDBaseOle : FNativeFieldType := 'G';
    ftInteger  :
      if DbfVersion = xBaseVII then
        FNativeFieldType := 'I'
      else
        FNativeFieldType := 'N';
    ftBCD, ftCurrency: 
      if (DbfVersion = xFoxPro) or (DBFVersion = xVisualFoxPro) then
        FNativeFieldType := 'Y';
  end;
  if FNativeFieldType = #0 then
    raise EDbfError.CreateFmt(STRING_INVALID_VCL_FIELD_TYPE, [GetDisplayName, Ord(FFieldType)]);
end;

procedure TDbfFieldDef.SetDefaultSize;
begin
  // choose default values for variable Size fields
  case FFieldType of
    ftFloat:
      begin
        FSize := 18;
        FPrecision := 8;
      end;
    ftCurrency, ftBCD:
      begin
        FSize := 8; // Stored in dbase as 8 bytes; up to 18 (or 20) characters including .-
        // FPC ftBCD/ftCurrency TFieldDef.Size has max 4 which is 4 bytes after decimal
        FPrecision := 4; //Total number of digits
      end;
    ftSmallInt, ftWord:
      begin
        FSize := DIGITS_SMALLINT;
        FPrecision := 0;
      end;
    ftInteger, ftAutoInc:
      begin
        if DbfVersion = xBaseVII then
          FSize := 4
        else
          FSize := DIGITS_INTEGER;
        FPrecision := 0;
      end;
{$ifdef SUPPORT_INT64}
    ftLargeInt:
      begin
        FSize := DIGITS_LARGEINT;
        FPrecision := 0;
      end;
{$endif}
    ftString {$ifdef SUPPORT_FIELDTYPES_V4}, ftFixedChar, ftWideString{$endif}:
      begin
        FSize := 30;
        FPrecision := 0;
      end;
  end; // case fieldtype

  // set sizes for fields that are restricted to single Size/precision
  CheckSizePrecision;
end;

procedure TDbfFieldDef.CheckSizePrecision;
begin
  case FNativeFieldType of
    'C': // Character
      begin
        if FSize < 0 then
          FSize := 0;
        if (DbfVersion = xFoxPro) or (DbfVersion = xVisualFoxPro) then
        begin
          if FSize >= $FFFF then
            FSize := $FFFF;
        end else begin
          if FSize >= $FF then
            FSize := $FF;
        end;
        FPrecision := 0;
      end;
    'L': // Logical/boolean
      begin
        FSize := 1;
        FPrecision := 0;
      end;
    'N','F': // Binary code decimal numeric, floating point binary numeric
      begin
        // ftBCD: precision=total number of digits; Delphi supports max 32
        // Note: this field can be stored as BCD or integer, depending on FPrecision;
        // that's why we allow 0 precision
        if FSize < 1   then FSize := 1;
        if FSize >= 20 then FSize := 20;
        if FPrecision > FSize-2 then FPrecision := FSize-2; //Leave space for . and -
        if FPrecision < 0       then FPrecision := 0;
      end;
    'D': // Date
      begin
        FSize := 8;
        FPrecision := 0;
      end;
    'B': // (Visual)Foxpro double, DBase binary
      begin
        if not(DbfVersion in [xFoxPro,xVisualFoxPro]) then
        begin
          FSize := 10;
          FPrecision := 0;
        end
        else
        begin
          FSize := 8; //Foxpro double
          FPrecision := 0;
        end;
      end;
    'M','G': // Memo, general
      begin
        if (DbfVersion = xFoxPro) or (DbfVersion = xVisualFoxPro) then
        begin
          if (FSize <> 4) and (FSize <> 10) then
            FSize := 4;
        end else
          FSize := 10;
        FPrecision := 0;
      end;
    '+','I': // Autoincrement, integer
      begin
        FSize := 4;
        FPrecision := 0;
      end;
    '@', 'O': //Timestamp, double (both DBase 7)
      begin
        FSize := 8;
        FPrecision := 0;
      end;
    'T': // DateTime
      begin
        if (DbfVersion = xFoxPro) or (DbfVersion = xVisualFoxPro) then
          FSize := 8
        else
          FSize := 14;
        FPrecision := 0;
      end;
    'Y': // Currency
      begin
        FSize := 8;
        FPrecision := 4;
      end;
  else
    {
    No check, includes:
    http://msdn.microsoft.com/en-US/library/ww305zh2%28v=vs.80%29.aspx
    P Picture (in at least Visual FoxPro)
    V Varchar/varchar binary (in Visual FoxPro 9) 1 byte up to 254 bytes.
      Same storage as char (padded spaces) but padding is removed on display
      http://foxcentral.net/microsoft/WhatsNewInVFP9_Chapter09.htm
    W Blob (Visual FoxPro 9), 4 bytes in a table; stored in .fpt
      http://foxcentral.net/microsoft/WhatsNewInVFP9_Chapter09.htm
    Q Varchar (binary) (in Visual Foxpro 9):
      accepts null, up to 254 characters (stored as padded with spaces), no code page translations
      note varchar (binary)<>varbinary
      http://foxcentral.net/microsoft/WhatsNewInVFP9_Chapter09.htm
    Varchar/varbinary storage:
      Uses _NullFlags:
      bit n=1: nullable field number n is null (as in previous versions)
      bit n=0: varchar/varbinary field is full/fills space
      bit n=1: varchar/varbinary is not full; last byte of field data contains size
      If varchar/varbinary field AND nullable field, 2 bits are used:
      - lower bit=full status
      - higher bit=null status


    }
  end; // case
end;

function TDbfFieldDef.GetDisplayName: string; {override;}
begin
  Result := FieldName;
end;

function TDbfFieldDef.IsBlob: Boolean; {override;}
begin
  // 'B' is float in (V)FP
  if (DbfVersion in [xFoxPro,xVisualFoxPro]) then
    Result := FNativeFieldType in ['M','G']
  else
    Result := FNativeFieldType in ['M','G','B'];
end;

procedure TDbfFieldDef.FreeBuffers;
begin
  if FDefaultBuf <> nil then
  begin
    // one buffer for all
    FreeMemAndNil(Pointer(FDefaultBuf));
    FMinBuf := nil;
    FMaxBuf := nil;
  end;
  FAllocSize := 0;
end;

procedure TDbfFieldDef.AllocBuffers;
begin
  // Size changed?
  if FAllocSize <> FSize then
  begin
    // free old buffers
    FreeBuffers;
    // alloc new
    GetMem(FDefaultBuf, FSize*3);
    FMinBuf := FDefaultBuf + FSize;
    FMaxBuf := FMinBuf + FSize;
    // store allocated Size
    FAllocSize := FSize;
  end;
end;

end.

