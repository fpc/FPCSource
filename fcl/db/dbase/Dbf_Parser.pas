unit Dbf_Parser;

interface

{$I Dbf_Common.inc}

uses
  SysUtils,
  Classes,
{$ifdef KYLIX}
  Libc,
{$endif}
{$ifndef WIN32}
  Dbf_Wtil,
{$endif}
  Db,
  Dbf_PrsCore,
  Dbf_Common,
  Dbf_Fields,
  Dbf_PrsDef,
  Dbf_PrsSupp;

type

  TDbfParser = class(TCustomExpressionParser)
  private
    FDbfFile: Pointer;
    FFieldVarList: TStringList;
    FResultLen: Integer;
    FIsExpression: Boolean;       // expression or simple field?
    FFieldType: TExpressionType;
    FCaseInsensitive: Boolean;
    FRawStringFields: Boolean;

  protected
    FCurrentExpression: string;

    procedure FillExpressList; override;
    procedure HandleUnknownVariable(VarName: string); override;
    function  GetVariableInfo(VarName: string): TDbfFieldDef;
    function  CurrentExpression: string; override;
    function  GetResultType: TExpressionType; override;

    procedure SetCaseInsensitive(NewInsensitive: Boolean);
    procedure SetRawStringFields(NewRawFields: Boolean);
  public
    constructor Create(ADbfFile: Pointer);
    destructor Destroy; override;

    procedure ClearExpressions; override;

    procedure ParseExpression(Expression: string); virtual;
    function ExtractFromBuffer(Buffer: PChar): PChar; virtual;

    property DbfFile: Pointer read FDbfFile write FDbfFile;
    property Expression: string read FCurrentExpression;
    property ResultLen: Integer read FResultLen;

    property CaseInsensitive: Boolean read FCaseInsensitive write SetCaseInsensitive;
    property RawStringFields: Boolean read FRawStringFields write SetRawStringFields;
  end;

//--Expression functions-----------------------------------------------------

procedure FuncFloatToStr(Param: PExpressionRec);
procedure FuncIntToStr_Gen(Param: PExpressionRec; Val: Integer);
procedure FuncIntToStr(Param: PExpressionRec);
procedure FuncDateToStr(Param: PExpressionRec);
procedure FuncSubString(Param: PExpressionRec);
procedure FuncUppercase(Param: PExpressionRec);
procedure FuncLowercase(Param: PExpressionRec);
procedure FuncAdd_F_FF(Param: PExpressionRec);
procedure FuncAdd_F_FI(Param: PExpressionRec);
procedure FuncAdd_F_II(Param: PExpressionRec);
procedure FuncAdd_F_IF(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure FuncAdd_F_FL(Param: PExpressionRec);
procedure FuncAdd_F_IL(Param: PExpressionRec);
procedure FuncAdd_F_LL(Param: PExpressionRec);
procedure FuncAdd_F_LF(Param: PExpressionRec);
procedure FuncAdd_F_LI(Param: PExpressionRec);
{$endif}
procedure FuncStrI_EQ(Param: PExpressionRec);
procedure FuncStrI_NEQ(Param: PExpressionRec);
procedure FuncStrI_LT(Param: PExpressionRec);
procedure FuncStrI_GT(Param: PExpressionRec);
procedure FuncStrI_LTE(Param: PExpressionRec);
procedure FuncStrI_GTE(Param: PExpressionRec);
procedure FuncStr_EQ(Param: PExpressionRec);
procedure FuncStr_NEQ(Param: PExpressionRec);
procedure FuncStr_LT(Param: PExpressionRec);
procedure FuncStr_GT(Param: PExpressionRec);
procedure FuncStr_LTE(Param: PExpressionRec);
procedure FuncStr_GTE(Param: PExpressionRec);
procedure Func_FF_EQ(Param: PExpressionRec);
procedure Func_FF_NEQ(Param: PExpressionRec);
procedure Func_FF_LT(Param: PExpressionRec);
procedure Func_FF_GT(Param: PExpressionRec);
procedure Func_FF_LTE(Param: PExpressionRec);
procedure Func_FF_GTE(Param: PExpressionRec);
procedure Func_FI_EQ(Param: PExpressionRec);
procedure Func_FI_NEQ(Param: PExpressionRec);
procedure Func_FI_LT(Param: PExpressionRec);
procedure Func_FI_GT(Param: PExpressionRec);
procedure Func_FI_LTE(Param: PExpressionRec);
procedure Func_FI_GTE(Param: PExpressionRec);
procedure Func_II_EQ(Param: PExpressionRec);
procedure Func_II_NEQ(Param: PExpressionRec);
procedure Func_II_LT(Param: PExpressionRec);
procedure Func_II_GT(Param: PExpressionRec);
procedure Func_II_LTE(Param: PExpressionRec);
procedure Func_II_GTE(Param: PExpressionRec);
procedure Func_IF_EQ(Param: PExpressionRec);
procedure Func_IF_NEQ(Param: PExpressionRec);
procedure Func_IF_LT(Param: PExpressionRec);
procedure Func_IF_GT(Param: PExpressionRec);
procedure Func_IF_LTE(Param: PExpressionRec);
procedure Func_IF_GTE(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure Func_LL_EQ(Param: PExpressionRec);
procedure Func_LL_NEQ(Param: PExpressionRec);
procedure Func_LL_LT(Param: PExpressionRec);
procedure Func_LL_GT(Param: PExpressionRec);
procedure Func_LL_LTE(Param: PExpressionRec);
procedure Func_LL_GTE(Param: PExpressionRec);
procedure Func_LF_EQ(Param: PExpressionRec);
procedure Func_LF_NEQ(Param: PExpressionRec);
procedure Func_LF_LT(Param: PExpressionRec);
procedure Func_LF_GT(Param: PExpressionRec);
procedure Func_LF_LTE(Param: PExpressionRec);
procedure Func_LF_GTE(Param: PExpressionRec);
procedure Func_FL_EQ(Param: PExpressionRec);
procedure Func_FL_NEQ(Param: PExpressionRec);
procedure Func_FL_LT(Param: PExpressionRec);
procedure Func_FL_GT(Param: PExpressionRec);
procedure Func_FL_LTE(Param: PExpressionRec);
procedure Func_FL_GTE(Param: PExpressionRec);
procedure Func_LI_EQ(Param: PExpressionRec);
procedure Func_LI_NEQ(Param: PExpressionRec);
procedure Func_LI_LT(Param: PExpressionRec);
procedure Func_LI_GT(Param: PExpressionRec);
procedure Func_LI_LTE(Param: PExpressionRec);
procedure Func_LI_GTE(Param: PExpressionRec);
procedure Func_IL_EQ(Param: PExpressionRec);
procedure Func_IL_NEQ(Param: PExpressionRec);
procedure Func_IL_LT(Param: PExpressionRec);
procedure Func_IL_GT(Param: PExpressionRec);
procedure Func_IL_LTE(Param: PExpressionRec);
procedure Func_IL_GTE(Param: PExpressionRec);
{$endif}
procedure Func_AND(Param: PExpressionRec);
procedure Func_OR(Param: PExpressionRec);
procedure Func_NOT(Param: PExpressionRec);

implementation

uses
  Dbf,
  Dbf_DbfFile,
  Dbf_Str
{$ifdef WIN32}
  ,Windows
{$endif}
  ;

type
// TFieldVar aids in retrieving field values from records
// in their proper type

  TFieldVar = class(TObject)
  private
    FFieldDef: TDbfFieldDef;
    FDbfFile: TDbfFile;
    FFieldName: string;
  protected
    function GetFieldVal: Pointer; virtual; abstract;
    function GetFieldType: TExpressionType; virtual; abstract;
  public
    constructor Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);

    procedure Refresh(Buffer: PChar); virtual; abstract;

    property FieldVal: Pointer read GetFieldVal;
    property FieldDef: TDbfFieldDef read FFieldDef;
    property FieldType: TExpressionType read GetFieldType;
    property DbfFile: TDbfFile read FDbfFile;
    property FieldName: string read FFieldName;
  end;

  TStringFieldVar = class(TFieldVar)
  protected
    FFieldVal: PChar;

    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  end;

  TRawStringFieldVar = class(TStringFieldVar)
  public
    constructor Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);

    procedure Refresh(Buffer: PChar); override;
  end;

  TAnsiStringFieldVar = class(TStringFieldVar)
  public
    constructor Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
    destructor Destroy; override;

    procedure Refresh(Buffer: PChar); override;
  end;

  TFloatFieldVar = class(TFieldVar)
  private
    FFieldVal: Double;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    constructor Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);

    procedure Refresh(Buffer: PChar); override;
  end;

  TIntegerFieldVar = class(TFieldVar)
  private
    FFieldVal: Integer;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    constructor Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);

    procedure Refresh(Buffer: PChar); override;
  end;

{$ifdef SUPPORT_INT64}
  TLargeIntFieldVar = class(TFieldVar)
  private
    FFieldVal: Int64;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    constructor Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);

    procedure Refresh(Buffer: PChar); override;
  end;
{$endif}

  TDateTimeFieldVar = class(TFieldVar)
  private
    FFieldVal: TDateTimeRec;
    function GetFieldType: TExpressionType; override;
  protected
    function GetFieldVal: Pointer; override;
  public
    constructor Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);

    procedure Refresh(Buffer: PChar); override;
  end;

//--TFieldVar----------------------------------------------------------------
constructor TFieldVar.Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
begin
  inherited Create;

  // store field
  FFieldDef := UseFieldDef;
  FDbfFile := ADbfFile;
  FFieldName := UseFieldDef.FieldName;
end;

//--TStringFieldVar-------------------------------------------------------------
function TStringFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TStringFieldVar.GetFieldType: TExpressionType;
begin
  Result := etString;
end;

//--TRawStringFieldVar----------------------------------------------------------
constructor TRawStringFieldVar.Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
begin
  inherited;
end;

procedure TRawStringFieldVar.Refresh(Buffer: PChar);
begin
  FFieldVal := Buffer + FieldDef.Offset;
end;

//--TAnsiStringFieldVar---------------------------------------------------------
constructor TAnsiStringFieldVar.Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
begin
  inherited;

  GetMem(FFieldVal, UseFieldDef.Size+1);
end;

destructor TAnsiStringFieldVar.Destroy;
begin
  FreeMem(FFieldVal);

  inherited;
end;

procedure TAnsiStringFieldVar.Refresh(Buffer: PChar);
var
  Len: Integer;
begin
  // copy field data
  Len := FieldDef.Size;
  Move(Buffer[FieldDef.Offset], FFieldVal[0], Len);
  // trim right side spaces by null-termination
  while (Len >= 1) and (FFieldVal[Len-1] = ' ') do Dec(Len);
  FFieldVal[Len] := #0;
  // translate to ANSI
  TranslateString(DbfFile.UseCodePage, GetACP, FFieldVal, FFieldVal, Len);
end;

//--TFloatFieldVar-----------------------------------------------------------
constructor TFloatFieldVar.Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
begin
  inherited;
end;

function TFloatFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TFloatFieldVar.GetFieldType: TExpressionType;
begin
  Result := etFloat;
end;

procedure TFloatFieldVar.Refresh(Buffer: PChar);
begin
  // database width is default 64-bit double
  if not FDbfFile.GetFieldDataFromDef(FieldDef, FieldDef.FieldType, Buffer, @FFieldVal) then
    FFieldVal := 0.0;
end;

//--TIntegerFieldVar----------------------------------------------------------
constructor TIntegerFieldVar.Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
begin
  inherited;
end;

function TIntegerFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TIntegerFieldVar.GetFieldType: TExpressionType;
begin
  Result := etInteger;
end;

procedure TIntegerFieldVar.Refresh(Buffer: PChar);
begin
  FFieldVal := 0;
  FDbfFile.GetFieldDataFromDef(FieldDef, FieldDef.FieldType, Buffer, @FFieldVal);
end;

{$ifdef SUPPORT_INT64}

//--TLargeIntFieldVar----------------------------------------------------------
constructor TLargeIntFieldVar.Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
begin
  inherited;
end;

function TLargeIntFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TLargeIntFieldVar.GetFieldType: TExpressionType;
begin
  Result := etLargeInt;
end;

procedure TLargeIntFieldVar.Refresh(Buffer: PChar);
begin
  if not FDbfFile.GetFieldDataFromDef(FieldDef, FieldDef.FieldType, Buffer, @FFieldVal) then
    FFieldVal := 0;
end;

{$endif}

//--TDateTimeFieldVar---------------------------------------------------------
constructor TDateTimeFieldVar.Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
begin
  inherited;
end;

function TDateTimeFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TDateTimeFieldVar.GetFieldType: TExpressionType;
begin
  Result := etDateTime;
end;

procedure TDateTimeFieldVar.Refresh(Buffer: PChar);
begin
  if FDbfFile.GetFieldDataFromDef(FieldDef, ftDateTime, Buffer, @FFieldVal) then
  begin
{$ifndef SUPPORT_NEW_FIELDDATA}
    // convert BDE timestamp to normal datetime
    FFieldVal.DateTime := BDETimeStampToDateTime(FFieldVal.DateTime);
{$endif}
  end else begin
    FFieldVal.DateTime := 0.0;
  end;
end;

//--Expression functions-----------------------------------------------------

procedure FuncFloatToStr(Param: PExpressionRec);
var
  width, numDigits, resWidth: Integer;
  extVal: Extended;
begin
  with Param^ do
  begin
    // get params;
    numDigits := 0;
    if Args[1] <> nil then
      width := PInteger(Args[1])^
    else
      width := 18;
    if Args[2] <> nil then
      numDigits := PInteger(Args[2])^;
    // convert to string
    Res.AssureSpace(width);
    extVal := PDouble(Args[0])^;
    resWidth := FloatToText(Res.MemoryPos^, extVal, {$ifndef FPC_VERSION}fvExtended,{$endif} ffFixed, 18, numDigits);
    // always use dot as decimal separator
    if numDigits > 0 then
      Res.MemoryPos^[resWidth-numDigits-1] := '.';
    // result width smaller than requested width? -> add space to compensate
    if (Args[1] <> nil) and (resWidth < width) then
    begin
      // move string so that it's right-aligned
      Move(Res.MemoryPos^^, (Res.MemoryPos^)[width-resWidth], resWidth);
      // fill gap with spaces
      FillChar(Res.MemoryPos^^, width-resWidth, ' ');
      // resWidth has been padded, update
      resWidth := width;
    end else if resWidth > width then begin
      // result width more than requested width, cut
      resWidth := width;
    end;
    // advance pointer
    Inc(Res.MemoryPos^, resWidth);
    // null-terminate
    Res.MemoryPos^^ := #0;
  end;
end;

procedure FuncIntToStr_Gen(Param: PExpressionRec; Val: Integer);
var
  width: Integer;
begin
  with Param^ do
  begin
    // width specified?
    if Args[1] <> nil then
    begin
      // convert to string
      width := PInteger(Args[1])^;
      GetStrFromInt_Width(Val, width, Res.MemoryPos^, #32);
      // advance pointer
      Inc(Res.MemoryPos^, width);
      // need to add decimal?
      if Args[2] <> nil then
      begin
        // get number of digits
        width := PInteger(Args[2])^;
        // add decimal dot
        Res.MemoryPos^^ := '.';
        Inc(Res.MemoryPos^);
        // add zeroes
        FillChar(Res.MemoryPos^^, width, '0');
        // go to end
        Inc(Res.MemoryPos^, width);
      end;
    end else begin
      // convert to string
      width := GetStrFromInt(Val, Res.MemoryPos^);
      // advance pointer
      Inc(Param.Res.MemoryPos^, width);
    end;
    // null-terminate
    Res.MemoryPos^^ := #0;
  end;
end;

procedure FuncIntToStr(Param: PExpressionRec);
begin
  FuncIntToStr_Gen(Param, PInteger(Param.Args[0])^);
end;

procedure FuncDateToStr(Param: PExpressionRec);
var
  TempStr: string;
begin
  with Param^ do
  begin
    // create in temporary string
    DateTimeToString(TempStr, 'yyyymmdd', PDateTimeRec(Args[0]).DateTime);
    // copy to buffer
    Res.Append(PChar(TempStr), Length(TempStr));
  end;
end;

procedure FuncSubString(Param: PExpressionRec);
var
  srcLen, index, count: Integer;
begin
  with Param^ do
  begin
    srcLen := StrLen(Args[0]);
    index := PInteger(Args[1])^ - 1;
    count := PInteger(Args[2])^;
    if index + count <= srcLen then
      Res.Append(Args[0]+index, count)
    else
      Res.MemoryPos^^ := #0;
  end;
end;

procedure FuncUppercase(Param: PExpressionRec);
var
  dest: PChar;
begin
  with Param^ do
  begin
    // first copy
    dest := (Res.MemoryPos)^;
    Res.Append(Args[0], StrLen(Args[0]));
    // make uppercase
    StrUpper(dest);
  end;
end;

procedure FuncLowercase(Param: PExpressionRec);
var
  dest: PChar;
begin
  with Param^ do
  begin
    // first copy
    dest := (Res.MemoryPos)^;
    Res.Append(Args[0], StrLen(Args[0]));
    // make lowercase
    StrLower(dest);
  end;
end;

procedure FuncAdd_F_FF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ + PDouble(Args[1])^;
end;

procedure FuncAdd_F_FI(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ + PInteger(Args[1])^;
end;

procedure FuncAdd_F_II(Param: PExpressionRec);
begin
  with Param^ do
    PInteger(Res.MemoryPos^)^ := PInteger(Args[0])^ + PInteger(Args[1])^;
end;

procedure FuncAdd_F_IF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PInteger(Args[0])^ + PDouble(Args[1])^;
end;

{$ifdef SUPPORT_INT64}

procedure FuncAdd_F_FL(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ + PInt64(Args[1])^;
end;

procedure FuncAdd_F_IL(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInteger(Args[0])^ + PInt64(Args[1])^;
end;

procedure FuncAdd_F_LL(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInt64(Args[0])^ + PInt64(Args[1])^;
end;

procedure FuncAdd_F_LF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PInt64(Args[0])^ + PDouble(Args[1])^;
end;

procedure FuncAdd_F_LI(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInt64(Args[0])^ + PInteger(Args[1])^;
end;

{$endif}

procedure FuncStrI_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrIComp(Args[0], Args[1]) = 0);
end;

procedure FuncStrI_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrIComp(Args[0], Args[1]) <> 0);
end;

procedure FuncStrI_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrIComp(Args[0], Args[1]) < 0);
end;

procedure FuncStrI_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrIComp(Args[0], Args[1]) > 0);
end;

procedure FuncStrI_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrIComp(Args[0], Args[1]) <= 0);
end;

procedure FuncStrI_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrIComp(Args[0], Args[1]) >= 0);
end;

procedure FuncStr_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrComp(Args[0], Args[1]) = 0);
end;

procedure FuncStr_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrComp(Args[0], Args[1]) <> 0);
end;

procedure FuncStr_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrComp(Args[0], Args[1]) < 0);
end;

procedure FuncStr_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrComp(Args[0], Args[1]) > 0);
end;

procedure FuncStr_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrComp(Args[0], Args[1]) <= 0);
end;

procedure FuncStr_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrComp(Args[0], Args[1]) >= 0);
end;

procedure Func_FF_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   =  PDouble(Args[1])^);
end;

procedure Func_FF_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   <> PDouble(Args[1])^);
end;

procedure Func_FF_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   <  PDouble(Args[1])^);
end;

procedure Func_FF_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   >  PDouble(Args[1])^);
end;

procedure Func_FF_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   <= PDouble(Args[1])^);
end;

procedure Func_FF_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   >= PDouble(Args[1])^);
end;

procedure Func_FI_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   =  PInteger(Args[1])^);
end;

procedure Func_FI_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   <> PInteger(Args[1])^);
end;

procedure Func_FI_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   <  PInteger(Args[1])^);
end;

procedure Func_FI_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   >  PInteger(Args[1])^);
end;

procedure Func_FI_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   <= PInteger(Args[1])^);
end;

procedure Func_FI_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   >= PInteger(Args[1])^);
end;

procedure Func_II_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  =  PInteger(Args[1])^);
end;

procedure Func_II_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  <> PInteger(Args[1])^);
end;

procedure Func_II_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  <  PInteger(Args[1])^);
end;

procedure Func_II_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  >  PInteger(Args[1])^);
end;

procedure Func_II_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  <= PInteger(Args[1])^);
end;

procedure Func_II_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  >= PInteger(Args[1])^);
end;

procedure Func_IF_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  =  PDouble(Args[1])^);
end;

procedure Func_IF_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  <> PDouble(Args[1])^);
end;

procedure Func_IF_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  <  PDouble(Args[1])^);
end;

procedure Func_IF_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  >  PDouble(Args[1])^);
end;

procedure Func_IF_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  <= PDouble(Args[1])^);
end;

procedure Func_IF_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  >= PDouble(Args[1])^);
end;

{$ifdef SUPPORT_INT64}

procedure Func_LL_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    =  PInt64(Args[1])^);
end;

procedure Func_LL_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    <> PInt64(Args[1])^);
end;

procedure Func_LL_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    <  PInt64(Args[1])^);
end;

procedure Func_LL_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    >  PInt64(Args[1])^);
end;

procedure Func_LL_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    <= PInt64(Args[1])^);
end;

procedure Func_LL_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    >= PInt64(Args[1])^);
end;

procedure Func_LF_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    =  PDouble(Args[1])^);
end;

procedure Func_LF_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    <> PDouble(Args[1])^);
end;

procedure Func_LF_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    <  PDouble(Args[1])^);
end;

procedure Func_LF_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    >  PDouble(Args[1])^);
end;

procedure Func_LF_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    <= PDouble(Args[1])^);
end;

procedure Func_LF_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    >= PDouble(Args[1])^);
end;

procedure Func_FL_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   =  PInt64(Args[1])^);
end;

procedure Func_FL_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   <> PInt64(Args[1])^);
end;

procedure Func_FL_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   <  PInt64(Args[1])^);
end;

procedure Func_FL_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   >  PInt64(Args[1])^);
end;

procedure Func_FL_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   <= PInt64(Args[1])^);
end;

procedure Func_FL_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PDouble(Args[0])^   >= PInt64(Args[1])^);
end;

procedure Func_LI_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    =  PInteger(Args[1])^);
end;

procedure Func_LI_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    <> PInteger(Args[1])^);
end;

procedure Func_LI_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    <  PInteger(Args[1])^);
end;

procedure Func_LI_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    >  PInteger(Args[1])^);
end;

procedure Func_LI_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    <= PInteger(Args[1])^);
end;

procedure Func_LI_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInt64(Args[0])^    >= PInteger(Args[1])^);
end;

procedure Func_IL_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  =  PInt64(Args[1])^);
end;

procedure Func_IL_NEQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  <> PInt64(Args[1])^);
end;

procedure Func_IL_LT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  <  PInt64(Args[1])^);
end;

procedure Func_IL_GT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  >  PInt64(Args[1])^);
end;

procedure Func_IL_LTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  <= PInt64(Args[1])^);
end;

procedure Func_IL_GTE(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(PInteger(Args[0])^  >= PInt64(Args[1])^);
end;

{$endif}

procedure Func_AND(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(Boolean(Args[0]^) and Boolean(Args[1]^));
end;

procedure Func_OR(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(Boolean(Args[0]^) or Boolean(Args[1]^));
end;

procedure Func_NOT(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(not Boolean(Args[0]^));
end;

//--TDbfParser---------------------------------------------------------------

var
  DbfWordsSensList, DbfWordsInsensList: TExpressList;
  DbfWordsAllList: TExpressList;

constructor TDbfParser.Create(ADbfFile: Pointer);
begin
  FDbfFile := ADbfFile;
  FFieldVarList := TStringList.Create;
  FCaseInsensitive := true;
  FRawStringFields := true;
  inherited Create;
end;

destructor TDbfParser.Destroy;
begin
  ClearExpressions;
  inherited;
  FreeAndNil(FFieldVarList);
end;

function TDbfParser.GetResultType: TExpressionType;
begin
  // if not a real expression, return type ourself
  if FIsExpression then
    Result := inherited GetResultType
  else
    Result := FFieldType;
end;

procedure TDbfParser.SetCaseInsensitive(NewInsensitive: Boolean);
begin
  if FCaseInsensitive <> NewInsensitive then
  begin
    // clear and regenerate functions
    FCaseInsensitive := NewInsensitive;
    FillExpressList;
    if Length(Expression) > 0 then
      ParseExpression(Expression);
  end;
end;

procedure TDbfParser.SetRawStringFields(NewRawFields: Boolean);
begin
  if FRawStringFields <> NewRawFields then
  begin
    // clear and regenerate functions, custom fields will be deleted too
    FRawStringFields := NewRawFields;
    if Length(Expression) > 0 then
      ParseExpression(Expression);
  end;
end;

procedure TDbfParser.FillExpressList;
begin
  FWordsList.FreeAll;
  if FCaseInsensitive then
  begin
    FWordsList.AddList(DbfWordsInsensList, 0, DbfWordsInsensList.Count - 1);
  end else begin
    FWordsList.AddList(DbfWordsSensList, 0, DbfWordsSensList.Count - 1);
  end;
end;

function TDbfParser.GetVariableInfo(VarName: string): TDbfFieldDef;
begin
  Result := TDbfFile(FDbfFile).GetFieldInfo(VarName);
end;

procedure TDbfParser.HandleUnknownVariable(VarName: string);
var
  FieldInfo: TDbfFieldDef;
  TempFieldVar: TFieldVar;
begin
  // is this variable a fieldname?
  FieldInfo := GetVariableInfo(VarName);
  if FieldInfo = nil then
    raise EDbfError.CreateFmt(STRING_INDEX_BASED_ON_UNKNOWN_FIELD, [VarName]);

  // define field in parser
  case FieldInfo.FieldType of
    ftString, ftBoolean:
      begin
        if RawStringFields then
        begin
          { raw string fields have fixed length, not null-terminated }
          TempFieldVar := TRawStringFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
          DefineStringVariableFixedLen(VarName, TempFieldVar.FieldVal, FieldInfo.Size);
        end else begin
          { ansi string field function translates and null-terminates field value }
          TempFieldVar := TAnsiStringFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
          DefineStringVariable(VarName, TempFieldVar.FieldVal);
        end;
      end;
    ftFloat:
      begin
        TempFieldVar := TFloatFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        DefineFloatVariable(VarName, TempFieldVar.FieldVal);
      end;
    ftAutoInc, ftInteger, ftSmallInt:
      begin
        TempFieldVar := TIntegerFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        DefineIntegerVariable(VarName, TempFieldVar.FieldVal);
      end;
{
    ftSmallInt:
      begin
        TempFieldVar := TSmallIntFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        DefineSmallIntVariable(VarName, TempFieldVar.FieldVal);
      end;
}
{$ifdef SUPPORT_INT64}
    ftLargeInt:
      begin
        TempFieldVar := TLargeIntFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        DefineLargeIntVariable(VarName, TempFieldVar.FieldVal);
      end;
{$endif}
    ftDate, ftDateTime:
      begin
        TempFieldVar := TDateTimeFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        DefineDateTimeVariable(VarName, TempFieldVar.FieldVal);
      end;
  else
    raise EDbfError.CreateFmt(STRING_INDEX_BASED_ON_INVALID_FIELD, [VarName]);
  end;

  // add to our own list
  FFieldVarList.AddObject(VarName, TempFieldVar);
end;

function TDbfParser.CurrentExpression: string;
begin
  Result := FCurrentExpression;
end;

procedure TDbfParser.ClearExpressions;
var
  I: Integer;
begin
  inherited;

  // test if already freed
  if FFieldVarList <> nil then
  begin
    // free field list
    for I := 0 to FFieldVarList.Count - 1 do
    begin
      // replacing with nil = undefining variable
      ReplaceFunction(TFieldVar(FFieldVarList.Objects[I]).FieldName, nil);
      TFieldVar(FFieldVarList.Objects[I]).Free;
    end;
    FFieldVarList.Clear;
  end;

  // clear expression
  FCurrentExpression := EmptyStr;
end;

procedure TDbfParser.ParseExpression(Expression: string);
var
  TempBuffer: array[0..4000] of Char;
begin
  // clear any current expression
  ClearExpressions;

  // is this a simple field or complex expression?
  FIsExpression := GetVariableInfo(Expression) = nil;
  if FIsExpression then
  begin
    // parse requested
    CompileExpression(Expression);

    // determine length of string length expressions
    if ResultType = etString then
    begin
      // make empty record
      TDbfFile(FDbfFile).InitRecord(@TempBuffer[0]);
      FResultLen := StrLen(ExtractFromBuffer(@TempBuffer[0]));
    end;
  end else begin
    // simple field, create field variable for it
    HandleUnknownVariable(Expression);
    FFieldType := TFieldVar(FFieldVarList.Objects[0]).FieldType;
    // set result len of variable length fields
    if FFieldType = etString then
      FResultLen := TFieldVar(FFieldVarList.Objects[0]).FieldDef.Size
  end;

  // set result len for fixed length expressions / fields
  case ResultType of
    etBoolean:  FResultLen := 1;
    etInteger:  FResultLen := 4;
    etFloat:    FResultLen := 8;
    etDateTime: FResultLen := 8;
  end;

  // check if expression not too long
  if FResultLen > 100 then
    raise EDbfError.CreateFmt(STRING_INDEX_EXPRESSION_TOO_LONG, [Expression, FResultLen]);

  // if no errors, assign current expression
  FCurrentExpression := Expression;
end;

function TDbfParser.ExtractFromBuffer(Buffer: PChar): PChar;
var
  I: Integer;
begin
  // prepare all field variables
  for I := 0 to FFieldVarList.Count - 1 do
    TFieldVar(FFieldVarList.Objects[I]).Refresh(Buffer);

  // complex expression?
  if FIsExpression then
  begin
    // execute expression
    EvaluateCurrent;
    Result := ExpResult;
  end else begin
    // simple field, get field result
    Result := TFieldVar(FFieldVarList.Objects[0]).FieldVal;
    // if string then dereference
    if FFieldType = etString then
      Result := PPChar(Result)^;
  end;
end;

initialization

  DbfWordsSensList := TExpressList.Create;
  DbfWordsInsensList := TExpressList.Create;
  DbfWordsAllList := TExpressList.Create;

  with DbfWordsAllList do
  begin
    // basic function functionality
    Add(TLeftBracket.Create('(', nil));
    Add(TRightBracket.Create(')', nil));
    Add(TComma.Create(',', nil));

    // operators - name, param types, result type, func addr, precedence
    Add(TFunction.CreateOper('+', 'SS', etString,   nil,          40));
    Add(TFunction.CreateOper('+', 'FF', etFloat,    FuncAdd_F_FF, 40));
    Add(TFunction.CreateOper('+', 'FI', etFloat,    FuncAdd_F_FI, 40));
    Add(TFunction.CreateOper('+', 'IF', etFloat,    FuncAdd_F_IF, 40));
    Add(TFunction.CreateOper('+', 'II', etInteger,  FuncAdd_F_II, 40));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('+', 'FL', etFloat,    FuncAdd_F_FL, 40));
    Add(TFunction.CreateOper('+', 'IL', etLargeInt, FuncAdd_F_IL, 40));
    Add(TFunction.CreateOper('+', 'LF', etFloat,    FuncAdd_F_LF, 40));
    Add(TFunction.CreateOper('+', 'LL', etLargeInt, FuncAdd_F_LI, 40));
    Add(TFunction.CreateOper('+', 'LI', etLargeInt, FuncAdd_F_LL, 40));
{$endif}

    Add(TFunction.CreateOper('=', 'FF', etBoolean, Func_FF_EQ , 80));
    Add(TFunction.CreateOper('<', 'FF', etBoolean, Func_FF_LT , 80));
    Add(TFunction.CreateOper('>', 'FF', etBoolean, Func_FF_GT , 80));
    Add(TFunction.CreateOper('<=','FF', etBoolean, Func_FF_LTE, 80));
    Add(TFunction.CreateOper('>=','FF', etBoolean, Func_FF_GTE, 80));
    Add(TFunction.CreateOper('<>','FF', etBoolean, Func_FF_NEQ, 80));
    Add(TFunction.CreateOper('=', 'FI', etBoolean, Func_FI_EQ , 80));
    Add(TFunction.CreateOper('<', 'FI', etBoolean, Func_FI_LT , 80));
    Add(TFunction.CreateOper('>', 'FI', etBoolean, Func_FI_GT , 80));
    Add(TFunction.CreateOper('<=','FI', etBoolean, Func_FI_LTE, 80));
    Add(TFunction.CreateOper('>=','FI', etBoolean, Func_FI_GTE, 80));
    Add(TFunction.CreateOper('<>','FI', etBoolean, Func_FI_NEQ, 80));
    Add(TFunction.CreateOper('=', 'II', etBoolean, Func_II_EQ , 80));
    Add(TFunction.CreateOper('<', 'II', etBoolean, Func_II_LT , 80));
    Add(TFunction.CreateOper('>', 'II', etBoolean, Func_II_GT , 80));
    Add(TFunction.CreateOper('<=','II', etBoolean, Func_II_LTE, 80));
    Add(TFunction.CreateOper('>=','II', etBoolean, Func_II_GTE, 80));
    Add(TFunction.CreateOper('<>','II', etBoolean, Func_II_NEQ, 80));
    Add(TFunction.CreateOper('=', 'IF', etBoolean, Func_IF_EQ , 80));
    Add(TFunction.CreateOper('<', 'IF', etBoolean, Func_IF_LT , 80));
    Add(TFunction.CreateOper('>', 'IF', etBoolean, Func_IF_GT , 80));
    Add(TFunction.CreateOper('<=','IF', etBoolean, Func_IF_LTE, 80));
    Add(TFunction.CreateOper('>=','IF', etBoolean, Func_IF_GTE, 80));
    Add(TFunction.CreateOper('<>','IF', etBoolean, Func_IF_NEQ, 80));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('=', 'LL', etBoolean, Func_LL_EQ , 80));
    Add(TFunction.CreateOper('<', 'LL', etBoolean, Func_LL_LT , 80));
    Add(TFunction.CreateOper('>', 'LL', etBoolean, Func_LL_GT , 80));
    Add(TFunction.CreateOper('<=','LL', etBoolean, Func_LL_LTE, 80));
    Add(TFunction.CreateOper('>=','LL', etBoolean, Func_LL_GTE, 80));
    Add(TFunction.CreateOper('<>','LL', etBoolean, Func_LL_NEQ, 80));
    Add(TFunction.CreateOper('=', 'LF', etBoolean, Func_LF_EQ , 80));
    Add(TFunction.CreateOper('<', 'LF', etBoolean, Func_LF_LT , 80));
    Add(TFunction.CreateOper('>', 'LF', etBoolean, Func_LF_GT , 80));
    Add(TFunction.CreateOper('<=','LF', etBoolean, Func_LF_LTE, 80));
    Add(TFunction.CreateOper('>=','LF', etBoolean, Func_LF_GTE, 80));
    Add(TFunction.CreateOper('<>','FI', etBoolean, Func_LF_NEQ, 80));
    Add(TFunction.CreateOper('=', 'LI', etBoolean, Func_LI_EQ , 80));
    Add(TFunction.CreateOper('<', 'LI', etBoolean, Func_LI_LT , 80));
    Add(TFunction.CreateOper('>', 'LI', etBoolean, Func_LI_GT , 80));
    Add(TFunction.CreateOper('<=','LI', etBoolean, Func_LI_LTE, 80));
    Add(TFunction.CreateOper('>=','LI', etBoolean, Func_LI_GTE, 80));
    Add(TFunction.CreateOper('<>','LI', etBoolean, Func_LI_NEQ, 80));
    Add(TFunction.CreateOper('=', 'FL', etBoolean, Func_FL_EQ , 80));
    Add(TFunction.CreateOper('<', 'FL', etBoolean, Func_FL_LT , 80));
    Add(TFunction.CreateOper('>', 'FL', etBoolean, Func_FL_GT , 80));
    Add(TFunction.CreateOper('<=','FL', etBoolean, Func_FL_LTE, 80));
    Add(TFunction.CreateOper('>=','FL', etBoolean, Func_FL_GTE, 80));
    Add(TFunction.CreateOper('<>','FL', etBoolean, Func_FL_NEQ, 80));
    Add(TFunction.CreateOper('=', 'IL', etBoolean, Func_IL_EQ , 80));
    Add(TFunction.CreateOper('<', 'IL', etBoolean, Func_IL_LT , 80));
    Add(TFunction.CreateOper('>', 'IL', etBoolean, Func_IL_GT , 80));
    Add(TFunction.CreateOper('<=','IL', etBoolean, Func_IL_LTE, 80));
    Add(TFunction.CreateOper('>=','IL', etBoolean, Func_IL_GTE, 80));
    Add(TFunction.CreateOper('<>','IL', etBoolean, Func_IL_NEQ, 80));
{$endif}

    Add(TFunction.CreateOper('NOT', 'B',  etBoolean, Func_NOT, 85));
    Add(TFunction.CreateOper('AND', 'BB', etBoolean, Func_AND, 90));
    Add(TFunction.CreateOper('OR',  'BB', etBoolean, Func_OR, 100));

    // functions - name, description, param types, min params, result type, func addr
    Add(TFunction.Create('STR',       '',      'FII', 1, etString, FuncFloatToStr, ''));
    Add(TFunction.Create('STR',       '',      'III', 1, etString, FuncIntToStr, ''));
    Add(TFunction.Create('DTOS',      '',      'D',   1, etString, FuncDateToStr, ''));
    Add(TFunction.Create('SUBSTR',    'SUBS',  'SII', 3, etString, FuncSubString, ''));
    Add(TFunction.Create('UPPERCASE', 'UPPER', 'S',   1, etString, FuncUppercase, ''));
    Add(TFunction.Create('LOWERCASE', 'LOWER', 'S',   1, etString, FuncLowercase, ''));
  end;

  with DbfWordsInsensList do
  begin
    AddList(DbfWordsAllList, 0, DbfWordsAllList.Count - 1);
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStrI_EQ , 80));
    Add(TFunction.CreateOper('<', 'SS', etBoolean, FuncStrI_LT , 80));
    Add(TFunction.CreateOper('>', 'SS', etBoolean, FuncStrI_GT , 80));
    Add(TFunction.CreateOper('<=','SS', etBoolean, FuncStrI_LTE, 80));
    Add(TFunction.CreateOper('>=','SS', etBoolean, FuncStrI_GTE, 80));
    Add(TFunction.CreateOper('<>','SS', etBoolean, FuncStrI_NEQ, 80));
  end;

  with DbfWordsSensList do
  begin
    AddList(DbfWordsAllList, 0, DbfWordsAllList.Count - 1);
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStr_EQ , 80));
    Add(TFunction.CreateOper('<', 'SS', etBoolean, FuncStr_LT , 80));
    Add(TFunction.CreateOper('>', 'SS', etBoolean, FuncStr_GT , 80));
    Add(TFunction.CreateOper('<=','SS', etBoolean, FuncStr_LTE, 80));
    Add(TFunction.CreateOper('>=','SS', etBoolean, FuncStr_GTE, 80));
    Add(TFunction.CreateOper('<>','SS', etBoolean, FuncStr_NEQ, 80));
  end;

finalization

  DbfWordsAllList.Free;
  DbfWordsInsensList.Free;
  DbfWordsSensList.Free;

end.

