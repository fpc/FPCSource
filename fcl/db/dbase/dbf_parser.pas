unit dbf_parser;

interface

{$I dbf_common.inc}

uses
  SysUtils,
  Classes,
{$ifdef KYLIX}
  Libc,
{$endif}
{$ifndef WIN32}
  dbf_wtil,
{$endif}
  db,
  dbf_prscore,
  dbf_common,
  dbf_fields,
  dbf_prsdef,
  dbf_prssupp;

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
    FPartialMatch: boolean;

  protected
    FCurrentExpression: string;

    procedure FillExpressList; override;
    procedure HandleUnknownVariable(VarName: string); override;
    function  GetVariableInfo(VarName: string): TDbfFieldDef;
    function  CurrentExpression: string; override;
    function  GetResultType: TExpressionType; override;

    procedure SetCaseInsensitive(NewInsensitive: Boolean);
    procedure SetRawStringFields(NewRawFields: Boolean);
    procedure SetPartialMatch(NewPartialMatch: boolean);
  public
    constructor Create(ADbfFile: Pointer);
    destructor Destroy; override;

    procedure ClearExpressions; override;

    procedure ParseExpression(AExpression: string); virtual;
    function ExtractFromBuffer(Buffer: PChar): PChar; virtual;

    property DbfFile: Pointer read FDbfFile write FDbfFile;
    property Expression: string read FCurrentExpression;
    property ResultLen: Integer read FResultLen;

    property CaseInsensitive: Boolean read FCaseInsensitive write SetCaseInsensitive;
    property RawStringFields: Boolean read FRawStringFields write SetRawStringFields;
    property PartialMatch: boolean read FPartialMatch write SetPartialMatch;
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
procedure FuncSub_F_FF(Param: PExpressionRec);
procedure FuncSub_F_FI(Param: PExpressionRec);
procedure FuncSub_F_II(Param: PExpressionRec);
procedure FuncSub_F_IF(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure FuncSub_F_FL(Param: PExpressionRec);
procedure FuncSub_F_IL(Param: PExpressionRec);
procedure FuncSub_F_LL(Param: PExpressionRec);
procedure FuncSub_F_LF(Param: PExpressionRec);
procedure FuncSub_F_LI(Param: PExpressionRec);
{$endif}
procedure FuncMul_F_FF(Param: PExpressionRec);
procedure FuncMul_F_FI(Param: PExpressionRec);
procedure FuncMul_F_II(Param: PExpressionRec);
procedure FuncMul_F_IF(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure FuncMul_F_FL(Param: PExpressionRec);
procedure FuncMul_F_IL(Param: PExpressionRec);
procedure FuncMul_F_LL(Param: PExpressionRec);
procedure FuncMul_F_LF(Param: PExpressionRec);
procedure FuncMul_F_LI(Param: PExpressionRec);
{$endif}
procedure FuncDiv_F_FF(Param: PExpressionRec);
procedure FuncDiv_F_FI(Param: PExpressionRec);
procedure FuncDiv_F_II(Param: PExpressionRec);
procedure FuncDiv_F_IF(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure FuncDiv_F_FL(Param: PExpressionRec);
procedure FuncDiv_F_IL(Param: PExpressionRec);
procedure FuncDiv_F_LL(Param: PExpressionRec);
procedure FuncDiv_F_LF(Param: PExpressionRec);
procedure FuncDiv_F_LI(Param: PExpressionRec);
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
  dbf,
  dbf_dbffile,
  dbf_str
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
    FExprWord: TExprWord;
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
    procedure Refresh(Buffer: PChar); override;
  end;

  TIntegerFieldVar = class(TFieldVar)
  private
    FFieldVal: Integer;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
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
    procedure Refresh(Buffer: PChar); override;
  end;

  TBooleanFieldVar = class(TFieldVar)
  private
    FFieldVal: boolean;
    function GetFieldType: TExpressionType; override;
  protected
    function GetFieldVal: Pointer; override;
  public
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
  if not FDbfFile.GetFieldDataFromDef(FieldDef, ftDateTime, Buffer, @FFieldVal) then
    FFieldVal.DateTime := 0.0;
end;

//--TBooleanFieldVar---------------------------------------------------------
function TBooleanFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TBooleanFieldVar.GetFieldType: TExpressionType;
begin
  Result := etBoolean;
end;

procedure TBooleanFieldVar.Refresh(Buffer: PChar);
var
  lFieldVal: word;
begin
  if FDbfFile.GetFieldDataFromDef(FieldDef, ftBoolean, Buffer, @lFieldVal) then
    FFieldVal := lFieldVal <> 0
  else
    FFieldVal := false;
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
      Inc(Param^.Res.MemoryPos^, width);
    end;
    // null-terminate
    Res.MemoryPos^^ := #0;
  end;
end;

procedure FuncIntToStr(Param: PExpressionRec);
begin
  FuncIntToStr_Gen(Param, PInteger(Param^.Args[0])^);
end;

procedure FuncDateToStr(Param: PExpressionRec);
var
  TempStr: string;
begin
  with Param^ do
  begin
    // create in temporary string
    DateTimeToString(TempStr, 'yyyymmdd', PDateTimeRec(Args[0])^.DateTime);
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
    AnsiStrUpper(dest);
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
    AnsiStrLower(dest);
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

procedure FuncSub_F_FF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ - PDouble(Args[1])^;
end;

procedure FuncSub_F_FI(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ - PInteger(Args[1])^;
end;

procedure FuncSub_F_II(Param: PExpressionRec);
begin
  with Param^ do
    PInteger(Res.MemoryPos^)^ := PInteger(Args[0])^ - PInteger(Args[1])^;
end;

procedure FuncSub_F_IF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PInteger(Args[0])^ - PDouble(Args[1])^;
end;

{$ifdef SUPPORT_INT64}

procedure FuncSub_F_FL(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ - PInt64(Args[1])^;
end;

procedure FuncSub_F_IL(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInteger(Args[0])^ - PInt64(Args[1])^;
end;

procedure FuncSub_F_LL(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInt64(Args[0])^ - PInt64(Args[1])^;
end;

procedure FuncSub_F_LF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PInt64(Args[0])^ - PDouble(Args[1])^;
end;

procedure FuncSub_F_LI(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInt64(Args[0])^ - PInteger(Args[1])^;
end;

{$endif}

procedure FuncMul_F_FF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ * PDouble(Args[1])^;
end;

procedure FuncMul_F_FI(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ * PInteger(Args[1])^;
end;

procedure FuncMul_F_II(Param: PExpressionRec);
begin
  with Param^ do
    PInteger(Res.MemoryPos^)^ := PInteger(Args[0])^ * PInteger(Args[1])^;
end;

procedure FuncMul_F_IF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PInteger(Args[0])^ * PDouble(Args[1])^;
end;

{$ifdef SUPPORT_INT64}

procedure FuncMul_F_FL(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ * PInt64(Args[1])^;
end;

procedure FuncMul_F_IL(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInteger(Args[0])^ * PInt64(Args[1])^;
end;

procedure FuncMul_F_LL(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInt64(Args[0])^ * PInt64(Args[1])^;
end;

procedure FuncMul_F_LF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PInt64(Args[0])^ * PDouble(Args[1])^;
end;

procedure FuncMul_F_LI(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInt64(Args[0])^ * PInteger(Args[1])^;
end;

{$endif}

procedure FuncDiv_F_FF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ / PDouble(Args[1])^;
end;

procedure FuncDiv_F_FI(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ / PInteger(Args[1])^;
end;

procedure FuncDiv_F_II(Param: PExpressionRec);
begin
  with Param^ do
    PInteger(Res.MemoryPos^)^ := PInteger(Args[0])^ div PInteger(Args[1])^;
end;

procedure FuncDiv_F_IF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PInteger(Args[0])^ / PDouble(Args[1])^;
end;

{$ifdef SUPPORT_INT64}

procedure FuncDiv_F_FL(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PDouble(Args[0])^ / PInt64(Args[1])^;
end;

procedure FuncDiv_F_IL(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInteger(Args[0])^ div PInt64(Args[1])^;
end;

procedure FuncDiv_F_LL(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInt64(Args[0])^ div PInt64(Args[1])^;
end;

procedure FuncDiv_F_LF(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := PInt64(Args[0])^ / PDouble(Args[1])^;
end;

procedure FuncDiv_F_LI(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := PInt64(Args[0])^ div PInteger(Args[1])^;
end;

{$endif}

procedure FuncStrI_EQ(Param: PExpressionRec);
begin
  with Param^ do
    Res.MemoryPos^^ := Char(AnsiStrIComp(Args[0], Args[1]) = 0);
end;

procedure FuncStrIP_EQ(Param: PExpressionRec);
var
  arg0len, arg1len: integer;
  match: boolean;
  str0, str1: string;
begin
  with Param^ do
  begin
    arg1len := StrLen(Args[1]);
    if Args[1][0] = '*' then
    begin
      if Args[1][arg1len-1] = '*' then
      begin
        str0 := AnsiStrUpper(Args[0]);
        str1 := AnsiStrUpper(Args[1]+1);
        setlength(str1, arg1len-2);
        match := AnsiPos(str0, str1) = 0;
      end else begin
        arg0len := StrLen(Args[0]);
        // at least length without asterisk
        match := arg0len >= arg1len - 1;
        if match then
          match := AnsiStrLIComp(Args[0]+(arg0len-arg1len+1), Args[1]+1, arg1len-1) = 0;
      end;
    end else
    if Args[1][arg1len-1] = '*' then
    begin
      arg0len := StrLen(Args[0]);
      match := arg0len >= arg1len - 1;
      if match then
        match := AnsiStrLIComp(Args[0], Args[1], arg1len-1) = 0;
    end else begin
      match := AnsiStrIComp(Args[0], Args[1]) = 0;
    end;
    Res.MemoryPos^^ := Char(match);
  end;
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

procedure FuncStrP_EQ(Param: PExpressionRec);
var
  arg0len, arg1len: integer;
  match: boolean;
begin
  with Param^ do
  begin
    arg1len := StrLen(Args[1]);
    if Args[1][0] = '*' then
    begin
      if Args[1][arg1len-1] = '*' then
      begin
        Args[1][arg1len-1] := #0;
        match := AnsiStrPos(Args[0], Args[1]+1) <> nil;
        Args[1][arg1len-1] := '*';
      end else begin
        arg0len := StrLen(Args[0]);
        // at least length without asterisk
        match := arg0len >= arg1len - 1;
        if match then
          match := AnsiStrLComp(Args[0]+(arg0len-arg1len+1), Args[1]+1, arg1len-1) = 0;
      end;
    end else
    if Args[1][arg1len-1] = '*' then
    begin
      arg0len := StrLen(Args[0]);
      match := arg0len >= arg1len - 1;
      if match then
        match := AnsiStrLComp(Args[0], Args[1], arg1len-1) = 0;
    end else begin
      match := AnsiStrComp(Args[0], Args[1]) = 0;
    end;
    Res.MemoryPos^^ := Char(match);
  end;
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
  DbfWordsSensGeneralList, DbfWordsInsensGeneralList: TExpressList;
  DbfWordsSensPartialList, DbfWordsInsensPartialList: TExpressList;
  DbfWordsSensNoPartialList, DbfWordsInsensNoPartialList: TExpressList;
  DbfWordsGeneralList: TExpressList;

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
  end;
end;

procedure TDbfParser.SetPartialMatch(NewPartialMatch: boolean);
begin
  if FPartialMatch <> NewPartialMatch then
  begin
    // refill function list
    FPartialMatch := NewPartialMatch;
    FillExpressList;
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
var
  lExpression: string;
begin
  lExpression := FCurrentExpression;
  ClearExpressions;
  FWordsList.FreeAll;
  FWordsList.AddList(DbfWordsGeneralList, 0, DbfWordsGeneralList.Count - 1);
  if FCaseInsensitive then
  begin
    FWordsList.AddList(DbfWordsInsensGeneralList, 0, DbfWordsInsensGeneralList.Count - 1);
    if FPartialMatch then
    begin
      FWordsList.AddList(DbfWordsInsensPartialList, 0, DbfWordsInsensPartialList.Count - 1);
    end else begin
      FWordsList.AddList(DbfWordsInsensNoPartialList, 0, DbfWordsInsensNoPartialList.Count - 1);
    end;
  end else begin
    FWordsList.AddList(DbfWordsSensGeneralList, 0, DbfWordsSensGeneralList.Count - 1);
    if FPartialMatch then
    begin
      FWordsList.AddList(DbfWordsSensPartialList, 0, DbfWordsSensPartialList.Count - 1);
    end else begin
      FWordsList.AddList(DbfWordsSensNoPartialList, 0, DbfWordsSensNoPartialList.Count - 1);
    end;
  end;
  if Length(lExpression) > 0 then
    ParseExpression(lExpression);
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
    ftString:
      begin
        if RawStringFields then
        begin
          { raw string fields have fixed length, not null-terminated }
          TempFieldVar := TRawStringFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
          TempFieldVar.FExprWord := DefineStringVariableFixedLen(VarName, TempFieldVar.FieldVal, FieldInfo.Size);
        end else begin
          { ansi string field function translates and null-terminates field value }
          TempFieldVar := TAnsiStringFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
          TempFieldVar.FExprWord := DefineStringVariable(VarName, TempFieldVar.FieldVal);
        end;
      end;
    ftBoolean:
      begin
        TempFieldVar := TBooleanFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.FExprWord := DefineBooleanVariable(VarName, TempFieldVar.FieldVal);
      end;
    ftFloat:
      begin
        TempFieldVar := TFloatFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.FExprWord := DefineFloatVariable(VarName, TempFieldVar.FieldVal);
      end;
    ftAutoInc, ftInteger, ftSmallInt:
      begin
        TempFieldVar := TIntegerFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.FExprWord := DefineIntegerVariable(VarName, TempFieldVar.FieldVal);
      end;
{$ifdef SUPPORT_INT64}
    ftLargeInt:
      begin
        TempFieldVar := TLargeIntFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.FExprWord := DefineLargeIntVariable(VarName, TempFieldVar.FieldVal);
      end;
{$endif}
    ftDate, ftDateTime:
      begin
        TempFieldVar := TDateTimeFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.FExprWord := DefineDateTimeVariable(VarName, TempFieldVar.FieldVal);
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
      FWordsList.DoFree(TFieldVar(FFieldVarList.Objects[I]).FExprWord);
      TFieldVar(FFieldVarList.Objects[I]).Free;
    end;
    FFieldVarList.Clear;
  end;

  // clear expression
  FCurrentExpression := EmptyStr;
end;

procedure TDbfParser.ParseExpression(AExpression: string);
var
  TempBuffer: pchar;
begin
  // clear any current expression
  ClearExpressions;

  // is this a simple field or complex expression?
  FIsExpression := GetVariableInfo(AExpression) = nil;
  if FIsExpression then
  begin
    // parse requested
    CompileExpression(AExpression);

    // determine length of string length expressions
    if ResultType = etString then
    begin
      // make empty record
      GetMem(TempBuffer, TDbfFile(FDbfFile).RecordSize);
      try
        TDbfFile(FDbfFile).InitRecord(TempBuffer);
        FResultLen := StrLen(ExtractFromBuffer(TempBuffer));
      finally
        FreeMem(TempBuffer);
      end;
    end;
  end else begin
    // simple field, create field variable for it
    HandleUnknownVariable(AExpression);
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
    raise EDbfError.CreateFmt(STRING_INDEX_EXPRESSION_TOO_LONG, [AExpression, FResultLen]);

  // if no errors, assign current expression
  FCurrentExpression := AExpression;
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

  DbfWordsGeneralList := TExpressList.Create;
  DbfWordsInsensGeneralList := TExpressList.Create;
  DbfWordsInsensNoPartialList := TExpressList.Create;
  DbfWordsInsensPartialList := TExpressList.Create;
  DbfWordsSensGeneralList := TExpressList.Create;
  DbfWordsSensNoPartialList := TExpressList.Create;
  DbfWordsSensPartialList := TExpressList.Create;

  with DbfWordsGeneralList do
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
    Add(TFunction.CreateOper('-', 'FF', etFloat,    FuncSub_F_FF, 40));
    Add(TFunction.CreateOper('-', 'FI', etFloat,    FuncSub_F_FI, 40));
    Add(TFunction.CreateOper('-', 'IF', etFloat,    FuncSub_F_IF, 40));
    Add(TFunction.CreateOper('-', 'II', etInteger,  FuncSub_F_II, 40));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('-', 'FL', etFloat,    FuncSub_F_FL, 40));
    Add(TFunction.CreateOper('-', 'IL', etLargeInt, FuncSub_F_IL, 40));
    Add(TFunction.CreateOper('-', 'LF', etFloat,    FuncSub_F_LF, 40));
    Add(TFunction.CreateOper('-', 'LL', etLargeInt, FuncSub_F_LI, 40));
    Add(TFunction.CreateOper('-', 'LI', etLargeInt, FuncSub_F_LL, 40));
{$endif}
    Add(TFunction.CreateOper('*', 'FF', etFloat,    FuncMul_F_FF, 40));
    Add(TFunction.CreateOper('*', 'FI', etFloat,    FuncMul_F_FI, 40));
    Add(TFunction.CreateOper('*', 'IF', etFloat,    FuncMul_F_IF, 40));
    Add(TFunction.CreateOper('*', 'II', etInteger,  FuncMul_F_II, 40));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('*', 'FL', etFloat,    FuncMul_F_FL, 40));
    Add(TFunction.CreateOper('*', 'IL', etLargeInt, FuncMul_F_IL, 40));
    Add(TFunction.CreateOper('*', 'LF', etFloat,    FuncMul_F_LF, 40));
    Add(TFunction.CreateOper('*', 'LL', etLargeInt, FuncMul_F_LI, 40));
    Add(TFunction.CreateOper('*', 'LI', etLargeInt, FuncMul_F_LL, 40));
{$endif}
    Add(TFunction.CreateOper('/', 'FF', etFloat,    FuncDiv_F_FF, 40));
    Add(TFunction.CreateOper('/', 'FI', etFloat,    FuncDiv_F_FI, 40));
    Add(TFunction.CreateOper('/', 'IF', etFloat,    FuncDiv_F_IF, 40));
    Add(TFunction.CreateOper('/', 'II', etInteger,  FuncDiv_F_II, 40));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('/', 'FL', etFloat,    FuncDiv_F_FL, 40));
    Add(TFunction.CreateOper('/', 'IL', etLargeInt, FuncDiv_F_IL, 40));
    Add(TFunction.CreateOper('/', 'LF', etFloat,    FuncDiv_F_LF, 40));
    Add(TFunction.CreateOper('/', 'LL', etLargeInt, FuncDiv_F_LI, 40));
    Add(TFunction.CreateOper('/', 'LI', etLargeInt, FuncDiv_F_LL, 40));
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

    // Functions - name, description, param types, min params, result type, Func addr
    Add(TFunction.Create('STR',       '',      'FII', 1, etString, FuncFloatToStr, ''));
    Add(TFunction.Create('STR',       '',      'III', 1, etString, FuncIntToStr, ''));
    Add(TFunction.Create('DTOS',      '',      'D',   1, etString, FuncDateToStr, ''));
    Add(TFunction.Create('SUBSTR',    'SUBS',  'SII', 3, etString, FuncSubString, ''));
    Add(TFunction.Create('UPPERCASE', 'UPPER', 'S',   1, etString, FuncUppercase, ''));
    Add(TFunction.Create('LOWERCASE', 'LOWER', 'S',   1, etString, FuncLowercase, ''));
  end;

  with DbfWordsInsensGeneralList do
  begin
    Add(TFunction.CreateOper('<', 'SS', etBoolean, FuncStrI_LT , 80));
    Add(TFunction.CreateOper('>', 'SS', etBoolean, FuncStrI_GT , 80));
    Add(TFunction.CreateOper('<=','SS', etBoolean, FuncStrI_LTE, 80));
    Add(TFunction.CreateOper('>=','SS', etBoolean, FuncStrI_GTE, 80));
    Add(TFunction.CreateOper('<>','SS', etBoolean, FuncStrI_NEQ, 80));
  end;

  with DbfWordsInsensNoPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStrI_EQ , 80));

  with DbfWordsInsensPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStrIP_EQ, 80));

  with DbfWordsSensGeneralList do
  begin
    Add(TFunction.CreateOper('<', 'SS', etBoolean, FuncStr_LT , 80));
    Add(TFunction.CreateOper('>', 'SS', etBoolean, FuncStr_GT , 80));
    Add(TFunction.CreateOper('<=','SS', etBoolean, FuncStr_LTE, 80));
    Add(TFunction.CreateOper('>=','SS', etBoolean, FuncStr_GTE, 80));
    Add(TFunction.CreateOper('<>','SS', etBoolean, FuncStr_NEQ, 80));
  end;
    
  with DbfWordsSensNoPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStr_EQ , 80));

  with DbfWordsSensPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStrP_EQ , 80));

finalization

  DbfWordsGeneralList.Free;
  DbfWordsInsensGeneralList.Free;
  DbfWordsInsensNoPartialList.Free;
  DbfWordsInsensPartialList.Free;
  DbfWordsSensGeneralList.Free;
  DbfWordsSensNoPartialList.Free;
  DbfWordsSensPartialList.Free;

end.

