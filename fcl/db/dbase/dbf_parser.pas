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


//--TDbfParser---------------------------------------------------------------
(*
var
  DbfWordsSensGeneralList, DbfWordsInsensGeneralList: TExpressList;
  DbfWordsSensPartialList, DbfWordsInsensPartialList: TExpressList;
  DbfWordsSensNoPartialList, DbfWordsInsensNoPartialList: TExpressList;
  DbfWordsGeneralList: TExpressList;
*)
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
(*
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
*)
end.

