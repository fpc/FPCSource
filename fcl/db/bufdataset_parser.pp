unit bufdataset_parser;

{$h+}
{$mode delphi}


interface

uses
  SysUtils,
  Classes,
  db,
  dbf_prscore,
  dbf_prsdef;

type

  TBufDatasetParser = class(TCustomExpressionParser)
  private
    FDataset: TDataSet;
    FFieldVarList: TStringList;
    FResultLen: Integer;
    FIsExpression: Boolean;       // expression or simple field?
    FFieldType: TExpressionType;
    FCaseInsensitive: Boolean;
    FPartialMatch: boolean;

  protected
    FCurrentExpression: string;

    procedure FillExpressList; override;
    procedure HandleUnknownVariable(VarName: string); override;
    function  GetVariableInfo(VarName: string): TField;
    function  CurrentExpression: string; override;
    function  GetResultType: TExpressionType; override;

    procedure SetCaseInsensitive(NewInsensitive: Boolean);
    procedure SetPartialMatch(NewPartialMatch: boolean);
  public
    constructor Create(ADataset: TDataset);
    destructor Destroy; override;

    procedure ClearExpressions; override;

    procedure ParseExpression(AExpression: string); virtual;
    function ExtractFromBuffer(Buffer: PChar): PChar; virtual;

    property Dataset: TDataSet read FDataset; // write FDataset;
    property Expression: string read FCurrentExpression;
    property ResultLen: Integer read FResultLen;

    property CaseInsensitive: Boolean read FCaseInsensitive write SetCaseInsensitive;
    property PartialMatch: boolean read FPartialMatch write SetPartialMatch;
  end;

implementation

uses dbf_parser, dbconst;

type
// TFieldVar aids in retrieving field values from records
// in their proper type

  TFieldVar = class(TObject)
  private
    FField: TField;
    FFieldName: string;
    FExprWord: TExprWord;
  protected
    function GetFieldVal: Pointer; virtual; abstract;
    function GetFieldType: TExpressionType; virtual; abstract;
  public
    constructor Create(UseField: TField);

    procedure Refresh(Buffer: PChar); virtual; abstract;

    property FieldVal: Pointer read GetFieldVal;
    property FieldDef: TField read FField;
    property FieldType: TExpressionType read GetFieldType;
    property FieldName: string read FFieldName;
  end;

  TStringFieldVar = class(TFieldVar)
  protected
    FFieldVal: PChar;

    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    constructor Create(UseField: TField);
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

  TLargeIntFieldVar = class(TFieldVar)
  private
    FFieldVal: Int64;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    procedure Refresh(Buffer: PChar); override;
  end;

  TDateTimeFieldVar = class(TFieldVar)
  private
    FFieldVal: TDateTime;
    function GetFieldType: TExpressionType; override;
  protected
    function GetFieldVal: Pointer; override;
  public
    procedure Refresh(Buffer: PChar); override;
  end;

  TBooleanFieldVar = class(TFieldVar)
  private
    FFieldVal: wordbool;
    function GetFieldType: TExpressionType; override;
  protected
    function GetFieldVal: Pointer; override;
  public
    procedure Refresh(Buffer: PChar); override;
  end;

//--TFieldVar----------------------------------------------------------------
constructor TFieldVar.Create(UseField: TField);
begin
  inherited Create;

  // store field
  //FDataset := ADataset;
  FField := UseField;
  FFieldName := UseField.FieldName;
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

constructor TStringFieldVar.Create(UseField: TField);
begin
  inherited;

  GetMem(FFieldVal, dsMaxStringSize+1);
end;

destructor TStringFieldVar.Destroy;
begin
  FreeMem(FFieldVal);

  inherited;
end;

procedure TStringFieldVar.Refresh(Buffer: PChar);
var Fieldbuf : TStringFieldBuffer;
    s        : string;
begin
  if not FField.DataSet.GetFieldData(FField,@Fieldbuf) then
    s := ''
  else
    s := Fieldbuf;
  strcopy(FFieldVal,@s[1]);
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
  if not FField.DataSet.GetFieldData(FField,@FFieldVal) then
    FFieldVal := 0;
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
  if not FField.DataSet.GetFieldData(FField,@FFieldVal) then
    FFieldVal := 0;
end;

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
  if not FField.DataSet.GetFieldData(FField,@FFieldVal) then
    FFieldVal := 0;
end;

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
  if not FField.DataSet.GetFieldData(FField,@FFieldVal) then
    FFieldVal := 0;
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
begin
  if not FField.DataSet.GetFieldData(FField,@FFieldVal) then
    FFieldVal := False;
end;

//--Expression functions-----------------------------------------------------

//These functions are in the unit dbf_parser, but they are forgotten in the interface section

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

//--TBufDatasetParser---------------------------------------------------------------

var
  BufWordsSensGeneralList, BufWordsInsensGeneralList: TExpressList;
  BufWordsSensPartialList, BufWordsInsensPartialList: TExpressList;
  BufWordsSensNoPartialList, BufWordsInsensNoPartialList: TExpressList;
  BufWordsGeneralList: TExpressList;

constructor TBufDatasetParser.Create(Adataset: TDataSet);
begin
  FDataset := Adataset;
  FFieldVarList := TStringList.Create;
  FCaseInsensitive := true;
  inherited Create;
end;

destructor TBufDatasetParser.Destroy;
begin
  ClearExpressions;
  inherited;
  FreeAndNil(FFieldVarList);
end;

function TBufDatasetParser.GetResultType: TExpressionType;
begin
  // if not a real expression, return type ourself
  if FIsExpression then
    Result := inherited GetResultType
  else
    Result := FFieldType;
end;

procedure TBufDatasetParser.SetCaseInsensitive(NewInsensitive: Boolean);
begin
  if FCaseInsensitive <> NewInsensitive then
  begin
    // clear and regenerate functions
    FCaseInsensitive := NewInsensitive;
    FillExpressList;
  end;
end;

procedure TBufDatasetParser.SetPartialMatch(NewPartialMatch: boolean);
begin
  if FPartialMatch <> NewPartialMatch then
  begin
    // refill function list
    FPartialMatch := NewPartialMatch;
    FillExpressList;
  end;
end;

procedure TBufDatasetParser.FillExpressList;
var
  lExpression: string;
begin
  lExpression := FCurrentExpression;
  ClearExpressions;
  FWordsList.FreeAll;
  FWordsList.AddList(BufWordsGeneralList, 0, BufWordsGeneralList.Count - 1);
  if FCaseInsensitive then
  begin
    FWordsList.AddList(BufWordsInsensGeneralList, 0, BufWordsInsensGeneralList.Count - 1);
    if FPartialMatch then
    begin
      FWordsList.AddList(BufWordsInsensPartialList, 0, BufWordsInsensPartialList.Count - 1);
    end else begin
      FWordsList.AddList(BufWordsInsensNoPartialList, 0, BufWordsInsensNoPartialList.Count - 1);
    end;
  end else begin
    FWordsList.AddList(BufWordsSensGeneralList, 0, BufWordsSensGeneralList.Count - 1);
    if FPartialMatch then
    begin
      FWordsList.AddList(BufWordsSensPartialList, 0, BufWordsSensPartialList.Count - 1);
    end else begin
      FWordsList.AddList(BufWordsSensNoPartialList, 0, BufWordsSensNoPartialList.Count - 1);
    end;
  end;
  if Length(lExpression) > 0 then
    ParseExpression(lExpression);
end;

function TBufDatasetParser.GetVariableInfo(VarName: string): TField;
begin
  Result := FDataset.FindField(VarName);
end;

function TBufDatasetParser.CurrentExpression: string;
begin
  Result := FCurrentExpression;
end;

procedure TBufDatasetParser.HandleUnknownVariable(VarName: string);
var
  FieldInfo: TField;
  TempFieldVar: TFieldVar;
begin
  // is this variable a fieldname?
  FieldInfo := GetVariableInfo(VarName);
  if FieldInfo = nil then
    raise EDatabaseError.CreateFmt(SErrIndexBasedOnUnkField, [VarName]);

  // define field in parser
  case FieldInfo.DataType of
    ftString:
      begin
      TempFieldVar := TStringFieldVar.Create(FieldInfo);
      TempFieldVar.FExprWord := DefineStringVariableFixedLen(VarName, TempFieldVar.FieldVal, FieldInfo.Size);
      end;
    ftBoolean:
      begin
        TempFieldVar := TBooleanFieldVar.Create(FieldInfo);
        TempFieldVar.FExprWord := DefineBooleanVariable(VarName, TempFieldVar.FieldVal);
      end;
    ftFloat:
      begin
        TempFieldVar := TFloatFieldVar.Create(FieldInfo);
        TempFieldVar.FExprWord := DefineFloatVariable(VarName, TempFieldVar.FieldVal);
      end;
    ftAutoInc, ftInteger, ftSmallInt:
      begin
        TempFieldVar := TIntegerFieldVar.Create(FieldInfo);
        TempFieldVar.FExprWord := DefineIntegerVariable(VarName, TempFieldVar.FieldVal);
      end;
    ftLargeInt:
      begin
        TempFieldVar := TLargeIntFieldVar.Create(FieldInfo);
        TempFieldVar.FExprWord := DefineLargeIntVariable(VarName, TempFieldVar.FieldVal);
      end;
    ftDate, ftDateTime:
      begin
        TempFieldVar := TDateTimeFieldVar.Create(FieldInfo);
        TempFieldVar.FExprWord := DefineDateTimeVariable(VarName, TempFieldVar.FieldVal);
      end;
  else
    raise EDatabaseError.CreateFmt(SErrIndexBasedOnInvField, [VarName]);
  end;

  // add to our own list
  FFieldVarList.AddObject(VarName, TempFieldVar);
end;

procedure TBufDatasetParser.ClearExpressions;
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

procedure TBufDatasetParser.ParseExpression(AExpression: string);
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
      GetMem(TempBuffer, FDataset.RecordSize);
      try
        FillChar(TempBuffer^, FDataset.RecordSize, #0);
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
    raise EDatabaseError.CreateFmt(SErrIndexResultTooLong, [AExpression, FResultLen]);

  // if no errors, assign current expression
  FCurrentExpression := AExpression;
end;

function TBufDatasetParser.ExtractFromBuffer(Buffer: PChar): PChar;
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

  BufWordsGeneralList := TExpressList.Create;
  BufWordsInsensGeneralList := TExpressList.Create;
  BufWordsInsensNoPartialList := TExpressList.Create;
  BufWordsInsensPartialList := TExpressList.Create;
  BufWordsSensGeneralList := TExpressList.Create;
  BufWordsSensNoPartialList := TExpressList.Create;
  BufWordsSensPartialList := TExpressList.Create;

  with BufWordsGeneralList do
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

  with BufWordsInsensGeneralList do
  begin
    Add(TFunction.CreateOper('<', 'SS', etBoolean, FuncStrI_LT , 80));
    Add(TFunction.CreateOper('>', 'SS', etBoolean, FuncStrI_GT , 80));
    Add(TFunction.CreateOper('<=','SS', etBoolean, FuncStrI_LTE, 80));
    Add(TFunction.CreateOper('>=','SS', etBoolean, FuncStrI_GTE, 80));
    Add(TFunction.CreateOper('<>','SS', etBoolean, FuncStrI_NEQ, 80));
  end;

  with BufWordsInsensNoPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStrI_EQ , 80));

  with BufWordsInsensPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStrIP_EQ, 80));

  with BufWordsSensGeneralList do
  begin
    Add(TFunction.CreateOper('<', 'SS', etBoolean, FuncStr_LT , 80));
    Add(TFunction.CreateOper('>', 'SS', etBoolean, FuncStr_GT , 80));
    Add(TFunction.CreateOper('<=','SS', etBoolean, FuncStr_LTE, 80));
    Add(TFunction.CreateOper('>=','SS', etBoolean, FuncStr_GTE, 80));
    Add(TFunction.CreateOper('<>','SS', etBoolean, FuncStr_NEQ, 80));
  end;
    
  with BufWordsSensNoPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStr_EQ , 80));

  with BufWordsSensPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStrP_EQ , 80));

finalization

  BufWordsGeneralList.Free;
  BufWordsInsensGeneralList.Free;
  BufWordsInsensNoPartialList.Free;
  BufWordsInsensPartialList.Free;
  BufWordsSensGeneralList.Free;
  BufWordsSensNoPartialList.Free;
  BufWordsSensPartialList.Free;

end.

