{$IFNDEF FPC_DOTTEDUNITS}
unit bufdataset_parser;
{$ENDIF FPC_DOTTEDUNITS}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Joost van der Sluis and other members of the
    Free Pascal development team

    BufDataset parser

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode delphi}
{$h+}


interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,
  System.Classes,
  Data.Db,
  Data.Dbf.Prscore,
  Data.Dbf.Prsdef;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils,
  Classes,
  db,
  dbf_prscore,
  dbf_prsdef;
{$ENDIF FPC_DOTTEDUNITS}

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
    FCurrentExpression: AnsiString;

    procedure FillExpressList; override;
    procedure HandleUnknownVariable(VarName: AnsiString); override;
    function  GetVariableInfo(const VarName: AnsiString): TField;
    function  CurrentExpression: AnsiString; override;
    function  GetResultType: TExpressionType; override;

    procedure SetCaseInsensitive(NewInsensitive: Boolean);
    procedure SetPartialMatch(NewPartialMatch: boolean);
  public
    constructor Create(ADataset: TDataset);
    destructor Destroy; override;

    procedure ClearExpressions; override;

    procedure ParseExpression(const AExpression: AnsiString); virtual;
    function ExtractFromBuffer(Buffer: TRecordBuffer): PAnsiChar; virtual;

    property Dataset: TDataSet read FDataset; // write FDataset;
    property Expression: AnsiString read FCurrentExpression;
    property ResultLen: Integer read FResultLen;

    property CaseInsensitive: Boolean read FCaseInsensitive write SetCaseInsensitive;
    property PartialMatch: boolean read FPartialMatch write SetPartialMatch;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Data.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses dbconst;
{$ENDIF FPC_DOTTEDUNITS}

type
// TFieldVar aids in retrieving field values from records
// in their proper type

  TFieldVar = class(TObject)
  private
    FField: TField;
    FFieldName: AnsiString;
    FFieldIsNull: boolean;
    FExprWord: TExprWord;
  protected
    function GetFieldVal: Pointer; virtual; abstract;
    function GetFieldType: TExpressionType; virtual; abstract;
    function GetFieldIsNull: PBoolean;
  public
    constructor Create(UseField: TField);

    procedure Refresh(Buffer: TRecordBuffer); virtual; abstract;

    property FieldVal: Pointer read GetFieldVal;
    property FieldDef: TField read FField;
    property FieldType: TExpressionType read GetFieldType;
    property FieldName: AnsiString read FFieldName;
    property FieldIsNull: PBoolean read GetFieldIsNull;
  end;

  TStringFieldVar = class(TFieldVar)
  protected
    FFieldVal: PAnsiChar;

    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    constructor Create(UseField: TField);
    destructor Destroy; override;

    procedure Refresh(Buffer: TRecordBuffer); override;
  end;

  TFloatFieldVar = class(TFieldVar)
  private
    FFieldVal: Double;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    procedure Refresh(Buffer: TRecordBuffer); override;
  end;

  TIntegerFieldVar = class(TFieldVar)
  private
    FFieldVal: Integer;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    procedure Refresh(Buffer: TRecordBuffer); override;
  end;

  TLargeIntFieldVar = class(TFieldVar)
  private
    FFieldVal: Int64;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    procedure Refresh(Buffer: TRecordBuffer); override;
  end;

  TDateTimeFieldVar = class(TFieldVar)
  private
    FFieldVal: TDateTime;
    function GetFieldType: TExpressionType; override;
  protected
    function GetFieldVal: Pointer; override;
  public
    procedure Refresh(Buffer: TRecordBuffer); override;
  end;

  TBooleanFieldVar = class(TFieldVar)
  private
    FFieldVal: wordbool;
    function GetFieldType: TExpressionType; override;
  protected
    function GetFieldVal: Pointer; override;
  public
    procedure Refresh(Buffer: TRecordBuffer); override;
  end;

  TBCDFieldVar = class(TFloatFieldVar)
  public
    procedure Refresh(Buffer: TRecordBuffer); override;
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

function TFieldVar.GetFieldIsNull: PBoolean;
begin
  Result := @FFieldIsNull;
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

procedure TStringFieldVar.Refresh(Buffer: TRecordBuffer);
var Fieldbuf : TStringFieldBuffer;
begin
  FFieldIsNull := not FField.DataSet.GetFieldData(FField,@Fieldbuf);
  if FFieldIsNull  then
    FFieldVal^:=#0
  else
    strcopy(FFieldVal,@Fieldbuf[0]);
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

procedure TFloatFieldVar.Refresh(Buffer: TRecordBuffer);
begin
  FFieldIsNull := not FField.DataSet.GetFieldData(FField,@FFieldVal);
  if FFieldIsNull then
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

procedure TIntegerFieldVar.Refresh(Buffer: TRecordBuffer);
begin
  FFieldIsNull := not FField.DataSet.GetFieldData(FField,@FFieldVal);
  if FFieldIsNull then
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

procedure TLargeIntFieldVar.Refresh(Buffer: TRecordBuffer);
begin
  FFieldIsNull := not FField.DataSet.GetFieldData(FField,@FFieldVal);
  if FFieldIsNull then
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

procedure TDateTimeFieldVar.Refresh(Buffer:TRecordBuffer );
begin
  FFieldIsNull := not FField.DataSet.GetFieldData(FField,@FFieldVal);
  if FFieldIsNull then
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

procedure TBooleanFieldVar.Refresh(Buffer: TRecordBuffer);
begin
  FFieldIsNull := not FField.DataSet.GetFieldData(FField,@FFieldVal);
  if FFieldIsNull then
    FFieldVal := False;
end;

procedure TBCDFieldVar.Refresh(Buffer: TRecordBuffer);
var c: currency;
begin
  FFieldIsNull := not FField.DataSet.GetFieldData(FField,@c);
  if FFieldIsNull then
    FFieldVal := 0
  else
    FFieldVal := c;
end;


//--TBufDatasetParser---------------------------------------------------------------

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
  lExpression: AnsiString;
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

function TBufDatasetParser.GetVariableInfo(const VarName: AnsiString): TField;

begin
  Result := FDataset.FindField(VarName);
end;

function TBufDatasetParser.CurrentExpression: AnsiString;
begin
  Result := FCurrentExpression;
end;

procedure TBufDatasetParser.HandleUnknownVariable(VarName: AnsiString);
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
    ftString, ftFixedChar:
      begin
      TempFieldVar := TStringFieldVar.Create(FieldInfo);
      TempFieldVar.FExprWord := DefineStringVariable(VarName, TempFieldVar.FieldVal, TempFieldVar.FieldIsNull);
      TempFieldVar.FExprWord.fixedlen := Fieldinfo.Size;
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
    ftAutoInc, ftInteger, ftSmallInt, ftWord:
      begin
        TempFieldVar := TIntegerFieldVar.Create(FieldInfo);
        TempFieldVar.FExprWord := DefineIntegerVariable(VarName, TempFieldVar.FieldVal, TempFieldVar.FieldIsNull);
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
    ftBCD:
      begin
        TempFieldVar := TBCDFieldVar.Create(FieldInfo);
        TempFieldVar.FExprWord := DefineFloatVariable(VarName, TempFieldVar.FieldVal);
      end;
  else
    raise EDatabaseError.CreateFmt(SErrIndexBasedOnInvField, [VarName,Fieldtypenames[FieldInfo.DataType]]);
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

procedure TBufDatasetParser.ParseExpression(const AExpression: AnsiString);

var
  TempBuffer: TRecordBuffer;
begin
  // clear any current expression
  ClearExpressions;

  // is this a simple field or complex expression?
  FIsExpression := GetVariableInfo(AExpression) = nil;
  if FIsExpression then
  begin
    // parse requested
    CompileExpression(AExpression);

    // determine length of AnsiString length expressions
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

function TBufDatasetParser.ExtractFromBuffer(Buffer: TRecordBuffer): PAnsiChar;
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
    // if AnsiString then dereference
    if FFieldType = etString then
      Result := PPAnsiChar(Result)^;
  end;
end;

end.

