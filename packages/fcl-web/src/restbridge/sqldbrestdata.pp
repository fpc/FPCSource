{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    SQLDB REST data manipulation routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit sqldbrestdata;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Data.BufDataset, Data.Sqldb, Data.Db, FpJson.Data, FpWeb.RestBridge.IO, FpWeb.RestBridge.Schema;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, bufdataset, sqldb, db, fpjson, sqldbrestio, sqldbrestschema;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TSQLQueryClass = Class of TSQLQuery;

  TRestFilterPair = Record
    Field : TSQLDBRestField;
    Operation : TRestFieldFilter;
    ValueParam : TParam;
    Value : String;
  end;
  TRestFilterPairArray = Array of TRestFilterPair;

  { TSQLDBRestDBHandler }
  TSQLDBRestDBHandlerOption = (rhoLegacyPut,rhoCheckupdateCount,rhoAllowMultiUpdate,rhoSingleEmptyOK);
  TSQLDBRestDBHandlerOptions = set of TSQLDBRestDBHandlerOption;

  TSQLDBRestDBHandler = Class(TComponent)
  private
    FDeriveResourceFromDataset: Boolean;
    FEmulateOffsetLimit: Boolean;
    FEnforceLimit: Int64;
    FExternalDataset: TDataset;
    FOptions: TSQLDBRestDBHandlerOptions;
    FPostParams: TParams;
    FQueryClass: TSQLQueryClass;
    FRestIO: TRestIO;
    FStrings : TRestStringsConfig;
    FResource : TSQLDBRestResource;
    FOwnsResource : Boolean;
    FUpdatedData: TBufDataset;
    procedure CheckAllRequiredFieldsPresent;
    function GetAllowMultiUpdate: Boolean;
    function GetCheckUpdateCount: Boolean;
    function GetUseLegacyPUT: Boolean;
    procedure SetExternalDataset(AValue: TDataset);
  Protected
    Procedure CreateUpdatedData(aSrc : TDataset);
    function StreamRecord(O: TRestOutputStreamer; D: TDataset; FieldList: TRestFieldPairArray): Boolean; virtual;
    function FindExistingRecord(D: TDataset): Boolean;
    function GetRequestFields: TSQLDBRestFieldArray;
    procedure CreateResourceFromDataset(D: TDataset); virtual;
    procedure DoNotFound; virtual;
    procedure SetParamFromStringAndType(P: TParam; S: UTF8String; aDataType: TFieldType); virtual;
    procedure SetPostParams(aParams: TParams; Old : TFields = Nil);virtual;
    procedure SetPostFields(aFields: TFields);virtual;
    procedure SetFieldFromData(DataField: TField; ResField: TSQLDBRestField; D: TJSONData); virtual;
    procedure FillParams(aOperation: TRestOperation; aParams: TParams;  FilteredFields: TRestFilterPairArray); virtual;

    procedure InsertNewRecord; virtual;
    procedure UpdateExistingRecord(OldData: TDataset; IsPatch : Boolean); virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function SpecialResource: Boolean; virtual;
    function GetGeneratorValue(const aGeneratorName: String): Int64; virtual;
    function GetSpecialDatasetForResource(aFieldList: TRestFieldPairArray): TDataset; virtual;
    function FindFieldForParam(aOperation: TRestOperation; P: TParam): TSQLDBRestField; virtual;
    function BuildFieldList(ForceAll : Boolean): TRestFieldPairArray; virtual;
    function CreateQuery(const aSQL: String): TSQLQuery; virtual;
    function GetDatasetForResource(aFieldList: TRestFieldPairArray; Singleton : Boolean): TDataset; virtual;
    function GetOrderByFieldArray: TRestFieldOrderPairArray;
    function GetOrderBy: UTF8String;virtual;
    function GetIDWhere(Out FilteredFields : TRestFilterPairArray): UTF8String; virtual;
    function GetWhere(Out FilteredFields : TRestFilterPairArray): UTF8String; virtual;
    function GetLimit: UTF8String;
    // Handle 4 basic operations
    procedure DoHandleGet;virtual;
    procedure DoHandleDelete;virtual;
    procedure DoHandlePost;virtual;
    procedure DoHandlePutPatch(IsPatch : Boolean); virtual;
    procedure DoHandlePut; virtual;
    procedure DoHandlePatch; virtual;
    // Parameters used when executing update SQLs. Used to get values for return dataset params.
    Property PostParams : TParams Read FPostParams;
    Property UseLegacyPUT : Boolean Read GetUseLegacyPUT;
    Property CheckUpdateCount : Boolean Read GetCheckUpdateCount;
    Property AllowMultiUpdate : Boolean Read GetAllowMultiUpdate;
  Public
    Destructor Destroy; override;
    // Get limi
    Function GetLimitOffset(out aLimit, aOffset: Int64) : Boolean; virtual;
    Procedure Init(aIO: TRestIO; aStrings : TRestStringsConfig;AQueryClass : TSQLQueryClass); virtual;
    Procedure ExecuteOperation;
    Function StreamDataset(O: TRestOutputStreamer; D: TDataset; FieldList: TRestFieldPairArray; CurrentOnly : Boolean = False) : Int64;
    procedure SetParamFromData(P: TParam; F: TSQLDBRestField; D: TJSONData); virtual;
    function GetDataForParam(P: TParam; F: TSQLDBRestField; Sources : TVariableSources = AllVariableSources): TJSONData; virtual;
    Function GetString(aString : TRestStringProperty) : UTF8String;
    class function DefaultGetString(aConfig: TRestStringsConfig; aString: TRestStringProperty): UTF8String;
    class procedure DefaultParamFromStringAndType(P: TParam; S: UTF8String; aDataType: TFieldType; aStrings: TRestStringsConfig);
      virtual;
    Property IO : TRestIO Read FRestIO;
    Property Strings : TRestStringsConfig Read FStrings;
    Property QueryClass : TSQLQueryClass Read FQueryClass;
    Property EnforceLimit : Int64 Read FEnforceLimit Write FEnforceLimit;
    Property ExternalDataset : TDataset Read FExternalDataset Write SetExternalDataset;
    Property EmulateOffsetLimit : Boolean Read FEmulateOffsetLimit Write FEmulateOffsetLimit;
    Property DeriveResourceFromDataset : Boolean Read FDeriveResourceFromDataset Write FDeriveResourceFromDataset;
    Property Options : TSQLDBRestDBHandlerOptions Read FOptions Write FOptions;
    Property UpdatedData : TBufDataset Read FUpdatedData Write FUpdatedData;
  end;
  TSQLDBRestDBHandlerClass = class of TSQLDBRestDBHandler;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.TypInfo, System.StrUtils, System.Variants, System.DateUtils, System.Hash.Base64, FpWeb.RestBridge.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses typinfo, strutils, variants, dateutils, base64, sqldbrestconst;
{$ENDIF FPC_DOTTEDUNITS}


Const
  FilterParamPrefix : Array [TRestFieldFilter] of string = ('eq_','lt_','gt_','lte_','gte_','');
  FilterOps : Array [TRestFieldFilter] of string = ('=','<','>','<=','>=','IS NULL');

{ TSQLDBRestDBHandler }


procedure TSQLDBRestDBHandler.Init(aIO: TRestIO; aStrings: TRestStringsConfig; AQueryClass: TSQLQueryClass);
begin
  FRestIO:=aIO;
  FQueryClass:=aQueryClass;
  FStrings:=aStrings;
end;

procedure TSQLDBRestDBHandler.ExecuteOperation;

begin
  if Not DeriveResourceFromDataset then
    FResource:=IO.Resource;
  Case IO.Operation of
    roGet : DoHandleGet;
    roPut : DoHandlePut;
    roPatch : DoHandlePatch;
    roPost : DoHandlePost;
    roDelete : DoHandleDelete;
  else
    ;
  end;
end;

function TSQLDBRestDBHandler.GetString(aString: TRestStringProperty): UTF8String;
begin
  Result:=DefaultGetString(FStrings, aString);
end;

class function TSQLDBRestDBHandler.DefaultGetString(aConfig : TRestStringsConfig; aString: TRestStringProperty): UTF8String;
begin
  if Assigned(aConfig) then
    Result:=aConfig.GetRestString(aString)
  else
    Result:=TRestStringsConfig.GetDefaultString(aString);
end;

class procedure TSQLDBRestDBHandler.DefaultParamFromStringAndType(P: TParam; S: UTF8String; aDataType: TFieldType; aStrings : TRestStringsConfig);

var
  F : Double;
  C : Integer;

begin
  Case aDataType of

    ftFmtMemo,
    ftFixedChar,
    ftFixedWideChar,
    ftWideMemo,
    ftMemo,
    ftString : P.AsString:=S;

    ftSmallint : P.AsSmallInt:=StrToInt(S);
    ftInteger : P.AsInteger:=StrToInt(S);
    ftWord : P.AsWord:=StrToInt(S);
    ftLargeint : P.AsLargeInt:=StrToInt64(S);
    ftWideString : P.AsUnicodeString:=UTF8Decode(S);
    ftBoolean : P.AsBoolean:=StrToBool(S);
    ftFloat,
    ftCurrency,
    ftFMTBcd,
    ftBCD :
      begin
      Val(S,F,C);
      if C=0 then
        P.AsFloat:=F
      else
        Raise EConvertError.Create('Invalid float value : '+S);
      end;
    ftDate : P.AsDateTime:=ScanDateTime(DefaultGetString(aStrings, rpDateFormat),S);
    ftTime : P.AsDateTime:=ScanDateTime(DefaultGetString(aStrings, rpDateFormat),S);
    ftTimeStamp,
    ftDateTime : P.AsDateTime:=ScanDateTime(DefaultGetString(aStrings, rpDateTimeFormat),S);
    ftVariant : P.Value:=S;
    ftBytes : P.AsBytes:=TENcoding.UTF8.GetAnsiBytes(S);
    ftVarBytes : P.AsBytes:=TENcoding.UTF8.GetAnsiBytes(S);
    ftBlob : P.AsBytes:=TENcoding.UTF8.GetAnsiBytes(S);
    ftGUID : P.AsString:=S;
  else
    Raise EConvertError.CreateFmt('Unsupported data type: %s',[GetEnumName(TypeInfo(TFieldType),Ord(aDataType))]);
  end;
end;


function TSQLDBRestDBHandler.GetIDWhere(out FilteredFields: TRestFilterPairArray): UTF8String;

Var
  Qry : UTF8String;
  L : TSQLDBRestFieldArray;
  F: TSQLDBRestField;
  I : Integer;

begin
  FilteredFields:=Default(TRestFilterPairArray);
  Result:='';
  if (IO.GetVariable('ID',Qry,[vsQuery,vsRoute,vsHeader])=vsNone) or (Qry='') then
    if not Assigned(PostParams) then
      raise ESQLDBRest.Create(IO.RestStatuses.GetStatusCode(rsInvalidParam),SErrNoKeyParam);
  L:=FResource.GetFieldArray(flWhereKey);
  SetLength(FilteredFields,Length(L));
  for I:=0 to Length(L)-1 do
    begin
    F:=L[i];
    FilteredFields[I].Field:=F;
    FilteredFields[I].Operation:=rfEqual;
    // If we have postparams, it means we're building a dataset for return data.
    // So check for actual DB value there
    if Assigned(PostParams) then
      FilteredFields[I].ValueParam:=PostParams.FindParam(F.FieldName);
    if (FilteredFields[I].ValueParam=nil) then
      FilteredFields[I].Value:=ExtractWord(1,Qry,['|']);
    If (Result<>'') then
      Result:=Result+' and ';
    Result:='('+F.FieldName+' = :'+FilterParamPrefix[rfEqual]+F.FieldName+')';
    end;
end;

function TSQLDBRestDBHandler.GetWhere(out FilteredFields: TRestFilterPairArray
  ): UTF8String;

Const
  MaxFilterCount = 1+ Ord(High(TRestFieldFilter)) - Ord(Low(TRestFieldFilter));

Var
  Qry : UTF8String;
  L : TSQLDBRestFieldArray;
  RF : TSQLDBRestField;
  fo : TRestFieldFilter;
  aLen : integer;

begin
  FilteredFields:=Default(TRestFilterPairArray);
  Result:='';
  L:=FResource.GetFieldArray(flFilter);
  SetLength(FilteredFields,Length(L)*MaxFilterCount);
  aLen:=0;
  for RF in L do
    for FO in RF.Filters do
      if IO.GetFilterVariable(RF.PublicName,FO,Qry)<>vsNone then
        begin
        FilteredFields[aLen].Field:=RF;
        FilteredFields[aLen].Operation:=FO;
        FilteredFields[aLen].Value:=Qry;
        Inc(aLen);
        If (Result<>'') then Result:=Result+' AND ';
        if FO<>rfNull then
          Result:=Result+Format('(%s %s :%s%s)',[RF.FieldName,FilterOps[FO],FilterParamPrefix[FO],RF.FieldName])
        else
          Case IO.StrToNullBoolean(Qry,True) of
            nbTrue : Result:=Result+Format('(%s IS NULL)',[RF.FieldName]);
            nbFalse : Result:=Result+Format('(%s IS NOT NULL)',[RF.FieldName]);
            nbNone :  Raise ESQLDBRest.CreateFmt(IO.RestStatuses.GetStatusCode(rsInvalidParam),SErrInvalidBooleanForField,[RF.PublicName])
          end;
        end;
  SetLength(FilteredFields,aLen);
end;

function TSQLDBRestDBHandler.GetOrderByFieldArray : TRestFieldOrderPairArray;

  Procedure AddField(Idx : Integer; F : TSQLDBRestField; aDesc : boolean);

  begin
    Result[Idx].RestField:=F;
    Result[Idx].Desc:=aDesc;
  end;

Var
  L : TSQLDBRestFieldArray;
  I,J,aLen : Integer;
  F : TSQLDBRestField;
  V,FN : UTF8String;
  Desc : Boolean;

begin
  Result:=Default(TRestFieldOrderPairArray);
  if IO.GetVariable(GetString(rpOrderBy),V,[vsQuery])=vsNone then
    begin
    L:=FResource.GetFieldArray(flWhereKey);
    SetLength(Result,Length(L));
    I:=0;
    For F in L do
      begin
      AddField(I,F,False);
      Inc(I);
      end
    end
  else
    begin
    L:=FResource.GetFieldArray(flOrderBy);
    aLen:=WordCount(V,[',']);
    SetLength(Result,aLen);
    For I:=1 to WordCount(V,[',']) do
      begin
      FN:=ExtractWord(I,V,[',']);
      Desc:=SameText(ExtractWord(2,FN,[' ']),'desc');
      FN:=ExtractWord(1,FN,[' ']);
      J:=Length(L)-1;
      While (J>=0) and Not SameText(L[J].PublicName,FN) do
        Dec(J);
      if J<0 then
        Raise ESQLDBRest.CreateFmt(IO.RestStatuses.GetStatusCode(rsInvalidParam),SErrInvalidSortField,[FN]);
      F:=L[J];
      if Desc then
        if not (foOrderByDesc in F.Options) then
          Raise ESQLDBRest.CreateFmt(IO.RestStatuses.GetStatusCode(rsInvalidParam),SErrInvalidSortDescField,[FN]);
      AddField(I-1,F,Desc)
      end;
    end;
end;

function TSQLDBRestDBHandler.GetOrderBy: UTF8String;

Const
  AscDesc : Array[Boolean] of string = ('ASC','DESC');

Var
  L : TRestFieldOrderPairArray;
  P : TRestFieldOrderPair;

begin
  Result:='';
  L:=GetOrderByFieldArray;
  For P in L do
    begin
    if Result<>'' then
      Result:=Result+', ';
    Result:=Result+P.RestField.FieldName+' '+AscDesc[P.Desc];
    end;
end;

function TSQLDBRestDBHandler.CreateQuery(const aSQL: String): TSQLQuery;

begin
  Result:=FQueryClass.Create(Self);
  Result.DataBase:=IO.Connection;
  Result.Transaction:=IO.Transaction;
  Result.SQL.Text:=aSQL;
end;

function TSQLDBRestDBHandler.BuildFieldList(ForceAll : Boolean): TRestFieldPairArray;

Var
  L : TSQLDBRestFieldArray;
  F : TSQLDBRestField;
  aCount : Integer;
  Fi,Fe : TStrings;

  Function ML(const N : String) : TStrings;
  Var
    V : UTF8String;
  begin
    Result:=Nil;
    if ForceAll then
      exit;
    IO.GetVariable(N,V);
    if (V<>'') then
      begin
      Result:=TStringList.Create;
      Result.StrictDelimiter:=True;
      Result.CommaText:=V;
      end;
  end;

  Function IsIncluded(F : TSQLDBRestField) : Boolean;
  begin
    Result:=(FI=Nil) or (FI.IndexOf(F.PublicName)<>-1)
  end;

  Function IsExcluded(F : TSQLDBRestField) : Boolean;
  begin
    Result:=(FE<>Nil) and (FE.IndexOf(F.PublicName)<>-1)
  end;

begin
  Result:=Default(TRestFieldPairArray);
  if Not Assigned(FResource) then
    exit;
  FE:=Nil;
  FI:=ML(GetString(rpFieldList));
  try
    FE:=ML(GetString(rpExcludeFieldList));
    L:=FResource.GetFieldArray(flSelect);
    SetLength(Result,Length(L));
    aCount:=0;
    For F in L do
      if IsIncluded(F) and not IsExcluded(F) then
        begin
        Result[aCount].RestField:=F;
        Result[aCount].DBField:=Nil;
        Inc(aCount);
        end;
     SetLength(Result,aCount);
  finally
    FI.Free;
    FE.Free;
  end;
end;

function TSQLDBRestDBHandler.GetDataForParam(P: TParam; F: TSQLDBRestField;
  Sources: TVariableSources): TJSONData;

Var
  vs : TVariableSource;
  S,N : UTF8String;
  RP: TSQLDBRestParam;

begin
  Result:=Nil;
  if Assigned(F) then
    begin
    N:=F.PublicName;
    vs:=IO.GetVariable(N,S,Sources);
    if (vs<>vsNone) then
      Result:=TJSONString.Create(S)
    else if (vsContent in Sources) then
      Result:=IO.GetContentField(N);
    end;
  If (Result=Nil) then
    begin
    N:=P.Name;
    if N='ID_' then
      N:='ID';
    vs:=IO.GetVariable(N,S);
    if (vs<>vsNone) then
      Result:=TJSONString.Create(S)
    else if (vsContent in Sources) then
      Result:=IO.GetContentField(N)
    else if vsParam in Sources then
      begin
      RP:=FResource.Parameters.Find(N);
      if Assigned(RP) and (RP.DefaultValue<>'') then
        Result:=TJSONString.Create(RP.DefaultValue)
      end;
    end;
end;

procedure TSQLDBRestDBHandler.SetParamFromStringAndType(P : TParam; S : UTF8String; aDataType: TFieldType);

begin
  DefaultParamFromStringAndType(P,S,aDataType,FStrings);
end;

procedure TSQLDBRestDBHandler.SetParamFromData(P: TParam; F: TSQLDBRestField;
  D: TJSONData);

  Procedure OtherParamValue(const S,N : String);

  var
    RP : TSQLDBRestParam;
  begin
    RP:=Self.FResource.Parameters.Find(N);
    if assigned(RP) then
      SetParamFromStringAndType(P,S,RP.DataType)
    else
      P.asString:=S;
  end;

Var
  S : UTF8String;
  N : String;

begin
  N:=P.Name;
  if Assigned(D) and not ((D.JSONType in StructuredJSONTypes) or D.IsNull) then
    S:=D.AsString;
  if (not Assigned(D)) or D.IsNull then
    P.Clear
  else if Assigned(F) then
    Case F.FieldType of
      rftInteger : P.AsInteger:=D.AsInteger;
      rftLargeInt : P.AsLargeInt:=D.AsInt64;
      rftFloat : P.AsFloat:=D.AsFloat;
      rftDate : P.AsDateTime:=ScanDateTime(GetString(rpDateFormat),S);
      rftTime : P.AsDateTime:=ScanDateTime(GetString(rpTimeFormat),S);
      rftDateTime : P.AsDateTime:=ScanDateTime(GetString(rpDateTimeFormat),S);
      rftString : P.AsString:=S;
      rftBoolean : P.AsBoolean:=D.AsBoolean;
      rftBlob :
{$IFNDEF VER3_0}
         P.AsBlob:=BytesOf(DecodeStringBase64(S));
{$ELSE}
         P.AsBlob:=DecodeStringBase64(S);
{$ENDIF}
    else
      OtherParamValue(S,N);
    end
  else
    OtherParamValue(S,N);
end;

function TSQLDBRestDBHandler.FindFieldForParam(aOperation: TRestOperation;
  P: TParam): TSQLDBRestField;

Var
  N : UTF8String;
  A : TSQLDBRestFieldArray;

begin
  Result:=Nil;
  N:=P.Name;
  Result:=FResource.Fields.FindByFieldName(N);
  if (Result=Nil) and (N='ID') then
    begin
    A:=FResource.GetFieldArray(flWhereKey);
    if (Length(A)=1) then
      Result:=A[0];
    end;
end;

procedure TSQLDBRestDBHandler.FillParams(aOperation : TRestOperation; aParams: TParams;FilteredFields : TRestFilterPairArray);

Var
  I : Integer;
  P : TParam;
  D : TJSONData;
  F : TSQLDBRestField;
  FF : TRestFilterPair;
  Sources : TVariableSources;


begin
  // Fill known params
  for FF in FilteredFields do
    begin
    F:=FF.Field;
    if FF.Operation<>rfNull then
      begin
      P:=aParams.FindParam(FilterParamPrefix[FF.Operation]+F.FieldName);
      // If there is no %where% macro, the parameter can be absent
      if Assigned(P) then
        begin
        if Assigned(FF.ValueParam) then
          P.Value:=FF.ValueParam.Value
        else
          begin
          D:=TJSONString.Create(FF.Value);
          try
            SetParamFromData(P,F,D)
          finally
            D.Free;
          end;
          end;
        end;
      end;
    end;
  // Fill in remaining params. Determine source
  case aOperation of
    roGet : Sources:=[vsQuery,vsRoute,vsParam];
    roPost,
    roPatch,
    roPut : Sources:=[vsQuery,vsContent,vsRoute];
    roDelete : Sources:=[vsQuery,vsRoute];
  else
    Sources:=AllVariableSources;
  end;
  For I:=0 to aParams.Count-1 do
    begin
    P:=aParams[i];
    if P.IsNull then
      try
        D:=Nil;
        F:=FindFieldForParam(aOperation,P);
        D:=GetDataForParam(P,F,Sources);
        if (D<>Nil) then
          SetParamFromData(P,F,D)
        else if (aOperation in [roDelete]) then
          Raise ESQLDBRest.CreateFmt(IO.RestStatuses.GetStatusCode(rsInvalidParam),SErrMissingParameter,[P.Name])
        else
          P.Clear;
      finally
        FreeAndNil(D);
      end;
    end;
end;

function TSQLDBRestDBHandler.GetLimitOffset(out aLimit, aOffset: Int64
  ): Boolean;

begin
  Result:=IO.GetLimitOffset(EnforceLimit,aLimit,aoffset);
end;

function TSQLDBRestDBHandler.GetLimit: UTF8String;

var
  aOffset, aLimit : Int64;
  CT : String;

begin
  Result:='';
  GetLimitOffset(aLimit,aOffset);
  if aLimit=0 then
    exit;
  if Not (IO.Connection is TSQLConnector) then
    Raise ESQLDBRest.Create(IO.RestStatuses.GetStatusCode(rsError),SErrLimitNotSupported);
  CT:=lowerCase(TSQLConnector(IO.Connection).ConnectorType);
  if Copy(CT,1,5)='mysql' then
    CT:='mysql';
  case CT of
    'mysql' : Result:=Format('LIMIT %d, %d',[aOffset,aLimit]);
    'postgresql',
    'sqlite3' : Result:=Format('LIMIT %d offset %d',[aLimit,aOffset]);
    'interbase',
    'firebird' : Result:=Format('ROWS %d TO %d',[aOffset,aOffset+aLimit-1]);
    'oracle',
    'sybase',
    'odbc',
    'MSSQLServer' : Result:=Format('OFFSET %d ROWS FETCH NEXT %d ROWS ONLY',[aOffset,aLimit]);
  end;
end;


function TSQLDBRestDBHandler.StreamRecord(O: TRestOutputStreamer; D: TDataset;
  FieldList: TRestFieldPairArray): Boolean;

Var
  i : Integer;

begin
  Result:=IO.Resource.AllowRecord(IO.RestContext,D);
  if not Result then
    exit;
  O.StartRow;
  For I:=0 to Length(FieldList)-1 do
    O.WriteField(FieldList[i]);
  O.EndRow;
end;

function TSQLDBRestDBHandler.StreamDataset(O: TRestOutputStreamer; D: TDataset;
  FieldList: TRestFieldPairArray; CurrentOnly : Boolean = False): Int64;

Var
  aLimit,aOffset : Int64;

  Function LimitReached : boolean;

  begin
    Result:=EmulateOffsetLimit and (aLimit<=0);
  end;

Var
  I : Integer;

begin
  Result:=0;
  if EmulateOffsetLimit then
    GetLimitOffset(aLimit,aOffset)
  else
    begin
    aLimit:=0;
    aOffset:=0;
    end;
  For I:=0 to Length(FieldList)-1 do
    FieldList[i].DBField:=D.FieldByName(FieldList[i].RestField.FieldName);
  if O.HasOption(ooMetadata) then
    O.WriteMetadata(FieldList);
  O.StartData;
  if CurrentOnly then
    StreamRecord(O,D,FieldList)
  else
    begin
    if EmulateOffsetLimit then
      While (aOffset>0) and not D.EOF do
        begin
        D.Next;
        Dec(aOffset);
        end;
    While not (D.EOF or LimitReached) do
      begin
      If StreamRecord(O,D,FieldList) then
        begin
        Dec(aLimit);
        inc(Result);
        end;
      D.Next;
      end;
    end;
  O.EndData;
end;

function TSQLDBRestDBHandler.GetSpecialDatasetForResource(
  aFieldList: TRestFieldPairArray): TDataset;


Var
  aLimit,aOffset : Int64;

begin
  Result:=ExternalDataset;
  if (Result=Nil) then
    begin
    GetLimitOffset(aLimit,aOffset);
    Result:=FResource.GetDataset(IO.RestContext,aFieldList,GetOrderByFieldArray,aLimit,aOffset);
    end;
end;

procedure TSQLDBRestDBHandler.SetExternalDataset(AValue: TDataset);
begin
  if FExternalDataset=AValue then Exit;
  if Assigned(FExternalDataset) then
    FExternalDataset.RemoveFreeNotification(Self);
  FExternalDataset:=AValue;
  if Assigned(FExternalDataset) then
    FExternalDataset.FreeNotification(Self);
end;

procedure TSQLDBRestDBHandler.CreateUpdatedData(aSrc: TDataset);

begin
  if not Assigned(FUpdatedData) then
    Exit;
  aSrc.First;
  FUpdatedData.CopyFromDataset(aSrc,True);
  FUpdatedData.First;
  aSrc.First;
end;

function TSQLDBRestDBHandler.SpecialResource: Boolean;

begin
  Result:=(ExternalDataset<>Nil) or Assigned(FResource.OnGetDataset);
end;

function TSQLDBRestDBHandler.GetDatasetForResource(aFieldList: TRestFieldPairArray; Singleton : Boolean): TDataset;

Var
  aWhere,aOrderby,aLimit,SQL : UTF8String;
  Q : TSQLQuery;
  WhereFilterList : TRestFilterPairArray;

begin
  if SpecialResource then
    Exit(GetSpecialDatasetForResource(aFieldList));
  if Singleton then
    aWhere:=GetIDWhere(WhereFilterList)
  else
    aWhere:=GetWhere(WhereFilterList);
  aWhere:=IO.Resource.DoCompleteWhere(IO.RestContext,skSelect,aWhere);
  aOrderBy:=GetOrderBy;
  aLimit:=GetLimit;
  SQL:=FResource.GetResolvedSQl(skSelect,aWhere,aOrderBy,aLimit);
  Q:=CreateQuery(SQL);
  Try
    Q.UsePrimaryKeyAsKey:=False;
    FillParams(roGet,Q.Params,WhereFilterList);
    if Not SpecialResource then
      IO.Resource.CheckParams(IO.RestContext,roGet,Q.Params);
    Result:=Q;
  except
    Q.Free;
    raise;
  end;
end;

procedure TSQLDBRestDBHandler.CreateResourceFromDataset(D : TDataset);

begin
  FOwnsResource:=True;
  FResource:=TCustomViewResource.Create(Nil);
  FResource.PopulateFieldsFromFieldDefs(D.FieldDefs,Nil,Nil,[]);
end;

procedure TSQLDBRestDBHandler.DoNotFound;

begin
  IO.Response.Code:=IO.RestStatuses.GetStatusCode(rsRecordNotFound);
  IO.Response.CodeText:='NOT FOUND';  // Do not localize
  IO.CreateErrorResponse;
end;

procedure TSQLDBRestDBHandler.DoHandleGet;

Var
  D : TDataset;
  FieldList : TRestFieldPairArray;
  qID : UTF8string;
  Single : Boolean;

begin
  FieldList:=BuildFieldList(False);
  Single:=(IO.GetVariable('ID',qId,[vsRoute,vsQuery])<>vsNone);
  D:=GetDatasetForResource(FieldList,Single);
  try
    D.Open;
    if DeriveResourceFromDataset then
      begin
      CreateResourceFromDataset(D);
      FieldList:=BuildFieldList(False);
      end;
    if not (D.EOF and D.BOF) then
      StreamDataset(IO.RESTOutput,D,FieldList)
    else
      begin
      if Single and not (rhoSingleEmptyOK in Self.Options) then
        DoNotFound
      else
        StreamDataset(IO.RESTOutput,D,FieldList)
      end;
  finally
    D.Free;
  end;
end;

function TSQLDBRestDBHandler.GetGeneratorValue(const aGeneratorName: String
  ): Int64;

begin
{$IFDEF VER3_0_4}
  // The 'get next value' SQL in 3.0.4 is wrong, so we need to do this sep
  if (IO.Connection is TSQLConnector) and SameText((IO.Connection as TSQLConnector).ConnectorType,'Sqlite3') then
    begin
    With CreateQuery('SELECT seq+1 FROM sqlite_sequence WHERE name=:aName') do
      Try
        ParamByName('aName').AsString:=aGeneratorName;
        Open;
        if (EOF and BOF) then
          DatabaseErrorFmt('Generator %s does not exist',[aGeneratorName]);
        Result:=Fields[0].asLargeint;
      Finally
        Free;
      end;
    end
  else
{$ENDIF}
  Result:=IO.Connection.GetNextValue(aGeneratorName,1);
end;

procedure TSQLDBRestDBHandler.SetPostFields(aFields : TFields);

Var
  I : Integer;
  FData : TField;
  D : TJSONData;
  RF : TSQLDBRestField;
  V : UTF8string;

begin
  // Another approach would be to create params for all fields,
  // call setPostParams, and copy field data from all set params
  // That would allow the use of checkparams...
  For I:=0 to aFields.Count-1 do
    try
      D:=Nil;
      FData:=aFields[i];
      RF:=FResource.Fields.FindByFieldName(FData.FieldName);
      if (RF<>Nil) then
        begin
        if (RF.GeneratorName<>'')  then // Only when doing POST
          D:=TJSONInt64Number.Create(GetGeneratorValue(RF.GeneratorName))
        else
          D:=IO.GetContentField(RF.PublicName);
        end
      else if IO.GetVariable(FData.Name,V,[vsContent,vsQuery])<>vsNone then
        D:=TJSONString.Create(V);
      if (D<>Nil) then
        SetFieldFromData(FData,RF,D); // Use new value, if any
    finally
      D.Free;
    end;
end;

procedure TSQLDBRestDBHandler.SetFieldFromData(DataField: TField; ResField: TSQLDBRestField; D: TJSONData);

begin
  if not Assigned(D) then
    DataField.Clear
  else if Assigned(ResField) then
    Case ResField.FieldType of
      rftInteger : DataField.AsInteger:=D.AsInteger;
      rftLargeInt : DataField.AsLargeInt:=D.AsInt64;
      rftFloat : DataField.AsFloat:=D.AsFloat;
      rftDate : DataField.AsDateTime:=ScanDateTime(GetString(rpDateFormat),D.AsString);
      rftTime : DataField.AsDateTime:=ScanDateTime(GetString(rpTimeFormat),D.AsString);
      rftDateTime : DataField.AsDateTime:=ScanDateTime(GetString(rpDateTimeFormat),D.AsString);
      rftString : DataField.AsString:=D.AsString;
      rftBoolean : DataField.AsBoolean:=D.AsBoolean;
      rftBlob :
{$IFNDEF VER3_0}
         DataField.AsBytes:=BytesOf(DecodeStringBase64(D.AsString));
{$ELSE}
         DataField.AsString:=DecodeStringBase64(D.AsString);
{$ENDIF}
    else
      DataField.AsString:=D.AsString;
    end
  else
    DataField.AsString:=D.AsString;
end;


procedure TSQLDBRestDBHandler.SetPostParams(aParams : TParams; Old : TFields = Nil);

Var
  I : Integer;
  P : TParam;
  D : TJSONData;
  F : TSQLDBRestField;
  FOld : TField;
  V : UTF8string;

begin
  For I:=0 to aParams.Count-1 do
    try
      D:=Nil;
      FOld:=Nil;
      P:=aParams[i];
      F:=FResource.Fields.FindByFieldName(P.Name);
      If Assigned(Old) then
        Fold:=Old.FindField(P.Name);
      if (F<>Nil) then
        begin
        if (F.GeneratorName<>'') and (Old=Nil) then // Only when doing POST
          D:=TJSONInt64Number.Create(GetGeneratorValue(F.GeneratorName))
        else
          D:=IO.GetContentField(F.PublicName);
        end
      else if IO.GetVariable(P.Name,V,[vsContent,vsQuery])<>vsNone then
        D:=TJSONString.Create(V);
      if (D=Nil) and Assigned(Fold) then
        begin
{$IFDEF VER3_2_2}
        // ftLargeInt is missing
        if Fold.DataType=ftLargeInt then
          P.AsLargeInt:=FOld.AsLargeInt
        else
{$ENDIF}
          P.AssignFromField(Fold) // use old value
        end
      else
        SetParamFromData(P,F,D); // Use new value, if any
    finally
      D.Free;
    end;
  // Give user a chance to look at it.
  FResource.CheckParams(io.RestContext,roPost,aParams);
  // Save so it can be used in GetWHereID for return
  FPostParams:=TParams.Create(TParam);
  FPostParams.Assign(aParams);
end;

procedure TSQLDBRestDBHandler.InsertNewRecord;

Var
  S : TSQLStatement;
  SQL : UTF8String;

begin
  if Assigned(ExternalDataset) then
    begin
    ExternalDataset.Append;
    SetPostFields(ExternalDataset.Fields);
    try
      ExternalDataset.Post;
    except
      ExternalDataset.Cancel;
      Raise;
    end
    end
  else
    begin
    SQL:=FResource.GetResolvedSQl(skInsert,'','','');
    S:=TSQLStatement.Create(Self);
    try
      S.Database:=IO.Connection;
      S.Transaction:=IO.Transaction;
      S.SQL.Text:=SQL;
      SetPostParams(S.Params);
      S.Execute;
      PostParams.Assign(S.Params);
      S.Transaction.Commit;
    Finally
      S.Free;
    end;
    end;
end;

procedure TSQLDBRestDBHandler.DoHandlePost;

Var
  D : TDataset;
  FieldList : TRestFieldPairArray;

begin
  // We do this first, so we don't run any unnecessary queries
  if not IO.RESTInput.SelectObject(0) then
    raise ESQLDBRest.Create(IO.RestStatuses.GetStatusCode(rsInvalidParam), SErrNoResourceDataFound);
  InsertNewRecord;
  // Now build response. We can imagine not doing a select again, and simply supply back the fields as sent...
  FieldList:=BuildFieldList(False);
  D:=GetDatasetForResource(FieldList,True);
  try
    D.Open;
    IO.RESTOutput.OutputOptions:=IO.RESTOutput.OutputOptions-[ooMetadata];
    CreateUpdatedData(D);
    StreamDataset(IO.RESTOutput,D,FieldList);
  finally
    D.Free;
  end;
  if Assigned(UpdatedData) then
    UpdatedData.First;
end;

procedure TSQLDBRestDBHandler.DoHandlePutPatch(IsPatch: Boolean);

Var
  D : TDataset;
  FieldList : TRestFieldPairArray;

begin
  // We do this first, so we don't run any unnecessary queries
  if not IO.RESTInput.SelectObject(0) then
    Raise ESQLDBRest.Create(IO.RestStatuses.GetStatusCode(rsInvalidParam),SErrNoResourceDataFound);
  // Get the original record.
  FieldList:=BuildFieldList(True);
  D:=GetDatasetForResource(FieldList,True);
  try
    if not FindExistingRecord(D) then
      begin
      DoNotFound;
      exit;
      end;
    UpdateExistingRecord(D,IsPatch);
    // Now build response
    if D<>ExternalDataset then
      begin;
      // Now build response. We can imagine not doing a select again, and simply supply back the fields as sent...
      FreeAndNil(D);
      D:=GetDatasetForResource(FieldList,True);
      FieldList:=BuildFieldList(False);
      D.Open;
      end;
    IO.RESTOutput.OutputOptions:=IO.RESTOutput.OutputOptions-[ooMetadata];
    CreateUpdatedData(D);
    StreamDataset(IO.RESTOutput,D,FieldList);
  finally
    D.Free;
  end;
  if Assigned(UpdatedData) then
    UpdatedData.First;
end;


function TSQLDBRestDBHandler.GetRequestFields : TSQLDBRestFieldArray;

Var
  F : TSQLDBRestField;
  aSize : Integer;

begin
  Result:=[];
  SetLength(Result,FResource.Fields.Count);
  aSize:=0;
  For F in FResource.Fields do
    if FRestIO.RESTInput.HaveInputData(F.PublicName) then
      begin
      Result[aSize]:=F;
      Inc(aSize);
      end;
  SetLength(Result,aSize);
end;


procedure TSQLDBRestDBHandler.CheckAllRequiredFieldsPresent;

Var
  F : TSQLDBRestField;
  Missing : UTF8String;

begin
  Missing:='';
  For F in FResource.Fields do
    if (foRequired in F.Options) and (F.GeneratorName='') then
      if not IO.RESTInput.HaveInputData(F.PublicName) then
        begin
        if Missing<>'' then
          Missing:=Missing+', ';
        Missing:=Missing+F.PublicName;
        end;
  if Missing<>'' then
    Raise ESQLDBRest.CreateFmt(500,SErrMissingInputFields,[Missing]);
end;

function TSQLDBRestDBHandler.GetAllowMultiUpdate: Boolean;
begin
  Result:=rhoAllowMultiUpdate in Options;
end;

function TSQLDBRestDBHandler.GetCheckUpdateCount: Boolean;
begin
  Result:=rhoCheckupdateCount in Options;
end;

function TSQLDBRestDBHandler.GetUseLegacyPUT: Boolean;
begin
  Result:=rhoLegacyPut in Options;
end;

procedure TSQLDBRestDBHandler.UpdateExistingRecord(OldData: TDataset;
  IsPatch: Boolean);

const
  putpatch : Array [Boolean] of TRestOperation = (roPut,roPatch);

Var
  S : TSQLQuery;
  aRowsAffected: Integer;
  SQl : UTF8String;
  aWhere : UTF8String;
  WhereFilterList : TRestFilterPairArray;
  RequestFields : TSQLDBRestFieldArray;


begin
  if (OldData=ExternalDataset) then
    begin
    ExternalDataset.Edit;
    try
      SetPostFields(ExternalDataset.Fields);
      ExternalDataset.Post;
    except
      ExternalDataset.Cancel;
      Raise;
    end
    end
  else
    begin
    if isPatch then
      RequestFields:=GetRequestFields
    else if not (isPatch or UseLegacyPUT) then
      begin
      CheckAllRequiredFieldsPresent;
      RequestFields:=[];
      end;
    S:=TSQLQuery.Create(Self);
    try
      aWhere:=GetIDWhere(WhereFilterList);
      aWhere:=IO.Resource.DoCompleteWhere(IO.RestContext,skUpdate,aWhere);
      SQL:=FResource.GetResolvedSQl(skUpdate,aWhere ,'','',RequestFields);
      S.Database:=IO.Connection;
      S.Transaction:=IO.Transaction;
      S.SQL.Text:=SQL;
      if (not isPatch) and UseLegacyPUT then
        SetPostParams(S.Params,OldData.Fields);
      FillParams(PutPatch[isPatch],S.Params,WhereFilterList);
      // Give user a chance to look at it.
      FResource.CheckParams(io.RestContext,PutPatch[IsPatch],S.Params);
      S.ExecSQL;
      if CheckUpdateCount then
        begin
        aRowsAffected:=S.RowsAffected;
        if (aRowsAffected<1) then
          Raise ESQLDBRest.Create(500,SErrNoRecordsUpdated);
        if (aRowsAffected>1) and not AllowMultiUpdate then
          Raise ESQLDBRest.CreateFmt(500,SErrTooManyRecordsUpdated,[aRowsAffected]);
        end;
      S.SQLTransaction.Commit;
    finally
      S.Free;
    end;
    end;
end;

function TSQLDBRestDBHandler.FindExistingRecord(D: TDataset): Boolean;

Var
  KeyFields : String;
  FieldList : TRestFilterPairArray;
  FP : TRestFilterPair;
  V : Variant;
  I : Integer;

begin
  D.Open;
  if D<>ExternalDataset then
    Result:=Not (D.BOF and D.EOF)
  else
    begin
    GetIDWhere(FieldList);
    V:=VarArrayCreate([0,Length(FieldList)-1],varVariant);
    KeyFields:='';
    I:=0;
    For FP in FieldList do
      begin
      if KeyFields<>'' then
        KeyFields:=KeyFields+';';
      KeyFields:=KeyFields+FP.Field.FieldName;
      if Assigned(FP.ValueParam) then
        V[i]:=FP.ValueParam.Value
      else
        V[i]:=FP.Value;
      Inc(i);
      end;
    Result:=D.Locate(KeyFields,V,[loCaseInsensitive]);
    end;
end;

procedure TSQLDBRestDBHandler.DoHandlePut;

begin
  DoHandlePutPatch(False);
end;



procedure TSQLDBRestDBHandler.DoHandlePatch;
begin
  DoHandlePutPatch(True);
end;

destructor TSQLDBRestDBHandler.Destroy;
begin
  if Assigned(FUpdatedData) and (FUpdatedData.Owner=Self) then
    FreeAndNil(FUpdatedData);
  FreeAndNil(FPostParams);
  If FOwnsResource then
     FreeAndNil(FResource);
  inherited Destroy;
end;

procedure TSQLDBRestDBHandler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  If Operation=opRemove then
    begin
    if (aComponent=FExternalDataset) then
      FExternalDataset:=Nil;
    end;
end;

procedure TSQLDBRestDBHandler.DoHandleDelete;

Var
  aWhere,SQL : UTF8String;
  Q : TSQLQuery;
  FilteredFields : TRestFilterPairArray;

begin
  if Assigned(ExternalDataset) then
    begin
    If FindExistingRecord(ExternalDataset) then
      ExternalDataset.Delete
    else
      DoNotFound;
    end
  else
    begin
    aWhere:=GetIDWhere(FilteredFields);
    aWhere:=IO.Resource.DoCompleteWhere(IO.RestContext,skDelete,aWhere);
    SQL:=FResource.GetResolvedSQl(skDelete,aWhere,'');
    Q:=CreateQuery(SQL);
    try
      FillParams(roDelete,Q.Params,FilteredFields);
      Q.ExecSQL;
      if Q.RowsAffected<>1 then
        DoNotFound;
    finally
      Q.Free;
    end;
    end;
end;

end.

