unit extjsjson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, fphttp,fpwebdata, fpextjs, fpjson, db, jsonparser;

type

  { TExtJSJSonWebdataInputAdaptor }

  TExtJSJSonWebdataInputAdaptor = CLass(TCustomWebdataInputAdaptor)
  private
    FRows : TJSONArray;
    FCurrentRow : TJSONObject;
    FIDValue : TJSONData;
    FRowIndex : integer;
    function CheckData: Boolean;
  Public
    Function GetNextBatch : Boolean; override;
    Function TryFieldValue(Const AFieldName : String; out AValue : String) : Boolean; override;
    Destructor destroy; override;
  end;
  { TExtJSJSONDataFormatter }

  TExtJSJSONDataFormatter = Class(TExtJSDataFormatter)
  private
    procedure SendSuccess(ResponseContent: TStream; AddIDValue : Boolean = False);
  protected
    Function CreateAdaptor(ARequest : TRequest) : TCustomWebdataInputAdaptor; override;
    Function AddFieldToJSON(O: TJSONObject; AFieldName: String; F: TField): TJSONData;
    function GetDataContentType: String; override;
    Function GetJSONMetaData: TJSONObject;
    function RowToJSON: TJSONObject;
    procedure DatasetToStream(Stream: TStream); override;
    Procedure DoExceptionToStream(E : Exception; ResponseContent : TStream); override;
    Procedure DoInsertRecord(ResponseContent : TStream); override;
    Procedure DoUpdateRecord(ResponseContent : TStream); override;
    Procedure DoDeleteRecord(ResponseContent : TStream); override;
  end;

implementation
{$define wmdebug}
{$ifdef wmdebug}
uses dbugintf;
{$endif wmdebug}

Resourcestring
  SErrWrongDataFormat = 'Post ROWS data has wrong value type. Expected array or object, got : %s.';
  SerrNoExceptionMessage = 'No exception to take error message from.';

Const
  // Do not localize these strings
  SDefMetaDataProperty  = 'metaData';
  SDefFieldsProperty    = 'fields';
  SDefFieldProperty     = 'field';
  SDefFieldNameProperty = 'name';
  SDefDirectionProperty = 'direction';
  SDefSortInfoProperty  = 'sortInfo';
  SIdProperty           = 'idProperty';
  SSuccessProperty      = 'successProperty';
  SRootProperty         = 'root';
  STotalProperty        = 'totalProperty';
  SDefAscDesc : Array[Boolean] of string = ('ASC','DESC');

function TExtJSJSONDataFormatter.GetDataContentType: String;
begin
  Result:='text/html';
end;

function TExtJSJSONDataFormatter.CreateAdaptor(ARequest: TRequest
  ): TCustomWebdataInputAdaptor;
begin
  Result:=TExtJSJSonWebdataInputAdaptor.Create(Self);
  Result.Request:=ARequest;
end;

function TExtJSJSONDataFormatter.AddFieldToJSON(O : TJSONObject; AFieldName : String; F : TField): TJSONData;

begin
  Case F.DataType of
    ftSmallint,
    ftInteger,
    ftAutoInc,
    ftWord:
      Result:=O.Items[O.Add(AFieldName,F.AsInteger)];
    ftBoolean:
      Result:=O.Items[O.Add(AFieldName,F.AsBoolean)];
    ftLargeint:
      Result:=O.Items[O.Add(AFieldName,F.AsLargeInt)];
    ftDate:
      Result:=O.Items[O.Add(AFieldName,FormatDateTime('yyyy-mm-dd',F.AsDateTime))];
    ftDateTime:
      Result:=O.Items[O.Add(AFieldName,FormatDateTime('yyyy-mm-dd hh":"nn":"ss',F.AsDateTime))];
    ftTime:
      Result:=O.Items[O.Add(AFieldName,FormatDateTime('hh":"nn":"ss',F.AsDateTime))];
    ftMemo,
    ftFmtMemo,
    ftWideMemo,
    ftBlob :O.Items[O.Add(AFieldName,F.AsString)];
  else
    Result:=O.Items[O.Add(AFieldName,F.DisplayText)];
  end;
end;

function TExtJSJSONDataFormatter.RowToJSON: TJSONObject;

Var
  F : TField;
  I : Integer;


begin
  Result:=TJSONObject.Create();
  For I:=0 to Dataset.Fields.Count-1 do
    begin
    F:=Dataset.Fields[I];
    AddFieldToJSON(Result,F.FieldName,F);
    end;
end;

Function TExtJSJSONDataFormatter.GetJSONMetaData: TJSONObject;

  Function DefReplace(S : String) : String;

  begin
    Result:=StringReplace(Result,'/',DateSeparator,[rfReplaceAll]);
    Result:=StringReplace(Result,':',TimeSeparator,[rfReplaceAll]);
    Result:=StringReplace(Result,'hh','H',[rfReplaceAll]);
    Result:=StringReplace(Result,'nn','i',[rfReplaceAll]);
    Result:=StringReplace(S,'n','i',[rfReplaceAll]);
  end;

Var
  F : TJSONArray;
  Fi : TField;
  I : Integer;
  O : TJSONObject;
  SF,FT : String;

begin
  If (SortField='') then
    SF:=Dataset.Fields[0].FieldName
  else
    SF:=SortField;
  Result:=TJSonObject.Create;
  try
    F:=TJSONArray.Create;
    Result.add(SDefFieldsProperty,F);
    For I:=0 to Dataset.Fields.Count-1 do
      begin
      Fi:=Dataset.Fields[i];
      O:=TJSONObject.Create();
      O.Add(SDefFieldNameProperty,Fi.FieldName);
      Ft:='';
      Case Fi.DataType of
        ftInteger,
        ftSmallint,
        ftWord,
        ftLargeInt : FT:='int';
        ftCurrency,
        ftFloat,
        ftBCD : FT:='float';
        ftBoolean : ft:='boolean';
        ftDate,
        ftDateTime,
        ftTimeStamp,
        ftTime : ft:='date';
        ftString,
        ftMemo,
        ftFmtMemo,
        ftFixedChar,
        ftWideString,
        ftWideMemo : ft:='string'
      end;
      if (FT<>'') then
        begin
        O.Add('type',FT);
        if (FT='date') then
          // Needs improving
          Case Fi.DataType of
            ftDate : O.Add('dateFormat','Y-m-d');
            ftTime : O.Add('dateFormat','h:i:s');
            ftDateTime,
            ftTimeStamp : O.Add('dateFormat','Y-m-d h:i:s');
          end;
        end;
      F.Add(O);
      end;
    O:=TJSONObject.Create();
    O.Add(SDefFieldProperty,SF);
    O.Add(SDefDirectionProperty,SDefAscDesc[SortDescending]);
    Result.Add(SDefSortInfoProperty,O);
 {$ifdef wmdebug}senddebug('ID property: '+Provider.IDFieldName);{$endif}
    Result.Add(SIdProperty,Provider.IDFieldName);
    Result.Add(SSuccessProperty, SuccessProperty);
    Result.Add(SRootProperty, RowsProperty);
    Result.Add(STotalProperty, totalProperty)
  except
    Result.free;
    Raise;
  end;
end;

procedure TExtJSJSONDataFormatter.DatasetToStream(Stream: TStream);

Var
  Rows : TJSONArray;
  Meta,Resp : TJSONObject;
  L : String;
  DS : TDataset;
  i,RCount,ACount : Integer;

begin
  Rows:=Nil;
  Resp:=TJSONObject.Create;
  try
    Rows:=TJSONArray.Create();
    DS:=Dataset;
    DS.First;
    RCount:=0;
    If MetaData then
      begin
      Meta:=GetJSONMetaData;
      Resp.Add(SDefMetaDataProperty,Meta);
      end;
    // Go to start
    ACount:=PageStart;
    While (Not DS.EOF) and (ACount>0) do
      begin
      DS.Next;
      Dec(ACount);
      Inc(RCount);
      end;
    ACount:=PageSize;
    While (not DS.EOF) and ((PageSize=0) or (ACount>0)) do
      begin
      Inc(RCount);
      Dec(ACount);
      Rows.Add(RowToJSON);
      DS.Next;
      end;
    If (PageSize>0) then
      While (not DS.EOF) do
        begin
        Inc(RCount);
        DS.Next;
        end;
    Resp.Add(RowsProperty,Rows);
    Resp.Add(SuccessProperty,True);
    If (PageSize>0) then
       Resp.Add(TotalProperty,RCount);
    L:=Resp.AsJSON;
    Stream.WriteBuffer(L[1],Length(L));
  finally
    Resp.Free;
  end;
end;

procedure TExtJSJSONDataFormatter.DoExceptionToStream(E: Exception;
  ResponseContent: TStream);

Var
   Resp : TJSonObject;
   L : String;

begin
  Resp:=tjsonObject.Create();
  try
    Resp.Add(SuccessProperty,False);
    If Assigned(E) then
      Resp.Add(MessageProperty,E.Message)
    else
      Resp.Add(MessageProperty,SerrNoExceptionMessage);
    L:=Resp.AsJSON;
    If Length(L)>0 then
      ResponseContent.WriteBuffer(L[1],Length(L));
  finally
    Resp.Free;
  end;
end;

procedure TExtJSJSONDataFormatter.SendSuccess(ResponseContent: TStream; AddIDValue : Boolean = False);

Var
   Resp : TJSonObject;
   L : String;

begin
  try
    Resp:=TJsonObject.Create;
    Resp.Add(SuccessProperty,True);
    Resp.Add(Provider.IDFieldName,Provider.IDFieldValue);
    L:=Resp.AsJSON;
    ResponseContent.WriteBuffer(L[1],Length(L));
  finally
    Resp.Free;
  end;
end;

procedure TExtJSJSONDataFormatter.DoInsertRecord(ResponseContent: TStream);

begin
  Inherited;
  SendSuccess(ResponseContent,True);
end;

procedure TExtJSJSONDataFormatter.DoUpdateRecord(ResponseContent: TStream);
begin
  inherited DoUpdateRecord(ResponseContent);
  SendSuccess(ResponseContent,False);
end;

procedure TExtJSJSONDataFormatter.DoDeleteRecord(ResponseContent: TStream);
begin
  inherited DoDeleteRecord(ResponseContent);
  SendSuccess(ResponseContent,False);
end;

{ TExtJSJSonWebdataInputAdaptor }

function TExtJSJSonWebdataInputAdaptor.CheckData : Boolean;

Var
  D : TJSONData;
  P : TJSONParser;
  S : String;

begin
  Result:=Assigned(FCurrentRow);
  If Not (Result) and TryParamValue('rows',S) then
    begin
    {$ifdef wmdebug}senddebug('Check data: '+GetParamValue('rows'));{$endif}
    P:=TJSONParser.Create(S);
    try
      D:=P.Parse;
      {$ifdef wmdebug}senddebug('Classname : '+D.ClassName);{$endif}
      If D is TJSONArray then
        begin
        FRows:=TJSONArray(D);
        FRowIndex:=0;
        FCurrentRow:=FRows.Items[0] as TJSONObject;
        end
      else If D is TJSONObject then
        begin
        FRows:=Nil;
        FCurrentRow:=TJSONObject(D);
        end
      else if D is TJSONInt64Number then
        FIDValue:=D
      else
        begin
        FreeAndNil(D);
        Raise EFPHTTPError.CreateFmt(SErrWrongDataFormat,[D.ClassName]);
        end;
      Result:=True;
    finally
      P.Free;
    end;
    end;
end;

function TExtJSJSonWebdataInputAdaptor.GetNextBatch: Boolean;
begin
  If (FRows=Nil) then
    Result:=inherited GetNextBatch
  else
    begin
    Result:=FRowindex<FRows.Count-1;
    Inc(FRowIndex);
    If Result then
      FCurrentRow:=FRows.Items[FRowIndex] as TJSONObject
    else
      FCurrentRow:=Nil;
    end;
end;

function TExtJSJSonWebdataInputAdaptor.TryFieldValue(const AFieldName: String;
  out AValue: String): Boolean;

Var
  I : Integer;

begin
  Result:=False;
  if CheckData then
    begin
    If Assigned(FIDValue) and (0=CompareText(AFieldName,'ID')) then
      begin
      AValue:=FIDValue.AsString;
      Result:=True;
      end
    else
      begin
      I:=FCurrentRow.IndexOfName(AFieldName);
      Result:=I<>-1;
      if result then
        AValue:=FCurrentRow.Items[I].AsString;
      end;
    end;
end;

destructor TExtJSJSonWebdataInputAdaptor.destroy;
begin
  If Assigned(FRows) then
    FreeAndNil(FRows)
  else if assigned(FCurrentRow) then
    FreeAndNil(FCurrentRow)
  else if Assigned(FIDValue) then
    FreeAndNil(FIDValue);
  inherited destroy;
end;

initialization
  WebDataProviderManager.RegisterInputAdaptor('ExtJS - JSON',TExtJSJSONWebdataInputAdaptor);
  WebDataProviderManager.RegisterDataProducer('ExtJS - JSON',TExtJSJSONDataFormatter);

finalization
  WebDataProviderManager.UnRegisterInputAdaptor('ExtJS - JSON');
  WebDataProviderManager.UnRegisterDataProducer('ExtJS - JSON')
end.

