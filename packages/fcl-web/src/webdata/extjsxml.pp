unit extjsxml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, fpextjs, dom, xmlread, xmlwrite, fpwebdata, db;

Type

  { TExtJSXMLWebdataInputAdaptor }

  TExtJSXMLWebdataInputAdaptor = CLass(TCustomWebdataInputAdaptor)
  private
    FDE: String;
    FRE: String;
    FREEL: String;
    FXML : TXMLDocument;
    FDocRoot : TDOMElement;
    FRoot : TDOMElement;
    FCurrentRow : TDOMElement;
    FIDValue : TDOMElement;
    function isDocumentStored: boolean;
    function IsRecordStored: boolean;
    function isRootStored: boolean;
    function CheckData: Boolean;
  protected
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor destroy; override;
    Function TryFieldValue(Const AFieldName : String; out AValue : String) : Boolean; override;
    Property DocumentElement : String Read FDE Write FDE stored isDocumentStored;
    Property RootElement : String Read FRE Write FRE stored isRootStored;
    Property RecordElement : String Read FREEL Write FREEL stored IsRecordStored;
  end;
  { TExtJSJSONDataFormatter }

  { TExtJSXMLDataFormatter }
  TXMLElementEvent = Procedure (Sender : TObject; AElement : TDOMElement) of object;
  TXMLExceptionObjectEvent = Procedure(Sender : TObject; E : Exception; AResponse : TDOMElement) of Object;

  TExtJSXMLDataFormatter = Class(TExtJSDataFormatter)
  private
    FAfterDataToXML: TXMLElementEvent;
    FAfterRowToXML: TXMLElementEvent;
    FBeforeDataToXML: TXMLElementEvent;
    FBeforeRowToXML: TXMLElementEvent;
    FDP: String;
    FOnErrorResponse: TXmlExceptionObjectEvent;
    FReP: String;
    FRP: String;
    function IsDocumentStored: boolean;
    function IsRecordStored: boolean;
    function IsRootStored: boolean;
  protected
    Function CreateAdaptor(ARequest : TRequest) : TCustomWebdataInputAdaptor; override;
    Procedure DoExceptionToStream(E : Exception; ResponseContent : TStream); override;
    Function GetDataContentType : String; override;
    function RowToXML(Doc: TXMLDocument): TDOMelement;
    Procedure DoBeforeRow(ARow : TDOMElement); virtual;
    Procedure DoAfterRow(ARow : TDOMElement); virtual;
    Procedure DoBeforeData(Data : TDOMElement); virtual;
    Procedure DoAfterData(Data: TDOMElement); virtual;
    procedure DatasetToStream(Stream: TStream); override;
  public
    Constructor Create(AOwner : TComponent); override;
  published
    Property RootProperty : String Read FRP Write FRP Stored IsRootStored;
    Property RecordProperty : String Read FReP Write FReP Stored IsRecordStored;
    Property DocumentProperty : String Read FDP Write FDP Stored IsDocumentStored;
    // Called before row element (passed to handler) is filled with fields.
    Property BeforeRowToXML : TXMLElementEvent Read FBeforeRowToXML Write FBeforeRowToXML;
    // Called after row element (passed to handler) was filled with fields.
    Property AfterRowToXML : TXMLElementEvent Read FAfterRowToXML Write FAfterRowToXML;
    // Called before any rows are added to root element (passed to handler).
    Property BeforeDataToXML : TXMLElementEvent Read FBeforeDataToXML Write FBeforeDataToXML;
    // Called after all rows are appended to root element (passed to handler).
    Property AfterDataToXML : TXMLElementEvent Read FAfterDataToXML Write FAfterDataToXML;
    // Called when an exception is caught and formatted.
    Property OnErrorResponse : TXmlExceptionObjectEvent Read FOnErrorResponse Write FOnErrorResponse;
  end;

implementation
{ $define wmdebug}
{$ifdef wmdebug}
uses dbugintf;
{$endif wmdebug}

Resourcestring
  SerrNoExceptionMessage = 'No exception to take error message from.';

Const
  // For TExtJSXMLDataFormatter.
  SDefDocumentProperty = 'xrequest';
  SDefRecordProperty   = 'row';
  SDefRootProperty     = 'dataset';

  // Fpr TExtJSXMLWebdataInputAdaptor
  SDefRootElement     = SDefRootProperty;
  SDefRecordElement   = SDefRecordProperty;
  SDefDocumentElement = SDefDocumentProperty;

function TExtJSXMLDataFormatter.IsRootStored: boolean;
begin
  Result:=RootProperty<>SDefRootProperty;
end;

function TExtJSXMLDataFormatter.CreateAdaptor(ARequest: TRequest
  ): TCustomWebdataInputAdaptor;

Var
  R : TExtJSXMLWebdataInputAdaptor;

begin
  R:=TExtJSXMLWebdataInputAdaptor.Create(Self);
  R.Request:=ARequest;
  R.DocumentElement:=Self.DocumentProperty;
  R.RootElement:=Self.RootProperty;
  R.RecordElement:=Self.RecordProperty;
  Result:=R;
end;

function TExtJSXMLDataFormatter.IsRecordStored: boolean;
begin
  Result:=RecordProperty<>SDefRecordProperty;
end;

function TExtJSXMLDataFormatter.IsDocumentStored: boolean;
begin
  Result:=DocumentProperty<>SDefDocumentProperty
end;

procedure TExtJSXMLDataFormatter.DoExceptionToStream(E: Exception;
  ResponseContent: TStream);

Var
  Xml : TXMLDocument;
  El,C : TDOMElement;

begin
  XML:=TXMLDocument.Create;
  try
    El:=XML.CreateElement(RootProperty);
    XML.AppendChild(El);
    El[SuccessProperty]:='false';
    C:=XML.CreateElement(SuccessProperty);
    C.AppendChild(XML.CreateTextNode('false'));
    El.AppendChild(c);
    C:=XML.CreateElement(MessageProperty);
    El.AppendChild(C);
    If Assigned(E) then
      C.AppendChild(XML.CreateTextNode(E.Message))
    else
      C.AppendChild(XML.CreateTextNode(SerrNoExceptionMessage));
    If Assigned(FOnErrorResponse) then
      FOnErrorResponse(Self,E,El);
    WriteXMLFile(XML,ResponseContent);
  Finally
    XML.Free;
  end;
end;

function TExtJSXMLDataFormatter.GetDataContentType: String;
begin
  Result:='text/xml';
end;

Function TExtJSXMLDataFormatter.RowToXML(Doc : TXMLDocument) : TDOMelement;

Var
  E : TDOMElement;
  F : TField;
  I : Integer;
  S : String;
begin
  Result:=Doc.CreateElement(RecordProperty);
  try
    DoBeforeRow(Result);
    For I:=0 to Dataset.Fields.Count-1 do
      begin
      F:=Dataset.Fields[i];
      E:=Doc.CreateElement(F.FieldName);
      If F.DataType in [ftMemo, ftFmtMemo, ftWideMemo, ftBlob ] then
        S:=F.AsString
      else
        S:=F.DisplayText;
      If (OnTranscode<>Nil) then
        OnTranscode(Self,F,S,True);
      E.AppendChild(Doc.CreateTextNode(S));
      Result.AppendChild(E);
      end;
    DoAfterRow(Result);
  except
    Result.Free;
    Raise;
  end;
end;

procedure TExtJSXMLDataFormatter.DoBeforeRow(ARow: TDOMElement);
begin
  If Assigned(FBEforeRowToXml) then
    FBEforeRowToXml(Self,ARow);
end;

procedure TExtJSXMLDataFormatter.DoAfterRow(ARow: TDOMElement);
begin
  If Assigned(FAfterRowToXml) then
    FAfterRowToXml(Self,ARow);
end;

procedure TExtJSXMLDataFormatter.DoBeforeData(Data: TDOMElement);
begin
  If Assigned(FBeforeDataToXML) then
    FBeforeDataToXML(Self,Data);
end;

procedure TExtJSXMLDataFormatter.DoAfterDAta(Data: TDOMElement);
begin
  If Assigned(FAfterDataToXML) then
    FAfterDataToXML(Self,Data);
end;

procedure TExtJSXMLDataFormatter.DatasetToStream(Stream: TStream);

Var
  Xml : TXMLDocument;
  E,C : TDOMElement;
  i,RCount,ACount : Integer;
  DS : TDataset;

begin
  RCount:=0;
  ACount:=0;
  DS:=Dataset;
   XML:=TXMLDocument.Create;
   try
     E:=XML.CreateElement(RootProperty);
     XML.AppendChild(E);
     DoBeforeData(E);
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
       E.AppendChild(RowToXML(XML));
       DS.Next;
       end;
     If (PageSize>0) then
       While (not DS.EOF) do
         begin
         Inc(RCount);
         DS.Next;
         end;
     C:=XML.CreateElement(TotalProperty);
     C.AppendChild(XML.CreateTextNode(IntToStr(RCount)));
     E.AppendChild(C);
     C:=XML.CreateElement(SuccessProperty);
     C.AppendChild(XML.CreateTextNode('true'));
     E.AppendChild(C);
     DoAfterData(E);
     WriteXMLFile(XML,Stream);
   finally
     XML.Free;
   end;
end;

constructor TExtJSXMLDataFormatter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RootProperty:=SDefRootProperty;
  RecordProperty:=SDefRecordProperty;
  DocumentProperty:=SDefDocumentProperty
end;

{ TExtJSXMLWebdataInputAdaptor }


function TExtJSXMLWebdataInputAdaptor.isDocumentStored: boolean;
begin
  Result:=DocumentElement<>SDefDocumentElement;
end;

function TExtJSXMLWebdataInputAdaptor.IsRecordStored: boolean;
begin
  Result:=RecordElement<>SDefRecordElement;
end;

function TExtJSXMLWebdataInputAdaptor.isRootStored: boolean;
begin
  Result:=RootElement<>SDefRootElement;
end;

function TExtJSXMLWebdataInputAdaptor.CheckData: Boolean;

Var
  S : String;
  T : TStringSTream;
  E : TDomElement;
  P : Integer;

begin
  {$ifdef wmdebug}senddebug('Check data: '+Request.Content);{$endif}
  Result:=Assigned(FXML);
  If Not (Result)  then
    begin
    S:=Request.ContentType;
    P:=Pos(';',S);
    If (P<>0) then
      S:=Copy(S,1,P-1);
   {$ifdef wmdebug}senddebug('Check data: '+S);{$endif}
    Result:=CompareText(S,'application/x-www-form-urlencoded')=0;
    If not Result then
      begin
      T:=TStringStream.Create(Request.Content);
      try
        XmlRead.ReadXMLFile(FXML,T);
        If (DocumentElement<>'') and (FXML.DocumentElement.NodeName=DocumentElement) then
          begin
          {$ifdef wmdebug}senddebug('Document element is ExtJS DocumentElement');{$endif}
          FDocRoot:=FXML.DocumentElement;
          E:=FDocRoot;
          end
        else if (DocumentElement<>'') then
          begin
          //FXML.
          {$ifdef wmdebug}senddebug('Looking for ExtJS Documentelement "'+DocumentElement+'" in XML.DocumentElement');{$endif}
          FDocRoot:=FXML.DocumentElement.FindNode(DocumentElement) as TDOMElement;
          E:=FDocRoot;
          end;
        {$ifdef wmdebug}senddebug('Looking for DocRoot element "'+RootElement+'" in FDocRoot');{$endif}
        If Assigned(FDocRoot) then
          FRoot:=FDocRoot
        else
          FRoot:=FXML.FindNode(RootElement) as TDomElement;
        {$ifdef wmdebug}senddebug('Looking for current record element "'+RecordElement+'" in FRoot');{$endif}
        If Assigned(FRoot) then
          begin
          FCurrentRow:=FRoot.FindNode(RecordElement) as TDomElement;
          If Not Assigned(FCurrentRow) then
            FIDValue:=FRoot.FindNode('ID') as TDomElement;
          end
        else
          begin
          {$ifdef wmdebug}senddebug('Looking for current record element "'+RecordElement+'" in document');{$endif}
          FCurrentRow:=FXML.FindNode(RecordElement) as TDomElement;
          end;
        If (FCurrentRow=Nil) and (FXML.DocumentElement.NodeName=RecordElement) then
          begin
          {$ifdef wmdebug}senddebug('Documentelement is  record element "'+RecordElement+'"');{$endif}
          FCurrentRow:=FXML.DocumentElement;
          end;
        {$ifdef wmdebug}senddebug('Have current row: "'+IntToStr(Ord(Assigned(FCurrentRow)))+'"');{$endif}
        Result:=True;
      finally
        T.free;
      end;
      end;
    end;

end;

function TExtJSXMLWebdataInputAdaptor.TryFieldValue(const AFieldName: String;
  out AValue: String): Boolean;

Var
  I : Integer;
  E : TDOMElement;
  N : TDOMNode;

begin
  Result:=False;
  if CheckData then
    begin
    If Assigned(FIDValue) and (0=CompareText(AFieldName,'ID')) then
      begin
      AValue:=FIDValue.NodeValue;
      Result:=True;
      end
    else if Assigned(FCurrentRow) then
      begin
      E:=FCurrentRow.FindNode(AFieldName) as TDomElement;
      Result:=Assigned(E);
      if result then
        begin
        N:=E.FirstChild;
        If Assigned(N) then
          AValue:=N.NodeValue;
        end;
      end;
    end;
end;

constructor TExtJSXMLWebdataInputAdaptor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RootElement:=SDefRootElement;
  RecordElement:=SDefRecordElement;
  DocumentElement:=SDefDocumentElement;
end;

destructor TExtJSXMLWebdataInputAdaptor.destroy;
begin
  FreeAndNil(FXML);
  inherited destroy;
end;

initialization
  WebDataProviderManager.RegisterInputAdaptor('ExtJS - XML',TExtJSXMLWebdataInputAdaptor);
  WebDataProviderManager.RegisterDataProducer('ExtJS - XML',TExtJSXMLDataFormatter);

finalization
  WebDataProviderManager.UnRegisterInputAdaptor('ExtJS - XML');
  WebDataProviderManager.UnRegisterDataProducer('ExtJS - XML')
end.

