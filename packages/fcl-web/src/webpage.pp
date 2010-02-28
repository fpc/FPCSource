unit WebPage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphtml, htmlelements, htmlwriter, HTTPDefs, fpweb, contnrs, dom;

type
  TRequestResponseEvent = procedure(Sender: TObject; ARequest: TRequest; AResponse: TResponse) of object;
  TRequestEvent = procedure(Sender: TObject; ARequest: TRequest) of object;
  THandleAjaxRequest = procedure(Sender: TObject; ARequest: TRequest; AnAjaxResponse: TAjaxResponse; var handled: boolean) of object;
  TAjaxRequestResponseEvent = procedure(Sender: TObject; ARequest: TRequest; AResponse: TAjaxResponse) of object;

type
  IWebPageDesigner = interface(IUnknown)
    procedure Invalidate;
  end;

  { TStandardWebController }

  TStandardWebController = class(TWebController)
  private
    FScriptFileReferences: TStringList;
    FCurrentJavascriptStack: TJavaScriptStack;
    FScripts: TFPObjectList;
  protected
    function GetScriptFileReferences: TStringList; override;
    function GetScripts: TFPObjectList; override;
    function GetCurrentJavaScriptStack: TJavaScriptStack; override;
    procedure SetCurrentJavascriptStack(const AJavascriptStack: TJavaScriptStack);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InitializeJavaScriptStack: TJavaScriptStack; override;
    function GetUrl(ParamNames, ParamValues, KeepParams: array of string; Action: string = ''): string; override;
    procedure FreeJavascriptStack; override;
    procedure BindJavascriptCallstackToElement(AComponent: TComponent; AnElement: THtmlCustomElement; AnEvent: string); override;
    procedure AddScriptFileReference(AScriptFile: String); override;
    function DefaultMessageBoxHandler(Sender: TObject; AText: String; Buttons: TWebButtons): string; override;
    function CreateNewScript: TStringList; override;
    procedure FreeScript(var AScript: TStringList); override;
  end;

  { TWebPage }

  TWebPage = class(TDataModule, IHTMLContentProducerContainer)
  private
    FAfterAjaxRequest: TAjaxRequestResponseEvent;
    FBaseURL: string;
    FBeforeRequest: TRequestEvent;
    FBeforeShowPage: TRequestEvent;
    FDesigner: IWebPageDesigner;
    FOnAjaxRequest: THandleAjaxRequest;
    FRequest: TRequest;
    FWebController: TWebController;
    FWebModule: TFPWebModule;
    FContentProducers: TFPList; // list of THTMLContentProducer
    function GetContentProducer(Index: integer): THTMLContentProducer;
    function GetContentProducerList: TFPList;
    function GetContentProducers(Index: integer): THTMLContentProducer;
    function GetHasWebController: boolean;
    function GetWebController: TWebController;
  protected
    procedure DoAfterAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse); virtual;
    procedure DoHandleAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse; var Handled: boolean); virtual;
    procedure DoBeforeRequest(ARequest: TRequest); virtual;
    procedure DoBeforeShowPage(ARequest: TRequest); virtual;
    function IsAjaxCall: boolean; virtual;
    property WebModule: TFPWebModule read FWebModule;
    procedure DoCleanupAfterRequest(const AContentProducer: THTMLContentProducer);
    procedure SetRequest(ARequest: TRequest); virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property ContentProducerList: TFPList read GetContentProducerList;
  public
    function ContentProducerCount: integer;

    function ProduceContent : string;
    procedure AddContentProducer(AContentProducer: THTMLContentProducer);
    procedure RemoveContentProducer(AContentProducer: THTMLContentProducer);
    function ExchangeContentProducers(Child1, Child2: THTMLContentProducer) : boolean;
    function MoveContentProducer(MoveElement, MoveBeforeElement: THTMLContentProducer) : boolean;
    procedure ForeachContentProducer(AForeachChildsProc: TForeachContentProducerProc; Recursive: boolean);

    procedure HandlePage(ARequest: TRequest; AResponse: TResponse; AWriter: THTMLwriter; AWebModule: TFPWebModule = nil); virtual;
    procedure DoBeforeGenerateXML; virtual;
    procedure CleanupAfterRequest; virtual;
    property Designer: IWebPageDesigner read FDesigner write FDesigner;
    property Request: TRequest read FRequest;
    property ContentProducers[Index: integer]: THTMLContentProducer read GetContentProducer;
    property HasWebController: boolean read GetHasWebController;
    property WebController: TWebController read GetWebController write FWebController;
  published
    property BeforeRequest: TRequestEvent read FBeforeRequest write FBeforeRequest;
    property BeforeShowPage: TRequestEvent read FBeforeShowPage write FBeforeShowPage;
    property AfterAjaxRequest: TAjaxRequestResponseEvent read FAfterAjaxRequest write FAfterAjaxRequest;
    property OnAjaxRequest: THandleAjaxRequest read FOnAjaxRequest write FOnAjaxRequest;
    property BaseURL: string read FBaseURL write FBaseURL;
  end;

implementation

uses rtlconsts, typinfo, XMLWrite;

{ TWebPage }

function TWebPage.ProduceContent: string;
var i : integer;
begin
  result := '';
  for i := 0 to ContentProducerCount-1 do
    result := result + THTMLContentProducer(ContentProducers[i]).ProduceContent;
end;

procedure TWebPage.AddContentProducer(AContentProducer: THTMLContentProducer);
begin
  ContentProducerList.Add(AContentProducer);
end;

procedure TWebPage.RemoveContentProducer(AContentProducer: THTMLContentProducer);
begin
  ContentProducerList.Remove(AContentProducer);
end;

function TWebPage.ExchangeContentProducers(Child1, Child2: THTMLContentProducer): boolean;
var ChildIndex1, ChildIndex2: integer;
begin
  result := false;
  ChildIndex1:=GetContentProducerList.IndexOf(Child1);
  if (ChildIndex1=-1) then
    Exit;
  ChildIndex2:=GetContentProducerList.IndexOf(Child2);
  if (ChildIndex2=-1) then
    Exit;
  GetContentProducerList.Exchange(ChildIndex1,ChildIndex2);
  result := true;
end;

function TWebPage.MoveContentProducer(MoveElement, MoveBeforeElement: THTMLContentProducer): boolean;
var ChildIndex1, ChildIndex2: integer;
begin
  result := false;
  ChildIndex1:=GetContentProducerList.IndexOf(MoveElement);
  if (ChildIndex1=-1) then
    Exit;
  ChildIndex2:=GetContentProducerList.IndexOf(MoveBeforeElement);
  if (ChildIndex2=-1) then
    Exit;
  GetContentProducerList.Move(ChildIndex1,ChildIndex2);
  result := true;
end;

procedure TWebPage.ForeachContentProducer(AForeachChildsProc: TForeachContentProducerProc; Recursive: boolean);
var i : integer;
    tmpChild: THTMLContentProducer;
begin
  for i := 0 to ContentProducerCount -1 do
    begin
    tmpChild := ContentProducers[i];
    AForeachChildsProc(tmpChild);
    if recursive then
      tmpChild.ForeachContentProducer(AForeachChildsProc,Recursive);
    end;
end;

procedure TWebPage.HandlePage(ARequest: TRequest; AResponse: TResponse; AWriter: THTMLwriter; AWebModule: TFPWebModule=nil);
var Handled: boolean;
    CompName: string;
    AComponent: TComponent;
    AnAjaxResponse: TAjaxResponse;
begin
  SetRequest(ARequest);
  FWebModule := AWebModule;
  try
    try
      DoBeforeRequest(ARequest);
      if IsAjaxCall then
        begin
        AnAjaxResponse := TAjaxResponse.Create(GetWebController, AResponse);
        try
          try
            if HasWebController then
              WebController.InitializeAjaxRequest;
            Handled := false;
            DoHandleAjaxRequest(ARequest, AnAjaxResponse, Handled);
            if not Handled then
              begin
              CompName := Request.QueryFields.Values['AjaxID'];
              if CompName='' then CompName := Request.GetNextPathInfo;
              AComponent := FindComponent(CompName);
              if assigned(AComponent) and (AComponent is THTMLContentProducer) then
                THTMLContentProducer(AComponent).HandleAjaxRequest(ARequest, AnAjaxResponse);
              end;
            DoAfterAjaxRequest(ARequest, AnAjaxResponse);
          except on E: Exception do
            AnAjaxResponse.SetError(e.HelpContext, e.Message);
          end;
          AnAjaxResponse.BindToResponse;
        finally
          AnAjaxResponse.Free;
        end;
        end
      else
        begin
        if HasWebController then
          WebController.InitializeShowRequest;
        DoBeforeShowPage(ARequest);
        AResponse.Content := ProduceContent;
        end;
    finally
      CleanupAfterRequest;
    end;
  finally
    SetRequest(nil);
    AWebModule := nil;
  end;
end;

procedure TWebPage.DoBeforeGenerateXML;
begin
  // Do Nothing
end;

procedure TWebPage.CleanupAfterRequest;
begin
  ForeachContentProducer(@DoCleanupAfterRequest, True);
  if HasWebController then
    WebController.CleanupAfterRequest;
end;

procedure TWebPage.DoCleanupAfterRequest(const AContentProducer: THTMLContentProducer);
begin
  AContentProducer.CleanupAfterRequest;
end;

procedure TWebPage.SetRequest(ARequest: TRequest);
begin
  FRequest := ARequest;
end;

procedure TWebPage.GetChildren(Proc: TGetChildProc; Root: TComponent);
var i : integer;
begin
  inherited GetChildren(Proc, Root);
  if (Root=Self) then
    for I:=0 to ContentProducerCount-1 do
      Proc(ContentProducers[i]);
end;

function TWebPage.ContentProducerCount: integer;
begin
  if assigned(FContentProducers) then
    result := FContentProducers.Count
  else
    result := 0;
end;

function TWebPage.GetContentProducers(Index: integer): THTMLContentProducer;
begin
  Result:=THTMLContentProducer(ContentProducerList[Index]);
end;

function TWebPage.GetHasWebController: boolean;
begin
  result := assigned(FWebController);
end;

function TWebPage.GetWebController: TWebController;
begin
  if not assigned(FWebController) then
    raise exception.create('No webcontroller available');
  result := FWebController;
end;

function TWebPage.GetContentProducerList: TFPList;
begin
  if not assigned(FContentProducers) then
    FContentProducers := tfplist.Create;
  Result := FContentProducers;
end;

function TWebPage.GetContentProducer(Index: integer): THTMLContentProducer;
begin
  Result := THTMLContentProducer(ContentProducerList[Index]);
end;

procedure TWebPage.DoAfterAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse);
begin
  if assigned(AfterAjaxRequest) then
    AfterAjaxRequest(Self,ARequest,AnAjaxResponse);
end;

procedure TWebPage.DoHandleAjaxRequest(ARequest: TRequest; AnAjaxResponse: TAjaxResponse; var Handled: boolean);
begin
  if assigned(OnAjaxRequest) then
    OnAjaxRequest(Self,ARequest,AnAjaxResponse, Handled);
end;

procedure TWebPage.DoBeforeRequest(ARequest: TRequest);
begin
  if assigned(BeforeRequest) then
    BeforeRequest(Self,ARequest);
end;

procedure TWebPage.DoBeforeShowPage(ARequest: TRequest);
begin
  if assigned(BeforeShowPage) then
    BeforeShowPage(Self,ARequest);
end;

function TWebPage.IsAjaxCall: boolean;
var s : string;
begin
  s := Request.HTTPXRequestedWith;
  result := sametext(s,'XmlHttpRequest');
end;

{ TStandardWebController }

function TStandardWebController.GetScriptFileReferences: TStringList;
begin
  Result:=FScriptFileReferences;
end;

function TStandardWebController.GetScripts: TFPObjectList;
begin
  if not assigned(FScripts) then
    begin
    FScripts:=TFPObjectList.Create;
    FScripts.OwnsObjects:=true;
    end;
  Result:=FScripts;
end;

function TStandardWebController.GetCurrentJavaScriptStack: TJavaScriptStack;
begin
  Result:=FCurrentJavascriptStack;
end;

procedure TStandardWebController.SetCurrentJavascriptStack(const AJavascriptStack: TJavaScriptStack);
begin
  FCurrentJavascriptStack := AJavascriptStack;
end;

function TStandardWebController.CreateNewScript: TStringList;
begin
  Result:=TStringList.Create;
  GetScripts.Add(result);
end;

procedure TStandardWebController.FreeScript(var AScript: TStringList);
begin
  with GetScripts do
    GetScripts.Delete(IndexOf(AScript));
  AScript := nil;
end;

function TStandardWebController.DefaultMessageBoxHandler(Sender: TObject;
  AText: String; Buttons: TWebButtons): string;
var i : integer;
    HasCancel: boolean;
    OnOk: string;
    OnCancel: string;
begin
  HasCancel:=false;
  OnOk:='';
  OnCancel:='';
  for i := low(Buttons) to High(Buttons) do
    begin
    if Buttons[i].ButtonType=btOk then
      OnOk := Buttons[i].OnClick;
    if Buttons[i].ButtonType=btCancel then
      begin
      HasCancel := True;
      OnCancel := Buttons[i].OnClick;
      end;
    end;

  if HasCancel then
    result := 'if (confirm('''+AText+''')==true) {'+OnOk+'} else {'+OnCancel+'}'
  else
    result := 'alert('''+AText+''');'+OnOk;
end;

constructor TStandardWebController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScriptFileReferences := TStringList.Create;
  // For some reason the Duplicates property does not work when sorted is true,
  // But we don't want a sorted list so do a manual check in AddScriptFileReference
  //FScriptFileReferences.Sorted:=true;
  FScriptFileReferences.Duplicates:=dupIgnore;
end;

destructor TStandardWebController.Destroy;
begin
  FScriptFileReferences.Free;
  FScripts.Free;
  inherited Destroy;
end;

function TStandardWebController.InitializeJavaScriptStack: TJavaScriptStack;
begin
  if assigned(FCurrentJavascriptStack) then
    raise exception.Create('There is still an old JavascriptStack available');
  FCurrentJavascriptStack := TJavaScriptStack.Create(self);
  Result:=FCurrentJavascriptStack;
end;

function TStandardWebController.GetUrl(ParamNames, ParamValues,
  KeepParams: array of string; Action: string): string;

var qs,p : String;
    i,j  : integer;
    found: boolean;
    FancyTitle: boolean;
    ConnectChar: char;
    CGIScriptName: string;
    ActionVar: string;
    ARequest: TRequest;
    WebMod: TFPWebModule;

begin
  FancyTitle:=false;
  qs := '';
  result := Action;
  ARequest := GetRequest;
  ActionVar := '';
  if assigned(owner) then
    begin
    if (owner is TWebPage) and assigned(TWebPage(Owner).WebModule) then
      WebMod := TWebPage(Owner).WebModule
    else if (owner is TFPWebModule) then
      WebMod := TFPWebModule(Owner);

    ActionVar := WebMod.ActionVar;
    if (action = '') and assigned(WebMod.Actions) and assigned(WebMod.Actions.CurrentAction) then
      result := WebMod.Actions.CurrentAction.Name;
    end;
  if ActionVar='' then FancyTitle:=true;
  if Assigned(ARequest) then
    begin
    if  (high(KeepParams)>=0) and (KeepParams[0]='*') then
      begin
      for i := 0 to ARequest.QueryFields.Count-1 do
        begin
        p := ARequest.QueryFields.Names[i];
        found := False;
        for j := 0 to high(ParamNames) do if sametext(ParamNames[j],p) then
          begin
          found := True;
          break;
          end;
        if not FancyTitle and SameText(ActionVar,p) then
          found := true;
        if not found then
          qs := qs + p + '=' + ARequest.QueryFields.ValueFromIndex[i] + '&';
        end;
      end
    else for i := 0 to high(KeepParams) do
      begin
      p := ARequest.QueryFields.Values[KeepParams[i]];
      if p <> '' then
        qs := qs + KeepParams[i] + '=' + p + '&';
      end;
    end;
  for i := 0 to high(ParamNames) do
    qs := qs + ParamNames[i] + '=' + ParamValues[i] + '&';

  ConnectChar:='?';
  if ScriptName='' then CGIScriptName:='.'
  else
    begin
    CGIScriptName:=ScriptName;
    if pos('?',ScriptName)>0 then ConnectChar := '&';
    end;
  if FancyTitle then // use ? or /
    result := CGIScriptName + '/' + Result
  else
    begin
    result := CGIScriptName + ConnectChar +ActionVar+'=' + Result;
    ConnectChar:='&';
    end;

  p := copy(qs,1,length(qs)-1);
  if p <> '' then
    result := result + ConnectChar + p
end;

procedure TStandardWebController.FreeJavascriptStack;
begin
  FreeAndNil(FCurrentJavascriptStack);
end;

procedure TStandardWebController.BindJavascriptCallstackToElement(AComponent: TComponent; AnElement: THtmlCustomElement; AnEvent: string);
begin
  if AnEvent='onclick' then
    (AnElement as THTMLAttrsElement).onclick:=CurrentJavaScriptStack.GetScript
  else if AnEvent='onchange' then
    if AnElement is THTML_input then (AnElement as THTML_input).onchange:=CurrentJavaScriptStack.GetScript;
end;

procedure TStandardWebController.AddScriptFileReference(AScriptFile: String);
begin
  if FScriptFileReferences.IndexOf(AScriptFile)=-1 then
    FScriptFileReferences.Add(AScriptFile);
end;

end.

