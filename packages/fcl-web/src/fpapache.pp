{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
unit fpapache;

interface

uses SysUtils,Classes,CustApp,httpDefs,fpHTTP,httpd, apr;

Type

  TCustomApacheApplication = Class;

  { TApacheRequest }

  TApacheRequest = Class(TRequest)
  Private
    FApache : TCustomApacheApplication;
    FRequest : PRequest_rec;
    FContent : String;
    FContentRead : Boolean;
    procedure ReadContent;
  Protected
    Function GetFieldValue(Index : Integer) : String; override;
    Procedure InitFromRequest;
  Public
    Constructor CreateReq(App : TCustomApacheApplication; ARequest : PRequest_rec);
    Property ApacheRequest : Prequest_rec Read FRequest;
    Property ApacheApp : TCustomApacheApplication Read FApache;
  end;

  { TCGIResponse }

  { TApacheResponse }

  TApacheResponse = Class(TResponse)
  private
    FApache : TCustomApacheApplication;
    FRequest : PRequest_rec;
    procedure SendStream(S: TStream);
  Protected
    Procedure DoSendHeaders(Headers : TStrings); override;
    Procedure DoSendContent; override;
  Public
    Constructor CreateApache(Req : TApacheRequest);
    Property ApacheRequest : Prequest_rec Read FRequest;
    Property ApacheApp : TCustomApacheApplication Read FApache;
  end;


  { TCustomApacheApplication }
  THandlerPriority = (hpFirst,hpMiddle,hpLast);
  TGetModuleEvent = Procedure (Sender : TObject; ARequest : TRequest;
                               Var ModuleClass : TCustomHTTPModuleClass) of object;
  TBeforeRequestEvent = Procedure(Sender : TObject; Const AHandler : String;
                                  Var AllowRequest : Boolean) of object;

  TCustomApacheApplication = Class(TCustomApplication)
  private
    FAdministrator: String;
    FBaseLocation: String;
    FBeforeRequest: TBeforeRequestEvent;
    FEmail: String;
    FHandlerName: String;
    FModuleName: String;
    FOnGetModule: TGetModuleEvent;
    FAllowDefaultModule: Boolean;
    FModules : Array[0..1] of TStrings;
    FPriority: THandlerPriority;
    FModuleRecord : PModule;
    function GetModules(Index: integer): TStrings;
    procedure SetModules(Index: integer; const AValue: TStrings);
    procedure ShowRequestException(R: TResponse; E: Exception);
  Protected
    Function ProcessRequest(P : PRequest_Rec) : Integer; virtual;
    Function GetModuleName(ARequest : TRequest) : string;
    function FindModule(ModuleClass : TCustomHTTPModuleClass): TCustomHTTPModule;
    Procedure DoRun; override;
    Function AllowRequest(P : PRequest_Rec) : Boolean; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Procedure SetModuleRecord(Var ModuleRecord : Module);
    Procedure Initialize; override;
    Procedure ShowException(E : Exception); override;
    Procedure CreateForm(AClass : TComponentClass; Var Reference : TComponent);
    Procedure handleRequest(ARequest : TRequest; AResponse : TResponse); virtual;
    Property OnGetModule : TGetModuleEvent Read FOnGetModule Write FOnGetModule;
    Property AllowDefaultModule : Boolean Read FAllowDefaultModule Write FAllowDefaultModule;
    Property HandlerPriority : THandlerPriority Read FPriority Write FPriority default hpMiddle;
    Property BeforeModules : TStrings Index 0 Read GetModules Write SetModules;
    Property AfterModules : TStrings Index 1 Read GetModules Write SetModules;
    Property BaseLocation : String Read FBaseLocation Write FBaseLocation;
    Property ModuleName : String Read FModuleName Write FModuleName;
    Property HandlerName : String Read FHandlerName Write FHandlerName;
    Property BeforeRequest : TBeforeRequestEvent Read FBeforeRequest Write FBeforeRequest;
    Property Email : String Read FEmail Write FEmail;
    Property Administrator : String Read FAdministrator Write FAdministrator;
  end;

  TApacheApplication = Class(TCustomApacheApplication)
  Public
    Property HandlerPriority;
    Property BeforeModules;
    Property AfterModules;
    Property AllowDefaultModule;
    Property OnGetModule;
    Property BaseLocation;
    Property ModuleName;
  end;
  

  EFPApacheError = Class(Exception);
  
Var
  Application : TCustomApacheApplication = Nil;
  ShowCleanUpErrors : Boolean = False;
  AlternateHandler : ap_hook_handler_t = Nil;

Implementation

resourcestring
  SErrNoModuleNameForRequest = 'Could not determine HTTP module name for request';
  SErrNoModuleForRequest = 'Could not determine HTTP module for request "%s"';
  SErrNoModuleRecord = 'No module record location set.';
  SErrNoModuleName = 'No module name set';
  SModuleError  = 'Module Error';
  SAppEncounteredError = 'The application encountered the following error:';
  SError     = 'Error: ';
  SNotify    = 'Notify: ';

const
  HPRIO : Array[THandlerPriority] of Integer
        = (APR_HOOK_FIRST,APR_HOOK_MIDDLE,APR_HOOK_LAST);


Procedure InitApache;

begin
  Application:=TCustomApacheApplication.Create(Nil);
end;

Procedure DoneApache;

begin
  Try
    FreeAndNil(Application);
  except
    if ShowCleanUpErrors then
      Raise;
  end;
end;

Function DefaultApacheHandler(P : PRequest_Rec) : integer;cdecl;

begin
  If (AlternateHandler<>Nil) then
    Result:=AlterNateHandler(P)
  else
    If Application.AllowRequest(P) then
      Result:=Application.ProcessRequest(P)
    else
      Result:=DECLINED;
end;

Procedure RegisterApacheHooks(P: PApr_pool_t);cdecl;

Var
  H : ap_hook_handler_t;
  PP1,PP2 : PPChar;

begin
  H:=AlternateHandler;
  If (H=Nil) then
    H:=@DefaultApacheHandler;
  PP1:=Nil;
  PP2:=Nil;
  ap_hook_handler(H,PP1,PP2,HPRIO[Application.HandlerPriority]);
end;

{ TCustomApacheApplication }

function TCustomApacheApplication.GetModules(Index: integer): TStrings;
begin
  If (FModules[Index]=Nil) then
    FModules[Index]:=TStringList.Create;
  Result:=FModules[Index];
end;

procedure TCustomApacheApplication.SetModules(Index: integer;
  const AValue: TStrings);
begin
  If (FModules[Index]=Nil) then
    FModules[Index]:=TStringList.Create;
  FModules[Index].Assign(AValue);
end;

Function TCustomApacheApplication.ProcessRequest(P: PRequest_Rec) : Integer;

Var
  Req : TApacheRequest;
  Resp : TApacheResponse;

begin
  Req:=TApacheRequest.CreateReq(Self,P);
  Try
    Resp:=TApacheResponse.CreateApache(Req);
    Try
      HandleRequest(Req,Resp);
    Finally
      Result:=OK;
      Resp.Free;
    end;
  Finally
    Req.Free;
  end;
end;

function TCustomApacheApplication.GetModuleName(Arequest: TRequest): string;


begin
  Result:=ARequest.GetNextPathInfo;
end;

function TCustomApacheApplication.FindModule(ModuleClass : TCustomHTTPModuleClass): TCustomHTTPModule;

Var
  I : Integer;

begin
  I:=ComponentCount-1;
  While (I>=0) and (Not (Components[i] is ModuleClass)) do
    Dec(i);
  if (I>=0) then
    Result:=Components[i] as TCustomHTTPModule
  else
    Result:=Nil;
end;

procedure TCustomApacheApplication.DoRun;
begin
  inherited DoRun;
end;

function TCustomApacheApplication.AllowRequest(P: PRequest_Rec): Boolean;

Var
  Hn : String;

begin
  HN:=StrPas(p^.Handler);
  Result:=CompareText(HN,FHandlerName)=0;
  If Assigned(FBeforeRequest) then
    FBeforeRequest(Self,HN,Result);
end;

constructor TCustomApacheApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowDefaultModule:=True;
  FPriority:=hpMiddle;
end;

procedure TCustomApacheApplication.SetModuleRecord(var ModuleRecord: Module);
begin
  FModuleRecord:=@ModuleRecord;
  FillChar(ModuleRecord,SizeOf(ModuleRecord),0);
end;

procedure TCustomApacheApplication.Initialize;

begin
  If (FModuleRecord=nil) then
    Raise EFPApacheError.Create(SErrNoModuleRecord);
  if (FModuleName='') and (FModuleRecord^.Name=Nil) then
    Raise EFPApacheError.Create(SErrNoModuleName);
  STANDARD20_MODULE_STUFF(FModuleRecord^);
  If (StrPas(FModuleRecord^.name)<>FModuleName) then
    FModuleRecord^.Name:=PChar(FModuleName);
  FModuleRecord^.register_hooks:=@RegisterApacheHooks;
end;

procedure TCustomApacheApplication.ShowRequestException(R : TResponse; E: Exception);

Var
 TheEmail : String;
 FrameCount: integer;
 Frames: PPointer;
 FrameNumber:Integer;
 S : TStrings;

begin
  If not R.HeadersSent then
    begin
    R.ContentType:='text/html';
    R.SendHeaders;
    end;
  If (R.ContentType='text/html') then
    begin
    S:=TStringList.Create;
    Try
      With S do
        begin
        Add('<html><head><title>'+Title+': '+SModuleError+'</title></head>'+LineEnding);
        Add('<body>');
        Add('<center><hr><h1>'+Title+': ERROR</h1><hr></center><br><br>');
        Add(SAppEncounteredError+'<br>');
        Add('<ul>');
        Add('<li>'+SError+' <b>'+E.Message+'</b>');
        Add('<li> Stack trace:<br>');
        Add(BackTraceStrFunc(ExceptAddr)+'<br>');
        FrameCount:=ExceptFrameCount;
        Frames:=ExceptFrames;
        for FrameNumber := 0 to FrameCount-1 do
          Add(BackTraceStrFunc(Frames[FrameNumber])+'<br>');
        Add('</ul><hr>');
        TheEmail:=Email;
        If (TheEmail<>'') then
          Add('<h5><p><i>'+SNotify+Administrator+': <a href="mailto:'+TheEmail+'">'+TheEmail+'</a></i></p></h5>');
        Add('</body></html>');
        end;
      R.Content:=S.Text;
      R.SendContent;
    Finally
      FreeAndNil(S);
    end;
    end;
end;

procedure TCustomApacheApplication.ShowException(E: Exception);
begin
  ap_log_error(pchar(FModuleName),0,APLOG_ERR,0,Nil,'module: %s',[Pchar(E.Message)]);
end;

procedure TCustomApacheApplication.CreateForm(AClass: TComponentClass;
  var Reference: TComponent);
begin
  Reference:=AClass.Create(Self);
end;

procedure TCustomApacheApplication.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  MC : TCustomHTTPModuleClass;
  M  : TCustomHTTPModule;
  MN : String;
  MI : TModuleItem;
  
begin
  try
    MC:=Nil;
    If (OnGetModule<>Nil) then
      OnGetModule(Self,ARequest,MC);
    If (MC=Nil) then
      begin
      MN:=GetModuleName(ARequest);
      If (MN='') and Not AllowDefaultModule then
        Raise EFPApacheError.Create(SErrNoModuleNameForRequest);
      MI:=ModuleFactory.FindModule(MN);
      If (MI=Nil) and (ModuleFactory.Count=1) then
        MI:=ModuleFactory[0];
      if (MI=Nil) then
        begin
        Raise EFPApacheError.CreateFmt(SErrNoModuleForRequest,[MN]);
        end;
      MC:=MI.ModuleClass;
      end;
    M:=FindModule(MC); // Check if a module exists already
    If (M=Nil) then
      begin
      If MC.UseStreaming then
        M:=MC.Create(Self)
      else  
        M:=MC.CreateNew(Self,0);
      end;
    M.HandleRequest(ARequest,AResponse);
  except
    On E : Exception do
      ShowRequestException(AResponse,E);
  end;
end;

{ TApacheRequest }

function TApacheRequest.GetFieldValue(Index: Integer): String;

var
  P : Pchar;
  FN : String;
  I : Integer;
  
begin

  Result:='';
  If (Index in [1..NoHTTPFields]) then
    begin
    FN:=HTTPFieldNames[Index];
    P:=apr_table_get(FRequest^.headers_in,pchar(FN));
    If (P<>Nil) then
      Result:=StrPas(P);
    end;
  if (Result='') then
    case Index of
      0  : Result:=strpas(FRequest^.protocol); // ProtocolVersion
      7  : Result:=Strpas(FRequest^.content_encoding); //ContentEncoding
      25 : Result:=StrPas(FRequest^.path_info); // PathInfo
      26 : Result:=StrPas(FRequest^.filename); // PathTranslated
      27 : // RemoteAddr
           If (FRequest^.Connection<>Nil) then
             Result:=StrPas(FRequest^.Connection^.remote_ip);
      28 : // RemoteHost
           ap_get_remote_host(FRequest^.Connection,
                              FRequest^.Per_Dir_Config,
                              REMOTE_HOST,Nil);
      29 : begin // ScriptName
           Result:=StrPas(FRequest^.unparsed_uri);
           I:=Pos('?',Result)-1;
           If (I=-1) then
             I:=Length(Result);
           Result:=Copy(Result,1,I-Length(PathInfo));
           end;
      30 : Result:=IntToStr(ap_get_server_port(FRequest)); // ServerPort
      31 : Result:=StrPas(FRequest^.method); // Method
      32 : Result:=StrPas(FRequest^.unparsed_uri); // URL
      33 : Result:=StrPas(FRequest^.args); // Query
      34 : Result:=StrPas(FRequest^.HostName); // Host
      35 : begin // Content
           If Not FContentRead then
             ReadContent;
           Result:=FContent;
           end;
    else
      Result:=inherited GetFieldValue(Index);
    end;
end;

procedure TApacheRequest.ReadContent;

  Function MinS(A,B : Integer) : Integer;
  
  begin
    If A<B then
      Result:=A
    else
      Result:=B;
  end;

Var
  Left,Len,Count,Bytes : Integer;
  P : Pchar;
  
begin
  ap_setup_client_block(Frequest,REQUEST_CHUNKED_DECHUNK);
  If (ap_should_client_block(Frequest)=1) then
    begin
    Len:=ContentLength;
    If (Len>0) then
      begin
      SetLength(FContent,Len);
      P:=PChar(FContent);
      Left:=Len;
      Count:=0;
      Repeat
        Bytes:=ap_get_client_block(FRequest,P,MinS(10*1024,Left));
        Dec(Left,Bytes);
        Inc(P,Bytes);
        Inc(Count,Bytes);
      Until (Count>=Len) or (Bytes=0);
      SetLength(FContent,Count);
      end;
    end;
  FContentRead:=True;
end;

procedure TApacheRequest.InitFromRequest;

Var
  I : Integer;
  S : String;
  
begin
  // This fills the internal table. We should try
  // to get rid of it.
  For I:=0 to NoHTTPFields do
    begin
    S:=GetFieldValue(i);
    If (S<>'') then
      SetFieldValue(I,S);
    end;
end;

Constructor TApacheRequest.CreateReq(App : TCustomApacheApplication; ARequest : PRequest_rec);

begin
  FApache:=App;
  FRequest:=Arequest;
  ReturnedPathInfo:=App.BaseLocation;
  InitFromRequest;
  Inherited Create;
end;

{ TApacheResponse }

procedure TApacheResponse.DoSendHeaders(Headers: TStrings);

Var
  I,P : Integer;
  N,V : String;

begin
  For I:=0 to Headers.Count-1 do
    begin
    V:=Headers[i];
    P:=Pos(':',V);
    If (P<>0) and (P<Length(V)) then
      begin
      N:=Copy(V,1,P-1);
      System.Delete(V,1,P);
      apr_table_set(FRequest^.headers_out,Pchar(N),Pchar(V));
      end;
    end;
end;

procedure TApacheResponse.DoSendContent;

Var
  S : String;
  I : Integer;

begin
  S:=ContentType;
  If (S<>'') then
    FRequest^.content_type:=apr_pstrdup(FRequest^.pool,Pchar(S));
  If (ContentStream<>Nil) then
    SendStream(Contentstream)
  else
    for I:=0 to Contents.Count-1 do
      begin
      S:=Contents[i]+LineEnding;
      // If there is a null, it's written also with ap_rwrite
      ap_rwrite(PChar(S),Length(S),FRequest);
      end;
end;

Procedure TApacheResponse.SendStream(S : TStream);

Var
  Buf : Array[0..(10*1024)-1] of Byte;
  Count : Integer;

begin
  Repeat
    Count:=S.Read(Buf,SizeOf(Buf));
    If Count>0 then
      ap_rwrite(@Buf,Count,FRequest);
  Until (Count=0);
end;


Constructor TApacheResponse.CreateApache(Req : TApacheRequest);
begin
  FApache:=Req.ApacheApp;
  Frequest:=Req.ApacheRequest;
  Inherited Create(Req);
end;


Initialization
  InitApache;
  
Finalization
  DoneApache;
  
end.
