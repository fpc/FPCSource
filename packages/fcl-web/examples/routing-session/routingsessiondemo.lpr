program routingsessiondemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fphttpapp, // Standaline
  fphttp, // Session manager
  iniwebsession, // Ini based sessions
  httpdefs, // TRequest,TResponse
  httproute;

{$IFDEF VER3_0}
Type
  { TSessionState }

  TSessionState = Class(TObject)
    IsNew : Boolean;
    IsExpired : Boolean;
    Procedure OnNew(Sender : TObject);
    Procedure OnExpired(Sender : TObject);
  end;

  procedure TSessionState.OnNew(Sender: TObject);
  begin
    IsNew:=True;
  end;

  procedure TSessionState.OnExpired(Sender: TObject);
  begin
    IsExpired:=True;
  end;
{$ENDIF}

Procedure DisplayForm(aResponse : TResponse);

begin
  With AResponse.Contents do
    begin
    Add('<H1>Set variable value:</H1>');
    Add('The value of a variable can be stored in the session.<br> Here you can set the value of the variable.<p>');
    Add('<form action="insession" method="get">');
    Add('<input type="text" name="var"></input><p>');
    Add('<input type="submit" Value="Set new value"/>');
    Add('</form><p>');
    Add('<a href="newsession">Start new session</a><p>');
    Add('<a href="endsession">End the session</a>');
    end;
end;

Procedure DisplayNewSession(aResponse : TResponse; aSession : TCustomSession);

Var
  C : TCookie;

begin
  With AResponse.Contents do
    begin
    Add('<HTML><TITLE>Demo session was started</TITLE><BODY>');
    Add('<H1>New session started</H1>');
{$IFNDEF VER3_0}
    if (ssExpired in aSession.SessionState) then
      Add('The session associated with the cookie the browser sent, has expired.<P>');
{$ENDIF}
    Add('A new session was started.<P>');
    C:=AResponse.Cookies.FindCookie(aSession.SessionCookie);
    If Assigned(C) then
      begin
       Add('The issued session cookie is called <B>'+C.Name+'</B>.<BR> ');
       Add('The issued session cookie has value <B>'+C.Value+'</B>.<BR>');
       end
     else
       Add('No session cookie was found.');
    DisplayForm(AResponse);
    Add('</BODY></HTML>');
    end;
end;

Procedure NewSession(aRequest : TRequest; aResponse : TResponse);

Var
  Session : TCustomSession;

begin
  Session:=SessionFactory.CreateSession(aRequest);
  try
    Session.InitSession(aRequest,Nil,Nil);
    Session.InitResponse(aResponse);
    DisplayNewSession(aResponse,Session);
  Finally
    Session.Free;
  end;
end;

Procedure EndSession(aRequest : TRequest; aResponse : TResponse);

Var
  Session : TCustomSession;

begin
  Session:=SessionFactory.CreateSession(aRequest);
  try
    Session.InitSession(aRequest,Nil,Nil);
    // Stop the session
    Session.Terminate;
    Session.InitResponse(aResponse);
    With AResponse.Contents do
      begin
      Add('<HTML><TITLE>Demo Session Is Terminated</TITLE><BODY>');
      Add('<H1>Demo session Terminated</H1>');
      Add('The session was terminated, the cookie is cleared and the');
      Add('stored value is lost</p>');
      Add('<a href="newsession">Start new session</a>');
      Add('</BODY></HTML>');
      end;
  finally
    Session.Free;
    AResponse.SendContent;
  end;
end;

Procedure InSession(aRequest : TRequest; aResponse : TResponse);

Var
  V : string;
  C : TCookie;
  Session : TCustomSession;
  NewSession : Boolean;
{$IFDEF VER3_0}
  State : TSessionState;
{$ENDIF VER3_0}

begin
  {$IFDEF VER3_0}
  State:=nil;
  {$ENDIF VER3_0}
  Session:=SessionFactory.CreateSession(aRequest);
  try
    {$IFDEF VER3_0}
    State:=TSessionState.Create;
    Session.InitSession(aRequest,@State.OnNew,@State.OnExpired);
    NewSession:=State.IsNew;
    {$ELSE}
    Session.InitSession(aRequest,Nil,Nil);
    NewSession:=ssNew in Session.SessionState;
    {$ENDIF}
    Session.InitResponse(aResponse);
    // The url was called, but there was no session yet or it expired...
    if NewSession then
      DisplayNewSession(AResponse,Session)
    else
      With AResponse.Contents do
        begin
        Add('<HTML><TITLE>Demo session active</TITLE><BODY>');
        Add('<H1>Demo session active</H1>');
        Add('The demo session is still active<P>');
    //    If Session is TFPWebSession then
          begin
          C:=AResponse.Cookies.FindCookie(Session.SessionCookie);
          If Assigned(C) then
            begin
            Add('Current session Cookie is called <B>'+C.Name+'</B><BR>');
            Add('and has value <B>'+C.Value+'</B>.');
            end;
          V:=Session.Variables['Var'];
          If (V<>'') then
            Add('<P>Stored session value: <B>'+V+'</B>.')
          else
            Add('<P>No values stored in session.');
          V:=ARequest.QueryFields.Values['Var'];
          If V<>'' then
            begin
            Add('<P>Storing new session value: <B>'+V+'</B>.');
            Session.Variables['Var']:=V;
            end;
          end;
        DisplayForm(AResponse);
        Add('</BODY></HTML>');
        AResponse.SendContent; // Handles the response.
        end;
  finally
    {$IFDEF VER3_0}
    State.Free;
    {$ENDIF}
    Session.Free;
  end;
end;


begin
  HTTPRouter.RegisterRoute('/insession',@InSession);
  HTTPRouter.RegisterRoute('/endsession',@EndSession);
  HTTPRouter.RegisterRoute('/newsession',@NewSession,True);
  if ParamCount=1 then
    Application.Port:=StrToIntDef(ParamStr(1),8080)
  else
    Application.Port:=8080;
  Writeln('Server listens on port : ',Application.Port);
  Application.Initialize;
  Application.Run;
end.
