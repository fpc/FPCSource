{ Loopback HTTP listener that receives the OAuth2 authorization code
  from the browser redirect (http://127.0.0.1:port). }
unit GoogleAPI.OAuthRedirect;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Net.Ssockets;
  {$ELSE}
  Classes, SysUtils, ssockets;
  {$ENDIF}

type
  EOAuthRedirect = class(Exception);

  { TOAuthRedirectListener }

  TOAuthRedirectListener = class(TObject)
  private
    FPort: Word;
    FCode: String;
    FError: String;
    FServer: TInetServer;
    procedure DoConnect(aSender: TObject; aData: TSocketStream);
  public
    // Bind a listener to 127.0.0.1 on the given port.
    constructor Create(aPort: Word);
    destructor Destroy; override;
    // Wait for the browser redirect and return the authorization code.
    function WaitForCode: String;
    // Redirect URI that points to this listener.
    function RedirectURI: String;
    // Port the listener is bound to.
    property Port: Word read FPort;
  end;

// Return the code parameter of a redirect URL, or the trimmed input when it has no query.
function ExtractAuthCode(const aURLOrCode: String): String;

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  FpWeb.Http.Protocol;
  {$ELSE}
  httpprotocol;
  {$ENDIF}

// Return the decoded value of parameter aName in a query string.
function QueryParam(const aQuery, aName: String): String;

var
  lParams: TStringList;

begin
  lParams:=TStringList.Create;
  try
    lParams.Delimiter:='&';
    lParams.StrictDelimiter:=True;
    lParams.DelimitedText:=aQuery;
    Result:=HTTPDecode(lParams.Values[aName],True);
  finally
    lParams.Free;
  end;
end;


// Return the part of a URL or request target after the question mark.
function QueryPart(const aTarget: String): String;

var
  lPos: Integer;

begin
  lPos:=Pos('?',aTarget);
  if lPos=0 then
    Result:=''
  else
    Result:=Copy(aTarget,lPos+1,Length(aTarget)-lPos);
end;


function ExtractAuthCode(const aURLOrCode: String): String;

var
  lQuery: String;

begin
  lQuery:=QueryPart(Trim(aURLOrCode));
  if lQuery='' then
    Result:=Trim(aURLOrCode)
  else
    Result:=QueryParam(lQuery,'code');
end;


// Read the request head and return its first line.
function ReadRequestLine(aData: TSocketStream): String;

var
  lBuf: array[0..1023] of AnsiChar;
  lHead, lChunk: String;
  lCount, lPos: Integer;

begin
  lHead:='';
  repeat
    lCount:=aData.Read(lBuf,SizeOf(lBuf));
    if lCount>0 then
      begin
      SetString(lChunk,PAnsiChar(@lBuf[0]),lCount);
      lHead:=lHead+lChunk;
      end;
  until (lCount<=0) or (Pos(#13#10#13#10,lHead)>0) or (Length(lHead)>32768);
  lPos:=Pos(#13#10,lHead);
  if lPos=0 then
    Result:=lHead
  else
    Result:=Copy(lHead,1,lPos-1);
end;


// Write a minimal HTML response and close the connection.
procedure SendResponse(aData: TSocketStream; aStatus: Integer; const aMessage: String);

var
  lBody, lHead, lStatusText: String;

begin
  if aStatus=200 then
    lStatusText:='OK'
  else
    lStatusText:='Not Found';
  lBody:='<!DOCTYPE html><html><body><p>'+aMessage+'</p></body></html>';
  lHead:=Format('HTTP/1.1 %d %s'#13#10,[aStatus,lStatusText])
        +'Content-Type: text/html; charset=utf-8'#13#10
        +'Content-Length: '+IntToStr(Length(lBody))+#13#10
        +'Connection: close'#13#10#13#10;
  lHead:=lHead+lBody;
  aData.WriteBuffer(lHead[1],Length(lHead));
end;


{ TOAuthRedirectListener }

constructor TOAuthRedirectListener.Create(aPort: Word);

begin
  inherited Create;
  FPort:=aPort;
  FServer:=TInetServer.Create('127.0.0.1',aPort);
  FServer.OnConnect:=@DoConnect;
  FServer.Bind;
end;


destructor TOAuthRedirectListener.Destroy;

begin
  FreeAndNil(FServer);
  inherited Destroy;
end;


procedure TOAuthRedirectListener.DoConnect(aSender: TObject; aData: TSocketStream);

var
  lLine, lTarget, lQuery: String;
  lStart, lEnd: Integer;

begin
  try
    aData.IOTimeout:=5000;
    lLine:=ReadRequestLine(aData);
    lStart:=Pos(' ',lLine);
    lTarget:=Copy(lLine,lStart+1,Length(lLine)-lStart);
    lEnd:=Pos(' ',lTarget);
    if lEnd>0 then
      lTarget:=Copy(lTarget,1,lEnd-1);
    lQuery:=QueryPart(lTarget);
    FCode:=QueryParam(lQuery,'code');
    FError:=QueryParam(lQuery,'error');
    if FCode<>'' then
      SendResponse(aData,200,'Authorization received. You can close this window and return to the application.')
    else if FError<>'' then
      SendResponse(aData,200,'Authorization was refused. You can close this window.')
    else
      SendResponse(aData,404,'Not found');
    if (FCode<>'') or (FError<>'') then
      FServer.StopAccepting;
  finally
    aData.Free;
  end;
end;


function TOAuthRedirectListener.WaitForCode: String;

begin
  FCode:='';
  FError:='';
  FServer.StartAccepting;
  if FError<>'' then
    raise EOAuthRedirect.CreateFmt('Authorization was refused: %s',[FError]);
  Result:=FCode;
end;


function TOAuthRedirectListener.RedirectURI: String;

begin
  Result:=Format('http://127.0.0.1:%d',[FPort]);
end;


end.
