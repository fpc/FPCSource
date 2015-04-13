unit wmecho;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, webutil;

type

  { TEchoModule }

  TEchoModule = class(TFPWebModule)
    procedure EchoModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  EchoModule: TEchoModule;

implementation

{$R *.lfm}

{ TEchoModule }

procedure TEchoModule.EchoModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
Var
  S : TStrings;

begin

  S:=TStringList.Create;
  try
    S.Add('<HTML><HEAD><TITLE>Echo demo</TITLE></HEAD><BODY>');
    // Analyze request.
    DumpRequest(ARequest,S,True);
    S.Add('<H1>Extra headers (may or may not be passed):</H1>');
    S.Add('Do not track header (DNT):  '+ARequest.GetCustomHeader('DNT'));
    S.Add('</BODY></HTML>');
    AResponse.Contents:=S;
    Handled:=True;
  finally
    S.Free;
  end;
end;

initialization
  RegisterHTTPModule('TEchoModule', TEchoModule);
end.

