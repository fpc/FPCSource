unit webmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, HTTPDefs, fpHTTP, fpWeb;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure func1callRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FPWebModule1: TFPWebModule1; 

implementation

{$R *.lfm}

{ TFPWebModule1 }

procedure TFPWebModule1.func1callRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Content := '<html><body>Hello World!</body></html>';

  Handled := true;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.

