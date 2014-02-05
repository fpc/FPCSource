program httpget;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fphttpclient;

Type

  { TTestApp }

  TTestApp = Class(Tobject)
    procedure DoPassword(Sender: TObject; var RepeatRequest: Boolean);
  procedure ShowRedirect(ASender : TObject; Const ASrc : String; Var ADest : String);
  Procedure Run; 
  end;
  
procedure TTestApp.DoPassword(Sender: TObject; var RepeatRequest: Boolean);

Var
  H,UN,PW : String;
  P : Integer;
begin
  With TFPHTTPClient(Sender) do
    begin
    H:=GetHeader(ResponseHeaders,'WWW-Authenticate');
    end;
  P:=Pos('realm',LowerCase(H));
  if (P>0) then
    begin
    P:=Pos('"',H);
    Delete(H,1,P);
    P:=Pos('"',H);
    H:=Copy(H,1,Pos('"',H)-1);
    end;
  Writeln('Authorization required. Remote site says: ',H);
  Write('Enter username (empty quits): ');
  ReadLn(UN);
  RepeatRequest:=(UN<>'');
  if RepeatRequest then
    begin
    Write('Enter password: ');
    Readln(PW);
    TFPHTTPClient(Sender).UserName:=UN;
    TFPHTTPClient(Sender).Password:=PW;
    end;
end;

procedure TTestApp.ShowRedirect(ASender: TObject; const ASrc: String;
  var ADest: String);

begin
  Writeln('Following redirect from ',ASrc,'  ==> ',ADest);
end;  

procedure TTestApp.Run;
var
  i : Integer;

begin
  if (ParamCount<>2) then
    begin
    writeln('Usage : ',ExtractFileName(ParamStr(0)), 'URL filename');
    Halt(1);
    end;
  With TFPHTTPClient.Create(Nil) do
    try
      AllowRedirect:=True;
      OnRedirect:=@ShowRedirect;
      OnPassword:=@DoPassword;
      Get(ParamStr(1),ParamStr(2));
      Writeln('Response headers:');
      For I:=0 to ResponseHeaders.Count-1 do
        Writeln(ResponseHeaders[i]);
        
    finally
      Free;
    end;
end;

begin
  With TTestApp.Create do
    try
      Run;
    finally
      Free;
    end;
end.
