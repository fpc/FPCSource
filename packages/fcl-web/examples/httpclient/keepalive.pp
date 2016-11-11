program keepalive;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, fphttpclient;

const
  URL_DIRECT = 'https://www.google.com/humans.txt';
  URL_REDIRECTED = 'https://google.com/humans.txt';

type

  { TKeepConnectionDemo }

  TKeepConnectionDemo = class(TCustomApplication)
  private
    FURL : String;
    FShowResult : Boolean;
    FCount : Integer;
    FHttp: TFPHTTPClient;
    FData: TBytesStream;
    procedure DoRequests;
    procedure Usage(Msg: string);
  Protected
    Procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


constructor TKeepConnectionDemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException:=True;
  FHttp := TFPHTTPClient.Create(nil);
  FData := TBytesStream.Create;
end;

destructor TKeepConnectionDemo.Destroy;
begin
  FData.Free;
  FHttp.Free;
  inherited Destroy;
end;


procedure TKeepConnectionDemo.DoRequests;
var
  U: string;
  B, E: TDateTime;
  L : TStrings;
  I : Integer;

begin
  for I:=1 to FCount do
    begin
    FData.Clear;
    B := Now;
    if (FURL<>'') then
      U:=FURL
    else if FHTTP.AllowRedirect then
      U := URL_REDIRECTED
    else
      U := URL_DIRECT;
    FHttp.Get(U, FData);
    E := Now;
    Writeln('Request ',i,', Duration: ',FormatDateTime('hh:nn:ss.zzz', E - B));
    If FShowResult then
      begin
      FData.Seek(0, TSeekOrigin.soBeginning);
      With TStringList.Create do
        try
          LoadFromStream(FData);
          Writeln(text);
        finally
          Free;
        end;
     end;
    end;
end;

procedure TKeepConnectionDemo.Usage(Msg : string);

begin
  if (Msg<>'') then
    Writeln('Error : ',Msg);
  Writeln(' Usage : keepalive [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h  --help              This help');
  Writeln('-r  --redirect          Allow HTTP Redirect');
  Writeln('-k  --keep-connection   Keep connection');
  Writeln('-c  --count=N           Number of requests');
  Writeln('-u  --URL=uri           Specify url');
  Halt(Ord(Msg<>''));
end;
procedure TKeepConnectionDemo.DoRun;

Var
  S : String;

begin
  S:=CheckOptions('hrksc:u:',['count:','show','url:','redirect','keep-connection','help']);
  if (S<>'') or HasOption('h','help') then
    Usage(S);
  FCount:=StrToIntDef(GetOptionValue('c','count'),10);
  FShowResult:=HasOption('s','show');
  FURL:=GetOptionValue('u','url');
  FHTTP.AllowRedirect:=HasOption('r','redirect');
  FHTTP.KeepConnection:=HasOption('k','keep-connection');
  DoRequests;
  Terminate;
end;

begin
  With TKeepConnectionDemo.Create(Nil) do
    try
      Initialize;
      Run;
    Finally
      Free;
    end;
end.

