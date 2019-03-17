program cmdclient;

{$mode objfpc}{$H+}

uses
  cwstring,Classes, SysUtils, CustApp, fphttpclient, db, bufdataset, XMLDatapacketReader;

type

  { TSQLDBRestClientApplication }

  TSQLDBRestClientApplication = class(TCustomApplication)
  Private
    FURL: String;
    FUserName: string;
    FPassword: string;
    FShowRaw : Boolean;
  protected
    procedure RunQuery(aDataset: TBufDataset);
    Procedure ShowData(aDataset: TDataset);
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TSQLDBRestClientApplication }

procedure TSQLDBRestClientApplication.RunQuery(aDataset : TBufDataset);

Var
  C : TFPHTTPClient;
  S : TStringStream;
  U : String;

begin
  U:=FURL;
  S:=Nil;
  C:=TFPHTTPClient.Create(Self);
  try
    C.UserName:=FUserName;
    C.Password:=FPassword;
    S:=TStringStream.Create;
    if Pos('?',U)=0 then
      U:=U+'?'
    else
      U:=U+'&';
    U:=U+'fmt=buf';
    C.Get(U,S);
    if FShowRaw then
      begin
      Writeln('Raw request data:');
      Writeln('---');
      Writeln(S.Datastring);
      Writeln('---');
      end;
    S.Position:=0;
    aDataset.LoadFromStream(S,dfXML);
  finally
    S.Free;
    C.Free;
  end;
end;

procedure TSQLDBRestClientApplication.ShowData(aDataset: TDataset);

Var
  I : Integer;
  F : TField;
  FL : Integer;

begin
  FL:=0;
  With aDataset do
    begin
    For I:=0 to FieldDefs.Count-1 do
      if Length(FieldDefs[I].Name)>FL then
        FL:=Length(FieldDefs[I].Name);
    While not EOF do
      begin
      Writeln(StringOfChar('-',FL));
      Writeln('Record: ',RecNo:4);
      Writeln(StringOfChar('-',FL));
      For F in Fields do
        With F do
          begin
          Write(FieldName:FL,': ');
          if F.IsNull then
            Writeln('<NULL>')
          else
            Writeln(F.AsString);
          end;
      Next;
      end;
    end;
end;

procedure TSQLDBRestClientApplication.DoRun;
var
  ErrorMsg: String;
  D : TBufDataset;

begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hU:u:p:r', ['help','url:','username:','password:','raw']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  FURL:=GetOptionValue('U','url');
  FUserName:=GetOptionValue('u','username');
  FPassword:=GetOptionValue('p','password');
  FShowRaw:=HasOption('r','raw');
  D:=TBufDataset.Create(Self);
  try
    RunQuery(D);
    ShowData(D);
  Finally
    D.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TSQLDBRestClientApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSQLDBRestClientApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TSQLDBRestClientApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help            this message');
  Writeln('-p --password=PWD    HTTP Basic authentication password.');
  Writeln('-r --raw             Show raw request data');
  Writeln('-U --url=URL         URL to get data from. Do not add format (fmt) parameter');
  Writeln('-u --username=User   HTTP Basic authentication username');
end;

var
  Application: TSQLDBRestClientApplication;
begin
  Application:=TSQLDBRestClientApplication.Create(nil);
  Application.Title:='SQLDB Rest Bridge client application';
  Application.Run;
  Application.Free;
end.

