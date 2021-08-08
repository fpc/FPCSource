program closetodo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, fpjson, gitlabclient,opensslsockets, jsonparser ;

type

  { TCloseTodoApplication }

  TCloseTodoApplication = class(TCustomApplication)
  private
    FConfig : TGitlabConfig;
    FClient : TGitLabClient;
    FIDS : TStrings;
    FQuiet : Boolean;
    procedure CloseTodo(aID: int64);
    procedure DoClientLog(Sender: TObject; const aMessage: string);
    procedure DoResource(Sender: TObject; aPage, aIndex, aCount: Integer; aObject: TJSONObject; aContinue: Boolean);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const aError : String); virtual;
  end;

{ TCloseTodoApplication }

procedure TCloseTodoApplication.DoResource(Sender: TObject; aPage, aIndex,
  aCount: Integer; aObject: TJSONObject; aContinue: Boolean);

Var
  aData : TJSONData;
  Msg,aState : String;
  aBugID,aBugIID,aProjectID : Int64;

begin
  Msg:=Format('[Page %d [%d/%d]: ',[aPage,aIndex,aCount]);
  aProjectID:=0;
  aBugID:=0;
  aBugIID:=0;
  aState:='';
  aData:=aObject.FindPath('target.state');
  if Assigned(aData) then
    aState:=aData.AsString;
  aData:=aObject.FindPath('project.id');
  if Assigned(aData) then
    aProjectID:=aData.AsInt64;
  aData:=aObject.FindPath('target.id');
  if Assigned(aData) then
    aBugID:=aData.AsInt64;
  aData:=aObject.FindPath('target.iid');
  if Assigned(aData) then
    aBugIID:=aData.Asint64;
  DoClientLog(Self,Msg+Format('Project: %d, bug: %d, bug iid: %d, state : %s',[aProjectID,aBugID,aBugIID,aState]));
  if SameText(aState,'closed') then
    begin
    if (FConfig.ProjectID=0) or (aProjectID=FConfig.ProjectID) then
      FIDS.Add(IntToStr(aObject.Get('id',Int64(0))));
    end
end;

procedure TCloseTodoApplication.CloseTodo(aID : int64);

Var
  aResource : String;

begin
  if (aID=-1) then
    exit;
  aResource:=Format('todos/%d/mark_as_done',[aID]);
  Writeln('Posting ',aResource);
  FClient.CreateResource(aResource,Nil);
end;

procedure TCloseTodoApplication.DoClientLog(Sender: TObject;
  const aMessage: string);
begin
  if not FQuiet then
    Writeln(aMessage);
end;

procedure TCloseTodoApplication.DoRun;

var
  ErrorMsg: String;
  ListFN,ConfigFN : String;
  I : Integer;

begin
  Terminate;
  ErrorMsg:=CheckOptions('hc:l:q', ['help','config:','list:','quiet']);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  FQuiet:=HasOption('q','quiet');
  ConfigFN:=GetOptionValue('c','config');
  if ConfigFN='' then
    begin
    Usage('Need gitlab config file');
    Exit;
    end;
  if not FileExists(ConfigFN)then
    begin
    Usage('Gitlab config file "'+ConfigFN+'" does not exist');
    Exit;
    end;
  FConfig.LoadFromFile(ConfigFN,'');
  FClient.Config:=FConfig;
  ListFN:=GetOptionValue('l','list');
  if FileExists(ListFN)then
    FIDS.LoadFromFile(ListFN)
  else
    begin
    FClient.ForEachResource('todos',['action','assigned','state','pending'],@DoResource);
    FIDS.SaveToFile(ListFN);
    end;
  For I:=0 to FIDS.Count-1 do
    CloseTodo(StrToInt64Def(FIDS[i],-1));
  Writeln(Format('Closed %d todos',[FIDS.Count]));
  // stop program loop
  Terminate;
end;

constructor TCloseTodoApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfig.Reset;
  FClient:=TGitLabClient.Create;
  FClient.OnLog:=@DoClientLog;
  StopOnException:=True;
  FIDS:=TStringList.Create;
end;

destructor TCloseTodoApplication.Destroy;
begin
  FreeAndNil(FIDS);
  FreeAndNil(FClient);
  inherited Destroy;
end;

procedure TCloseTodoApplication.Usage(const aError: String);
begin
  if (aError<>'') then
    Writeln('Error : ',aError);
  Writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where [Options] is one or more of:');
  Writeln('-h   --help        This help');
  Writeln('-c --config=FILE   Config file');
  Writeln('-l --list=FILE     if file exists, read todo IDS from list. If file does not exist, write file after querying gitlab');
  Writeln('-q --quiet         less messages');
end;

var
  Application: TCloseTodoApplication;
begin
  Application:=TCloseTodoApplication.Create(nil);
  Application.Title:='Close Todos Application';
  Application.Run;
  Application.Free;
end.

