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
    FIssueState: String;
    FQuiet : Boolean;
    FNoWrite : Boolean;
    FToDoAction : String;
    procedure CloseTodo(aID: int64);
    procedure DoClientLog(Sender: TObject; const aMessage: string);
    procedure DoResource(Sender: TObject; aPage, aIndex, aCount: Integer; aObject: TJSONObject; aContinue: Boolean);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const aError : String); virtual;
    Property NoWrite : Boolean Read FNoWrite Write FNoWrite;
    Property Quiet : Boolean Read FQuiet Write FQuiet;
    Property ToDoAction : String Read FToDoAction Write  FToDoAction;
    Property IssueState : String Read FIssueState Write  FIssueState;
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
  if (IssueState='any') or SameText(aState,IssueState) then
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
  if not NoWrite then
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
  ErrorMsg:=CheckOptions('hc:l:qna:s:', ['help','config:','list:','quiet','no-write','action:','state:']);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  FQuiet:=HasOption('q','quiet');
  FNoWrite:=HasOption('n','no-write');
  ConfigFN:=GetOptionValue('c','config');
  FToDoAction:=GetOptionValue('a','action');
  if FToDoAction='' then
    FToDoAction:='assigned';
  FIssueState:=GetOptionValue('s','state');
  if FIssueState='' then
    FIssueState:='closed';
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
    FClient.ForEachResource('todos',['action',FTodoAction,'state','pending'],@DoResource);
    if ListFN<>'' then
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
  Writeln('-a --action=TYPE   Action of todo: default is "assigned". Other possibilities include:');
  Writeln('                   mentioned, build_failed, marked, approval_required, unmergeable, directly_addressed or merge_train_removed.');
  Writeln('-c --config=FILE   Config file');
  Writeln('-h   --help        This help');
  Writeln('-l --list=FILE     if file exists, read todo IDS from list. If file does not exist, write file after querying gitlab');
  Writeln('-n --no-write     Do not actaully change the TODO item');
  Writeln('-q --quiet         less messages');
  Writeln('-s --state=STATE   State of issue coupled to TODO. default is "closed". If set to "any" all issues will be marked.');
end;

var
  Application: TCloseTodoApplication;
begin
  Application:=TCloseTodoApplication.Create(nil);
  Application.Title:='Close Todos Application';
  Application.Run;
  Application.Free;
end.

