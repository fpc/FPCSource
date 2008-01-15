{$mode objfpc}
{$h+}
unit pkghandler;

interface

uses
  Classes,SysUtils,
  pkgglobals,
  pkgoptions,
  fprepos;

type
  { TActionStack }

  TActionArgs = array of string;

  TActionStackItem = record
    ActionPackage : TFPPackage;
    Action : string;
    Args   : TActionArgs;
  end;
  PActionStackItem = ^TActionStackItem;

  TActionStack = class
  private
    FList : TFPList;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Push(APackage:TFPPackage;const AAction:string;const Args:TActionArgs);
    procedure Push(APackage:TFPPackage;const AAction:string;const Args:array of string);
    function  Pop(out APackage:TFPPackage;out AAction:string;out Args:TActionArgs):boolean;
  end;


  { TPackageHandler }

  TPackageHandler = Class(TComponent)
  private
    FCurrentPackage : TFPPackage;
  Protected
    Procedure Log(Level: TVerbosity;Msg : String);
    Procedure Log(Level: TVerbosity;Fmt : String; const Args : array of const);
    Procedure Error(Msg : String);
    Procedure Error(Fmt : String; const Args : array of const);
    procedure ExecuteAction(APackage:TFPPackage;const AAction:string;const Args:TActionArgs=nil);
    Function ExecuteProcess(Const Prog,Args:String):Integer;
    Procedure SetCurrentDir(Const ADir:String);
    function PackageBuildPath:String;
    function PackageRemoteArchive:String;
    function PackageLocalArchive:String;
    function PackageManifestFile:String;
  Public
    Constructor Create(AOwner:TComponent;APackage:TFPPackage); virtual;
    function PackageLogPrefix:String;
    Function Execute(const Args:TActionArgs):boolean; virtual; abstract;
    Property CurrentPackage:TFPPackage Read FCurrentPackage Write FCurrentPackage;
  end;
  TPackageHandlerClass = class of TPackageHandler;

  EPackageHandler = Class(Exception);

// Actions/PkgHandler
procedure RegisterPkgHandler(const AAction:string;pkghandlerclass:TPackageHandlerClass);
function GetPkgHandler(const AAction:string):TPackageHandlerClass;
function ExecuteAction(APackage:TFPPackage;const AAction:string;const Args:TActionArgs=nil):Boolean;


Implementation

uses
  typinfo,
  contnrs,
  uriparser,
  pkgmessages;

var
  PkgHandlerList  : TFPHashList;
  ExecutedActions : TFPHashList;

procedure RegisterPkgHandler(const AAction:string;pkghandlerclass:TPackageHandlerClass);
begin
  if PkgHandlerList.Find(AAction)<>nil then
    begin
      Raise EPackageHandler.CreateFmt(SErrActionAlreadyRegistered,[AAction]);
      exit;
    end;
  PkgHandlerList.Add(AAction,pkghandlerclass);
end;


function GetPkgHandler(const AAction:string):TPackageHandlerClass;
begin
  result:=TPackageHandlerClass(PkgHandlerList.Find(AAction));
  if result=nil then
    Raise EPackageHandler.CreateFmt(SErrActionNotFound,[AAction]);
end;


function ExecuteAction(APackage:TFPPackage;const AAction:string;const Args:TActionArgs=nil):Boolean;
var
  pkghandlerclass : TPackageHandlerClass;
  i : integer;
  logargs : string;
  FullActionName : string;
begin
  result:=false;
  // Check if we have already executed or are executing the action
  if assigned(Apackage) then
    FullActionName:=APackage.Name+AAction
  else
    FullActionName:=AAction;
  if ExecutedActions.Find(FullActionName)<>nil then
    begin
      Log(vDebug,'Already executed or executing action '+FullActionName);
      result:=true;
      exit;
    end;
  ExecutedActions.Add(FullActionName,Pointer(PtrUInt(1)));
  // Create action handler class
  pkghandlerclass:=GetPkgHandler(AAction);
  With pkghandlerclass.Create(nil,APackage) do
    try
      logargs:='';
      for i:=Low(Args) to High(Args) do
        begin
          if logargs='' then
            logargs:=Args[i]
          else
            logargs:=logargs+','+Args[i];
        end;
      Log(vDebug,SLogRunAction+' start',[AAction,logargs]);
      result:=Execute(Args);
      Log(vDebug,SLogRunAction+' end',[AAction,logargs]);
    finally
      Free;
    end;
end;


{ TPackageHandler }

constructor TPackageHandler.Create(AOwner:TComponent;APackage:TFPPackage);
begin
  inherited Create(AOwner);
  FCurrentPackage:=APackage;
end;

Function TPackageHandler.ExecuteProcess(Const Prog,Args:String):Integer;
begin
  Log(vCommands,SLogExecute,[Prog,Args]);
  Result:=SysUtils.ExecuteProcess(Prog,Args);
end;


Procedure TPackageHandler.SetCurrentDir(Const ADir:String);
begin
  Log(vCommands,SLogChangeDir,[ADir]);
  if not SysUtils.SetCurrentDir(ADir) then
    Error(SErrChangeDirFailed,[ADir]);
end;


function TPackageHandler.PackageBuildPath:String;
begin
  if CurrentPackage=nil then
    Result:='.'
  else
    Result:=GlobalOptions.BuildDir+CurrentPackage.Name;
end;

function TPackageHandler.PackageRemoteArchive: String;
begin
  if not assigned(CurrentPackage) then
    Error(SErrNoPackageSpecified);
  if CurrentPackage.ExternalURL<>'' then
    Result:=CurrentPackage.ExternalURL
  else
    Result:=GlobalOptions.RemoteRepository+CurrentPackage.FileName;
end;

function TPackageHandler.PackageLocalArchive: String;
begin
  if not assigned(CurrentPackage) then
    Error(SErrNoPackageSpecified);
  Result:=GlobalOptions.PackagesDir+CurrentPackage.FileName;
end;


function TPackageHandler.PackageManifestFile: String;
begin
  Result:=DefaultManifestFile;
end;


function TPackageHandler.PackageLogPrefix:String;
begin
  if assigned(CurrentPackage) then
    Result:='['+CurrentPackage.Name+'] '
  else
//    Result:='[<currentdir>] ';
    Result:='';
end;


Procedure TPackageHandler.Log(Level:TVerbosity; Msg:String);
begin
  pkgglobals.Log(Level,PackageLogPrefix+Msg);
end;


Procedure TPackageHandler.Log(Level:TVerbosity; Fmt:String; const Args:array of const);
begin
  pkgglobals.Log(Level,PackageLogPrefix+Fmt,Args);
end;


Procedure TPackageHandler.Error(Msg:String);
begin
  pkgglobals.Error(PackageLogPrefix+Msg);
end;


Procedure TPackageHandler.Error(Fmt:String; const Args:array of const);
begin
  pkgglobals.Error(PackageLogPrefix+Fmt,Args);
end;


procedure TPackageHandler.ExecuteAction(APackage: TFPPackage; const AAction: string; const Args: TActionArgs=nil);
begin
  pkghandler.ExecuteAction(APackage,AAction,Args);
end;


{ TActionStack }

constructor TActionStack.Create;
begin
  FList:=TFPList.Create;
end;


destructor TActionStack.Destroy;
begin
  FreeAndNil(FList);
end;


procedure TActionStack.Push(APackage:TFPPackage;const AAction:string;const Args:TActionArgs);
var
  ActionItem : PActionStackItem;
begin
  New(ActionItem);
  ActionItem^.ActionPackage:=APackage;
  ActionItem^.Action:=AAction;
  ActionItem^.Args:=Args;
  FList.Add(ActionItem);
end;


procedure TActionStack.Push(APackage:TFPPackage;const AAction:string;const Args:array of string);
var
  ActionArgs : TActionArgs;
  i : integer;
begin
  SetLength(ActionArgs,high(Args)+1);
  for i:=low(Args) to high(Args) do
    ActionArgs[i]:=Args[i];
  Push(APackage,AAction,ActionArgs);
end;


function TActionStack.Pop(out APackage:TFPPackage;out AAction:string;out Args:TActionArgs):boolean;
var
  ActionItem : PActionStackItem;
  Idx : integer;
begin
  Result:=false;
  if FList.Count=0 then
    exit;
  // Retrieve Item from stack
  Idx:=FList.Count-1;
  ActionItem:=PActionStackItem(FList[Idx]);
  FList.Delete(Idx);
  // Copy contents and dispose stack item
  APackage:=ActionItem^.ActionPackage;
  AAction:=ActionItem^.Action;
  Args:=ActionItem^.Args;
  dispose(ActionItem);
  Result:=true;
end;


initialization
  PkgHandlerList:=TFPHashList.Create;
  ExecutedActions:=TFPHashList.Create;
finalization
  FreeAndNil(PkgHandlerList);
  FreeAndNil(ExecutedActions);
end.
