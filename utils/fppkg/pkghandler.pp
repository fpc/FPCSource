{$mode objfpc}
{$h+}
unit pkghandler;

interface

uses Classes,SysUtils, fpmktype, pkgropts;

Const
{$ifdef unix}
  ExeExt = '';
{$else unix}
  ExeExt = '.exe';
{$endif unix}

Type
  TVerbosity = (vError,vInfo,vCommands,vDebug);
  TVerbosities = Set of TVerbosity;

  { TActionStack }

  TActionArgs = array of string;

  TActionStackItem = record
    Action : string;
    Args   : TActionArgs;
  end;
  PActionStackItem = ^TActionStackItem;

  TActionStack = class
  private
    FList : TFPList;
  public
    constructor Create;
    destructor Destroy;
    procedure Push(const AAction:string;const Args:TActionArgs);
    procedure Push(const AAction:string;const Args:array of string);
    function  Pop(out AAction:string;out Args:TActionArgs):boolean;
  end;


  { TPackageHandler }

  TPackageHandler = Class(TComponent)
  private
    FBackupFile : Boolean;
    FDefaults   : TPackagerOptions;
  Protected
    Procedure BackupFile(Const FileName : String);
  Public
    Constructor Create(ADefaults:TPackagerOptions);
    Function Execute(const Args:array of string):boolean; virtual; abstract;
    Property BackupFiles : Boolean Read FBackupFile Write FBackupFile;
    Property Defaults:TPackagerOptions Read FDefaults;
  end;
  TPackageHandlerClass = class(TPackageHandler);

  EPackageHandler = Class(EInstallerError);

// Actions/PkgHandler
procedure RegisterPkgHandler(const AAction:string;pkghandlerclass:TPackageHandlerClass);
function GetPkgHandler(const AAction:string):TPackageHandlerClass;

// Logging
Function StringToVerbosity (S : String) : TVerbosity;
Function VerbosityToString (V : TVerbosity): String;
Procedure Log(Level: TVerbosity;Msg : String);
Procedure Log(Level: TVerbosity;Fmt : String; const Args : array of const);
Procedure Error(Msg : String);
Procedure Error(Fmt : String; const Args : array of const);

// Utils
function maybequoted(const s:ansistring):ansistring;

var
  Verbosity : TVerbosities;
  ActionStack : TActionStack;
  

Implementation

uses
  typinfo,
{$ifdef ver2_0}
  contnrs20,
{$else ver2_0}
  contnrs,
{$endif ver2_0}
  pkgmessages;

var
  PkgHandlerList : TFPHashObjectList;

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


function StringToVerbosity(S: String): TVerbosity;
Var
  I : integer;
begin
  I:=GetEnumValue(TypeInfo(TVerbosity),'v'+S);
  If (I<>-1) then
    Result:=TVerbosity(I)
  else
    Raise EPackageHandler.CreateFmt(SErrInvalidVerbosity,[S]);
end;

Function VerbosityToString (V : TVerbosity): String;
begin
  Result:=GetEnumName(TypeInfo(TVerbosity),Integer(V));
  Delete(Result,1,1);// Delete 'v'
end;


procedure Log(Level:TVerbosity;Msg: String);
begin
  if Level in Verbosity then
    Writeln(stdErr,Msg);
end;


Procedure Log(Level:TVerbosity; Fmt:String; const Args:array of const);
begin
  Log(Level,Format(Fmt,Args));
end;


procedure Error(Msg: String);
begin
  Raise EPackageHandler.Create(Msg);
end;


procedure Error(Fmt: String; const Args: array of const);
begin
  Raise EPackageHandler.CreateFmt(Fmt,Args);
end;


function maybequoted(const s:ansistring):ansistring;
const
  {$IFDEF MSWINDOWS}
    FORBIDDEN_CHARS = ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
                       '{', '}', '''', '`', '~'];
  {$ELSE}
    FORBIDDEN_CHARS = ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
                       '{', '}', '''', ':', '\', '`', '~'];
  {$ENDIF}
var
  s1 : ansistring;
  i  : integer;
  quoted : boolean;
begin
  quoted:=false;
  s1:='"';
  for i:=1 to length(s) do
   begin
     case s[i] of
       '"' :
         begin
           quoted:=true;
           s1:=s1+'\"';
         end;
       ' ',
       #128..#255 :
         begin
           quoted:=true;
           s1:=s1+s[i];
         end;
       else begin
         if s[i] in FORBIDDEN_CHARS then
           quoted:=True;
         s1:=s1+s[i];
       end;
     end;
   end;
  if quoted then
    maybequoted:=s1+'"'
  else
    maybequoted:=s;
end;


{ TPackageHandler }

procedure TPackageHandler.BackupFile(const FileName: String);
Var
  BFN : String;
begin
  BFN:=FileName+'.bak';
  If not RenameFile(FileName,BFN) then
    Error(SErrBackupFailed,[FileName,BFN]);
end;

constructor TPackageHandler.Create(ADefaults:TPackagerOptions);
begin
  inherited Create(nil);
  FDefaults:=ADefaults;
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


procedure TActionStack.Push(const AAction:string;const Args:TActionArgs);
var
  ActionItem : PActionStackItem;
begin
  New(ActionItem);
  ActionItem^.Action:=AAction;
  ActionItem^.Args:=Args;
  FList.Add(ActionItem);
end;


procedure TActionStack.Push(const AAction:string;const Args:array of string);
var
  ActionArgs : TActionArgs;
  i : integer;
begin
  SetLength(ActionArgs,high(Args)+1);
  for i:=low(Args) to high(Args) do
    ActionArgs[i]:=Args[i];
  Push(AAction,ActionArgs);
end;


function TActionStack.Pop(out AAction:string;out Args:TActionArgs):boolean;
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
  AAction:=ActionItem^.Action;
  Args:=ActionItem^.Args;
  dispose(ActionItem);
  Result:=true;
end;




initialization
  PkgHandlerList:=TFPHashObjectList.Create(true);
  ActionStack:=TActionStack.Create;
finalization
  FreeAndNil(PkgHandlerList);
  FreeAndNil(ActionStack);
end.
