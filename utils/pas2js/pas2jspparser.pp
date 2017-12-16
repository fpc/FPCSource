{ Author: Mattias Gaertner  2017  mattias@freepascal.org

  Abstract:
    Extends the FCL Pascal parser for the language subset of pas2js.
}
unit Pas2jsPParser;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, PParser, PScanner, PasTree, PasResolver, fppas2js,
  Pas2jsLogger;

const // Messages
  nFinalizationNotSupported = 3001;
  sFinalizationNotSupported = 'Finalization section is not supported.';

type

  { TPas2jsPasParser }

  TPas2jsPasParser = class(TPasParser)
  private
    FLog: TPas2jsLogger;
  public
    constructor Create(AScanner: TPascalScanner;
      AFileResolver: TBaseFileResolver; AEngine: TPasTreeContainer);
    procedure SetLastMsg(MsgType: TMessageType; MsgNumber: integer;
      Const Fmt : String; Args : Array of const);
    procedure RaiseParserError(MsgNumber: integer; Args: array of const);
    procedure ParseSubModule(var Module: TPasModule);
    property Log: TPas2jsLogger read FLog write FLog;
  end;

  TOnFindModule = function(const aUnitname: String): TPasModule of object;
  TOnCheckSrcName = procedure(const aElement: TPasElement) of object;

  { TPas2jsCompilerResolver }

  TPas2jsCompilerResolver = class(TPas2JSResolver)
  private
    FLog: TPas2jsLogger;
    FOnCheckSrcName: TOnCheckSrcName;
    FOnContinueParsing: TNotifyEvent;
    FOnFindModule: TOnFindModule;
    FP2JParser: TPas2jsPasParser;
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASrcPos: TPasSourcePos): TPasElement;
      overload; override;
    function FindModule(const aUnitname: String): TPasModule; override;
    procedure ContinueParsing; override;
  public
    Owner: TObject;
    property OnContinueParsing: TNotifyEvent read FOnContinueParsing write FOnContinueParsing;
    property OnFindModule: TOnFindModule read FOnFindModule write FOnFindModule;
    property OnCheckSrcName: TOnCheckSrcName read FOnCheckSrcName write FOnCheckSrcName;
    property Log: TPas2jsLogger read FLog write FLog;
    property P2JParser: TPas2jsPasParser read FP2JParser write FP2JParser;
  end;

procedure RegisterMessages(Log: TPas2jsLogger);

implementation

procedure RegisterMessages(Log: TPas2jsLogger);
var
  LastMsgNumber: integer;

  procedure r(MsgType: TMessageType; MsgNumber: integer; const MsgPattern: string);
  var
    s: String;
  begin
    if (LastMsgNumber>=0) and (MsgNumber<>LastMsgNumber+1) then
      begin
      s:='gap in registered message numbers: '+IntToStr(LastMsgNumber)+' '+IntToStr(MsgNumber);
      writeln('Pas2jsPParser.RegisterMessages ',s);
      raise Exception.Create(s);
      end;
    Log.RegisterMsg(MsgType,MsgNumber,MsgPattern);
    LastMsgNumber:=MsgNumber;
  end;

begin
  LastMsgNumber:=-1;
  r(mtError,nFinalizationNotSupported,sFinalizationNotSupported);
end;

{ TPas2jsPasParser }

constructor TPas2jsPasParser.Create(AScanner: TPascalScanner;
  AFileResolver: TBaseFileResolver; AEngine: TPasTreeContainer);
begin
  inherited Create(AScanner,AFileResolver,AEngine);
  Options:=Options+[po_asmwhole,po_resolvestandardtypes];
end;

procedure TPas2jsPasParser.SetLastMsg(MsgType: TMessageType;
  MsgNumber: integer; const Fmt: String; Args: array of const);
begin
  inherited SetLastMsg(MsgType,MsgNumber,Fmt,Args);
end;

procedure TPas2jsPasParser.RaiseParserError(MsgNumber: integer; Args: array of const);
var
  Msg: TPas2jsMessage;
begin
  Msg:=Log.FindMsg(MsgNumber,true);
  SetLastMsg(Msg.Typ,MsgNumber,Msg.Pattern,Args);
  raise EParserError.Create(LastMsg,Scanner.CurFilename,
                            Scanner.CurRow,Scanner.CurColumn);
end;

procedure TPas2jsPasParser.ParseSubModule(var Module: TPasModule);
begin
  Module:=nil;
  NextToken;
  SaveComments;
  case CurToken of
  tkUnit:
    ParseUnit(Module);
  tkLibrary:
    ParseLibrary(Module);
  else
    ExpectToken(tkUnit);
  end;
end;

{ TPas2jsCompilerResolver }

function TPas2jsCompilerResolver.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASrcPos: TPasSourcePos): TPasElement;
begin
  if AClass=TFinalizationSection then
    (CurrentParser as TPas2jsPasParser).RaiseParserError(nFinalizationNotSupported,[]);
  Result:=inherited;
  if (Result is TPasModule) then
    OnCheckSrcName(Result);
end;

function TPas2jsCompilerResolver.FindModule(const aUnitname: String): TPasModule;
begin
  Result:=OnFindModule(aUnitname);
end;

procedure TPas2jsCompilerResolver.ContinueParsing;
begin
  OnContinueParsing(Self);
end;

end.

