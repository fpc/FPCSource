{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Abstract:
    Extends the FCL Pascal parser for the language subset of pas2js.
}
unit Pas2jsPParser;

{$mode objfpc}{$H+}

{$i pas2js_defines.inc}

interface

uses
  Classes, SysUtils, PParser, PScanner, PasTree, PasResolver, fppas2js,
  Pas2jsLogger;

const // Messages
  nFinalizationNotSupported = 3001;
  sFinalizationNotSupported = 'Finalization section is not supported.';

type

  { TPas2jsPasScanner }

  TPas2jsPasScanner = class(TPascalScanner)
  private
    FCompilerVersion: string;
    FResolver: TPas2JSResolver;
    FTargetPlatform: TPasToJsPlatform;
    FTargetProcessor: TPasToJsProcessor;
  protected
    function HandleInclude(const Param: String): TToken; override;
  public
    property CompilerVersion: string read FCompilerVersion write FCompilerVersion;
    property Resolver: TPas2JSResolver read FResolver write FResolver;
    property TargetPlatform: TPasToJsPlatform read FTargetPlatform write FTargetPlatform;
    property TargetProcessor: TPasToJsProcessor read FTargetProcessor write FTargetProcessor;
  end;

  { TPas2jsPasParser }

  TPas2jsPasParser = class(TPasParser)
  private
    FLog: TPas2jsLogger;
  public
    constructor Create(AScanner: TPascalScanner;
      AFileResolver: TBaseFileResolver; AEngine: TPasTreeContainer); reintroduce;
    procedure RaiseParserError(MsgNumber: integer;
      Args: array of {$IFDEF Pas2JS}jsvalue{$ELSE}const{$ENDIF});
    procedure ParseSubModule(var Module: TPasModule);
    property Log: TPas2jsLogger read FLog write FLog;
  end;

  TOnFindModule = function(const AUnitName, InFilename: String; NameExpr,
      InFileExpr: TPasExpr): TPasModule of object;
  TOnCheckSrcName = procedure(const aElement: TPasElement) of object;

  { TPas2jsCompilerResolver }

  TPas2jsCompilerResolver = class(TPas2JSResolver)
  private
    FLog: TPas2jsLogger;
    FOnCheckSrcName: TOnCheckSrcName;
    FOnFindModule: TOnFindModule;
    FP2JParser: TPas2jsPasParser;
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASrcPos: TPasSourcePos): TPasElement;
      overload; override;
    function FindModule(const aUnitname: String): TPasModule; override;
    function FindUnit(const AName, InFilename: String; NameExpr,
      InFileExpr: TPasExpr): TPasModule; override;
    procedure UsedInterfacesFinished(Section: TPasSection); override;
  public
    Owner: TObject;
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
      {AllowWriteln}
      writeln('Pas2jsPParser.RegisterMessages ',s);
      {AllowWriteln-}
      raise Exception.Create(s);
      end;
    Log.RegisterMsg(MsgType,MsgNumber,MsgPattern);
    LastMsgNumber:=MsgNumber;
  end;

begin
  LastMsgNumber:=-1;
  r(mtError,nFinalizationNotSupported,sFinalizationNotSupported);
end;

{ TPas2jsPasScanner }

function TPas2jsPasScanner.HandleInclude(const Param: String): TToken;

  procedure SetStr(const s: string);
  begin
    Result:=tkString;
    SetCurTokenString(''''+s+'''');
  end;

var
  Year, Month, Day, Hour, Minute, Second, MilliSecond: word;
  i: Integer;
  Scope: TPasScope;
begin
  if (Param<>'') and (Param[1]='%') then
  begin
    case lowercase(Param) of
    '%date%':
      begin
        DecodeDate(Now,Year,Month,Day);
        SetStr('['+IntToStr(Year)+'/'+IntToStr(Month)+'/'+IntToStr(Day)+']');
        exit;
      end;
    '%time%':
      begin
        DecodeTime(Now,Hour,Minute,Second,MilliSecond);
        SetStr(Format('%2d:%2d:%2d',[Hour,Minute,Second]));
        exit;
      end;
    '%pas2jstarget%','%fpctarget%',
    '%pas2jstargetos%','%fpctargetos%':
      begin
        SetStr(PasToJsPlatformNames[TargetPlatform]);
        exit;
      end;
    '%pas2jstargetcpu%','%fpctargetcpu%':
      begin
        SetStr(PasToJsProcessorNames[TargetProcessor]);
        exit;
      end;
    '%pas2jsversion%','%fpcversion%':
      begin
        SetStr(CompilerVersion);
        exit;
      end;
    '%line%':
      begin
        SetStr(IntToStr(CurRow));
        exit;
      end;
    '%currentroutine%':
      begin
        if Resolver<>nil then
          for i:=Resolver.ScopeCount-1 downto 0 do
          begin
            Scope:=Resolver.Scopes[i];
            if (Scope.Element is TPasProcedure)
                and (Scope.Element.Name<>'') then
            begin
              SetStr(Scope.Element.Name);
              exit;
            end;
          end;
        SetStr('<anonymous>');
        exit;
      end;
    else
      DoLog(mtWarning,nWarnIllegalCompilerDirectiveX,SWarnIllegalCompilerDirectiveX,
        ['$i '+Param]);
    end;
  end;
  Result:=inherited HandleInclude(Param);
end;

{ TPas2jsPasParser }

constructor TPas2jsPasParser.Create(AScanner: TPascalScanner;
  AFileResolver: TBaseFileResolver; AEngine: TPasTreeContainer);
begin
  inherited Create(AScanner,AFileResolver,AEngine);
  Options:=Options+po_pas2js;
end;

procedure TPas2jsPasParser.RaiseParserError(MsgNumber: integer;
  Args: array of {$IFDEF Pas2JS}jsvalue{$ELSE}const{$ENDIF});
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
    CheckToken(tkUnit);
  end;
end;

{ TPas2jsCompilerResolver }

function TPas2jsCompilerResolver.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASrcPos: TPasSourcePos): TPasElement;
begin
  if AClass=TFinalizationSection then
    (CurrentParser as TPas2jsPasParser).RaiseParserError(nFinalizationNotSupported,[]);
  Result:=inherited CreateElement(AClass,AName,AParent,AVisibility,ASrcPos);
  if (Result is TPasModule) then
    OnCheckSrcName(Result);
end;

function TPas2jsCompilerResolver.FindModule(const aUnitname: String): TPasModule;
begin
  raise EPasResolve.Create('Call TPas2jsCompilerResolver.FindModule(name,expr,...) instead');
  Result:=nil;
  if aUnitname='' then ;
end;

function TPas2jsCompilerResolver.FindUnit(const AName, InFilename: String;
  NameExpr, InFileExpr: TPasExpr): TPasModule;
begin
  Result:=OnFindModule(AName,InFilename,NameExpr,InFileExpr);
end;

procedure TPas2jsCompilerResolver.UsedInterfacesFinished(Section: TPasSection);
begin
  if Section=nil then ;
end;

end.

