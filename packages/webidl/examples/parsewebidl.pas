program parsewebidl;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, webidlparser, webidlscanner,webidldefs;

ResourceString
  SErrNeedInputFiles = 'Need one or more input files';

type

  { TParseWebIDLApplication }

  TParseWebIDLApplication = class(TCustomApplication)
  private
    FContext : TWebIDLContext;
    procedure ParseWebIDL(const AFileName: String);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(Const Msg : String); virtual;
  end;


{ TParseWebIDLApplication }

procedure TParseWebIDLApplication.ParseWebIDL(Const AFileName : String);

Var
  F : TFileStream;
  P : TWebIDLParser;
  S : TWebIDLScanner;
  I : Integer;

begin
  FreeAndNil(FContext);
  FContext:=TWebIDLContext.Create;
  P:=Nil;
  S:=Nil;
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    S:=TWebIDLScanner.Create(F);
    P:=TWebIDLParser.Create(FContext,S);
    P.Version:=v2;
    P.Parse;
    Writeln('// Contents of '+AFileName);
    For I:=0 to FConText.Definitions.Count-1 do
      begin
      Writeln('// Definition ',I+1:3,': ',FConText.Definitions[i].ClassName);
      Writeln(FConText.Definitions[i].AsString(True)+';');
      end;
  finally
    F.Free;
    P.Free;
    S.Free;
  end;
end;


procedure TParseWebIDLApplication.DoRun;

var
  FN,ErrorMsg: UTF8String;
  NoF : TStringArray;

begin
  Terminate;
  ErrorMsg:=CheckOptions('hi:', ['help','input']);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    WriteHelp(ErrorMsg);
    Exit;
    end;
  FN:=GetOptionValue('i','input');
  if FN='' then
    NoF:=GetNonOptions('hi:', ['help','input'])
  else
    begin
    SetLength(NOF,1);
    NOF[0]:=FN;
    end;
  if Length(Nof)=0 then
    WriteHelp(SErrNeedInputFiles);
  For FN in NoF do
    ParseWebIDL(FN);
end;

constructor TParseWebIDLApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FContext:=TWebIDLContext.Create;
end;

destructor TParseWebIDLApplication.Destroy;
begin
  FreeAndNil(FContext);
  inherited Destroy;
end;

procedure TParseWebIDLApplication.WriteHelp(Const Msg : String);
begin
  if Msg<>'' then
    Writeln('Error : ',Msg);
  writeln('Usage: ', ExeName, ' -h');
  ExitCode:=Ord(Msg<>'');
end;

var
  Application: TParseWebIDLApplication;
begin
  Application:=TParseWebIDLApplication.Create(nil);
  Application.Title:='Parse WEB IDL Application';
  Application.Run;
  Application.Free;
end.

