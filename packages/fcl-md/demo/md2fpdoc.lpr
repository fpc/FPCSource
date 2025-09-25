program md2fpdoc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Markdown.Processors, MarkDown.Elements, Markdown.FPDocRender, MarkDown.Parser;

type

  { TMD2HTMLApplication }

  TMD2HTMLApplication = class(TCustomApplication)
  const
    ShortOptions = 'hi:o:p:q';
    LongOptions : Array of String = ('help','input:','output:','package:','quiet');
  private
    FQuiet : Boolean;
    FOutput : String;
    FInputs : Array of string;
    FPackage : String;
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    procedure ConvertMarkDown(const aInput, aOutput: string);
    function CreateOutputFileName(const aInput: string; isMulti: boolean): string;
  protected
    procedure DoRun; override;
    function ProcessOptions : boolean; virtual;
    procedure Usage(const ErrMsg: string); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMD2HTMLApplication }

function TMD2HTMLApplication.CreateOutputFileName(const aInput : string; isMulti : boolean) : string;
var
  lDir : string;
begin
  if isMulti then
    begin
    lDir:=Foutput;
    if lDir<>'' then
      lDir:=IncludeTrailingPathDelimiter(lDir);
    Result:=lDir+ChangeFileExt(ExtractFileName(aInput),'.xml');
    end
  else if FOutput<>'' then
    Result:=FOutput
  else
    Result:=ChangeFileExt(aInput,'.xml');
end;

procedure TMD2HTMLApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  if FQuiet then
    exit;
  Writeln(StdErr,'[',EventType,'] ',Msg);
end;

procedure TMD2HTMLApplication.ConvertMarkDown(const aInput,aOutput : string);
var
  lRenderer : TMarkDownFPDocRenderer;
  lParser : TMarkDownParser;
  lDoc : TMarkDownDocument;
  lMarkDown,lHTML : TStrings;

begin
  Log(etInfo,'Converting %s to %s',[aInput,aOutput]);
  try
    lParser:=Nil;
    lDoc:=Nil;
    lMarkDown:=TStringList.Create;
    try
      lMarkDown.LoadFromFile(aInput);
      lParser:=TMarkDownParser.Create(Self);
      lDoc:=lParser.Parse(lMarkDown);
      lRenderer:=TMarkDownFPDocRenderer.Create(Self);
      lRenderer.PackageName:=FPackage;
      lHTML:=TStringList.Create;
      lRenderer.RenderDocument(lDoc,lHTML);
      lHTML.SaveToFile(aOutput);
    finally
      lHTML.Free;
      lRenderer.Free;
      lDoc.Free;
      lParser.Free;
      lMarkDown.Free;
    end;
  except
    on E : Exception do
      Log(etError,'Error %s while onverting %s to %s : %s',[E.ClassName,aInput,aOutput,E.Message]);
  end;
end;

procedure TMD2HTMLApplication.DoRun;
var
  ErrorMsg: String;
  lInPut,lOutput : String;
begin
  Terminate;
  ErrorMsg:=CheckOptions(ShortOptions,LongOptions);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  if not ProcessOptions then
    Exit;
  For lInput in FInputs do
    begin
    lOutput:=CreateOutputFileName(lInput,Length(FInputs)>1);
    ConvertMarkDown(lInput,lOutput);
    end;
end;

function TMD2HTMLApplication.ProcessOptions: boolean;

begin
  Result:=False;
  FQuiet:=HasOption('q','quiet');
  FInputs:=GetOptionValues('i','input');
  if Length(FInputs)=0 then
    FInputs:=GetNonOptions(ShortOptions,LongOptions);
  FOutput:=GetOptionValue('o','output');
  if Length(FInputs)>1 then
    If not DirectoryExists(FOutput) then
      begin
      Usage('Directory does not exist or is not a directory: '+Foutput);
      exit;
      end;
  FPackage:=GetOptionValue('p','package');
  if FPackage='' then
    begin
    Usage('A package name is required');
    exit;
    end;
  Result:=True;
end;

constructor TMD2HTMLApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  StopOnException:=True;
end;

destructor TMD2HTMLApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMD2HTMLApplication.Usage(const ErrMsg : string);
begin
  if ErrMsg<>'' then
    begin
    Writeln(StdErr,'Error: ',ErrMsg);
    Flush(StdErr);
    end;
  Writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help         this message.');
  Writeln('-i --input=FILE   Input markdown file.');
  Writeln('-o --output=FILE  Output HTML file.');
  Writeln('-p --package=NAME Set package name.');
  Writeln('-q --quiet        Less messages.');
  ExitCode:=Ord(ErrMsg<>'');
end;

var
  Application: TMD2HTMLApplication;
begin
  Application:=TMD2HTMLApplication.Create(nil);
  Application.Title:='Markdown to FPDoc converter';
  Application.Run;
  Application.Free;
end.

