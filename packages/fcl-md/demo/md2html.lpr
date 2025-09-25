program md2html;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Markdown.Processors, MarkDown.Elements, Markdown.HtmlRender, MarkDown.Parser;

type

  { TMD2HTMLApplication }

  TMD2HTMLApplication = class(TCustomApplication)
  const
    ShortOptions = 'hfi:o:H:t:';
    LongOptions : Array of String = ('help','full','input:','output:','head:','title:');
  private
    FHead : TStrings;
    FOutput : String;
    FInputs : Array of string;
    FHTMLTitle : String;
    FFull : Boolean;
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
    Result:=lDir+ChangeFileExt(ExtractFileName(aInput),'.html');
    end
  else if FOutput<>'' then
    Result:=FOutput
  else
    Result:=ChangeFileExt(aInput,'.html');
  Writeln(aInput,' -> ',Result);
end;

procedure TMD2HTMLApplication.ConvertMarkDown(const aInput,aOutput : string);
var
  lRenderer : TMarkDownHTMLRenderer;
  lParser : TMarkDownParser;
  lDoc : TMarkDownDocument;
  lMarkDown,lHTML : TStrings;

begin
  lParser:=Nil;
  lDoc:=Nil;
  lMarkDown:=TStringList.Create;
  try
    lMarkDown.LoadFromFile(aInput);
    lParser:=TMarkDownParser.Create(Self);
    lDoc:=lParser.Parse(lMarkDown);
    lRenderer:=TMarkDownHTMLRenderer.Create(Self);
    if FFull then
      begin
      lRenderer.Options:=[hoEnvelope,hoHead];
      lRenderer.Title:=FHTMLTitle;
      if assigned(FHead) then
        lRenderer.Head:=FHead;
      end;
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
var
  lFileName : string;
begin
  Result:=False;
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
  FFull:=HasOption('f','full');
  FHTMLTitle:=GetOptionValue('t','title');
  lFileName:=GetOptionValue('H','head');
  if (lFilename<>'') then
    begin
    if not FileExists(lFileName) then
      begin
      Usage('Head matter file does not exist: '+lFileName);
      exit;
      end;
    FHead:=TStringList.Create;
    FHead.LoadFromFile(lFileName);
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
  FreeAndNil(FHead);
  inherited Destroy;
end;

procedure TMD2HTMLApplication.Usage(const ErrMsg : string);
begin
  if ErrMsg<>'' then
    Writeln('Error: ',ErrMsg);
  Writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help         this message');
  Writeln('-i --input=FILE   Input markdown file');
  Writeln('-o --output=FILE  Output HTML file');
  Writeln('-f --full         Output complete HTML file (html/body/head tags))');
  Writeln('-H --head=FILE    head tag content file');
  ExitCode:=Ord(ErrMsg<>'');
end;

var
  Application: TMD2HTMLApplication;
begin
  Application:=TMD2HTMLApplication.Create(nil);
  Application.Title:='Markdown to HTML converter';
  Application.Run;
  Application.Free;
end.

