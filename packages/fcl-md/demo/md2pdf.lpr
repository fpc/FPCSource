program md2pdf;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cwstring,
  {$ENDIF}
  dynlibs, Classes, SysUtils, CustApp, fppdf, fpttf,
  Markdown.Processors, Markdown.Parser, Markdown.Elements, MarkDown.PDFRender;

type

  { TMD2PDFApplication }

  TMD2PDFApplication = class(TCustomApplication)
  const
    ShortOptions = 'hi:o:f:s:t:';
    LongOptions : Array of String = ('help','input:','output:','font:','size:','title:');
  private
    FOutput : String;
    FInputs : Array of string;
    FFontName : String;
    FTitle : String;
    FBaseFontSize : Integer;
    procedure ConvertMarkDown(const aInput, aOutput: string);
    function CreateOutputFileName(const aInput: string; isMulti: boolean): string;
  protected
    procedure DoRun; override;
    function ProcessOptions : boolean; virtual;
    procedure Usage(const ErrMsg: string); virtual;
  public
    constructor Create(aOwner: TComponent); override;
  end;

{ TMD2PDFApplication }

// Determine the output PDF file name for a given input file
function TMD2PDFApplication.CreateOutputFileName(const aInput : string; isMulti : boolean) : string;
var
  lDir : string;
begin
  if isMulti then
    begin
    lDir:=FOutput;
    if lDir<>'' then
      lDir:=IncludeTrailingPathDelimiter(lDir);
    Result:=lDir+ChangeFileExt(ExtractFileName(aInput),'.pdf');
    end
  else if FOutput<>'' then
    Result:=FOutput
  else
    Result:=ChangeFileExt(aInput,'.pdf');
  Writeln(aInput,' -> ',Result);
end;


// Parse a single markdown file and render it to a PDF file
procedure TMD2PDFApplication.ConvertMarkDown(const aInput,aOutput : string);
var
  lRenderer : TMarkDownPDFRenderer;
  lPDF : TPDFDocument;
  lMarkDown : TStrings;
  lParser : TMarkDownParser;
  lDoc : TMarkDownDocument;
begin
  lRenderer:=Nil;
  lParser:=Nil;
  lDoc:=Nil;
  lMarkDown:=TStringList.Create;
  try
    lMarkDown.LoadFromFile(aInput);
    lPDF:=TPDFDocument.Create(Self);
    try
      // Origin at top, work in pixels to match the renderer layout
      lPDF.Options:=[poPageOriginAtTop];
      lPDF.DefaultUnitOfMeasure:=uomPixels;
      lPDF.Infos.Title:=FTitle;
      lPDF.Infos.Producer:='md2pdf (fcl-md demo)';
      lRenderer:=TMarkDownPDFRenderer.Create(Self);
      if FFontName<>'' then
        lRenderer.FontName:=FFontName;
      if FBaseFontSize>0 then
        lRenderer.BaseFontSize:=FBaseFontSize;
      // Resolve relative image paths against the markdown file's directory
      lRenderer.ImageBaseDir:=ExtractFilePath(ExpandFileName(aInput));
      // Parse the markdown, then render the document into the PDF
      lParser:=TMarkDownParser.Create(Self);
      lDoc:=lParser.Parse(lMarkDown);
      lRenderer.RenderDocument(lDoc,lPDF);
      lPDF.SaveToFile(aOutput);
      Writeln('  pages written: ',lPDF.Pages.Count);
    finally
      lRenderer.Free;
      lDoc.Free;
      lParser.Free;
      lPDF.Free;
    end;
  finally
    lMarkDown.Free;
  end;
end;


procedure TMD2PDFApplication.DoRun;
var
  ErrorMsg: String;
  lInput,lOutput : String;
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
  // The renderer relies on the system font cache to embed real fonts
  gTTFontCache.ReadStandardFonts;
  for lInput in FInputs do
    begin
    lOutput:=CreateOutputFileName(lInput,Length(FInputs)>1);
    ConvertMarkDown(lInput,lOutput);
    end;
end;


// Collect and validate command-line options
function TMD2PDFApplication.ProcessOptions: boolean;
begin
  Result:=False;
  FInputs:=GetOptionValues('i','input');
  if Length(FInputs)=0 then
    FInputs:=GetNonOptions(ShortOptions,LongOptions);
  if Length(FInputs)=0 then
    begin
    Usage('No input file specified');
    Exit;
    end;
  FOutput:=GetOptionValue('o','output');
  if Length(FInputs)>1 then
    if not DirectoryExists(FOutput) then
      begin
      Usage('Directory does not exist or is not a directory: '+FOutput);
      Exit;
      end;
  FFontName:=GetOptionValue('f','font');
  FTitle:=GetOptionValue('t','title');
  FBaseFontSize:=StrToIntDef(GetOptionValue('s','size'),0);
  Result:=True;
end;


// Display program usage and any error message
procedure TMD2PDFApplication.Usage(const ErrMsg : string);
begin
  if ErrMsg<>'' then
    Writeln('Error: ',ErrMsg);
  Writeln('Usage: ', ExeName, ' [options] file1.md [file2.md ...]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help         this message');
  Writeln('-i --input=FILE   Input markdown file (may be repeated)');
  Writeln('-o --output=FILE  Output PDF file, or output directory for multiple inputs');
  Writeln('-f --font=NAME    Font family to use (default: Sans Serif)');
  Writeln('-s --size=N       Base font size in points (default: 10)');
  Writeln('-t --title=TEXT   PDF document title');
  ExitCode:=Ord(ErrMsg<>'');
end;


constructor TMD2PDFApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  StopOnException:=True;
end;


var
  Application: TMD2PDFApplication;

begin
  Application:=TMD2PDFApplication.Create(nil);
  Application.Title:='Markdown to PDF converter';
  Application.Run;
  Application.Free;
end.
