unit wmreports;

{$mode objfpc}
{$H+}
{$I demos.inc}
interface

uses
  Classes, SysUtils, httpdefs, fphttp, fpweb, fpreport;

Type

  { TGenerateReportModule }

  TGenerateReportModule = class(TCustomHTTPModule)
  Private
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TPageReportModule }

  TPageReportModule = class(TCustomHTTPModule)
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TViewReportModule }

  TViewReportModule = class(TCustomHTTPModule)
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation


uses
  udapp,
  {$IFDEF ExportFPImage}
  fpreportfpimageexport,
  {$ENDIF}
  {$IFDEF ExportHTML}
  fpreporthtmlexport,
  {$ENDIF}
  {$IFDEF ExportPDF}
  fppdf,
  fpreportpdfexport,
  {$ENDIF}
  fpmimetypes;

Var Counter : Integer;

{ TViewReportModule }

procedure TViewReportModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);

Const
  LFN = '/tmp/request.log';

Var
  FN,TFN : String;

begin
  FN:=ARequest.PathInfo;
  if (FN<>'') and (FN[1]='/') then
    Delete(FN,1,1);
  Delete(FN,1,Pos('/',FN)); // Strip /View
  TFN:=GetTempDir+FN;
  With TStringList.Create do
    try
      if FileExists(LFN) then
        LoadFromFile(LFN);
      Add(FN+'='+TFN);
      SaveToFile(LFN);
    finally
      Free;
    end;
  If FileExists(TFN) then
    begin
    AResponse.ContentStream:=TFileStream.Create(GetTempDir+FN,fmOpenRead or fmShareDenyWrite);
    AResponse.FreeContentStream:=True;
    case lowercase(extractfileext(FN)) of
      '.png': AResponse.ContentType:='image/png';
      '.pdf' : AResponse.ContentType:='application/pdf';
      '.html' : AResponse.ContentType:='text/html';
    end;

    end
  else
    begin
    AResponse.Code:=404;
    AResponse.CodeText:='Not found';
    AResponse.Content:='File '+FN+' not found';
    AResponse.SendResponse;
    end;
end;

Type
  { TReportConfigurator }

  TReportConfigurator = Class
  Private
    FStartFileName: String;
    FVars: TStrings;
    Function GetVar(S : String) : String;
    Function GetBool(S : String) : Boolean;
{$IFDEF ExportHTML}
    procedure ConfigHTMLExporter(Exporter: TFPReportExportHTML);
{$ENDIF}
{$IFDEF ExportFPImage}
    procedure ConfigImageExporter(Exporter: TFPReportExportFPImage);
{$ENDIF}
{$IFDEF ExportPDF}
    procedure ConfigPDFExporter(Exporter: TFPReportExportPDF);
{$ENDIF}
  Public
    Constructor Create(AVar : TStrings);
    Procedure ConfigReport(Sender : TObject; Exporter : TFPReportExporter);
    Property StartFileName : String Read FStartFileName Write FStartFileName;
  end;

{ TReportConfigurator }

constructor TReportConfigurator.Create(AVar: TStrings);
begin
  FVars:=AVar;
end;

procedure TReportConfigurator.ConfigReport(Sender: TObject; Exporter: TFPReportExporter);
begin
  {$IFDEF ExportHTML}
  if (Exporter is TFPReportExportHTML) then
    ConfigHTMLExporter(Exporter as TFPReportExportHTML);
  {$ENDIF}
  {$IFDEF ExportFPImage}
  if (Exporter is TFPReportExportFPImage) then
    ConfigImageExporter(Exporter as TFPReportExportfpImage);
  {$ENDIF}
  {$IFDEF ExportPDF}
  if (Exporter is TFPReportExportPDF) then
    ConfigPDFExporter(Exporter as TFPReportExportPDF);
  {$ENDIF}
end;

{$IFDEF ExportHTML}

function TReportConfigurator.GetVar(S: String): String;
begin
  Result:=FVars.Values[S];
end;

function TReportConfigurator.GetBool(S: String): Boolean;

Var
  v : String;

begin
  v:=LowerCase(GetVar(S));
  Result:=(v<>'') and ((v='1') or (v='t') or (v='true') or (v='y') or (v='yes'));
end;

procedure TReportConfigurator.ConfigHTMLExporter(Exporter : TFPReportExportHTML);

begin
  Exporter.Options:=[heoTOCPage];
  StartFileName:='index.html'
end;
{$ENDIF}

{$IFDEF ExportFPImage}
procedure TReportConfigurator.ConfigImageExporter(Exporter : TFPReportExportFPImage);

begin
  Exporter.HTMLOptions:=[hoEnabled,hoTOCPage];
  StartFileName:='index.html'
end;
{$ENDIF}

{$IFDEF ExportPDF}
procedure TReportConfigurator.ConfigPDFExporter(Exporter: TFPReportExportPDF);

Const
  Prefix = 'pdf.';

Var
  O : TPDFOptions;

  Procedure MaybeAdd(aVar : String; aOption: TPDFOption);

  begin
    If GetBool(Prefix+aVar) then
      Include(O,aOption);
  end;

begin
  Exporter.AutoSave:=True;
  O:=[];
  MaybeAdd('pagelayout',poOutLine);
  MaybeAdd('compresstext',poCompressText);
  MaybeAdd('compressfonts',poCompressFonts);
  MaybeAdd('compressimages',poCompressImages);
  MaybeAdd('userawjpeg',poUseRawJPEG);
  MaybeAdd('noembeddedfonts',poNoEmbeddedFonts);
  MaybeAdd('pageoriginattop',poPageOriginAtTop);
  MaybeAdd('pageoriginattop',poSubsetFont);
  Exporter.Options:=O;
  Case GetVar(Prefix+'pagelayout') of
    'two':   Exporter.PageLayout:=lTwo;
    'continuous' : Exporter.PageLayout:=lContinuous;
  else
    Exporter.PageLayout:=lSingle;
  end;
end;
{$ENDIF}

{ TGenerateReportModule }

procedure TGenerateReportModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
Var
  F,D,FN : String;
  Conf : TReportConfigurator;
  Fmt : TRenderFormat;
  FRunner : TReportRunner;
  RC  : TFPReportExporterClass;

begin
  D:=ARequest.ContentFields.Values['demo'];
  if (D='') or (TReportDemoApplication.GetReportClass(D)=Nil) then
    Raise Exception.CreateFmt('Invalid or empty demo name : "%s"',[D]);
  F:=ARequest.ContentFields.Values['format'];
  Fmt:=High(TRenderFormat);
  While (fmt>rfDefault) and (CompareText(TReportDemoApplication.FormatName(fmt),F)<>0) do
    fmt:=Pred(fmt);
  if (fmt=rfDefault) then
    Raise Exception.CreateFmt('Invalid or empty format name : "%s"',[F]);
  FRunner:=TReportRunner.Create(Self);
  FRunner.Location:=ExtractFilePath(ParamStr(0));;
  FRunner.ReportApp:=TReportDemoApplication.GetReportClass(D).Create(Self);
  FRunner.ReportApp.rpt:=TFPReport.Create(FRunner.ReportApp);
  FRunner.Format:=Fmt ;
  FRunner.location:=ExtractFilePath(ParamStr(0));
  RC:=TReportDemoApplication.GetRenderClass(Fmt);
  Inc(Counter);
  FN:=D+IntToStr(Counter);
  FN:=FN+PathDelim+FN+RC.DefaultExtension;
  FRunner.BaseOutputFileName:=GetTempDir+FN;
  Conf:= TReportConfigurator.Create(ARequest.ContentFields);
  Try
    FRunner.OnInitExporter:=@Conf.ConfigReport;
    FRunner.Execute;
    Writeln('Conf.StartFileName : ',Conf.StartFileName);
    if (Conf.StartFileName<>'') then
      FN:=ExtractFilePath(FN)+Conf.StartFileName;
  Finally
    Conf.Free;
  end;
  AResponse.SendRedirect('../View/'+FN);
end;

{ TPageReportModule }

procedure TPageReportModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);

Var
  L,RL : TStrings;
  I : Integer;
  F : TRenderFormat;
  RC : TFPReportExporterClass;

begin
  RL:=Nil;
  L:=TStringList.Create;
  try
    RL:=TStringList.Create;
    L.Add('<HTML><HEAD><TITLE>FPReport web demo</TITLE></HEAD>');
    L.Add('<BODY>');
    L.Add('<H1>Select report and output type</H1>');
    L.Add('<FORM ACTION="../Generate" METHOD=POST>');
    L.Add('Report: ');
    L.Add('<SELECT NAME="demo">');
    //
    TReportDemoApplication.GetRegisteredReports(RL);
    For I:=0 to RL.Count-1 do
      L.Add('<OPTION>'+RL[i]+'</option>');
    L.Add('</SELECT>');
    L.Add('</p>');
    L.Add('Format: ');
    L.Add('<SELECT NAME="format">');
    for F in TRenderFormat do
      begin
      RC:=TReportDemoApplication.GetRenderClass(F);
      if (RC<>Nil) and (RC.DefaultExtension<>'') then
        L.Add('<OPTION>'+TReportDemoApplication.FormatName(F)+'</option>');
      end;
    L.Add('</SELECT>');
    L.Add('</p>');
    L.Add('<INPUT TYPE="Submit" Value="Generate"/>');
    L.Add('</FORM>');
    L.Add('</BODY>');
    L.Add('</HTML>');
    AResponse.Content:=L.Text;
  finally
    L.Free;
  end;
end;

initialization
  TPageReportModule.RegisterModule('Page',True);
  TGenerateReportModule.RegisterModule('Generate',True);
  TViewReportModule.RegisterModule('View',True);
end.

