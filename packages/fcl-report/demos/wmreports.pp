unit wmreports;

{$mode objfpc}{$H+}

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

uses udapp, fpmimetypes;

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

{ TGenerateReportModule }

procedure TGenerateReportModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
Var
  F,D,FN,RFN : String;
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
  FRunner.ReportApp:=TReportDemoApplication.GetReportClass(D).Create(Self);
  FRunner.ReportApp.rpt:=TFPReport.Create(FRunner.ReportApp);
  FRunner.Format:=Fmt ;
  FRunner.location:=ExtractFilePath(ParamStr(0));
  RC:=TReportDemoApplication.GetRenderClass(Fmt);
  Inc(Counter);
  FN:=D+IntToStr(Counter);
  FN:=FN+PathDelim+FN+RC.DefaultExtension;
  if RC.MultiFile then
    FN:=ChangeFileExt(FN,'01'+ExtractFileExt(FN));
  FRunner.BaseOutputFileName:=GetTempDir+FN;
  FRunner.Execute;
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

