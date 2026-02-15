{
  This program generates a PDF report. The last page should have a page header.
  Originally part of #35095, included here as demo. Needs LiberationSerif font.
}

program bugdemo;

uses dynlibs, Classes, Contnrs, fpreportcontnr, fpreport, fpttf, fpreportpdfexport, fppdf, SysUtils;

type
  TPerson = class(TPersistent)
    private
      FName,
      FCity: String;
    published
      property Name: String read FName write FName;
      property City: String read FCity write FCity;
  end;

var
  OL: TFPObjectList;
  rpt: TFPReport;
  repdat: TFPReportObjectListData;
  p: TFPReportPage;
  PageHeaderBand: TFPReportPageHeaderBand;
  DataBand: TFPReportDataBand;
  Memo: TFPReportMemo;
  RptExporter: TFPReportExportPDF;

procedure CreateData;
var
  P: TPerson;
  i: Integer;
begin
  OL := TFPObjectList.Create;
  OL.OwnsObjects := true;
  for i := 1 to 70 do
  begin
    P := TPerson.Create;
    P.Name := 'John #' + IntToStr(i);
    P.City := 'Somewhere';
    OL.Add(P);
  end;
end;

begin
  CreateData;

  gTTFontCache.ReadStandardFonts;
  PaperManager.RegisterStandardSizes;
  rpt := TFPReport.Create(nil);
  rpt.TwoPass := true;

  repdat := TFPReportObjectListData.Create(nil);
  repdat.List := OL;
  repdat.OwnsList := true;

  p := TFPReportPage.Create(rpt);
  p.Orientation := poPortrait;
  p.PageSize.PaperName := 'A4';

  { page margins }
  p.Margins.Left := 30;
  p.Margins.Top := 20;
  p.Margins.Right := 30;
  p.Margins.Bottom := 20;

  p.Font.Name := 'LiberationSerif';

  p.Data := repdat;

  PageHeaderBand := TFPReportPageHeaderBand.Create(p);
  PageHeaderBand.Layout.Height := 40;
  PageHeaderBand.VisibleOnPage := vpLastOnly;

  Memo := TFPReportMemo.Create(PageHeaderBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 140;
  Memo.Layout.Height := 15;
  Memo.Text := 'Bug Demo - this band should appear on last page only (vpLastOnly)';
  Memo.TextAlignment.Horizontal := taCentered;

  DataBand := TFPReportDataBand.Create(p);
  DataBand.Layout.Height := 10;
  DataBand.Data := repdat;

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 30;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 100;
  Memo.Layout.Height := 5;
  Memo.Text := 'Person: [name], [city]';

  rpt.RunReport;

  RptExporter := TFPReportExportPDF.Create(nil);
  rpt.RenderReport(RptExporter);

  RptExporter.Free;
  repdat.Free;
  rpt.Free;
end.

