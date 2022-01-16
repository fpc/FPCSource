unit rptgrouping2;

{$mode objfpc}{$H+}
{$I demos.inc}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  strutils,
  udapp;

type

  { TGrouping2Demo }

  TGrouping2Demo = class(TReportDemoApp)
  private
    lReportData: TFPReportUserData;
    sl: TStringList;
    procedure   GetReportDataFirst(Sender: TObject);
    procedure   GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);
    procedure   GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
    procedure GetReportFieldKind(Sender: TObject; aName: String; var AKind: TFPReportFieldKind);
    procedure   GetReportFieldNames(Sender: TObject; List: TStrings);
  Protected
    procedure   InitialiseData; override;
    procedure   CreateReportDesign;override;
    procedure   LoadDesignFromFile(const AFilename: string);
    procedure   HookupData(const AComponentName: string; const AData: TFPReportData);
  public
    constructor Create(AOWner :TComponent); override;
    destructor  Destroy; override;
    Class function Description : string; override;
  end;


implementation

uses
  fpReportStreamer,
  fpTTF,
  fpJSON,
  jsonparser,
  fpexprpars;

{ TGrouping2Demo }

procedure TGrouping2Demo.GetReportDataFirst(Sender: TObject);
begin
  {$IFDEF gdebug}
  writeln('GetReportDataFirst');
  {$ENDIF}
end;

procedure TGrouping2Demo.GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);

Var
  W : Integer;
  S : String;

begin
  {$IFDEF gdebug}
  writeln(Format('GetReportDataValue - %d', [lReportData.RecNo]));
  {$ENDIF}
  W:=0;
  Case LowerCase(AValueName) of
    'continent' : W:=1;
    'region' : W:=2;
    'country' : W:=3;
    'iso' : W:=4;
    'population' : W:=5;
  end;
  If W=0 then
    AValue:=''
  else
    begin  
    S:=ExtractWord(W,sl[lReportData.RecNo-1],[';']);
    if W=5 then
      AValue:=StrToIntDef(S,5)
    else
      AValue:=S;
    end;    
end;

procedure TGrouping2Demo.GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
begin
  {$IFDEF gdebug}
  writeln(Format('GetReportDataEOF - %d', [lReportData.RecNo]));
  {$ENDIF}
  if lReportData.RecNo > sl.Count then
    IsEOF := True
  else
    IsEOF := False;
end;

procedure TGrouping2Demo.GetReportFieldKind(Sender: TObject; aName: String; var AKind: TFPReportFieldKind);
begin
  if CompareText('population',aname)=0 then
    aKind:=rfkInteger;
end;

procedure TGrouping2Demo.GetReportFieldNames(Sender: TObject; List: TStrings);
begin
  {$IFDEF gdebug}
  writeln('********** GetReportFieldNames');
  {$ENDIF}
  List.Add('continent');
  List.Add('region');
  List.Add('country');
  List.Add('ISO');
  List.Add('population');
end;

procedure TGrouping2Demo.InitialiseData;
begin
  sl := TStringList.Create;
  {$I countries2.inc}
  sl.Sort;
end;

procedure TGrouping2Demo.CreateReportDesign;

var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  GroupHeader: TFPReportGroupHeaderBand;
  Memo: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;
  GroupFooter: TFPReportGroupFooterBand;

begin
  Inherited;
  rpt.Author := 'Graeme Geldenhuys';
  rpt.Title := 'FPReport Demo 3 - Grouping';

  {*** page ***}
  p :=  TFPReportPage.Create(rpt);
  p.Orientation := poPortrait;
  p.PageSize.PaperName := 'A4';

  { page margins }
  p.Margins.Left := 30;
  p.Margins.Top := 20;
  p.Margins.Right := 30;
  p.Margins.Bottom := 20;
  p.Data := lReportData;
  p.Font.Name := 'LiberationSans';


  {*** title ***}
  TitleBand := TFPReportTitleBand.Create(p);
  TitleBand.Layout.Height := 40;
  {$IFDEF ColorBands}
  TitleBand.Frame.Shape := fsRectangle;
  TitleBand.Frame.BackgroundColor := clReportTitleSummary;
  {$ENDIF}

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 20;
  Memo.Layout.Width := p.PageSize.Width - p.Margins.Left - p.Margins.Right;
  Memo.Layout.Height := 10;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Text := 'COUNTRY AND POPULATION AS OF 2014';

  GroupHeader := TFPReportGroupHeaderBand.Create(p);
  GroupHeader.Layout.Height := 15;
  GroupHeader.GroupCondition := 'data.continent';
  {$ifdef ColorBands}
  GroupHeader.Frame.Shape := fsRectangle;
  GroupHeader.Frame.BackgroundColor := clGroupHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(GroupHeader);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 3;
  Memo.Layout.Width := 10;
  Memo.Layout.Height := 8;
  Memo.UseParentFont := False;
  Memo.Text := '[data.continent]';
  Memo.Font.Size := 16;


  {*** detail ***}
  DataBand := TFPReportDataBand.Create(p);
  DataBand.Layout.Height := 8;
  {$ifdef ColorBands}
  DataBand.Frame.Shape := fsRectangle;
  DataBand.Frame.BackgroundColor := clDataBand;
  {$endif}
  //DataBand.VisibleExpr := 'StrToFloat(''[population]'') > 50000000';

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 15;
  Memo.Layout.Top := 2;
  Memo.Layout.Width := 45;
  Memo.Layout.Height := 5;
  Memo.Text := '[data.country]';

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 55;
  Memo.Layout.Top := 2;
  Memo.Layout.Width := 25;
  Memo.Layout.Height := 5;
  Memo.TextAlignment.Horizontal := taRightJustified;
  Memo.Text := '[data.population]';



  {*** group footer ***}
  GroupFooter := TFPReportGroupFooterBand.Create(p);
  GroupFooter.Layout.Height := 15;
  GroupFooter.GroupHeader := GroupHeader;
  {$ifdef ColorBands}
  GroupFooter.Frame.Shape := fsRectangle;
  GroupFooter.Frame.BackgroundColor := clGroupHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(GroupFooter);
  Memo.Layout.Left := 25;
  Memo.Layout.Top := 5;
  Memo.Layout.Width := 100;
  Memo.Layout.Height := 8;
  Memo.UseParentFont := False;
  Memo.TextAlignment.Horizontal := taRightJustified;
  Memo.Text := 'Total for [data.continent]: [FormatFloat(''#,###0.00'',sum(data.population/1000000))] million.';
//  Memo.Options:=Memo.Options+[moNoResetAggregateOnPrint];
  Memo.Font.Size := 16;


  {*** page footer ***}
  PageFooter := TFPReportPageFooterBand.Create(p);
  PageFooter.Layout.Height := 20;
  {$ifdef ColorBands}
  PageFooter.Frame.Shape := fsRectangle;
  PageFooter.Frame.BackgroundColor := clPageHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(PageFooter);
  Memo.Layout.Left := 100;
  Memo.Layout.Top := 13;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := 'Page [PageNo] of [PAGECOUNT]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taRightJustified;

end;

procedure TGrouping2Demo.LoadDesignFromFile(const AFilename: string);
var
  rs: TFPReportJSONStreamer;
  fs: TFileStream;
  lJSON: TJSONObject;
begin
  if AFilename = '' then
    Exit;
  if not FileExists(AFilename) then
    raise Exception.CreateFmt('The file "%s" can not be found', [AFilename]);

  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    lJSON := TJSONObject(GetJSON(fs));
  finally
    fs.Free;
  end;

  rs := TFPReportJSONStreamer.Create(nil);
  rs.JSON := lJSON; // rs takes ownership of lJSON
  try
    rpt.ReadElement(rs);
  finally
    rs.Free;
  end;
end;

procedure TGrouping2Demo.HookupData(const AComponentName: string; const AData: TFPReportData);
var
  b: TFPReportCustomBandWithData;
begin
  b := TFPReportCustomBandWithData(rpt.FindRecursive(AComponentName));
  if Assigned(b) then
    b.Data := AData;
end;

constructor TGrouping2Demo.Create(AOwner: TComponent);
begin
  inherited;
  lReportData := TFPReportUserData.Create(nil);
  lReportData.Name:='Data';
  lReportData.OnGetValue := @GetReportDataValue;
  lReportData.OnGetEOF := @GetReportDataEOF;
  lReportData.OnFirst := @GetReportDataFirst;
  lReportData.OnGetNames := @GetReportFieldNames;
  lReportData.OnGetFieldKind:=@GetReportFieldKind;
end;

destructor TGrouping2Demo.Destroy;
begin
  FreeAndNil(lReportData);
  FreeAndNil(sl);
  inherited Destroy;
end;

class function TGrouping2Demo.Description: string;
begin
  Result:='Demo showing grouping and totals';
end;

end.

