unit fpttf_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$ifdef fptest}
  ,TestFramework
  {$else}
  ,fpcunit, testutils, testregistry
  {$endif}
  ,fpttf
  ;

type

  TFPFontCacheItemTest = class(TTestCase)
  private
    FCacheItem: TFPFontCacheItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    property CI: TFPFontCacheItem read FCacheItem;
  published
    procedure TestIsRegular;
    procedure TestIsBold;
    procedure TestIsItalic;
    procedure TestIsFixedWidth;
    procedure TestRegularVsFixedWidth;
    procedure TestFileName;
    procedure TestTextWidth_FontUnits;
    procedure TestTextWidth_Pixels;
  end;


  TFPFontCacheListTest = class(TTestCase)
  private
    FFontCacheList: TFPFontCacheList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    property FC: TFPFontCacheList read FFontCacheList;
  published
    procedure TestCount;
    procedure TestBuildFontCache;
    procedure TestClear;
    procedure TestFind_FamilyName;
  end;

implementation

uses
  fpparsettf;

{ TFPFontCacheItemTest }

procedure TFPFontCacheItemTest.SetUp;
begin
  inherited SetUp;
  FCacheItem := TFPFontCacheItem.Create('mytest.ttf');
end;

procedure TFPFontCacheItemTest.TearDown;
begin
  FCacheItem.Free;
  inherited TearDown;
end;

procedure TFPFontCacheItemTest.TestIsRegular;
begin
  CheckEquals(True, CI.IsRegular, 'Failed on 1');
  CI.IsRegular := True;
  CI.IsRegular := True;  // to make sure bitwise masks work correctly
  CheckEquals(True, CI.IsRegular, 'Failed on 2');
  CI.IsItalic := True;
  CheckEquals(True, CI.IsRegular, 'Failed on 3');
  CI.IsRegular := False;
  CheckEquals(False, CI.IsRegular, 'Failed on 4');
  CI.IsRegular := False;  // to make sure bitwise masks work correctly. eg: xor usage
  CheckEquals(False, CI.IsRegular, 'Failed on 5');
end;

procedure TFPFontCacheItemTest.TestIsBold;
begin
  CheckEquals(False, CI.IsBold, 'Failed on 1');
  CI.IsBold := True;
  CI.IsBold := True;  // to make sure bitwise masks work correctly
  CheckEquals(True, CI.IsBold, 'Failed on 2');
  CI.IsBold := True;
  CI.IsItalic := True;
  CheckEquals(True, CI.IsBold, 'Failed on 3');
  CI.IsBold := False;
  CheckEquals(False, CI.IsBold, 'Failed on 4');
  CI.IsBold := False;  // to make sure bitwise masks work correctly. eg: xor usage
  CheckEquals(False, CI.IsBold, 'Failed on 5');
end;

procedure TFPFontCacheItemTest.TestIsItalic;
begin
  CheckEquals(False, CI.IsItalic, 'Failed on 1');
  CI.IsItalic := True;
  CI.IsItalic := True;  // to make sure bitwise masks work correctly
  CheckEquals(True, CI.IsItalic, 'Failed on 2');
  CI.IsBold := True;
  CI.IsItalic := True;
  CheckEquals(True, CI.IsItalic, 'Failed on 3');
  CI.IsItalic := False;
  CheckEquals(False, CI.IsItalic, 'Failed on 4');
  CI.IsItalic := False;  // to make sure bitwise masks work correctly. eg: xor usage
  CheckEquals(False, CI.IsItalic, 'Failed on 5');
end;

procedure TFPFontCacheItemTest.TestIsFixedWidth;
begin
  CheckEquals(False, CI.IsFixedWidth, 'Failed on 1');
  CI.IsFixedWidth := True;
  CheckEquals(True, CI.IsFixedWidth, 'Failed on 2');
  CI.IsFixedWidth := True;  // to make sure bitwise masks work correctly
  CheckEquals(True, CI.IsFixedWidth, 'Failed on 3');
  CI.IsItalic := True;  // changing another bitmask doesn't affect IsFixedWidth
  CheckEquals(True, CI.IsFixedWidth, 'Failed on 4');
  CI.IsFixedWidth := False;
  CheckEquals(False, CI.IsFixedWidth, 'Failed on 5');
  CI.IsFixedWidth := False;  // to make sure bitwise masks work correctly. eg: xor usage
  CheckEquals(False, CI.IsFixedWidth, 'Failed on 6');
end;

procedure TFPFontCacheItemTest.TestRegularVsFixedWidth;
begin
  CheckEquals(True, CI.IsRegular, 'Failed on 1');
  CheckEquals(False, CI.IsFixedWidth, 'Failed on 2');
  CI.IsFixedWidth := True;  // this should toggle IsRegular's value
  CheckEquals(False, CI.IsRegular, 'Failed on 3');
  CheckEquals(True, CI.IsFixedWidth, 'Failed on 4');
  CI.IsRegular := True;  // this should toggle IsFixedWidth's value
  CheckEquals(True, CI.IsRegular, 'Failed on 5');
  CheckEquals(False, CI.IsFixedWidth, 'Failed on 6');
end;

procedure TFPFontCacheItemTest.TestFileName;
begin
  CI.FileName := '';
  try
    CI.GetFontData;
    Fail('Failed on 1. GetFontData should work if FileName is empty.');
  except
    on e: Exception do
      begin
        CheckEquals(E.ClassName, 'ETTF', 'Failed on 2.');
      end;
  end;
end;

procedure TFPFontCacheItemTest.TestTextWidth_FontUnits;
var
  lFC: TFPFontCacheList;
  lCI: TFPFontCacheItem;
begin
  lFC := TFPFontCacheList.Create;
  try
    lFC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
    lFC.BuildFontCache;

    lCI := lFC.Find('Liberation Sans');
    AssertEquals('Failed on 1', 14684, round(lCI.TextWidth('Country Ppml01', 0.0)));

    lCI := lFC.Find('DejaVu Sans');
    AssertEquals('Failed on 2', 16492, round(lCI.TextWidth('Country Ppml01', 0.0)));

    lCI := lFC.Find('Ubuntu'); // 7333 is the raw glyph width, but with kerning it is 7339
    AssertEquals('Failed on 3', 7333, round(lCI.TextWidth('Country Ppml01', 0.0)));
  finally
    lFC.Free;
  end;
end;

procedure TFPFontCacheItemTest.TestTextWidth_Pixels;
var
  lFC: TFPFontCacheList;
  lCI: TFPFontCacheItem;
  px: single;
begin
  lFC := TFPFontCacheList.Create;
  try
    lFC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
    lFC.BuildFontCache;

    lCI := lFC.Find('Liberation Sans');
    px := 14684 * 10 * 96 / (72 * 2048);  // 95.599px
    AssertEquals('Failed on 1', px, lCI.TextWidth('Country Ppml01', 10.0));
    px := 14684 * 12 * 96 / (72 * 2048);  // 114.7188px
    AssertEquals('Failed on 2', px, lCI.TextWidth('Country Ppml01', 12.0));
    px := 14684 * 24 * 96 / (72 * 2048);  // 229.4375px
    AssertEquals('Failed on 3', px, lCI.TextWidth('Country Ppml01', 24.0));

    lCI := lFC.Find('DejaVu Sans');
    px := 16492 * 10 * 96 / (72 * 2048);  // 107.369px
    AssertEquals('Failed on 4', px, lCI.TextWidth('Country Ppml01', 10.0));
    px := 16492 * 12 * 96 / (72 * 2048);  // 128.8438px
    AssertEquals('Failed on 5', px, lCI.TextWidth('Country Ppml01', 12.0));
    px := 16492 * 24 * 96 / (72 * 2048);  // 205.6875px
    AssertEquals('Failed on 6', px, lCI.TextWidth('Country Ppml01', 24.0));

    lCI := lFC.Find('Ubuntu');
    px := 7333 * 10 * 96 / (72 * 1000);  // 97.7733px
    AssertEquals('Failed on 7', px, lCI.TextWidth('Country Ppml01', 10.0));
    px := 7333 * 12 * 96 / (72 * 1000);  // 117.328px
    AssertEquals('Failed on 8', px, lCI.TextWidth('Country Ppml01', 12.0));
    px := 7333 * 24 * 96 / (72 * 1000);  // 234.656px
    AssertEquals('Failed on 9', px, lCI.TextWidth('Country Ppml01', 24.0));
  finally
    lFC.Free;
  end;
end;

{ TFPFontCacheListTest }

procedure TFPFontCacheListTest.SetUp;
begin
  inherited SetUp;
  FFontCacheList := TFPFontCacheList.Create;
end;

procedure TFPFontCacheListTest.TearDown;
begin
  FFontCacheList.Free;
  inherited TearDown;
end;

procedure TFPFontCacheListTest.TestCount;
begin
  CheckEquals(0, FC.Count, 'Failed on 1');
  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  CheckEquals(0, FC.Count, 'Failed on 2');
  FC.BuildFontCache;
  CheckEquals(4, FC.Count, 'Failed on 3');
end;

procedure TFPFontCacheListTest.TestBuildFontCache;
begin
  CheckEquals(0, FC.Count, 'Failed on 1');
  try
    FC.BuildFontCache;
    Fail('Failed on 2. We don''t have font paths, so BuildFontCache shouldn''t run.');
  except
    on e: Exception do
      begin
        CheckEquals(E.ClassName, 'ETTF', 'Failed on 3.');
      end;
  end;

  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  CheckEquals(0, FC.Count, 'Failed on 4');
  FC.BuildFontCache;
  CheckEquals(4, FC.Count, 'Failed on 5');
end;

procedure TFPFontCacheListTest.TestClear;
begin
  CheckEquals(0, FC.Count, 'Failed on 1');
  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  FC.BuildFontCache;
  CheckEquals(4, FC.Count, 'Failed on 2');
  FC.Clear;
  CheckEquals(0, FC.Count, 'Failed on 3');
end;

procedure TFPFontCacheListTest.TestFind_FamilyName;
var
  lCI: TFPFontCacheItem;
begin
  lCI := nil;
  CheckEquals(0, FC.Count, 'Failed on 1');
  lCI := FC.Find('Ubuntu');
  CheckTrue(lCI = nil, 'Failed on 2');
  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  FC.BuildFontCache;
  CheckEquals(4, FC.Count, 'Failed on 3');
  lCI := FC.Find('Ubuntu');
  CheckTrue(Assigned(lCI), 'Failed on 4');

  { TODO: We should try and extend this to make font paths user configure
           thus the tests could be more flexible. }

  lCI := FC.Find('Ubuntu', True); // bold font
  CheckTrue(lCI = nil, 'Failed on 5');
  lCI := FC.Find('Ubuntu', False, True); // italic font
  CheckTrue(lCI = nil, 'Failed on 6');
  lCI := FC.Find('Ubuntu', True, True); // bold+italic font
  CheckTrue(lCI = nil, 'Failed on 7');

  lCI := FC.Find('DejaVu Sans');
  CheckTrue(Assigned(lCI), 'Failed on 8');
  lCI := FC.Find('DejaVu Sans Bold');
  CheckTrue(lCI = nil, 'Failed on 9');
end;


initialization
  RegisterTest(TFPFontCacheItemTest{$ifdef fptest}.Suite{$endif});
  RegisterTest(TFPFontCacheListTest{$ifdef fptest}.Suite{$endif});

end.

