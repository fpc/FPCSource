unit fpttf_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$ifdef fptest}
  ,TestFramework
  {$else}
  ,fpcunit, testregistry
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

resourcestring
  cErrFontCountWrong =   ' - make sure you only have the 4 test fonts in the "fonts" directory.';

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
  { regular should be the default flag set }
  AssertEquals('Failed on 1', True, CI.IsRegular);
end;

procedure TFPFontCacheItemTest.TestIsBold;
begin
  AssertEquals('Failed on 1', False, CI.IsBold);
end;

procedure TFPFontCacheItemTest.TestIsItalic;
begin
  AssertEquals('Failed on 1', False, CI.IsItalic);
end;

procedure TFPFontCacheItemTest.TestIsFixedWidth;
begin
  AssertEquals('Failed on 1', False, CI.IsFixedWidth);
end;

procedure TFPFontCacheItemTest.TestRegularVsFixedWidth;
begin
  AssertEquals('Failed on 1', True, CI.IsRegular);
  AssertEquals('Failed on 2', False, CI.IsFixedWidth);
end;

procedure TFPFontCacheItemTest.TestFileName;
begin
  AssertTrue('Failed on 1', CI.FileName <> '');
  { FileName is a non-existing file though, so FontData should be nil }
  AssertTrue('Failed on 2', CI.FontData = nil);
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
  AssertEquals('Failed on 1', 0, FC.Count);
  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  AssertEquals('Failed on 2', 0, FC.Count);
  FC.BuildFontCache;
  AssertEquals('Failed on 3' + cErrFontCountWrong, 4, FC.Count);
end;

procedure TFPFontCacheListTest.TestBuildFontCache;
begin
  AssertEquals('Failed on 1', 0, FC.Count);
  try
    FC.BuildFontCache;
    Fail('Failed on 2. We don''t have font paths, so BuildFontCache shouldn''t run.');
  except
    on e: Exception do
      begin
        AssertEquals('Failed on 3', E.ClassName, 'ETTF');
      end;
  end;

  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  AssertEquals('Failed on 4', 0, FC.Count);
  FC.BuildFontCache;
  AssertEquals('Failed on 5' + cErrFontCountWrong, 4, FC.Count);
end;

procedure TFPFontCacheListTest.TestClear;
begin
  AssertEquals('Failed on 1', 0, FC.Count);
  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  FC.BuildFontCache;
  AssertEquals('Failed on 2', 4, FC.Count);
  FC.Clear;
  AssertEquals('Failed on 3', 0, FC.Count);
end;

procedure TFPFontCacheListTest.TestFind_FamilyName;
var
  lCI: TFPFontCacheItem;
begin
  lCI := nil;
  AssertEquals('Failed on 1', 0, FC.Count);
  lCI := FC.Find('Ubuntu');
  AssertTrue('Failed on 2', lCI = nil);
  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  FC.BuildFontCache;
  AssertEquals('Failed on 3' + cErrFontCountWrong, 4, FC.Count);
  lCI := FC.Find('Ubuntu');
  AssertTrue('Failed on 4', Assigned(lCI));

  { TODO: We should try and extend this to make font paths user configure
           thus the tests could be more flexible. }

  lCI := FC.Find('Ubuntu', True); // bold font
  AssertTrue('Failed on 5', lCI = nil);
  lCI := FC.Find('Ubuntu', False, True); // italic font
  AssertTrue('Failed on 6', lCI = nil);
  lCI := FC.Find('Ubuntu', True, True); // bold+italic font
  AssertTrue('Failed on 7', lCI = nil);

  lCI := FC.Find('DejaVu Sans');
  AssertTrue('Failed on 8', Assigned(lCI));
  lCI := FC.Find('DejaVu Sans Bold');
  AssertTrue('Failed on 9', lCI = nil);
end;


initialization
  RegisterTest({$ifdef fptest}'fpTTF', {$endif}TFPFontCacheItemTest{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpTTF', {$endif}TFPFontCacheListTest{$ifdef fptest}.Suite{$endif});

end.

