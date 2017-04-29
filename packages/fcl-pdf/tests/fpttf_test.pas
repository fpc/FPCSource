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
  ,fpparsettf
  ;

type

  TMyTestFPFontCacheItem = class(TFPFontCacheItem)
  protected
    FFileInfo: TTFFileInfo;
  end;


  TFPFontCacheItemTest = class(TTestCase)
  private
    FCacheItem: TMyTestFPFontCacheItem;
    procedure SetupRealFont;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    property CI: TMyTestFPFontCacheItem read FCacheItem;
  published
    procedure TestIsRegularCantFind;
    procedure TestIsBoldCantFind;
    procedure TestIsItalicCantFind;
    procedure TestIsFixedWidthCantFind;
    procedure TestFileInfoCantFind;
    procedure TestIsRegular;
    procedure TestIsBold;
    procedure TestIsItalic;
    procedure TestIsFixedWidth;
    procedure TestRegularVsFixedWidth;
    procedure TestFileName;
    procedure TestFontInfoAfterCreate;
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
    procedure TestBuildFontCache_tests_for_bug;
    procedure TestClear;
    procedure TestFind_FamilyName;
    procedure TestFind_PostscriptName;
    procedure TestAssignFontList;
    procedure TestLoadFromFile;
    procedure TestReadStandardFonts;
  end;

implementation

const
  cFontCount = 5;

resourcestring
  cErrFontCountWrong =   ' - make sure you only have the 5 test fonts in the "fonts" directory.';


{ TFPFontCacheItemTest }

procedure TFPFontCacheItemTest.SetupRealFont;
begin
  FCacheItem.Free;
  FCacheItem := TMyTestFPFontCacheItem.Create('fonts' + PathDelim + 'DejaVuSans.ttf');
end;

procedure TFPFontCacheItemTest.SetUp;
begin
  inherited SetUp;
  FCacheItem := TMyTestFPFontCacheItem.Create('mytest.ttf');
end;

procedure TFPFontCacheItemTest.TearDown;
begin
  FCacheItem.Free;
  inherited TearDown;
end;

procedure TFPFontCacheItemTest.TestIsRegularCantFind;
begin
  try
    AssertFalse(CI.IsRegular);  // this should raise an error
    Fail('Failed on 1');
  except
    on E: Exception do
      begin
        AssertEquals('Failed on 2', 'ETTF', E.ClassName);
        AssertEquals('Failed on 3', 'The font file <mytest.ttf> can''t be found.', E.Message);
      end;
  end;
end;

procedure TFPFontCacheItemTest.TestIsBoldCantFind;
begin
  try
    AssertFalse(CI.IsBold);  // this should raise an error
    Fail('Failed on 1');
  except
    on E: Exception do
      begin
        AssertEquals('Failed on 2', 'ETTF', E.ClassName);
        AssertEquals('Failed on 3', 'The font file <mytest.ttf> can''t be found.', E.Message);
      end;
  end;
end;

procedure TFPFontCacheItemTest.TestIsItalicCantFind;
begin
  try
    AssertFalse(CI.IsItalic);  // this should raise an error
    Fail('Failed on 1');
  except
    on E: Exception do
      begin
        AssertEquals('Failed on 2', 'ETTF', E.ClassName);
        AssertEquals('Failed on 3', 'The font file <mytest.ttf> can''t be found.', E.Message);
      end;
  end;
end;

procedure TFPFontCacheItemTest.TestIsFixedWidthCantFind;
begin
  try
    AssertFalse(CI.IsFixedWidth);  // this should raise an error
    Fail('Failed on 1');
  except
    on E: Exception do
      begin
        AssertEquals('Failed on 2', 'ETTF', E.ClassName);
        AssertEquals('Failed on 3', 'The font file <mytest.ttf> can''t be found.', E.Message);
      end;
  end;end;

procedure TFPFontCacheItemTest.TestFileInfoCantFind;
begin
  try
    AssertFalse(CI.FontData <> nil);  // this should raise an error
    Fail('Failed on 1');
  except
    on E: Exception do
      begin
        AssertEquals('Failed on 2', 'ETTF', E.ClassName);
        AssertEquals('Failed on 3', 'The font file <mytest.ttf> can''t be found.', E.Message);
      end;
  end;
end;

procedure TFPFontCacheItemTest.TestIsRegular;
begin
  SetupRealFont;
  { regular should be the default flag set }
  AssertEquals('Failed on 1', True, CI.IsRegular);
end;

procedure TFPFontCacheItemTest.TestIsBold;
begin
  SetupRealFont;
  AssertEquals('Failed on 1', False, CI.IsBold);
end;

procedure TFPFontCacheItemTest.TestIsItalic;
begin
  SetupRealFont;
  AssertEquals('Failed on 1', False, CI.IsItalic);
end;

procedure TFPFontCacheItemTest.TestIsFixedWidth;
begin
  SetupRealFont;
  AssertEquals('Failed on 1', False, CI.IsFixedWidth);
end;

procedure TFPFontCacheItemTest.TestRegularVsFixedWidth;
begin
  SetupRealFont;
  AssertEquals('Failed on 1', True, CI.IsRegular);
  AssertEquals('Failed on 2', False, CI.IsFixedWidth);
end;

procedure TFPFontCacheItemTest.TestFileName;
begin
  AssertTrue('Failed on 1', CI.FileName <> '');
  { The Filename property doesn't trigger the loading of font info data }
  AssertTrue('Failed on 2', CI.FFileInfo = nil);
end;

procedure TFPFontCacheItemTest.TestFontInfoAfterCreate;
begin
  { Font info isn't loaded in the constructor any more - it is now loaded on demand }
  AssertTrue('Failed on 1', CI.FFileInfo = nil);
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

    lCI := lFC.Find('LiberationSans');
    AssertEquals('Failed on 1', 14684, round(lCI.TextWidth('Country Ppml01', 0.0)));

    lCI := lFC.Find('DejaVuSans');
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

    lCI := lFC.Find('LiberationSans');
    px := 14684 * 10 * 96 / (72 * 2048);  // 95.599px
    AssertEquals('Failed on 1', px, lCI.TextWidth('Country Ppml01', 10.0));
    px := 14684 * 12 * 96 / (72 * 2048);  // 114.7188px
    AssertEquals('Failed on 2', px, lCI.TextWidth('Country Ppml01', 12.0));
    px := 14684 * 24 * 96 / (72 * 2048);  // 229.4375px
    AssertEquals('Failed on 3', px, lCI.TextWidth('Country Ppml01', 24.0));

    lCI := lFC.Find('DejaVuSans');
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
  AssertEquals('Failed on 3' + cErrFontCountWrong, cFontCount, FC.Count);
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
  AssertEquals('Failed on 5' + cErrFontCountWrong, cFontCount, FC.Count);
end;

procedure TFPFontCacheListTest.TestBuildFontCache_tests_for_bug;
begin
  AssertEquals('Failed on 1', 0, FC.Count);
  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'path_doesnt_exist');
  FC.BuildFontCache;
  AssertEquals('Failed on 2', 0, FC.Count);
end;

procedure TFPFontCacheListTest.TestClear;
begin
  AssertEquals('Failed on 1', 0, FC.Count);
  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  FC.BuildFontCache;
  AssertEquals('Failed on 2' + cErrFontCountWrong, cFontCount, FC.Count);
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
  AssertEquals('Failed on 3' + cErrFontCountWrong, cFontCount, FC.Count);
  lCI := FC.Find('Ubuntu');
  AssertTrue('Failed on 4', Assigned(lCI));

  { TODO: We should try and extend this to make font paths user configure
           thus the tests could be more flexible. }

  lCI := FC.Find('Ubuntu', True, False); // bold font
  AssertTrue('Failed on 5', lCI = nil);
  lCI := FC.Find('Ubuntu', False, True); // italic font
  AssertTrue('Failed on 6', lCI = nil);
  lCI := FC.Find('Ubuntu', True, True); // bold+italic font
  AssertTrue('Failed on 7', lCI = nil);

  lCI := FC.Find('DejaVu Sans', False, False);
  AssertTrue('Failed on 8', Assigned(lCI));
  lCI := FC.Find('DejaVu Sans', True, False);
  AssertTrue('Failed on 9', lCI = nil);
end;

procedure TFPFontCacheListTest.TestFind_PostscriptName;
var
  lCI: TFPFontCacheItem;
begin
  lCI := nil;
  AssertEquals('Failed on 1', 0, FC.Count);
  lCI := FC.Find('Ubuntu');
  AssertTrue('Failed on 2', lCI = nil);
  FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
  FC.BuildFontCache;
  AssertEquals('Failed on 3' + cErrFontCountWrong, cFontCount, FC.Count);
  lCI := FC.Find('Ubuntu');
  AssertTrue('Failed on 4', Assigned(lCI));

  { TODO: We should try and extend this to make font paths user configure
           thus the tests could be more flexible. }

  lCI := FC.Find('Ubuntu-Bold');
  AssertTrue('Failed on 5', lCI = nil);
  lCI := FC.Find('Ubuntu-Italic');
  AssertTrue('Failed on 6', lCI = nil);
  lCI := FC.Find('Ubuntu-BoldItalic');
  AssertTrue('Failed on 7', lCI = nil);

  lCI := FC.Find('DejaVuSans');
  AssertTrue('Failed on 8', Assigned(lCI));
  lCI := FC.Find('DejaVuSans-Bold');
  AssertTrue('Failed on 9', lCI = nil);
end;

procedure TFPFontCacheListTest.TestAssignFontList;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    AssertEquals('Failed on 1', 0, FC.Count);
    FC.SearchPath.Add(ExtractFilePath(ParamStr(0)) + 'fonts');
    FC.BuildFontCache;
    AssertEquals('Failed on 2' + cErrFontCountWrong, cFontCount, FC.Count);
    FC.AssignFontList(sl);
    AssertEquals('Failed on 3', cFontCount, sl.Count);
  finally
    sl.Free;
  end;
end;

procedure TFPFontCacheListTest.TestLoadFromFile;
const
  cFontListFile = 'fontlist.txt';
var
  s: string;
  lCI: TFPFontCacheItem;
begin
  s := ExtractFilePath(ParamStr(0)) + cFontListFile;
  AssertEquals('Failed on 1', 0, FC.Count);
  FC.LoadFromFile(s);
  AssertEquals('Failed on 2', 3, FC.Count);

  lCI := FC.Find('DejaVuSans');
  AssertTrue('Failed on 3', Assigned(lCI));
  lCI := nil;

  lCI := FC.Find('FreeSans');
  AssertTrue('Failed on 4', Assigned(lCI));
  lCI := nil;

  lCI := FC.Find('LiberationSans-Italic');
  AssertTrue('Failed on 5', Assigned(lCI));
  lCI := nil;
end;

procedure TFPFontCacheListTest.TestReadStandardFonts;
begin
  AssertEquals('Failed on 1', 0, FC.Count);
  FC.ReadStandardFonts;
  AssertTrue('Failed on 2', FC.Count > 1);
end;


initialization
  RegisterTest({$ifdef fptest}'fpTTF', {$endif}TFPFontCacheItemTest{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpTTF', {$endif}TFPFontCacheListTest{$ifdef fptest}.Suite{$endif});

end.

