unit fpparsettf_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$ifdef fptest}
  ,TestFramework
  {$else}
  ,fpcunit, testregistry
  {$endif}
  ,fpparsettf
  ;

type
  { so we can access the protected methods }
  TMyTFFileInfo = class(TTFFileInfo);


  TBaseTestParseTTF = class(TTestCase)
  private
    FFileInfo: TMyTFFileInfo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure LoadFont(const AFilename: string);
  public
    property  FI: TMyTFFileInfo read FFileInfo;
  end;


  TTestEmptyParseTTF = class(TBaseTestParseTTF)
  published
    procedure TestEmptyTables;
    procedure TestEmptyDirectory;
  end;


  TTestLiberationFont = class(TBaseTestParseTTF)
  protected
    procedure SetUp; override;
  published
    { Offset Table }
    procedure TestDirectory_FontVersion;
    procedure TestDirectory_NumTables;
    procedure TestDirectory_SearchRange;
    procedure TestDirectory_EntrySelector;
    procedure TestDirectory_RangeShift;

    { THead data structure tests }
    procedure TestHead_FileVersion;
    procedure TestHead_FontRevision;
    procedure TestHead_CheckSumAdjustment;
    procedure TestHead_MagicNumber;
    procedure TestHead_Flags;
    procedure TestHead_UnitsPerEm;
    procedure TestHead_Created;
    procedure TestHead_Modified;
    procedure TestHead_BBox_xMin;
    procedure TestHead_BBox_yMin;
    procedure TestHead_BBox_xMax;
    procedure TestHead_BBox_yMax;
    procedure TestHead_MacStyle;
    procedure TestHead_LowestRecPPEM;
    procedure TestHead_FontDirectionHint;
    procedure TestHead_IndexToLocFormat;
    procedure TestHead_glyphDataFormat;

    { THHead data structure tests }
    procedure TestHHead_TableVersion;
    procedure TestHHead_Ascender;
    procedure TestHHead_Descender;
    procedure TestHHead_LineGap;
    procedure TestHHead_AdvanceWidthMax;
    procedure TestHHead_MinLeftSideBearing;
    procedure TestHHead_MinRightSideBearing;
    procedure TestHHead_XMaxExtent;
    procedure TestHHead_CaretSlopeRise;
    procedure TestHHead_CaretSlopeRun;
    procedure TestHHead_Reserved;
    procedure TestHHead_metricDataFormat;
    procedure TestHHead_numberOfHMetrics;

    { TCmapHeader data structure tests }
    procedure TestCMap_version;
    procedure TestCMap_SubTableCount;

    { TCmapSubTableEntry data structure tests }
    procedure TestCMapSubTables_1;
    procedure TestCMapSubTables_2;
    procedure TestCMapSubTables_3;

    { CmapUnicodeMap data structure tests }
    procedure TestCmapUnicodeMap_Format;
    procedure TestCmapUnicodeMap_Length;
    procedure TestCmapUnicodeMap_SegmentCount2;
    procedure TestCmapUnicodeMap_SearchRange;
    procedure TestCmapUnicodeMap_EntrySelector;
    procedure TestCmapUnicodeMap_RangeShift;

    { TUnicodeMapSegment data structure tests }
    procedure TestUnicodeMapSegment_1_StartCode;
    procedure TestUnicodeMapSegment_1_EndCode;
    procedure TestUnicodeMapSegment_1_IDDelta;
    procedure TestUnicodeMapSegment_1_IDRangeOffset;
    procedure TestUnicodeMapSegment_2_StartCode;
    procedure TestUnicodeMapSegment_2_EndCode;
    procedure TestUnicodeMapSegment_2_IDDelta;
    procedure TestUnicodeMapSegment_2_IDRangeOffset;
    procedure TestUnicodeMapSegment_127_StartCode;
    procedure TestUnicodeMapSegment_127_EndCode;
    procedure TestUnicodeMapSegment_127_IDDelta;
    procedure TestUnicodeMapSegment_127_IDRangeOffset;

    { hmtx - Horizontal Metrics data structure tests }
    procedure TestWidths_Size;
    procedure TestWidths_HorMetric_0_AdvanceWidth;
    procedure TestWidths_HorMetric_0_LSB;
    procedure TestWidths_HorMetric_1_AdvanceWidth;
    procedure TestWidths_HorMetric_1_LSB;
    procedure TestWidths_HorMetric_2586_AdvanceWidth;
    procedure TestWidths_HorMetric_2586_LSB;

    { maxp - Maximum Profile data structure tests }
    procedure TestMaxP_VersionNumber;
    procedure TestMaxP_numGlyphs;
    procedure TestMaxP_maxPoints;
    procedure TestMaxP_maxContours;
    procedure TestMaxP_maxCompositePoints;
    procedure TestMaxP_maxCompositeContours;
    procedure TestMaxP_maxZones;
    procedure TestMaxP_maxTwilightPoints;
    procedure TestMaxP_maxStorage;
    procedure TestMaxP_maxFunctionDefs;
    procedure TestMaxP_maxInstructionDefs;
    procedure TestMaxP_maxStackElements;
    procedure TestMaxP_maxSizeOfInstructions;
    procedure TestMaxP_maxComponentElements;
    procedure TestMaxP_maxComponentDepth;

    { OS2Data - OS/2 Data structure tests }
    procedure TestOS2Data_version;
    procedure TestOS2Data_xAvgCharWidth;
    procedure TestOS2Data_usWeightClass;
    procedure TestOS2Data_usWidthClass;
    procedure TestOS2Data_fsType;
    procedure TestOS2Data_ySubscriptXSize;
    Procedure TestOS2Data_ySubscriptYSize;
    Procedure TestOS2Data_ySubscriptXOffset;
    Procedure TestOS2Data_ySubscriptYOffset;
    Procedure TestOS2Data_ySuperscriptXSize;
    Procedure TestOS2Data_ySuperscriptYSize;
    Procedure TestOS2Data_ySuperscriptXOffset;
    Procedure TestOS2Data_ySuperscriptYOffset;
    Procedure TestOS2Data_yStrikeoutSize;
    Procedure TestOS2Data_yStrikeoutPosition;
    Procedure TestOS2Data_sFamilyClass;
    procedure TestOS2Data_Panose;
    procedure TestOS2Data_ulUnicodeRange1;
    procedure TestOS2Data_ulUnicodeRange2;
    procedure TestOS2Data_ulUnicodeRange3;
    procedure TestOS2Data_ulUnicodeRange4;
    procedure TestOS2Data_achVendID;
    procedure TestOS2Data_fsSelection;
    procedure TestOS2Data_usFirstCharIndex;
    procedure TestOS2Data_usLastCharIndex;
    procedure TestOS2Data_sTypoAscender;
    procedure TestOS2Data_sTypoDescender;
    procedure TestOS2Data_sTypoLineGap;
    procedure TestOS2Data_usWinAscent;
    procedure TestOS2Data_usWinDescent;
    procedure TestOS2Data_ulCodePageRange1;
    procedure TestOS2Data_ulCodePageRange2;
    procedure TestOS2Data_sxHeight;
    procedure TestOS2Data_sCapHeight;
    procedure TestOS2Data_usDefaultChar;
    procedure TestOS2Data_usBreakChar;
    procedure TestOS2Data_usMaxContext;

    { PostScript data structure }
    procedure TestPostScript_Format;
    procedure TestPostScript_ItalicAngle;
    procedure TestPostScript_UnderlinePosition;
    procedure TestPostScript_underlineThickness;
    procedure TestPostScript_isFixedPitch;
    procedure TestPostScript_minMemType42;
    procedure TestPostScript_maxMemType42;
    procedure TestPostScript_minMemType1;
    procedure TestPostScript_maxMemType1;

    { Utility functions }
    procedure TestGetGlyphIndex;
    procedure TestGetAdvanceWidth;
  end;


  TTestLiberationItalicFont = class(TBaseTestParseTTF)
  protected
    procedure SetUp; override;
  published
    { PostScript data structure }
    procedure TestPostScript_ItalicAngle;
  end;


  TTestFreeSansFont = class(TBaseTestParseTTF)
  protected
    procedure SetUp; override;
  published
    { Offset Table }
    procedure TestDirectory_FontVersion;
    procedure TestDirectory_NumTables;
    procedure TestDirectory_SearchRange;
    procedure TestDirectory_EntrySelector;
    procedure TestDirectory_RangeShift;

    { THead data structure tests }
    procedure TestHead_FileVersion;
    procedure TestHead_FontRevision;
    procedure TestHead_CheckSumAdjustment;
    procedure TestHead_MagicNumber;
    procedure TestHead_Flags;
    procedure TestHead_UnitsPerEm;
    procedure TestHead_Created;
    procedure TestHead_Modified;
    procedure TestHead_BBox_xMin;
    procedure TestHead_BBox_yMin;
    procedure TestHead_BBox_xMax;
    procedure TestHead_BBox_yMax;
    procedure TestHead_MacStyle;
    procedure TestHead_LowestRecPPEM;
    procedure TestHead_FontDirectionHint;
    procedure TestHead_IndexToLocFormat;
    procedure TestHead_glyphDataFormat;

    { THHead data structure tests }
    procedure TestHHead_TableVersion;
    procedure TestHHead_Ascender;
    procedure TestHHead_Descender;
    procedure TestHHead_LineGap;
    procedure TestHHead_AdvanceWidthMax;
    procedure TestHHead_MinLeftSideBearing;
    procedure TestHHead_MinRightSideBearing;
    procedure TestHHead_XMaxExtent;
    procedure TestHHead_CaretSlopeRise;
    procedure TestHHead_CaretSlopeRun;
    procedure TestHHead_Reserved;
    procedure TestHHead_metricDataFormat;
    procedure TestHHead_numberOfHMetrics;

    { TCmapHeader data structure tests }
    procedure TestCMap_version;
    procedure TestCMap_SubTableCount;

    { TCmapSubTableEntry data structure tests }
    procedure TestCMapSubTables_1;
    procedure TestCMapSubTables_2;
    procedure TestCMapSubTables_3;
    procedure TestCMapSubTables_4;
    procedure TestCMapSubTables_5;

    { CmapUnicodeMap data structure tests }
    procedure TestCmapUnicodeMap_Format;
    procedure TestCmapUnicodeMap_Length;
    procedure TestCmapUnicodeMap_SegmentCount2;
    procedure TestCmapUnicodeMap_SearchRange;
    procedure TestCmapUnicodeMap_EntrySelector;
    procedure TestCmapUnicodeMap_RangeShift;

    { TUnicodeMapSegment data structure tests }
    procedure TestUnicodeMapSegment_1_StartCode;
    procedure TestUnicodeMapSegment_1_EndCode;
    procedure TestUnicodeMapSegment_1_IDDelta;
    procedure TestUnicodeMapSegment_1_IDRangeOffset;
    procedure TestUnicodeMapSegment_2_StartCode;
    procedure TestUnicodeMapSegment_2_EndCode;
    procedure TestUnicodeMapSegment_2_IDDelta;
    procedure TestUnicodeMapSegment_2_IDRangeOffset;
    procedure TestUnicodeMapSegment_127_StartCode;
    procedure TestUnicodeMapSegment_127_EndCode;
    procedure TestUnicodeMapSegment_127_IDDelta;
    procedure TestUnicodeMapSegment_127_IDRangeOffset;

    { hmtx - Horizontal Metrics data structure tests }
    procedure TestWidths_Size;
    procedure TestWidths_HorMetric_0_AdvanceWidth;
    procedure TestWidths_HorMetric_0_LSB;
    procedure TestWidths_HorMetric_1_AdvanceWidth;
    procedure TestWidths_HorMetric_1_LSB;
    procedure TestWidths_HorMetric_2586_AdvanceWidth;
    procedure TestWidths_HorMetric_2586_LSB;

    { maxp - Maximum Profile data structure tests }
    procedure TestMaxP_VersionNumber;
    procedure TestMaxP_numGlyphs;
    procedure TestMaxP_maxPoints;
    procedure TestMaxP_maxContours;
    procedure TestMaxP_maxCompositePoints;
    procedure TestMaxP_maxCompositeContours;
    procedure TestMaxP_maxZones;
    procedure TestMaxP_maxTwilightPoints;
    procedure TestMaxP_maxStorage;
    procedure TestMaxP_maxFunctionDefs;
    procedure TestMaxP_maxInstructionDefs;
    procedure TestMaxP_maxStackElements;
    procedure TestMaxP_maxSizeOfInstructions;
    procedure TestMaxP_maxComponentElements;
    procedure TestMaxP_maxComponentDepth;

    { OS2Data - OS/2 Data structure tests }
    procedure TestOS2Data_version;
    procedure TestOS2Data_xAvgCharWidth;
    procedure TestOS2Data_usWeightClass;
    procedure TestOS2Data_usWidthClass;
    procedure TestOS2Data_fsType;
    procedure TestOS2Data_ySubscriptXSize;
    Procedure TestOS2Data_ySubscriptYSize;
    Procedure TestOS2Data_ySubscriptXOffset;
    Procedure TestOS2Data_ySubscriptYOffset;
    Procedure TestOS2Data_ySuperscriptXSize;
    Procedure TestOS2Data_ySuperscriptYSize;
    Procedure TestOS2Data_ySuperscriptXOffset;
    Procedure TestOS2Data_ySuperscriptYOffset;
    Procedure TestOS2Data_yStrikeoutSize;
    Procedure TestOS2Data_yStrikeoutPosition;
    Procedure TestOS2Data_sFamilyClass;
    procedure TestOS2Data_Panose;
    procedure TestOS2Data_ulUnicodeRange1;
    procedure TestOS2Data_ulUnicodeRange2;
    procedure TestOS2Data_ulUnicodeRange3;
    procedure TestOS2Data_ulUnicodeRange4;
    procedure TestOS2Data_achVendID;
    procedure TestOS2Data_fsSelection;
    procedure TestOS2Data_usFirstCharIndex;
    procedure TestOS2Data_usLastCharIndex;
    procedure TestOS2Data_sTypoAscender;
    procedure TestOS2Data_sTypoDescender;
    procedure TestOS2Data_sTypoLineGap;
    procedure TestOS2Data_usWinAscent;
    procedure TestOS2Data_usWinDescent;
    procedure TestOS2Data_ulCodePageRange1;
    procedure TestOS2Data_ulCodePageRange2;
    procedure TestOS2Data_sxHeight;
    procedure TestOS2Data_sCapHeight;
    procedure TestOS2Data_usDefaultChar;
    procedure TestOS2Data_usBreakChar;
    procedure TestOS2Data_usMaxContext;

    { PostScript data structure }
    procedure TestPostScript_Format;
    procedure TestPostScript_ItalicAngle;
    procedure TestPostScript_UnderlinePosition;
    procedure TestPostScript_underlineThickness;
    procedure TestPostScript_isFixedPitch;
    procedure TestPostScript_minMemType42;
    procedure TestPostScript_maxMemType42;
    procedure TestPostScript_minMemType1;
    procedure TestPostScript_maxMemType1;
  end;

implementation

uses
  dateutils
  ,strutils
  ;

const
  cFont1 = 'fonts' + PathDelim + 'LiberationSans-Regular.ttf';
  cFont2 = 'fonts' + PathDelim + 'FreeSans.ttf';
  cFont3 = 'fonts' + PathDelim + 'LiberationSans-Italic.ttf';

{ TTestEmptyParseTTF }

procedure TBaseTestParseTTF.SetUp;
begin
  FFileInfo := TMyTFFileInfo.Create;
end;

procedure TBaseTestParseTTF.TearDown;
begin
  FFileInfo.Free;
end;

procedure TBaseTestParseTTF.LoadFont(const AFilename: string);
begin
  Assert(FFileInfo <> nil, 'FFileInfo has not been instantiated');
  FFileInfo.LoadFromFile(AFilename);
end;

procedure TTestEmptyParseTTF.TestEmptyTables;
begin
  AssertTrue('Failed on 1', Length(FI.Tables) = 0)
end;

procedure TTestEmptyParseTTF.TestEmptyDirectory;
begin
  AssertEquals('Failed on 1', 0, FI.Directory.FontVersion.Major);
  AssertEquals('Failed on 2', 0, FI.Directory.FontVersion.Minor);
  AssertEquals('Failed on 3', 0, FI.Directory.Numtables);
end;

{ TTestLiberationFont }

procedure TTestLiberationFont.SetUp;
begin
  inherited SetUp;
  LoadFont(cFont1);
end;

procedure TTestLiberationFont.TestDirectory_FontVersion;
begin
  AssertEquals('Failed on 0', '00010000', IntToHex(FI.Directory.FontVersion.Version, 8));
  AssertEquals('Failed on Major Version', 1, FI.Directory.FontVersion.Major);
  AssertEquals('Failed on Minor Version', 0, FI.Directory.FontVersion.Minor);
end;

procedure TTestLiberationFont.TestDirectory_NumTables;
begin
  AssertEquals('Failed on Numtables', 19, Int(FI.Directory.Numtables));
end;

procedure TTestLiberationFont.TestDirectory_SearchRange;
begin
  AssertEquals('Failed on SearchRange', 256, Int(FI.Directory.SearchRange));
end;

procedure TTestLiberationFont.TestDirectory_EntrySelector;
begin
  AssertEquals('Failed on EntrySelector', 4, Int(FI.Directory.EntrySelector));
end;

procedure TTestLiberationFont.TestDirectory_RangeShift;
begin
  AssertEquals('Failed on RangeShift', 48, Int(FI.Directory.RangeShift));
end;

procedure TTestLiberationFont.TestHead_FileVersion;
begin
  AssertEquals('Failed on 1', '00010000', IntToHex(FI.Head.FileVersion.Version, 8));
  AssertEquals('Failed on 2', 1, FI.Head.FileVersion.Major);
  AssertEquals('Failed on 3', 0, FI.Head.FileVersion.Minor);
end;

procedure TTestLiberationFont.TestHead_FontRevision;
begin
  AssertEquals('Failed on 1', '00020000', IntToHex(FI.Head.FontRevision.Version, 8));
  AssertEquals('Failed on 2', 2, FI.Head.FontRevision.Major);
  AssertEquals('Failed on 3', 0, FI.Head.FontRevision.Minor);
end;

procedure TTestLiberationFont.TestHead_CheckSumAdjustment;
begin
  AssertEquals('Failed on 1', $D6572D59, FI.Head.CheckSumAdjustment);
end;

procedure TTestLiberationFont.TestHead_MagicNumber;
begin
  AssertEquals('Failed on 1', $5F0F3CF5, FI.Head.MagicNumber);
end;

procedure TTestLiberationFont.TestHead_Flags;
begin
  AssertEquals('Failed on 1', $021F, FI.Head.Flags);
end;

procedure TTestLiberationFont.TestHead_UnitsPerEm;
begin
  AssertEquals('Failed on 1', 2048, FI.Head.UnitsPerEm);
end;

procedure TTestLiberationFont.TestHead_Created;
var
  dt: TDateTime;
  s: string;
begin
  // LONGDATETIME: Date represented in number of seconds since 12:00 midnight,
  //              January 1, 1904. The value is represented as a signed 64-bit integer.

  dt := MacToDateTime(FI.Head.Created);

  // value verified with Microsoft's ttfdump tool and GMT timezone (no daylight saving applied).
  //    created:             Thu Oct 04 11:02:31 2012
  //    modified:            Thu Oct 04 11:02:31 2012
  AssertEquals('Failed on 1', EncodeDateTime(2012, 10, 4, 11, 2, 31, 0), dt);

  // Instead we use this - which shows human readable dates.
  s := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
  AssertEquals('Failed on 2', '2012-10-04 11:02:31', s);
end;

procedure TTestLiberationFont.TestHead_Modified;
var
  dt: TDateTime;
  s: string;
begin
  // value verified with Microsoft's ttfdump tool and GMT timezone (no daylight saving applied).
  //    created:             Thu Oct 04 11:02:31 2012
  //    modified:            Thu Oct 04 11:02:31 2012

  dt := MacToDateTime(FI.Head.Modified);
  s := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
  AssertEquals('Failed on 2', '2012-10-04 11:02:31', s);
end;

procedure TTestLiberationFont.TestHead_BBox_xMin;
begin
  AssertEquals('Failed on 1', -1114, FI.Head.BBox[0]);
end;

procedure TTestLiberationFont.TestHead_BBox_yMin;
begin
  AssertEquals('Failed on 1', -621, FI.Head.BBox[1]);
end;

procedure TTestLiberationFont.TestHead_BBox_xMax;
begin
  AssertEquals('Failed on 1', 2666, FI.Head.BBox[2]);
end;

procedure TTestLiberationFont.TestHead_BBox_yMax;
begin
  AssertEquals('Failed on 1', 2007, FI.Head.BBox[3]);
end;

procedure TTestLiberationFont.TestHead_MacStyle;
begin
  AssertEquals('Failed on 1', $0000, FI.Head.MacStyle);
end;

procedure TTestLiberationFont.TestHead_LowestRecPPEM;
begin
  AssertEquals('Failed on 1', 8, FI.Head.LowestRecPPEM);
end;

procedure TTestLiberationFont.TestHead_FontDirectionHint;
begin
  AssertEquals('Failed on 1', 0, FI.Head.FontDirectionHint);
end;

procedure TTestLiberationFont.TestHead_IndexToLocFormat;
begin
  AssertEquals('Failed on 1', 1, FI.Head.IndexToLocFormat);
end;

procedure TTestLiberationFont.TestHead_glyphDataFormat;
begin
  AssertEquals('Failed on 1', 0, FI.Head.glyphDataFormat);
end;

procedure TTestLiberationFont.TestHHead_TableVersion;
begin
  AssertEquals('Failed on 1', '00010000', IntToHex(FI.HHead.TableVersion.Version, 8));
  AssertEquals('Failed on 2', 1, FI.HHead.TableVersion.Major);
  AssertEquals('Failed on 3', 0, FI.HHead.TableVersion.Minor);
end;

procedure TTestLiberationFont.TestHHead_Ascender;
begin
  AssertEquals('Failed on 1', 1854, FI.HHead.Ascender);
end;

procedure TTestLiberationFont.TestHHead_Descender;
begin
  AssertEquals('Failed on 1', -434, FI.HHead.Descender);
end;

procedure TTestLiberationFont.TestHHead_LineGap;
begin
  AssertEquals('Failed on 1', 67, FI.HHead.LineGap);
end;

procedure TTestLiberationFont.TestHHead_AdvanceWidthMax;
begin
  AssertEquals('Failed on 1', 2740, FI.HHead.AdvanceWidthMax);
end;

procedure TTestLiberationFont.TestHHead_MinLeftSideBearing;
begin
  AssertEquals('Failed on 1', -1114, FI.HHead.MinLeftSideBearing);
end;

procedure TTestLiberationFont.TestHHead_MinRightSideBearing;
begin
  AssertEquals('Failed on 1', -1414, FI.HHead.MinRightSideBearing);
end;

procedure TTestLiberationFont.TestHHead_XMaxExtent;
begin
  AssertEquals('Failed on 1', 2666, FI.HHead.XMaxExtent);
end;

procedure TTestLiberationFont.TestHHead_CaretSlopeRise;
begin
  AssertEquals('Failed on 1', 1, FI.HHead.CaretSlopeRise);
end;

procedure TTestLiberationFont.TestHHead_CaretSlopeRun;
begin
  AssertEquals('Failed on 1', 0, FI.HHead.CaretSlopeRun);
end;

procedure TTestLiberationFont.TestHHead_Reserved;
begin
  AssertEquals('Failed on 1', 0, FI.HHead.Reserved[0]);
  AssertEquals('Failed on 2', 0, FI.HHead.Reserved[1]);
  AssertEquals('Failed on 3', 0, FI.HHead.Reserved[2]);
  AssertEquals('Failed on 4', 0, FI.HHead.Reserved[3]);
  AssertEquals('Failed on 5', 0, FI.HHead.Reserved[4]);
end;

procedure TTestLiberationFont.TestHHead_metricDataFormat;
begin
  AssertEquals('Failed on 1', 0, FI.HHead.metricDataFormat);
end;

procedure TTestLiberationFont.TestHHead_numberOfHMetrics;
begin
  AssertEquals('Failed on 1', 2587, FI.HHead.numberOfHMetrics);
end;

procedure TTestLiberationFont.TestCMap_version;
begin
  AssertEquals('Failed on 1', '0000', IntToHex(FI.CmapH.Version, 4));
  AssertEquals('Failed on 2', 0, FI.CmapH.Version);
end;

procedure TTestLiberationFont.TestCMap_SubTableCount;
begin
  AssertEquals('Failed on 1', 3, FI.CmapH.SubTableCount);
end;

procedure TTestLiberationFont.TestCMapSubTables_1;
var
  lSubTable: TCmapSubTableEntry;
begin
  lSubTable := FI.CmapSubtables[0];
  AssertEquals('Failed on 1', 0, lSubTable.PlatformID);
  AssertEquals('Failed on 2', 3, lSubTable.EncodingID);
  AssertEquals('Failed on 3', '0000001C', IntToHex(lSubTable.Offset, 8));
end;

procedure TTestLiberationFont.TestCMapSubTables_2;
var
  lSubTable: TCmapSubTableEntry;
begin
  lSubTable := FI.CmapSubtables[1];
  AssertEquals('Failed on 1', 1, lSubTable.PlatformID);
  AssertEquals('Failed on 2', 0, lSubTable.EncodingID);
  AssertEquals('Failed on 3', '00000424', IntToHex(lSubTable.Offset, 8));
end;

procedure TTestLiberationFont.TestCMapSubTables_3;
var
  lSubTable: TCmapSubTableEntry;
begin
  lSubTable := FI.CmapSubtables[2];
  AssertEquals('Failed on 1', 3, lSubTable.PlatformID);
  AssertEquals('Failed on 2', 1, lSubTable.EncodingID);
  AssertEquals('Failed on 3', '0000001C', IntToHex(lSubTable.Offset, 8));
end;

procedure TTestLiberationFont.TestCmapUnicodeMap_Format;
begin
  AssertEquals('Failed on 1', 4, FI.CmapUnicodeMap.Format);
end;

procedure TTestLiberationFont.TestCmapUnicodeMap_Length;
begin
  AssertEquals('Failed on 1', 1032, FI.CmapUnicodeMap.Length);
end;

procedure TTestLiberationFont.TestCmapUnicodeMap_SegmentCount2;
begin
  AssertEquals('Failed on 1', 254, FI.CmapUnicodeMap.SegmentCount2);
end;

procedure TTestLiberationFont.TestCmapUnicodeMap_SearchRange;
begin
  AssertEquals('Failed on 1', 128, FI.CmapUnicodeMap.SearchRange);
end;

procedure TTestLiberationFont.TestCmapUnicodeMap_EntrySelector;
begin
  AssertEquals('Failed on 1', 6, FI.CmapUnicodeMap.EntrySelector);
end;

procedure TTestLiberationFont.TestCmapUnicodeMap_RangeShift;
begin
  AssertEquals('Failed on 1', 126, FI.CmapUnicodeMap.RangeShift);
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_1_StartCode;
begin
  AssertEquals('Failed on 1', '0020', IntToHex(FI.CmapUnicodeMapSegments[0].StartCode, 4));
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_1_EndCode;
begin
  AssertEquals('Failed on 1', '007E', IntToHex(FI.CmapUnicodeMapSegments[0].EndCode, 4));
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_1_IDDelta;
begin
  AssertEquals('Failed on 1', -29, FI.CmapUnicodeMapSegments[0].IDDelta);
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_1_IDRangeOffset;
begin
  AssertEquals('Failed on 1', 0, FI.CmapUnicodeMapSegments[0].IDRangeOffset);
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_2_StartCode;
begin
  AssertEquals('Failed on 1', '00A0', IntToHex(FI.CmapUnicodeMapSegments[1].StartCode, 4));
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_2_EndCode;
begin
  AssertEquals('Failed on 1', '036F', IntToHex(FI.CmapUnicodeMapSegments[1].EndCode, 4));
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_2_IDDelta;
begin
  AssertEquals('Failed on 1', -62, FI.CmapUnicodeMapSegments[1].IDDelta);
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_2_IDRangeOffset;
begin
  AssertEquals('Failed on 1', 0, FI.CmapUnicodeMapSegments[1].IDRangeOffset);
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_127_StartCode;
begin
  AssertEquals('Failed on 1', 'FFFF', IntToHex(FI.CmapUnicodeMapSegments[126].StartCode, 4));
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_127_EndCode;
begin
  AssertEquals('Failed on 1', 'FFFF', IntToHex(FI.CmapUnicodeMapSegments[126].EndCode, 4));
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_127_IDDelta;
begin
  AssertEquals('Failed on 1', 1, FI.CmapUnicodeMapSegments[126].IDDelta);
end;

procedure TTestLiberationFont.TestUnicodeMapSegment_127_IDRangeOffset;
begin
  AssertEquals('Failed on 1', 0, FI.CmapUnicodeMapSegments[126].IDRangeOffset);
end;

procedure TTestLiberationFont.TestWidths_Size;
begin
  AssertEquals('Failed on 1', 2587, Length(FI.Widths));
end;

procedure TTestLiberationFont.TestWidths_HorMetric_0_AdvanceWidth;
begin
  AssertEquals('Failed on 1', 1536, FI.Widths[0].AdvanceWidth);
end;

procedure TTestLiberationFont.TestWidths_HorMetric_0_LSB;
begin
  AssertEquals('Failed on 1', 205, FI.Widths[0].LSB);
end;

procedure TTestLiberationFont.TestWidths_HorMetric_1_AdvanceWidth;
begin
  AssertEquals('Failed on 1', 0, FI.Widths[1].AdvanceWidth);
end;

procedure TTestLiberationFont.TestWidths_HorMetric_1_LSB;
begin
  AssertEquals('Failed on 1', 0, FI.Widths[1].LSB);
end;

procedure TTestLiberationFont.TestWidths_HorMetric_2586_AdvanceWidth;
begin
  AssertEquals('Failed on 1', 1140, FI.Widths[2586].AdvanceWidth);
end;

procedure TTestLiberationFont.TestWidths_HorMetric_2586_LSB;
begin
  AssertEquals('Failed on 1', 86, FI.Widths[2586].LSB);
end;

procedure TTestLiberationFont.TestMaxP_VersionNumber;
begin
  AssertEquals('Failed on 1', '00010000', IntToHex(FI.MaxP.VersionNumber.Version, 8));
  AssertEquals('Failed on 2', 1, FI.MaxP.VersionNumber.Major);
  AssertEquals('Failed on 3', 0, FI.MaxP.VersionNumber.Minor);
end;

procedure TTestLiberationFont.TestMaxP_numGlyphs;
begin
  AssertEquals('Failed on 1', 2587, FI.MaxP.numGlyphs);
end;

procedure TTestLiberationFont.TestMaxP_maxPoints;
begin
  AssertEquals('Failed on 1', 338, FI.MaxP.maxPoints);
end;

procedure TTestLiberationFont.TestMaxP_maxContours;
begin
  AssertEquals('Failed on 1', 84, FI.MaxP.maxContours);
end;

procedure TTestLiberationFont.TestMaxP_maxCompositePoints;
begin
  AssertEquals('Failed on 1', 92, FI.MaxP.maxCompositePoints);
end;

procedure TTestLiberationFont.TestMaxP_maxCompositeContours;
begin
  AssertEquals('Failed on 1', 6, FI.MaxP.maxCompositeContours);
end;

procedure TTestLiberationFont.TestMaxP_maxZones;
begin
  AssertEquals('Failed on 1', 2, FI.MaxP.maxZones);
end;

procedure TTestLiberationFont.TestMaxP_maxTwilightPoints;
begin
  AssertEquals('Failed on 1', 16, FI.MaxP.maxTwilightPoints);
end;

procedure TTestLiberationFont.TestMaxP_maxStorage;
begin
  AssertEquals('Failed on 1', 47, FI.MaxP.maxStorage);
end;

procedure TTestLiberationFont.TestMaxP_maxFunctionDefs;
begin
  AssertEquals('Failed on 1', 92, FI.MaxP.maxFunctionDefs);
end;

procedure TTestLiberationFont.TestMaxP_maxInstructionDefs;
begin
  AssertEquals('Failed on 1', 0, FI.MaxP.maxInstructionDefs);
end;

procedure TTestLiberationFont.TestMaxP_maxStackElements;
begin
  AssertEquals('Failed on 1', 676, FI.MaxP.maxStackElements);
end;

procedure TTestLiberationFont.TestMaxP_maxSizeOfInstructions;
begin
  AssertEquals('Failed on 1', 516, FI.MaxP.maxSizeOfInstructions);
end;

procedure TTestLiberationFont.TestMaxP_maxComponentElements;
begin
  AssertEquals('Failed on 1', 4, FI.MaxP.maxComponentElements);
end;

procedure TTestLiberationFont.TestMaxP_maxComponentDepth;
begin
  AssertEquals('Failed on 1', 1, FI.MaxP.maxComponentDepth);
end;

procedure TTestLiberationFont.TestOS2Data_version;
begin
  AssertEquals('Failed on 1', 3, FI.OS2Data.version);
end;

procedure TTestLiberationFont.TestOS2Data_xAvgCharWidth;
begin
  AssertEquals('Failed on 1', 1186, FI.OS2Data.xAvgCharWidth);
end;

procedure TTestLiberationFont.TestOS2Data_usWeightClass;
begin
  AssertEquals('Failed on 1', 400, FI.OS2Data.usWeightClass);
end;

procedure TTestLiberationFont.TestOS2Data_usWidthClass;
begin
  AssertEquals('Failed on 1', 5, FI.OS2Data.usWidthClass);
end;

procedure TTestLiberationFont.TestOS2Data_fsType;
begin
  AssertEquals('Failed on 1', '0000', IntToHex(FI.OS2Data.fsType, 4));
end;

procedure TTestLiberationFont.TestOS2Data_ySubscriptXSize;
begin
  AssertEquals('Failed on 1', 1434, FI.OS2Data.ySubscriptXSize);
end;

procedure TTestLiberationFont.TestOS2Data_ySubscriptYSize;
begin
  AssertEquals('Failed on 1', 1331, FI.OS2Data.ySubscriptYSize);
end;


procedure TTestLiberationFont.TestOS2Data_ySubscriptXOffset;
begin
  AssertEquals('Failed on 1', 0, FI.OS2Data.ySubscriptXOffset);
end;


procedure TTestLiberationFont.TestOS2Data_ySubscriptYOffset;
begin
  AssertEquals('Failed on 1', 283, FI.OS2Data.ySubscriptYOffset);
end;


procedure TTestLiberationFont.TestOS2Data_ySuperscriptXSize;
begin
  AssertEquals('Failed on 1', 1434, FI.OS2Data.ySuperscriptXSize);
end;


procedure TTestLiberationFont.TestOS2Data_ySuperscriptYSize;
begin
  AssertEquals('Failed on 1', 1331, FI.OS2Data.ySuperscriptYSize);
end;


procedure TTestLiberationFont.TestOS2Data_ySuperscriptXOffset;
begin
  AssertEquals('Failed on 1', 0, FI.OS2Data.ySuperscriptXOffset);
end;


procedure TTestLiberationFont.TestOS2Data_ySuperscriptYOffset;
begin
  AssertEquals('Failed on 1', 977, FI.OS2Data.ySuperscriptYOffset);
end;


procedure TTestLiberationFont.TestOS2Data_yStrikeoutSize;
begin
  AssertEquals('Failed on 1', 102, FI.OS2Data.yStrikeoutSize);
end;


procedure TTestLiberationFont.TestOS2Data_yStrikeoutPosition;
begin
  AssertEquals('Failed on 1', 530, FI.OS2Data.yStrikeoutPosition);
end;


procedure TTestLiberationFont.TestOS2Data_sFamilyClass;
begin
  AssertEquals('Failed on 1', 5, Lo(FI.OS2Data.sFamilyClass));
  AssertEquals('Failed on 2', 8, Hi(FI.OS2Data.sFamilyClass));
  AssertEquals('Failed on 3', '0805', IntToHex(FI.OS2Data.sFamilyClass, 4));
end;

procedure TTestLiberationFont.TestOS2Data_Panose;
begin
  AssertEquals('Failed on 1', 2, FI.OS2Data.panose[0]);
  AssertEquals('Failed on 2', 11, FI.OS2Data.panose[1]);
  AssertEquals('Failed on 3', 6, FI.OS2Data.panose[2]);
  AssertEquals('Failed on 4', 4, FI.OS2Data.panose[3]);
  AssertEquals('Failed on 5', 2, FI.OS2Data.panose[4]);
  AssertEquals('Failed on 6', 2, FI.OS2Data.panose[5]);
  AssertEquals('Failed on 7', 2, FI.OS2Data.panose[6]);
  AssertEquals('Failed on 8', 2, FI.OS2Data.panose[7]);
  AssertEquals('Failed on 9', 2, FI.OS2Data.panose[8]);
  AssertEquals('Failed on 10', 4, FI.OS2Data.panose[9]);
end;

procedure TTestLiberationFont.TestOS2Data_ulUnicodeRange1;
begin
//  AssertEquals('Failed on 1', '1110 0000 0000 0000 0000 1010 1111 1111', IntToBin(FI.OS2Data.ulUnicodeRange1, 32, 4));
  AssertEquals('Failed on 2', 'E0000AFF', IntToHex(FI.OS2Data.ulUnicodeRange1, 8));
end;

procedure TTestLiberationFont.TestOS2Data_ulUnicodeRange2;
begin
  AssertEquals('Failed on 1', '0101 0000 0000 0000 0111 1000 1111 1111', IntToBin(FI.OS2Data.ulUnicodeRange2, 32, 4));
  AssertEquals('Failed on 2', '500078FF', IntToHex(FI.OS2Data.ulUnicodeRange2, 8));
end;

procedure TTestLiberationFont.TestOS2Data_ulUnicodeRange3;
begin
  AssertEquals('Failed on 1', '0000 0000 0000 0000 0000 0000 0010 0001', IntToBin(FI.OS2Data.ulUnicodeRange3, 32, 4));
  AssertEquals('Failed on 2', '00000021', IntToHex(FI.OS2Data.ulUnicodeRange3, 8));
end;

procedure TTestLiberationFont.TestOS2Data_ulUnicodeRange4;
begin
  AssertEquals('Failed on 1', '0000 0000 0000 0000 0000 0000 0000 0000', IntToBin(FI.OS2Data.ulUnicodeRange4, 32, 4));
  AssertEquals('Failed on 2', '00000000', IntToHex(FI.OS2Data.ulUnicodeRange4, 8));
end;

procedure TTestLiberationFont.TestOS2Data_achVendID;
var
  s: string;
begin
  s := FI.OS2Data.achVendID[0] + FI.OS2Data.achVendID[1] + FI.OS2Data.achVendID[2] +
        FI.OS2Data.achVendID[3];
  AssertEquals('Failed on 1', '1ASC', s);
end;

procedure TTestLiberationFont.TestOS2Data_fsSelection;
begin
  AssertEquals('Failed on 1', '0040', IntToHex(FI.OS2Data.fsSelection, 4));
end;

procedure TTestLiberationFont.TestOS2Data_usFirstCharIndex;
begin
  AssertEquals('Failed on 1', '0020', IntToHex(FI.OS2Data.usFirstCharIndex, 4));
end;

procedure TTestLiberationFont.TestOS2Data_usLastCharIndex;
begin
  AssertEquals('Failed on 1', 'FFFC', IntToHex(FI.OS2Data.usLastCharIndex, 4));
end;

procedure TTestLiberationFont.TestOS2Data_sTypoAscender;
begin
  AssertEquals('Failed on 1', 1491, FI.OS2Data.sTypoAscender);
end;

procedure TTestLiberationFont.TestOS2Data_sTypoDescender;
begin
  AssertEquals('Failed on 1', -431, FI.OS2Data.sTypoDescender);
end;

procedure TTestLiberationFont.TestOS2Data_sTypoLineGap;
begin
  AssertEquals('Failed on 1', 307, FI.OS2Data.sTypoLineGap);
end;

procedure TTestLiberationFont.TestOS2Data_usWinAscent;
begin
  AssertEquals('Failed on 1', 1854, FI.OS2Data.usWinAscent);
end;

procedure TTestLiberationFont.TestOS2Data_usWinDescent;
begin
  AssertEquals('Failed on 1', 434, FI.OS2Data.usWinDescent);
end;

procedure TTestLiberationFont.TestOS2Data_ulCodePageRange1;
begin
  AssertEquals('Failed on 1', '600001BF', IntToHex(FI.OS2Data.ulCodePageRange1, 8));
end;

procedure TTestLiberationFont.TestOS2Data_ulCodePageRange2;
begin
  AssertEquals('Failed on 1', 'DFF70000', IntToHex(FI.OS2Data.ulCodePageRange2, 8));
end;

procedure TTestLiberationFont.TestOS2Data_sxHeight;
begin
  AssertEquals('Failed on 1', 1082, FI.OS2Data.sxHeight);
end;

procedure TTestLiberationFont.TestOS2Data_sCapHeight;
begin
  AssertEquals('Failed on 1', 1409, FI.OS2Data.sCapHeight);
end;

procedure TTestLiberationFont.TestOS2Data_usDefaultChar;
begin
  AssertEquals('Failed on 1', '0000', IntToHex(FI.OS2Data.usDefaultChar, 4));
end;

procedure TTestLiberationFont.TestOS2Data_usBreakChar;
begin
  AssertEquals('Failed on 1', '0020', IntToHex(FI.OS2Data.usBreakChar, 4));
end;

procedure TTestLiberationFont.TestOS2Data_usMaxContext;
begin
  AssertEquals('Failed on 1', 14, FI.OS2Data.usMaxContext);
end;

procedure TTestLiberationFont.TestPostScript_Format;
begin
  AssertEquals('Failed on 1', '00020000', IntToHex(FI.PostScript.Format.Version, 8));
  AssertEquals('Failed on 2', 2, FI.PostScript.Format.Major);
  AssertEquals('Failed on 3', 0, FI.PostScript.Format.Minor);
end;

procedure TTestLiberationFont.TestPostScript_ItalicAngle;
begin
  AssertEquals('Failed on 1', 0.0, FI.PostScript.ItalicAngle);
end;

procedure TTestLiberationFont.TestPostScript_UnderlinePosition;
begin
  AssertEquals('Failed on 1', -217, FI.PostScript.UnderlinePosition);
end;

procedure TTestLiberationFont.TestPostScript_underlineThickness;
begin
  AssertEquals('Failed on 1', 150, FI.PostScript.underlineThickness);
end;

procedure TTestLiberationFont.TestPostScript_isFixedPitch;
begin
  AssertEquals('Failed on 1', 0, FI.PostScript.isFixedPitch);
end;

procedure TTestLiberationFont.TestPostScript_minMemType42;
begin
  AssertEquals('Failed on 1', 0, FI.PostScript.minMemType42);
end;

procedure TTestLiberationFont.TestPostScript_maxMemType42;
begin
  AssertEquals('Failed on 1', 0, FI.PostScript.maxMemType42);
end;

procedure TTestLiberationFont.TestPostScript_minMemType1;
begin
  AssertEquals('Failed on 1', 0, FI.PostScript.minMemType1);
end;

procedure TTestLiberationFont.TestPostScript_maxMemType1;
begin
  AssertEquals('Failed on 1', 0, FI.PostScript.maxMemType1);
end;

procedure TTestLiberationFont.TestGetGlyphIndex;
begin
  AssertEquals('Failed on 1.1', 67, Ord('C'));
  AssertEquals('Failed on 1.2', 111, Ord('o'));

  AssertEquals('Failed on 2.1', 38, FI.GetGlyphIndex(Ord('C')));
  AssertEquals('Failed on 2.2', 82, FI.GetGlyphIndex(Ord('o')));
  AssertEquals('Failed on 2.3', 88, FI.GetGlyphIndex(Ord('u')));
  AssertEquals('Failed on 2.4', 87, FI.GetGlyphIndex(Ord('t')));
  AssertEquals('Failed on 2.5', 85, FI.GetGlyphIndex(Ord('r')));
  AssertEquals('Failed on 2.6', 92, FI.GetGlyphIndex(Ord('y')));
  AssertEquals('Failed on 2.7', 3, FI.GetGlyphIndex(Ord(' ')));
  AssertEquals('Failed on 2.8', 51, FI.GetGlyphIndex(Ord('P')));
  AssertEquals('Failed on 2.9', 80, FI.GetGlyphIndex(Ord('m')));
  AssertEquals('Failed on 2.10', 79, FI.GetGlyphIndex(Ord('l')));
  AssertEquals('Failed on 2.11', 19, FI.GetGlyphIndex(Ord('0')));
  AssertEquals('Failed on 2.12', 20, FI.GetGlyphIndex(Ord('1')));
end;

procedure TTestLiberationFont.TestGetAdvanceWidth;
begin
  AssertEquals('Failed on 1', 1479, FI.GetAdvanceWidth(38));  // 'C'
  AssertEquals('Failed on 2', 1139, FI.GetAdvanceWidth(82));  // 'o'
  AssertEquals('Failed on 3', 1139, FI.GetAdvanceWidth(88));  // 'u'
  AssertEquals('Failed on 4', 569, FI.GetAdvanceWidth(87));   // 't'
  AssertEquals('Failed on 5', 682, FI.GetAdvanceWidth(85));   // 'r'
  AssertEquals('Failed on 6', 1024, FI.GetAdvanceWidth(92));  // 'y'
  AssertEquals('Failed on 7', 569, FI.GetAdvanceWidth(3));    // ' '
  AssertEquals('Failed on 8', 1366, FI.GetAdvanceWidth(51));  // 'P'
  AssertEquals('Failed on 9', 1706, FI.GetAdvanceWidth(80));  // 'm'
  AssertEquals('Failed on 10', 455, FI.GetAdvanceWidth(79));  // 'l'
  AssertEquals('Failed on 11', 1139, FI.GetAdvanceWidth(19));  // '0'
  AssertEquals('Failed on 12', 1139, FI.GetAdvanceWidth(20));  // '1'
end;

{ TTestLiberationItalicFont }

procedure TTestLiberationItalicFont.SetUp;
begin
  inherited SetUp;
  AssertTrue('Failed to find TTF font file <' + cFont3 + '>' + LineEnding +
    'You can download it from [https://fedorahosted.org/releases/l/i/liberation-fonts/liberation-fonts-ttf-2.00.1.tar.gz]',
    FileExists(cFont3) = True);
  LoadFont(cFont3);
end;

procedure TTestLiberationItalicFont.TestPostScript_ItalicAngle;
begin
  AssertEquals('Failed on 1', -12.0, FI.PostScript.ItalicAngle / 65536.0);
  AssertEquals('Failed on 2', -12.0, FI.ItalicAngle);
end;

{ TTestFreeSansFont }

procedure TTestFreeSansFont.SetUp;
begin
  inherited SetUp;
  AssertTrue('Failed to find TTF font file <' + cFont2 + '>' + LineEnding +
    'You can download it from [http://ftp.gnu.org/gnu/freefont/freefont-ttf-20140503.zip]',
    FileExists(cFont2) = True);
  LoadFont(cFont2);
end;

procedure TTestFreeSansFont.TestDirectory_FontVersion;
begin
  AssertEquals('Failed on 0', '00010000', IntToHex(FI.Directory.FontVersion.Version, 8));
  AssertEquals('Failed on Major Version', 1, FI.Directory.FontVersion.Major);
  AssertEquals('Failed on Minor Version', 0, FI.Directory.FontVersion.Minor);
end;

procedure TTestFreeSansFont.TestDirectory_NumTables;
begin
  AssertEquals('Failed on Numtables', 19, Int(FI.Directory.Numtables));
end;

procedure TTestFreeSansFont.TestDirectory_SearchRange;
begin
  AssertEquals('Failed on SearchRange', 256, Int(FI.Directory.SearchRange));
end;

procedure TTestFreeSansFont.TestDirectory_EntrySelector;
begin
  AssertEquals('Failed on EntrySelector', 4, Int(FI.Directory.EntrySelector));
end;

procedure TTestFreeSansFont.TestDirectory_RangeShift;
begin
  AssertEquals('Failed on RangeShift', 48, Int(FI.Directory.RangeShift));
end;

procedure TTestFreeSansFont.TestHead_FileVersion;
begin
  AssertEquals('Failed on 1', '00010000', IntToHex(FI.Head.FileVersion.Version, 8));
  AssertEquals('Failed on 2', 1, FI.Head.FileVersion.Major);
  AssertEquals('Failed on 3', 0, FI.Head.FileVersion.Minor);
end;

procedure TTestFreeSansFont.TestHead_FontRevision;
begin
  { graemeg (2015-09-11): Microsoft's ttfdump tools says the version of this
    font is 412.2 and Linux's FontForge says it is 412.2268. I'm making the
    assumption that Microsoft's tool simply truncated the version number to
    one decimal. }
  AssertEquals('Failed on 1', 412, FI.Head.FontRevision.Major);
  AssertEquals('Failed on 2', 2268, FI.Head.FontRevision.Minor);
end;

procedure TTestFreeSansFont.TestHead_CheckSumAdjustment;
begin
  AssertEquals('Failed on 1', $BDD896C1, FI.Head.CheckSumAdjustment);
end;

procedure TTestFreeSansFont.TestHead_MagicNumber;
begin
  AssertEquals('Failed on 1', $5F0F3CF5, FI.Head.MagicNumber);
end;

procedure TTestFreeSansFont.TestHead_Flags;
begin
  AssertEquals('Failed on 1', $021F, FI.Head.Flags);
end;

procedure TTestFreeSansFont.TestHead_UnitsPerEm;
begin
  AssertEquals('Failed on 1', 1000, FI.Head.UnitsPerEm);
end;

procedure TTestFreeSansFont.TestHead_Created;
var
  dt: TDateTime;
  s: string;
begin
  // LONGDATETIME: Date represented in number of seconds since 12:00 midnight,
  //              January 1, 1904. The value is represented as a signed 64-bit integer.

  // value verified with Microsoft's ttfdump tool and GMT timezone (no daylight saving applied).
  //  created:             Thu May 03 13:34:25 2012
  //  modified:            Thu May 03 13:34:25 2012

  dt := MacToDateTime(FI.Head.Created);

  // We don't use this AssertEquals() because it shows a huge Double data-type
  // value as the result.
  AssertEquals('Failed on 1', EncodeDateTime(2012, 5, 3, 13, 34, 25, 0), dt);

  // Instead we use this - which shows human readable dates.
  s := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
  AssertEquals('Failed on 2', '2012-05-03 13:34:25', s);
end;

procedure TTestFreeSansFont.TestHead_Modified;
var
  dt: TDateTime;
  s: string;
begin
  // value verified with Microsoft's ttfdump tool and GMT timezone (no daylight saving applied).
  //  created:             Thu May 03 13:34:25 2012
  //  modified:            Thu May 03 13:34:25 2012
  dt := MacToDateTime(FI.Head.Modified);
  s := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
  AssertEquals('Failed on 2', '2012-05-03 13:34:25', s);
end;

procedure TTestFreeSansFont.TestHead_BBox_xMin;
begin
  AssertEquals('Failed on 1', -1166, FI.Head.BBox[0]);
end;

procedure TTestFreeSansFont.TestHead_BBox_yMin;
begin
  AssertEquals('Failed on 1', -638, FI.Head.BBox[1]);
end;

procedure TTestFreeSansFont.TestHead_BBox_xMax;
begin
  AssertEquals('Failed on 1', 2260, FI.Head.BBox[2]);
end;

procedure TTestFreeSansFont.TestHead_BBox_yMax;
begin
  AssertEquals('Failed on 1', 1050, FI.Head.BBox[3]);
end;

procedure TTestFreeSansFont.TestHead_MacStyle;
begin
  AssertEquals('Failed on 1', $0000, FI.Head.MacStyle);
end;

procedure TTestFreeSansFont.TestHead_LowestRecPPEM;
begin
  AssertEquals('Failed on 1', 8, FI.Head.LowestRecPPEM);
end;

procedure TTestFreeSansFont.TestHead_FontDirectionHint;
begin
  AssertEquals('Failed on 1', 0, FI.Head.FontDirectionHint);
end;

procedure TTestFreeSansFont.TestHead_IndexToLocFormat;
begin
  AssertEquals('Failed on 1', 1, FI.Head.IndexToLocFormat);
end;

procedure TTestFreeSansFont.TestHead_glyphDataFormat;
begin
  AssertEquals('Failed on 1', 0, FI.Head.glyphDataFormat);
end;

procedure TTestFreeSansFont.TestHHead_TableVersion;
begin
  AssertEquals('Failed on 1', '00010000', IntToHex(FI.HHead.TableVersion.Version, 8));
  AssertEquals('Failed on 2', 1, FI.HHead.TableVersion.Major);
  AssertEquals('Failed on 3', 0, FI.HHead.TableVersion.Minor);
end;

procedure TTestFreeSansFont.TestHHead_Ascender;
begin
  AssertEquals('Failed on 1', 900, FI.HHead.Ascender);
end;

procedure TTestFreeSansFont.TestHHead_Descender;
begin
  AssertEquals('Failed on 1', -200, FI.HHead.Descender);
end;

procedure TTestFreeSansFont.TestHHead_LineGap;
begin
  AssertEquals('Failed on 1', 100, FI.HHead.LineGap);
end;

procedure TTestFreeSansFont.TestHHead_AdvanceWidthMax;
begin
  AssertEquals('Failed on 1', 2256, FI.HHead.AdvanceWidthMax);
end;

procedure TTestFreeSansFont.TestHHead_MinLeftSideBearing;
begin
  AssertEquals('Failed on 1', -1166, FI.HHead.MinLeftSideBearing);
end;

procedure TTestFreeSansFont.TestHHead_MinRightSideBearing;
begin
  AssertEquals('Failed on 1', -724, FI.HHead.MinRightSideBearing);
end;

procedure TTestFreeSansFont.TestHHead_XMaxExtent;
begin
  AssertEquals('Failed on 1', 2260, FI.HHead.XMaxExtent);
end;

procedure TTestFreeSansFont.TestHHead_CaretSlopeRise;
begin
  AssertEquals('Failed on 1', 1, FI.HHead.CaretSlopeRise);
end;

procedure TTestFreeSansFont.TestHHead_CaretSlopeRun;
begin
  AssertEquals('Failed on 1', 0, FI.HHead.CaretSlopeRun);
end;

procedure TTestFreeSansFont.TestHHead_Reserved;
begin
  AssertEquals('Failed on 1', 0, FI.HHead.Reserved[0]);
  AssertEquals('Failed on 2', 0, FI.HHead.Reserved[1]);
  AssertEquals('Failed on 3', 0, FI.HHead.Reserved[2]);
  AssertEquals('Failed on 4', 0, FI.HHead.Reserved[3]);
  AssertEquals('Failed on 5', 0, FI.HHead.Reserved[4]);
end;

procedure TTestFreeSansFont.TestHHead_metricDataFormat;
begin
  AssertEquals('Failed on 1', 0, FI.HHead.metricDataFormat);
end;

procedure TTestFreeSansFont.TestHHead_numberOfHMetrics;
begin
  AssertEquals('Failed on 1', 6272, FI.HHead.numberOfHMetrics);
end;

procedure TTestFreeSansFont.TestCMap_version;
begin
  AssertEquals('Failed on 1', '0000', IntToHex(FI.CmapH.Version, 4));
  AssertEquals('Failed on 2', 0, FI.CmapH.Version);
end;

procedure TTestFreeSansFont.TestCMap_SubTableCount;
begin
  AssertEquals('Failed on 1', 5, FI.CmapH.SubTableCount);
end;

procedure TTestFreeSansFont.TestCMapSubTables_1;
var
  lSubTable: TCmapSubTableEntry;
begin
  lSubTable := FI.CmapSubtables[0];
  AssertEquals('Failed on 1', 0, lSubTable.PlatformID);
  AssertEquals('Failed on 2', 3, lSubTable.EncodingID);
  AssertEquals('Failed on 3', '0000002C', IntToHex(lSubTable.Offset, 8));
end;

procedure TTestFreeSansFont.TestCMapSubTables_2;
var
  lSubTable: TCmapSubTableEntry;
begin
  lSubTable := FI.CmapSubtables[1];
  AssertEquals('Failed on 1', 0, lSubTable.PlatformID);
  AssertEquals('Failed on 2', 10, lSubTable.EncodingID);
  AssertEquals('Failed on 3', '00000644', IntToHex(lSubTable.Offset, 8));
end;

procedure TTestFreeSansFont.TestCMapSubTables_3;
var
  lSubTable: TCmapSubTableEntry;
begin
  lSubTable := FI.CmapSubtables[2];
  AssertEquals('Failed on 1', 1, lSubTable.PlatformID);
  AssertEquals('Failed on 2', 0, lSubTable.EncodingID);
  AssertEquals('Failed on 3', '00000FA8', IntToHex(lSubTable.Offset, 8));
end;

procedure TTestFreeSansFont.TestCMapSubTables_4;
var
  lSubTable: TCmapSubTableEntry;
begin
  lSubTable := FI.CmapSubtables[3];
  AssertEquals('Failed on 1', 3, lSubTable.PlatformID);
  AssertEquals('Failed on 2', 1, lSubTable.EncodingID);
  AssertEquals('Failed on 3', '0000002C', IntToHex(lSubTable.Offset, 8));
end;

procedure TTestFreeSansFont.TestCMapSubTables_5;
var
  lSubTable: TCmapSubTableEntry;
begin
  lSubTable := FI.CmapSubtables[4];
  AssertEquals('Failed on 1', 3, lSubTable.PlatformID);
  AssertEquals('Failed on 2', 10, lSubTable.EncodingID);
  AssertEquals('Failed on 3', '00000644', IntToHex(lSubTable.Offset, 8));
end;

procedure TTestFreeSansFont.TestCmapUnicodeMap_Format;
begin
  AssertEquals('Failed on 1', 4, FI.CmapUnicodeMap.Format);
end;

procedure TTestFreeSansFont.TestCmapUnicodeMap_Length;
begin
  AssertEquals('Failed on 1', 1560, FI.CmapUnicodeMap.Length);
end;

procedure TTestFreeSansFont.TestCmapUnicodeMap_SegmentCount2;
begin
  AssertEquals('Failed on 1', 386, FI.CmapUnicodeMap.SegmentCount2);
end;

procedure TTestFreeSansFont.TestCmapUnicodeMap_SearchRange;
begin
  AssertEquals('Failed on 1', 256, FI.CmapUnicodeMap.SearchRange);
end;

procedure TTestFreeSansFont.TestCmapUnicodeMap_EntrySelector;
begin
  AssertEquals('Failed on 1', 7, FI.CmapUnicodeMap.EntrySelector);
end;

procedure TTestFreeSansFont.TestCmapUnicodeMap_RangeShift;
begin
  AssertEquals('Failed on 1', 130, FI.CmapUnicodeMap.RangeShift);
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_1_StartCode;
begin
  AssertEquals('Failed on 1', '0020', IntToHex(FI.CmapUnicodeMapSegments[0].StartCode, 4));
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_1_EndCode;
begin
  AssertEquals('Failed on 1', '007E', IntToHex(FI.CmapUnicodeMapSegments[0].EndCode, 4));
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_1_IDDelta;
begin
  AssertEquals('Failed on 1', -29, FI.CmapUnicodeMapSegments[0].IDDelta);
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_1_IDRangeOffset;
begin
  AssertEquals('Failed on 1', 0, FI.CmapUnicodeMapSegments[0].IDRangeOffset);
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_2_StartCode;
begin
  AssertEquals('Failed on 1', '00A0', IntToHex(FI.CmapUnicodeMapSegments[1].StartCode, 4));
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_2_EndCode;
begin
  AssertEquals('Failed on 1', '01BB', IntToHex(FI.CmapUnicodeMapSegments[1].EndCode, 4));
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_2_IDDelta;
begin
  AssertEquals('Failed on 1', -62, FI.CmapUnicodeMapSegments[1].IDDelta);
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_2_IDRangeOffset;
begin
  AssertEquals('Failed on 1', 0, FI.CmapUnicodeMapSegments[1].IDRangeOffset);
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_127_StartCode;
begin
  AssertEquals('Failed on 1', '2190', IntToHex(FI.CmapUnicodeMapSegments[126].StartCode, 4));
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_127_EndCode;
begin
  AssertEquals('Failed on 1', '2199', IntToHex(FI.CmapUnicodeMapSegments[126].EndCode, 4));
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_127_IDDelta;
begin
  AssertEquals('Failed on 1', -5123, FI.CmapUnicodeMapSegments[126].IDDelta);
end;

procedure TTestFreeSansFont.TestUnicodeMapSegment_127_IDRangeOffset;
begin
  AssertEquals('Failed on 1', 0, FI.CmapUnicodeMapSegments[126].IDRangeOffset);
end;

procedure TTestFreeSansFont.TestWidths_Size;
begin
  AssertEquals('Failed on 1', 6272, Length(FI.Widths));
end;

procedure TTestFreeSansFont.TestWidths_HorMetric_0_AdvanceWidth;
begin
  AssertEquals('Failed on 1', 800, FI.Widths[0].AdvanceWidth);
end;

procedure TTestFreeSansFont.TestWidths_HorMetric_0_LSB;
begin
  AssertEquals('Failed on 1', 35, FI.Widths[0].LSB);
end;

procedure TTestFreeSansFont.TestWidths_HorMetric_1_AdvanceWidth;
begin
  AssertEquals('Failed on 1', 0, FI.Widths[1].AdvanceWidth);
end;

procedure TTestFreeSansFont.TestWidths_HorMetric_1_LSB;
begin
  AssertEquals('Failed on 1', 0, FI.Widths[1].LSB);
end;

procedure TTestFreeSansFont.TestWidths_HorMetric_2586_AdvanceWidth;
begin
  AssertEquals('Failed on 1', 516, FI.Widths[2586].AdvanceWidth);
end;

procedure TTestFreeSansFont.TestWidths_HorMetric_2586_LSB;
begin
  AssertEquals('Failed on 1', 50, FI.Widths[2586].LSB);
end;

procedure TTestFreeSansFont.TestMaxP_VersionNumber;
begin
  AssertEquals('Failed on 1', '00010000', IntToHex(FI.MaxP.VersionNumber.Version, 8));
  AssertEquals('Failed on 2', 1, FI.MaxP.VersionNumber.Major);
  AssertEquals('Failed on 3', 0, FI.MaxP.VersionNumber.Minor);
end;

procedure TTestFreeSansFont.TestMaxP_numGlyphs;
begin
  AssertEquals('Failed on 1', 6272, FI.MaxP.numGlyphs);
end;

procedure TTestFreeSansFont.TestMaxP_maxPoints;
begin
  AssertEquals('Failed on 1', 439, FI.MaxP.maxPoints);
end;

procedure TTestFreeSansFont.TestMaxP_maxContours;
begin
  AssertEquals('Failed on 1', 100, FI.MaxP.maxContours);
end;

procedure TTestFreeSansFont.TestMaxP_maxCompositePoints;
begin
  AssertEquals('Failed on 1', 117, FI.MaxP.maxCompositePoints);
end;

procedure TTestFreeSansFont.TestMaxP_maxCompositeContours;
begin
  AssertEquals('Failed on 1', 8, FI.MaxP.maxCompositeContours);
end;

procedure TTestFreeSansFont.TestMaxP_maxZones;
begin
  AssertEquals('Failed on 1', 2, FI.MaxP.maxZones);
end;

procedure TTestFreeSansFont.TestMaxP_maxTwilightPoints;
begin
  AssertEquals('Failed on 1', 1, FI.MaxP.maxTwilightPoints);
end;

procedure TTestFreeSansFont.TestMaxP_maxStorage;
begin
  AssertEquals('Failed on 1', 2, FI.MaxP.maxStorage);
end;

procedure TTestFreeSansFont.TestMaxP_maxFunctionDefs;
begin
  AssertEquals('Failed on 1', 22, FI.MaxP.maxFunctionDefs);
end;

procedure TTestFreeSansFont.TestMaxP_maxInstructionDefs;
begin
  AssertEquals('Failed on 1', 0, FI.MaxP.maxInstructionDefs);
end;

procedure TTestFreeSansFont.TestMaxP_maxStackElements;
begin
  AssertEquals('Failed on 1', 256, FI.MaxP.maxStackElements);
end;

procedure TTestFreeSansFont.TestMaxP_maxSizeOfInstructions;
begin
  AssertEquals('Failed on 1', 1424, FI.MaxP.maxSizeOfInstructions);
end;

procedure TTestFreeSansFont.TestMaxP_maxComponentElements;
begin
  AssertEquals('Failed on 1', 8, FI.MaxP.maxComponentElements);
end;

procedure TTestFreeSansFont.TestMaxP_maxComponentDepth;
begin
  AssertEquals('Failed on 1', 4, FI.MaxP.maxComponentDepth);
end;

procedure TTestFreeSansFont.TestOS2Data_version;
begin
  AssertEquals('Failed on 1', 3, FI.OS2Data.version);
end;

procedure TTestFreeSansFont.TestOS2Data_xAvgCharWidth;
begin
  AssertEquals('Failed on 1', 657, FI.OS2Data.xAvgCharWidth);
end;

procedure TTestFreeSansFont.TestOS2Data_usWeightClass;
begin
  AssertEquals('Failed on 1', 400, FI.OS2Data.usWeightClass);
end;

procedure TTestFreeSansFont.TestOS2Data_usWidthClass;
begin
  AssertEquals('Failed on 1', 5, FI.OS2Data.usWidthClass);
end;

procedure TTestFreeSansFont.TestOS2Data_fsType;
begin
  AssertEquals('Failed on 1', '0000', IntToHex(FI.OS2Data.fsType, 4));
end;

procedure TTestFreeSansFont.TestOS2Data_ySubscriptXSize;
begin
  AssertEquals('Failed on 1', 650, FI.OS2Data.ySubscriptXSize);
end;

procedure TTestFreeSansFont.TestOS2Data_ySubscriptYSize;
begin
  AssertEquals('Failed on 1', 666, FI.OS2Data.ySubscriptYSize);
end;


procedure TTestFreeSansFont.TestOS2Data_ySubscriptXOffset;
begin
  AssertEquals('Failed on 1', 0, FI.OS2Data.ySubscriptXOffset);
end;


procedure TTestFreeSansFont.TestOS2Data_ySubscriptYOffset;
begin
  AssertEquals('Failed on 1', 200, FI.OS2Data.ySubscriptYOffset);
end;


procedure TTestFreeSansFont.TestOS2Data_ySuperscriptXSize;
begin
  AssertEquals('Failed on 1', 650, FI.OS2Data.ySuperscriptXSize);
end;


procedure TTestFreeSansFont.TestOS2Data_ySuperscriptYSize;
begin
  AssertEquals('Failed on 1', 666, FI.OS2Data.ySuperscriptYSize);
end;


procedure TTestFreeSansFont.TestOS2Data_ySuperscriptXOffset;
begin
  AssertEquals('Failed on 1', 0, FI.OS2Data.ySuperscriptXOffset);
end;


procedure TTestFreeSansFont.TestOS2Data_ySuperscriptYOffset;
begin
  AssertEquals('Failed on 1', 390, FI.OS2Data.ySuperscriptYOffset);
end;


procedure TTestFreeSansFont.TestOS2Data_yStrikeoutSize;
begin
  AssertEquals('Failed on 1', 49, FI.OS2Data.yStrikeoutSize);
end;


procedure TTestFreeSansFont.TestOS2Data_yStrikeoutPosition;
begin
  AssertEquals('Failed on 1', 258, FI.OS2Data.yStrikeoutPosition);
end;


procedure TTestFreeSansFont.TestOS2Data_sFamilyClass;
begin
  AssertEquals('Failed on 1', 8, Hi(FI.OS2Data.sFamilyClass));
  AssertEquals('Failed on 2', 5, Lo(FI.OS2Data.sFamilyClass));
  AssertEquals('Failed on 3', '0805', IntToHex(FI.OS2Data.sFamilyClass, 4));
end;

procedure TTestFreeSansFont.TestOS2Data_Panose;
begin
  AssertEquals('Failed on 1', 2, FI.OS2Data.panose[0]);
  AssertEquals('Failed on 2', 11, FI.OS2Data.panose[1]);
  AssertEquals('Failed on 3', 5, FI.OS2Data.panose[2]);
  AssertEquals('Failed on 4', 4, FI.OS2Data.panose[3]);
  AssertEquals('Failed on 5', 2, FI.OS2Data.panose[4]);
  AssertEquals('Failed on 6', 2, FI.OS2Data.panose[5]);
  AssertEquals('Failed on 7', 2, FI.OS2Data.panose[6]);
  AssertEquals('Failed on 8', 2, FI.OS2Data.panose[7]);
  AssertEquals('Failed on 9', 2, FI.OS2Data.panose[8]);
  AssertEquals('Failed on 10', 4, FI.OS2Data.panose[9]);
end;

procedure TTestFreeSansFont.TestOS2Data_ulUnicodeRange1;
begin
  AssertEquals('Failed on 1', 'E4839EFF', IntToHex(FI.OS2Data.ulUnicodeRange1, 8));
end;

procedure TTestFreeSansFont.TestOS2Data_ulUnicodeRange2;
begin
  AssertEquals('Failed on 1', '4600FDFF', IntToHex(FI.OS2Data.ulUnicodeRange2, 8));
end;

procedure TTestFreeSansFont.TestOS2Data_ulUnicodeRange3;
begin
  AssertEquals('Failed on 1', '000030A0', IntToHex(FI.OS2Data.ulUnicodeRange3, 8));
end;

procedure TTestFreeSansFont.TestOS2Data_ulUnicodeRange4;
begin
  AssertEquals('Failed on 1', '00000584', IntToHex(FI.OS2Data.ulUnicodeRange4, 8));
end;

procedure TTestFreeSansFont.TestOS2Data_achVendID;
var
  s: string;
begin
  s := FI.OS2Data.achVendID[0] + FI.OS2Data.achVendID[1] + FI.OS2Data.achVendID[2] +
        FI.OS2Data.achVendID[3];
  AssertEquals('Failed on 1', 'GNU ', s);
end;

procedure TTestFreeSansFont.TestOS2Data_fsSelection;
begin
  AssertEquals('Failed on 1', '0040', IntToHex(FI.OS2Data.fsSelection, 4));
end;

procedure TTestFreeSansFont.TestOS2Data_usFirstCharIndex;
begin
  AssertEquals('Failed on 1', '0020', IntToHex(FI.OS2Data.usFirstCharIndex, 4));
end;

procedure TTestFreeSansFont.TestOS2Data_usLastCharIndex;
begin
  AssertEquals('Failed on 1', 'FFFF', IntToHex(FI.OS2Data.usLastCharIndex, 4));
end;

procedure TTestFreeSansFont.TestOS2Data_sTypoAscender;
begin
  AssertEquals('Failed on 1', 800, FI.OS2Data.sTypoAscender);
end;

procedure TTestFreeSansFont.TestOS2Data_sTypoDescender;
begin
  AssertEquals('Failed on 1', -200, FI.OS2Data.sTypoDescender);
end;

procedure TTestFreeSansFont.TestOS2Data_sTypoLineGap;
begin
  AssertEquals('Failed on 1', 100, FI.OS2Data.sTypoLineGap);
end;

procedure TTestFreeSansFont.TestOS2Data_usWinAscent;
begin
  AssertEquals('Failed on 1', 900, FI.OS2Data.usWinAscent);
end;

procedure TTestFreeSansFont.TestOS2Data_usWinDescent;
begin
  AssertEquals('Failed on 1', 300, FI.OS2Data.usWinDescent);
end;

procedure TTestFreeSansFont.TestOS2Data_ulCodePageRange1;
begin
  AssertEquals('Failed on 1', '600001BF', IntToHex(FI.OS2Data.ulCodePageRange1, 8));
end;

procedure TTestFreeSansFont.TestOS2Data_ulCodePageRange2;
begin
  AssertEquals('Failed on 1', 'DFF70000', IntToHex(FI.OS2Data.ulCodePageRange2, 8));
end;

procedure TTestFreeSansFont.TestOS2Data_sxHeight;
begin
  AssertEquals('Failed on 1', 524, FI.OS2Data.sxHeight);
end;

procedure TTestFreeSansFont.TestOS2Data_sCapHeight;
begin
  AssertEquals('Failed on 1', 729, FI.OS2Data.sCapHeight);
end;

procedure TTestFreeSansFont.TestOS2Data_usDefaultChar;
begin
  AssertEquals('Failed on 1', '0000', IntToHex(FI.OS2Data.usDefaultChar, 4));
end;

procedure TTestFreeSansFont.TestOS2Data_usBreakChar;
begin
  AssertEquals('Failed on 1', '0020', IntToHex(FI.OS2Data.usBreakChar, 4));
end;

procedure TTestFreeSansFont.TestOS2Data_usMaxContext;
begin
  AssertEquals('Failed on 1', 10, FI.OS2Data.usMaxContext);
end;

procedure TTestFreeSansFont.TestPostScript_Format;
begin
  AssertEquals('Failed on 1', '00020000', IntToHex(FI.PostScript.Format.Version, 8));
  AssertEquals('Failed on 2', 2, FI.PostScript.Format.Major);
  AssertEquals('Failed on 3', 0, FI.PostScript.Format.Minor);
end;

procedure TTestFreeSansFont.TestPostScript_ItalicAngle;
begin
  AssertEquals('Failed on 1', 0.0, FI.PostScript.ItalicAngle);
end;

procedure TTestFreeSansFont.TestPostScript_UnderlinePosition;
begin
  AssertEquals('Failed on 1', -176, FI.PostScript.UnderlinePosition);
end;

procedure TTestFreeSansFont.TestPostScript_underlineThickness;
begin
  AssertEquals('Failed on 1', 50, FI.PostScript.underlineThickness);
end;

procedure TTestFreeSansFont.TestPostScript_isFixedPitch;
begin
  AssertEquals('Failed on 1', 0, FI.PostScript.isFixedPitch);
end;

procedure TTestFreeSansFont.TestPostScript_minMemType42;
begin
  AssertEquals('Failed on 1', 0, FI.PostScript.minMemType42);
end;

procedure TTestFreeSansFont.TestPostScript_maxMemType42;
begin
  AssertEquals('Failed on 1', 0, FI.PostScript.maxMemType42);
end;

procedure TTestFreeSansFont.TestPostScript_minMemType1;
begin
  AssertEquals('Failed on 1', 0, FI.PostScript.minMemType1);
end;

procedure TTestFreeSansFont.TestPostScript_maxMemType1;
begin
  AssertEquals('Failed on 1', 0, FI.PostScript.maxMemType1);
end;


initialization
  RegisterTest({$ifdef fptest}'fpParseTTF',{$endif}TTestEmptyParseTTF{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpParseTTF',{$endif}TTestLiberationFont{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpParseTTF',{$endif}TTestFreeSansFont{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpParseTTF',{$endif}TTestLiberationItalicFont{$ifdef fptest}.Suite{$endif});

end.

