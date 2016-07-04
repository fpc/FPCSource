{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    This unit reads and extracts info from a TTF font file.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpparsettf;

{$mode objfpc}
{$h+}
{ $R+}

{.$define gdebug}

interface

uses
  Classes, SysUtils, fpttfencodings;

type
  ETTF = Class(Exception);

  // Tables recognized in this unit.
  TTTFTableType = (ttUnknown,ttHead,tthhea,ttmaxp,tthmtx,ttcmap,ttname,ttOS2,ttpost);

  TSmallintArray = Packed Array of Int16;
  TWordArray = Packed Array of UInt16;

  { Signed Fixed 16.16 Float }
  TF16Dot16 = type Int32;

  TFixedVersionRec = packed record
    case Integer of
      0:  (Minor, Major: Word);
      1:  (Version: Cardinal);
  end;

  TTableDirectory = Packed Record
    FontVersion : TFixedVersionRec;
    Numtables : Word;
    SearchRange : Word;
    EntrySelector : Word;
    RangeShift : Word;
  end;

  TTableDirectoryEntry = Packed Record
    Tag: Array[1..4] of char;
    checkSum : Cardinal;
    offset : Cardinal;
    Length : Cardinal;
  end;
  TTableDirectoryEntries = Array of TTableDirectoryEntry;

  TLongHorMetric = Packed record
    AdvanceWidth : Word;
    LSB: Smallint;              { leftSideBearing }
  end;
  TLongHorMetrics = Packed Array of TLongHorMetric;

Type
  TPostScript = Packed Record
    Format : TFixedVersionRec;
    ItalicAngle : TF16Dot16;
    UnderlinePosition : SmallInt;
    underlineThickness : SmallInt;
    isFixedPitch : Cardinal;
    minMemType42 : Cardinal;
    maxMemType42 : Cardinal;
    minMemType1 : Cardinal;
    maxMemType1 : Cardinal;
  end;

  TMaxP = Packed Record
    VersionNumber : TFixedVersionRec;
    numGlyphs : Word;
    maxPoints : Word;
    maxContours : Word;
    maxCompositePoints : word;
    maxCompositeContours : word;
    maxZones : Word;
    maxTwilightPoints : word;
    maxStorage : Word;
    maxFunctionDefs : Word;
    maxInstructionDefs : Word;
    maxStackElements : Word;
    maxSizeOfInstructions : word;
    maxComponentElements : Word;
    maxComponentDepth : Word;
  end;

  TOS2Data = Packed Record
    version : Word;
    xAvgCharWidth : SmallInt;
    usWeightClass : Word;
    usWidthClass : Word;
    fsType : SmallInt;
    ySubscriptXSize : SmallInt;
    ySubscriptYSize : SmallInt;
    ySubscriptXOffset : SmallInt;
    ySubscriptYOffset : Smallint;
    ySuperscriptXSize : Smallint;
    ySuperscriptYSize : Smallint;
    ySuperscriptXOffset : Smallint;
    ySuperscriptYOffset : Smallint;
    yStrikeoutSize : SmallInt;
    yStrikeoutPosition : Smallint;
    sFamilyClass : SmallInt;    // we could split this into a record of Class & SubClass values.
    panose : Array[0..9] of byte;
    ulUnicodeRange1 : Cardinal;
    ulUnicodeRange2 : Cardinal;
    ulUnicodeRange3 : Cardinal;
    ulUnicodeRange4 : Cardinal;
    achVendID : Array[0..3] of char;
    fsSelection : word;
    usFirstCharIndex : Word;
    usLastCharIndex : Word;
    sTypoAscender: Smallint;
    sTypoDescender : Smallint;
    sTypoLineGap : Smallint;
    usWinAscent : Word;
    usWinDescent : Word;
    ulCodePageRange1 : Cardinal;
    ulCodePageRange2 : Cardinal;
    sxHeight : smallint;
    sCapHeight : smallint;
    usDefaultChar : word;
    usBreakChar : word;
    usMaxContext  : word;
  end;

  { Nicely described at [https://www.microsoft.com/typography/otspec/head.htm] }
  THead = Packed record
    FileVersion : TFixedVersionRec;
    FontRevision : TFixedVersionRec;
    CheckSumAdjustment : Cardinal;
    MagicNumber : Cardinal;
    Flags : Word;
    UnitsPerEm: word;
    Created : Int64;
    Modified : Int64;
    BBox: Packed array[0..3] of Smallint;
    MacStyle : word;
    LowestRecPPEM : word;
    FontDirectionHint : smallint;
    IndexToLocFormat : Smallint;
    glyphDataFormat : Smallint;
  end;

  { structure described at [https://www.microsoft.com/typography/otspec/hhea.htm] }
  THHead = packed record
    TableVersion : TFixedVersionRec;
    Ascender : Smallint;
    Descender : Smallint;
    LineGap : Smallint;
    AdvanceWidthMax : Word;
    MinLeftSideBearing : Smallint;
    MinRightSideBearing : Smallint;
    XMaxExtent : Smallint;
    CaretSlopeRise : Smallint;
    CaretSlopeRun : Smallint;
    Reserved : Array[0..4] of Smallint;
    metricDataFormat : Smallint;
    numberOfHMetrics : Word;
  end;

  { Character to glyph mapping
    Structure described at [https://www.microsoft.com/typography/otspec/cmap.htm] }
  TCmapHeader = packed record
    Version: word;
    SubTableCount: word;
  end;

  TCmapSubTableEntry = packed record
    PlatformID: word;
    EncodingID: word;
    Offset: Cardinal;
  end;
  TCmapSubTables = Array of TCmapSubTableEntry;

  TCmapFmt4 = packed record
    Format: word;
    Length: word;
    LanguageID: word;
    SegmentCount2: word;
    SearchRange: word;
    EntrySelector: word;
    RangeShift: word;
  end;

  TUnicodeMapSegment = Packed Record
    StartCode : Word;
    EndCode : Word;
    IDDelta : Smallint;
    IDRangeOffset : Word;
  end;
  TUnicodeMapSegmentArray = Array of TUnicodeMapSegment;

  TNameRecord = Packed Record
    PlatformID : Word;
    EncodingID : Word;
    LanguageID : Word;
    NameID : Word;
    StringLength : Word;
    StringOffset : Word;
  end;

  TNameEntry = Packed Record
    Info: TNameRecord;
    Value : String;
  end;
  TNameEntries = Array of TNameEntry;


  TTFFileInfo = class(TObject)
  private
    FFilename: string;
    FTableDir : TTableDirectory;
    FTables : TTableDirectoryEntries;
    FMaxp : TMaxP;
    FCmapH : TCMapHeader;
    FSubtables : TCmapSubTables;
    FUnicodeMap : TCmapFmt4;
    FUnicodeMapSegments : TUnicodeMapSegmentArray;
    FHead : THead;
    FHHEad : THHead;
    FOS2Data : TOS2Data;
    FPostScript : TPostScript;
    FWidths: TLongHorMetrics; // hmtx data
    // Needed to create PDF font def.
    FOriginalSize : Cardinal;
    FMissingWidth: Integer;
    FNameEntries: TNameEntries;
    { This only applies to TFixedVersionRec values. }
    function FixMinorVersion(const AMinor: word): word;
    function GetMissingWidth: integer;
  Protected
    // Stream reading functions.
    Function IsNativeData : Boolean; virtual;
    function ReadShort(AStream: TStream): Smallint; inline;
    function ReadULong(AStream: TStream): Longword; inline;
    function ReadUShort(AStream: TStream): Word; inline;
    // Parse the various well-known tables
    procedure ParseHead(AStream : TStream); virtual;
    procedure ParseHhea(AStream : TStream); virtual;
    procedure ParseMaxp(AStream : TStream); virtual;
    procedure ParseHmtx(AStream : TStream); virtual;
    procedure ParseCmap(AStream : TStream); virtual;
    procedure ParseName(AStream : TStream); virtual;
    procedure ParseOS2(AStream : TStream); virtual;
    procedure ParsePost(AStream : TStream); virtual;
    // Make differences for postscript fonts
    procedure PrepareEncoding(Const AEncoding : String);
    function MakeDifferences: String; virtual;
    // Utility function to convert FShort to natural units
    Function ToNatural(AUnit: Smallint) : Smallint;
  public
    Chars: TWordArray;
    CharWidth: array[0..255] of SmallInt;
    CharNames: PTTFEncodingNames;
    CharCodes: PTTFEncodingValues;
    CharBase:  PTTFEncodingNames;
    PostScriptName: string;
    FamilyName: string;
    destructor Destroy; override;
    { Returns the Glyph Index value in the TTF file, where AValue is the ordinal value of a character. }
    function  GetGlyphIndex(AValue: word): word;
    // Load a TTF file from file or stream.
    Procedure LoadFromFile(const AFileName : String);
    Procedure LoadFromStream(AStream: TStream); virtual;
    // Checks if Embedded is allowed, and also prepares CharWidths array. NOTE: this is possibly not needed any more.
    procedure PrepareFontDefinition(const Encoding:string; Embed: Boolean);

    // The following are only valid after the file was succesfully read.

    Function Flags : Integer;
    Function Bold: Boolean;
    Function StemV: SmallInt;
    Function Embeddable : Boolean;
    Function Ascender: SmallInt;
    Function Descender: SmallInt;
    { Also know as the linegap. "Leading" is the gap between two lines. }
    Function Leading: SmallInt;
    Function CapHeight: SmallInt;
    { Returns the glyph advance width, based on the AIndex (glyph index) value. The result is in font units. }
    function GetAdvanceWidth(AIndex: word): word;
    function ItalicAngle: single;
    { max glyph bounding box values - as space separated values }
    function BBox: string;
    property MissingWidth: Integer read GetMissingWidth;
    { original font file size }
    property OriginalSize: Cardinal read FOriginalSize;
    property Filename: string read FFilename;
    Property Directory : TTableDirectory Read FTableDir;
    Property Tables : TTableDirectoryEntries Read FTables;

    Property Head : THead Read FHead;
    Property HHead : THHead Read FHHead;
    property CmapH : TCMapHeader Read FCmapH;
    property CmapSubtables : TCmapSubTables Read FSubtables;
    property CmapUnicodeMap : TCmapFmt4 Read FUnicodeMap;
    property CmapUnicodeMapSegments : TUnicodeMapSegmentArray Read FUnicodeMapSegments;
    Property Widths : TLongHorMetrics Read FWidths;
    Property MaxP : TMaxP Read FMaxP;
    Property OS2Data : TOS2Data Read FOS2Data;
    Property PostScript : TPostScript Read FPostScript;
    property NameEntries: TNameEntries read FNameEntries;
  end;

type
  TConvertResult = (trNoError, trNullSrc, trNullDest, trDestExhausted,
    trInvalidChar, trUnfinishedChar);

  TConvertOption = (toInvalidCharError, toInvalidCharToSymbol,
    toUnfinishedCharError, toUnfinishedCharToSymbol);
  TConvertOptions = set of TConvertOption;

// Convert string to known table type
Function GetTableType(Const AName : String) : TTTFTableType;
function StrToUTF16Hex(const AValue: UnicodeString; AIncludeBOM: boolean = True): AnsiString;
{ To overcome the annoying compiler hint: "Local variable does not seem to be initialized" }
procedure FillMem(Dest: pointer; Size: longint; Data: Byte );


Const
  TTFTableNames : Array[TTTFTableType] of String
                 = ('','head','hhea','maxp','hmtx','cmap','name','OS/2','post');


Const
  // Platform IDs used in the Name section
  NamePlatFormIDAppleUnicode = 0;
  NamePlatFormIDMacIntosh    = 1;
  NamePlatFormIDISO          = 2;
  NamePlatFormIDMicrosoft    = 3;

  // Name IDs used in the Name section
  NameIDCopyRight      = 0;
  NameIDFontFamily     = 1;
  NameIDFontSubFamily  = 2;
  NameIDFontIdentifier = 3;
  NameIDFullFontName   = 4;
  NamdIDVersionString  = 5;
  NameIDPostScriptName = 6;
  NameIDTradeMark      = 7;

  NameMSEncodingUndefined = 0;
  NameMSEncodingUGL       = 1;

implementation


resourcestring
  rsFontEmbeddingNotAllowed = 'Font licence does not allow embedding';

Function GetTableType(Const AName : String) : TTTFTableType;
begin
  Result:=High(TTTFTableType);
  While (Result<>ttUnknown) and (CompareText(AName,TTFTableNames[Result])<>0) do
    Result:=Pred(Result);
end;

function StrToUTF16Hex(const AValue: UnicodeString; AIncludeBOM: boolean = True): AnsiString;
var
  pc: ^Word;
  i: integer;
begin
  if AIncludeBOM then
    Result := 'FEFF'   // BOM marker to indicate UTF-16BE (big-endian) encoding scheme
  else
    Result := '';
  for i := 1 to Length(AValue) do
  begin
    pc := @AValue[i];
    Result := Result + AnsiString(IntToHex(pc^, 4));
  end;
end;

procedure FillMem(Dest: pointer; Size: longint; Data: Byte );
begin
  FillChar(Dest^, Size, Data);
end;

function TTFFileInfo.ReadULong(AStream: TStream): Longword;inline;
begin
  Result:=0;
  AStream.ReadBuffer(Result,SizeOf(Result));
  if Not IsNativeData then
    Result:=BEtoN(Result);
end;

function TTFFileInfo.ReadUShort(AStream: TStream): Word;inline;
begin
  Result:=0;
  AStream.ReadBuffer(Result,SizeOf(Result));
  if Not IsNativeData then
    Result:=BEtoN(Result);
end;

function TTFFileInfo.ReadShort(AStream: TStream): Smallint;inline;
begin
  Result:=SmallInt(ReadUShort(AStream));
end;

procedure TTFFileInfo.ParseHead(AStream : TStream);
var
  i : Integer;
begin
  AStream.ReadBuffer(FHead,SizeOf(FHead));
  if IsNativeData then
    exit;
  FHead.FileVersion.Version := BEtoN(FHead.FileVersion.Version);
  FHead.FileVersion.Minor := FixMinorVersion(FHead.FileVersion.Minor);
  FHead.FontRevision.Version := BEtoN(FHead.FontRevision.Version);
  FHead.FontRevision.Minor := FixMinorVersion(FHead.FontRevision.Minor);
  FHead.Created := BEtoN(FHead.Created);
  FHead.Modified := BEtoN(FHead.Modified);
  For i:=0 to 3 do
    FHead.BBox[i]:=BEtoN(FHead.BBox[i]);
  FHead.CheckSumAdjustment:=BEtoN(FHead.CheckSumAdjustment);
  FHead.MagicNumber:=BEtoN(FHead.MagicNumber);
  FHead.Flags:=BEtoN(FHead.Flags);
  FHead.UnitsPerEm:=BEtoN(FHead.UnitsPerEm);
  FHead.MacStyle:=BEtoN(FHead.MacStyle);
  FHead.LowestRecPPEM:=BEtoN(FHead.LowestRecPPEM);
  FHead.FontDirectionHint:=BEtoN(FHead.FontDirectionHint);
  FHead.IndexToLocFormat:=BEtoN(FHead.IndexToLocFormat);
  FHead.glyphDataFormat:=BEtoN(FHead.glyphDataFormat);
end;

procedure TTFFileInfo.ParseHhea(AStream : TStream);

begin
  AStream.ReadBuffer(FHHEad,SizeOf(FHHEad));
  if IsNativeData then
    exit;
  FHHEad.TableVersion.Version := BEToN(FHHEad.TableVersion.Version);
  FHHEad.TableVersion.Minor := FixMinorVersion(FHHEad.TableVersion.Minor);
  FHHEad.Ascender:=BEToN(FHHEad.Ascender);
  FHHEad.Descender:=BEToN(FHHEad.Descender);
  FHHEad.LineGap:=BEToN(FHHEad.LineGap);
  FHHEad.MinLeftSideBearing:=BEToN(FHHEad.MinLeftSideBearing);
  FHHEad.MinRightSideBearing:=BEToN(FHHEad.MinRightSideBearing);
  FHHEad.XMaxExtent:=BEToN(FHHEad.XMaxExtent);
  FHHEad.CaretSlopeRise:=BEToN(FHHEad.CaretSlopeRise);
  FHHEad.CaretSlopeRun:=BEToN(FHHEad.CaretSlopeRun);
  FHHEad.metricDataFormat:=BEToN(FHHEad.metricDataFormat);
  FHHEad.numberOfHMetrics:=BEToN(FHHEad.numberOfHMetrics);
  FHHead.AdvanceWidthMax := BEToN(FHHead.AdvanceWidthMax);
end;

procedure TTFFileInfo.ParseMaxp(AStream : TStream);

begin
  AStream.ReadBuffer(FMaxP,SizeOf(TMaxP));
  if IsNativeData then
    exit;
  With FMaxP do
    begin
    VersionNumber.Version := BEtoN(VersionNumber.Version);
    VersionNumber.Minor := FixMinorVersion(VersionNumber.Minor);
    numGlyphs:=BEtoN(numGlyphs);
    maxPoints:=BEtoN(maxPoints);
    maxContours:=BEtoN(maxContours);
    maxCompositePoints :=BEtoN(maxCompositePoints);
    maxCompositeContours :=BEtoN(maxCompositeContours);
    maxZones :=BEtoN(maxZones);
    maxTwilightPoints :=BEtoN(maxTwilightPoints);
    maxStorage :=BEtoN(maxStorage);
    maxFunctionDefs :=BEtoN(maxFunctionDefs);
    maxInstructionDefs :=BEtoN(maxInstructionDefs);
    maxStackElements :=BEtoN(maxStackElements);
    maxSizeOfInstructions :=BEtoN(maxSizeOfInstructions);
    maxComponentElements :=BEtoN(maxComponentElements);
    maxComponentDepth :=BEtoN(maxComponentDepth);
    end;
end;

procedure TTFFileInfo.ParseHmtx(AStream : TStream);

var
  i : Integer;

begin
  SetLength(FWidths,FHHead.numberOfHMetrics);
  AStream.ReadBuffer(FWidths[0],SizeOf(TLongHorMetric)*Length(FWidths));
  if IsNativeData then
    exit;
  for I:=0 to FHHead.NumberOfHMetrics-1 do
    begin
    FWidths[I].AdvanceWidth:=BEtoN(FWidths[I].AdvanceWidth);
    FWidths[I].LSB:=BEtoN(FWidths[I].LSB);
    end;
end;


procedure TTFFileInfo.ParseCmap(AStream : TStream);
var
  SegCount: Word;
  GiD,I,J,UE: Integer;
  TT,TableStartPos: LongWord;
  Segm : TUnicodeMapSegment;
  GlyphIDArray : Array of word;
  S : TStream;

begin
  TableStartPos:=AStream.Position;
  FCMapH.Version:=ReadUShort(AStream);
  FCMapH.SubtableCount:=ReadUShort(AStream);
  SetLength(FSubtables,CMapH.SubtableCount);
  for I:= 0 to FCMapH.SubtableCount-1 do
    begin
    FSubtables[i].PlatformID:=ReadUShort(AStream);
    FSubtables[i].EncodingID:=ReadUShort(AStream);
    FSubtables[i].Offset:=ReadULong(AStream); // 4 bytes - Offset of subtable
    end;
  UE:=FCMapH.SubtableCount-1;
  While (UE>=0) and ((FSubtables[UE].PlatformID<>3) or (FSubtables[UE].EncodingID<> 1)) do
    Dec(UE);
  if (UE=-1) then
    Raise ETTF.Create('No Format 4 map (unicode) table found <'+FFileName + ' - ' + PostScriptName+'>');
  TT:=TableStartPos+FSubtables[UE].Offset;
  AStream.Position:=TT;
  FUnicodeMap.Format:= ReadUShort(AStream);               // 2 bytes - Format of subtable
  if (FUnicodeMap.Format<>4) then
    Raise ETTF.CreateFmt('Unexpected unicode subtable format, expected 4, got %s',[FUnicodeMap.Format]);
  FUnicodeMap.Length:=ReadUShort(AStream);
  S:=TMemoryStream.Create;
  try
    // Speed up the process, read everything in a single mem block.
    S.CopyFrom(AStream,Int64(FUnicodeMap.Length)-4);
    S.Position:=0;
    FUnicodeMap.LanguageID:=ReadUShort(S);
    FUnicodeMap.SegmentCount2:=ReadUShort(S);            // 2 bytes - Segments count
    FUnicodeMap.SearchRange:=ReadUShort(S);
    FUnicodeMap.EntrySelector:=ReadUShort(S);
    FUnicodeMap.RangeShift:=ReadUShort(S);
    SegCount:=FUnicodeMap.SegmentCount2 div 2;
    SetLength(FUnicodeMapSegments,SegCount);
    for i:=0 to SegCount-1 do
      FUnicodeMapSegments[i].EndCode:=ReadUShort(S);
    ReadUShort(S);
    for i:=0 to SegCount-1 do
      FUnicodeMapSegments[i].StartCode:=ReadUShort(S);
    for i:=0 to SegCount-1 do
      FUnicodeMapSegments[i].IDDelta:=ReadShort(S);
    for i:=0 to SegCount-1 do
      FUnicodeMapSegments[i].IDRangeOffset:=ReadUShort(S);
    UE:=S.Position;
    UE:=(S.Size-UE) div 2;
    SetLength(GlyphIDArray,UE);
    For J:=0 to UE-1 do
      GlyphIDArray[J]:=ReadUShort(S);
    J:=0;
    for i:=0 to SegCount-1 do
      With FUnicodeMapSegments[i] do
        if (EndCode>J) then
          J:=EndCode;
    SetLength(Chars,J+1);
    for i:=0 to SegCount-1 do
      begin
      Segm:=FUnicodeMapSegments[i];
      for J:=Segm.StartCode to Segm.EndCode do
        if J<>$FFFF then // Last block has $FFFF as start/end code.
          begin
          if Segm.IDRangeOffset=0 then
            Gid:=J+Segm.IDDelta
          else
            begin
            Gid:=GlyphIDArray[Segm.IDRangeOffset div 2 + i-segcount - Segm.startCode+j];
            if (Gid>0) then
              Gid:= Gid+Segm.IDDelta;
            end;
          if (Gid>=65536) then
            Gid:=Gid-65536;
          if Gid>0 then
            Chars[J]:=Gid
          else
            Chars[J]:=0;
          end;
      end;
  finally
    S.Free;
  end;
end;

procedure TTFFileInfo.ParseName(AStream : TStream);
var
  I,J,Count : Integer;
  StringOffset: Word;
  TableStartPos: LongWord;
  S : AnsiString;
  W : Widestring;
  N : TNameRecord;
  E : TNameEntries;
  WA : Array of word;

begin
  TableStartPos:= AStream.Position;                   // memorize Table start position
  ReadUShort(AStream);                  // skip 2 bytes - Format
  Count:=ReadUShort(AStream);                        // 2 bytes
  StringOffset:=ReadUShort(AStream);                 // 2 bytes
  E := FNameEntries;
  SetLength(E,Count);
  FillMem(@N, SizeOf(TNameRecord), 0);
  //  Read Descriptors
  for I:=0 to Count-1 do
  begin
    AStream.ReadBuffer(N,SizeOf(TNameRecord));
    N.PlatFormID:=BeTon(N.PlatFormID);
    N.EncodingID:=BeTon(N.EncodingID);
    N.LanguageID:=BeTon(N.LanguageID);
    N.NameID:=BeTon(N.NameID);
    N.StringLength:=BeTon(N.StringLength);
    N.StringOffset:=BeToN(N.StringOffset);
    E[i].Info:=N;
  end;  { for i ... }
  //  Read Values
  for I:=0 to Count-1 do
  begin
    AStream.Position:=Int64(TableStartPos)+StringOffset+E[i].Info.StringOffset;
    if E[i].Info.EncodingID=1 then
    begin
      SetLength(WA,E[i].Info.StringLength div 2);
      SetLength(W,Length(WA));
      AStream.Read(WA[0],SizeOf(Word)*Length(W));    // 1 byte
      For J:=0 to Length(WA)-1 do
        W[J+1]:=WideChar(Beton(WA[J]));
      E[i].Value:=string(W);
    end
    else
    begin
      SetLength(S,E[i].Info.StringLength);
      AStream.Read(S[1],SizeOf(AnsiChar)*Length(S));    // 1 byte
      E[i].Value:=S;
    end;
    {$IFDEF gdebug}
      writeln('-------------------');
      writeln('LanguageID = ', E[i].Info.LanguageID);
      writeln('EncodingID = ', E[i].Info.EncodingID);
      writeln('NameID = ', E[i].Info.NameID);
      writeln('Value = ', E[i].Value);
    {$ENDIF}
    if (PostScriptName='')
       and (E[i].Info.NameID=NameIDPostScriptName)
       and (E[i].Info.EncodingID=NameMSEncodingUGL) then
      PostScriptName:=E[i].Value;
    if (FamilyName = '')
        and (E[i].Info.NameID = NameIDFontFamily)
        and (E[i].Info.LanguageID = 1033)
        and (E[i].Info.EncodingID = 1) then
      FamilyName := E[i].Value;
  end; { for i ... }
end;

procedure TTFFileInfo.ParseOS2(AStream : TStream);

begin
  FillWord(FOS2Data,SizeOf(TOS2Data) div 2,0);
  // -18, so version 1 will not overflow
  AStream.ReadBuffer(FOS2Data,SizeOf(TOS2Data)-18);
  if Not isNativeData then
    With FOS2Data do
      begin
      version:=BeToN(version);
      xAvgCharWidth:=BeToN(xAvgCharWidth);
      usWeightClass:=BeToN(usWeightClass);
      usWidthClass:=BeToN(usWidthClass);
      fsType:=BeToN(fsType);
      ySubscriptXSize:=BeToN(ySubscriptXSize);
      ySubscriptYSize:=BeToN(ySubscriptYSize);
      ySubscriptXOffset:=BeToN(ySubscriptXOffset);
      ySubscriptYOffset:=BeToN(ySubscriptYOffset);
      ySuperscriptXSize:=BeToN(ySuperscriptXSize);
      ySuperscriptYSize:=BeToN(ySuperscriptYSize);
      ySuperscriptXOffset:=BeToN(ySuperscriptXOffset);
      ySuperscriptYOffset:=BeToN(ySuperscriptYOffset);
      yStrikeoutSize:=BeToN(yStrikeoutSize);
      yStrikeoutPosition:=BeToN(yStrikeoutPosition);
      sFamilyClass:=BeToN(sFamilyClass);
      ulUnicodeRange1:=BeToN(ulUnicodeRange1);
      ulUnicodeRange2:=BeToN(ulUnicodeRange2);
      ulUnicodeRange3:=BeToN(ulUnicodeRange3);
      ulUnicodeRange4:=BeToN(ulUnicodeRange4);
      fsSelection:=BeToN(fsSelection);
      usFirstCharIndex:=BeToN(usFirstCharIndex);
      usLastCharIndex:=BeToN(usLastCharIndex);
      sTypoAscender:=BeToN(sTypoAscender);
      sTypoDescender:=BeToN(sTypoDescender);
      sTypoLineGap:=BeToN(sTypoLineGap);
      usWinAscent:=BeToN(usWinAscent);
      usWinDescent:=BeToN(usWinDescent);
      // We miss 7 fields
      end;
  With FOS2Data do
    begin
    // Read remaining 7 fields' data depending on version
    if Version>=1 then
      begin
      ulCodePageRange1:=ReadULong(AStream);
      ulCodePageRange2:=ReadULong(AStream);
      end;
    if Version>=2 then
      begin
      sxHeight:=ReadShort(AStream);
      sCapHeight:=ReadShort(AStream);
      usDefaultChar:=ReadUShort(AStream);
      usBreakChar:=ReadUShort(AStream);
      usMaxContext:=ReadUShort(AStream);
      end;
    end;
end;

procedure TTFFileInfo.ParsePost(AStream : TStream);

begin
  AStream.ReadBuffer(FPostScript,SizeOf(TPostScript));
  if not IsNativeData then
    With FPostScript do
      begin
      Format.Version := BEtoN(Format.Version);
      Format.Minor := FixMinorVersion(Format.Minor);
      ItalicAngle:=BeToN(ItalicAngle);
      UnderlinePosition:=BeToN(UnderlinePosition);
      underlineThickness:=BeToN(underlineThickness);
      isFixedPitch:=BeToN(isFixedPitch);
      minMemType42:=BeToN(minMemType42);
      maxMemType42:=BeToN(maxMemType42);
      minMemType1:=BeToN(minMemType1);
      maxMemType1:=BeToN(maxMemType1);
      end;
end;

procedure TTFFileInfo.LoadFromFile(const AFileName: String);

Var
  AStream: TFileStream;
begin
  FFilename := AFilename;
  AStream:= TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TTFFileInfo.LoadFromStream(AStream : TStream);
var
  i: Integer;
  tt : TTTFTableType;
begin
  FOriginalSize:= AStream.Size;
  AStream.ReadBuffer(FTableDir,Sizeof(TTableDirectory));
  if not isNativeData then
    With FTableDir do
      begin
      FontVersion.Version := BEtoN(FontVersion.Version);
      FontVersion.Minor := FixMinorVersion(FontVersion.Minor);
      Numtables:=BeToN(Numtables);
      SearchRange:=BeToN(SearchRange);
      EntrySelector:=BeToN(EntrySelector);
      RangeShift:=BeToN(RangeShift);
      end;
  SetLength(FTables,FTableDir.Numtables);
  AStream.ReadBuffer(FTables[0],FTableDir.NumTables*Sizeof(TTableDirectoryEntry));
  if Not IsNativeData then
    For I:=0 to Length(FTables)-1 do
      With FTables[I] do
        begin
        checkSum:=BeToN(checkSum);
        offset:=BeToN(offset);
        Length:=BeToN(Length);
        end;
  for I:=0 to FTableDir.NumTables-1 do
    begin
    TT:=GetTableType(FTables[I].Tag);
    if (TT<>ttUnknown) then
      begin
      AStream.Position:=FTables[i].Offset;
      Case TT of
        tthead: ParseHead(AStream);
        ttHhea: ParseHhea(AStream);
        ttmaxp: ParseMaxp(AStream);
        tthmtx: ParseHmtx(AStream);
        ttcmap: ParseCmap(AStream);
        ttname: ParseName(AStream);
        ttos2 : ParseOS2(AStream);
        ttPost: ParsePost(AStream);
      end;
      end;
    end;
end;

procedure TTFFileInfo.PrepareFontDefinition(const Encoding: string; Embed: Boolean);
var
  I : Integer;
begin
  if embed and not Embeddable then
    raise ETTF.Create(rsFontEmbeddingNotAllowed);
  PrepareEncoding(Encoding);
//  MissingWidth:=ToNatural(Widths[Chars[CharCodes^[32]]].AdvanceWidth);  // Char(32) - Space character
  FMissingWidth := Widths[Chars[CharCodes^[32]]].AdvanceWidth;  // Char(32) - Space character
  for I:=0 to 255 do
    begin
    if (CharCodes^[i]>=0) and (CharCodes^[i]<=High(Chars))
    and (Widths[Chars[CharCodes^[i]]].AdvanceWidth> 0) and (CharNames^[i]<> '.notdef') then
      CharWidth[I]:= ToNatural(Widths[Chars[CharCodes^[I]]].AdvanceWidth)
    else
      CharWidth[I]:= FMissingWidth;
    end;
end;

procedure TTFFileInfo.PrepareEncoding(const AEncoding: String);
var
  TE : TTTFEncoding;
  V : PTTFEncodingValues;
begin
  TE:=GetEncoding(AEncoding);
  if (TE<>teUnknown) then
    GetEncodingTables(Te,CharNames,CharCodes);
  // Needed to make difference
  GetEncodingTables(Te,CharBase,V);
end;

function TTFFileInfo.MakeDifferences: String;
var
  i,l: Integer;
begin
  Result:= '';
  L:= 0;
  for i:=32 to 255 do
    if CharNames^[i]<>CharBase^[i]  then
      begin
      if (i<>l+1) then
        Result:= Result+IntToStr(i)+' ';
      l:=i;
      Result:= Result+'/'+CharNames^[i]+' ';
      end;
end;

function TTFFileInfo.Bold: Boolean;
begin
  Bold:=(FOS2Data.fsSelection and 32)<>0;
end;

function TTFFileInfo.StemV: SmallInt;
begin
  if Bold then
    StemV:= 120
  else
    StemV:= 70;
end;

function TTFFileInfo.Embeddable: Boolean;
begin
  With FOS2Data do
    Result:=(FsType<> 2) and ((FsType and 512)= 0);
end;

function TTFFileInfo.Ascender: SmallInt;
begin
  Result:=FOS2Data.sTypoAscender;
end;

function TTFFileInfo.Descender: SmallInt;
begin
  Result := FOS2Data.sTypoDescender;
end;

function TTFFileInfo.Leading: SmallInt;
begin
  Result := FOS2Data.sTypoLineGap;
end;

function TTFFileInfo.CapHeight: SmallInt;
begin
  With FOS2Data do
    begin
    if Version>= 2 then
      Result:=sCapHeight
    else
      Result:=Ascender;
    end;
end;

function TTFFileInfo.GetGlyphIndex(AValue: word): word;
begin
  result := Chars[AValue];
end;

function TTFFileInfo.GetAdvanceWidth(AIndex: word): word;
begin
  Result := Widths[AIndex].AdvanceWidth;
end;

function TTFFileInfo.ItalicAngle: single;
begin
  Result := FPostScript.ItalicAngle / 65536.0;
end;

function TTFFileInfo.BBox: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to 3 do
  begin
    if i > 0 then
      Result := Result + ' ';
    Result := Result + IntToStr(ToNatural(FHead.BBox[I]));
  end;
end;

destructor TTFFileInfo.Destroy;
begin
  SetLength(FNameEntries, 0);
  inherited Destroy;
end;

{ Implementation based on a PHP ttf reader unit.
  http://www.4real.gr/TTF.php.txt }
function TTFFileInfo.FixMinorVersion(const AMinor: word): word;
var
  d: double;
begin
  d := AMinor / 65536;
  Result := round(d*10000);
end;

function TTFFileInfo.GetMissingWidth: integer;
begin
  if FMissingWidth = 0 then
  begin
    FMissingWidth := Widths[Chars[CharCodes^[32]]].AdvanceWidth;  // Char(32) - Space character
  end;
  Result := FMissingWidth;
end;

function TTFFileInfo.IsNativeData: Boolean;
begin
  Result:=False;
end;

function TTFFileInfo.ToNatural(AUnit: Smallint): Smallint;
begin
  if FHead.UnitsPerEm=0 then
    Result:=0
  else
    Result:=Round(AUnit*1000/FHead.UnitsPerEm);
end;

function TTFFileInfo.Flags: Integer;
begin
  Result := 32;
  if FPostScript.IsFixedPitch<>0 then
    Result := Result+1;
  if FPostScript.ItalicAngle<>0 then
    Result := Result+64;
end;

end.
