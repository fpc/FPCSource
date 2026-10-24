{
    Tests for fpqrcodegen and fpimgqrcode: symbol structure after ISO/IEC 18004,
    codewords checked by a decoder of the test itself, and drawing into images.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcqrcode;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     fpqrcodegen, fpimgqrcode, fpreadbmp, fpwritebmp;

type
  TTestQRCode = class(TTestCase)
  private
    FCode: TQRBuffer;
    FTemp: TQRBuffer;
    FGenerator: TQRCodeGenerator;
    // Encodes aText into FCode and returns the result of QREncodeText.
    function Encode(const aText: String; aEcl: TQRErrorLevelCorrection; aMinVersion, aMaxVersion: TQRVersion;
      aMask: TQRMask; aBoost: Boolean): Boolean;
    // Encodes aText into FCode and fails if that does not succeed.
    procedure MustEncode(const aText: String; aEcl: TQRErrorLevelCorrection; aMinVersion, aMaxVersion: TQRVersion;
      aMask: TQRMask; aBoost: Boolean);
    // The module at (aX, aY) of FCode.
    function Module(aX, aY: Integer): Boolean;
    // The version of FCode, from its size.
    function Version: Integer;
    // The 15 format bits of the first copy, next to the top left finder.
    function FormatBits1: Integer;
    // The 15 format bits of the second copy, split between the other two finders.
    function FormatBits2: Integer;
    // The 18 version bits of the block left of the top right finder.
    function VersionBits1: Integer;
    // The 18 version bits of the block above the bottom left finder.
    function VersionBits2: Integer;
    // True if (aX, aY) is a function module of a symbol of version 1 to 6.
    function IsFunctionModule(aX, aY: Integer): Boolean;
    // The codewords of FCode, version 1 or 2, unmasked and read in the placement order.
    function ReadCodewords: TBytes;
    // The number of data codewords of version 1 or 2 at the ECC level of the format bits.
    function DataCodewordCount: Integer;
    // The text of the data codewords of a version 1 to 9 symbol.
    function DecodeData(const aCodewords: TBytes; aCount: Integer): String;
    // Encodes aText at version 1 or 2 and fails unless the symbol decodes back to it.
    procedure CheckDecodes(const aText: String; aEcl: TQRErrorLevelCorrection; aMask: TQRMask);
    // Fails unless a finder pattern and its separator are at the top left corner (aX, aY).
    procedure CheckFinder(const aMessage: String; aX, aY: Integer);
    // Fails unless an alignment pattern is centred on (aX, aY).
    procedure CheckAlignment(const aMessage: String; aX, aY: Integer);
    // Fails unless the codewords of FCode equal aExpected.
    procedure CheckCodewords(const aMessage: String; const aExpected: array of Byte);
    // The mode indicator, the first 4 bits of the data.
    function ModeIndicator: Integer;
    // Generates an over-long text with FGenerator.
    procedure GenerateTooLong;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSizeOfEachVersion;
    procedure TestBufferLengthForVersion;
    procedure TestFinderPatterns;
    procedure TestTimingPatterns;
    procedure TestDarkModule;
    procedure TestAlignmentPatternVersion2;
    procedure TestAlignmentPatternsVersion7;
    procedure TestVersionInformation;
    procedure TestFormatBitsOfTheTest;
    procedure TestFormatInformationForEachLevelAndMask;
    procedure TestHelloWorldCodewords;
    procedure TestNumericCodewords;
    procedure TestErrorCorrectionIsReedSolomon;
    procedure TestNumericModeChosen;
    procedure TestAlphanumericModeChosen;
    procedure TestByteModeChosen;
    procedure TestDecodesBack;
    procedure TestEmptyText;
    procedure TestVersionSelectionNumeric;
    procedure TestVersionSelectionAlphanumeric;
    procedure TestVersionSelectionBytes;
    procedure TestMinVersionHonoured;
    procedure TestTooLongFails;
    procedure TestBoostErrorCorrection;
    procedure TestAutomaticMask;
    procedure TestGetModuleOutOfRange;
    procedure TestIsNumeric;
    procedure TestIsAlphanumeric;
    procedure TestCalcSegmentBufferSize;
    procedure TestMakeNumeric;
    procedure TestMakeAlphanumeric;
    procedure TestMakeBytes;
    procedure TestMakeECI;
    procedure TestGeneratorDefaults;
    procedure TestGeneratorBitsMatchModules;
    procedure TestGeneratorBitsOutOfRange;
    procedure TestGeneratorNumber;
    procedure TestGeneratorTooLongRaises;
  end;

  TTestQRCodeImage = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    FCode: TQRBuffer;
    FGenerator: TImageQRCodeGenerator;
    // Encodes aText into FCode at medium ECC with mask 3.
    procedure EncodeText(const aText: String);
    // Fails unless the image at (aX, aY) shows the modules of FCode, or of FGenerator, aPixel pixels each.
    procedure CheckModules(const aMessage: String; aImage: TFPCustomImage; aX, aY, aPixel: Integer; aFromGenerator: Boolean);
    // Fails unless the rectangle has aColor on every pixel.
    procedure CheckArea(const aMessage: String; aImage: TFPCustomImage; aLeft, aTop, aRight, aBottom: Integer; const aColor: TFPColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDrawQRCodeOnePixel;
    procedure TestDrawQRCodeAtOffset;
    procedure TestDrawQRCodeLeavesTheRestAlone;
    procedure TestGeneratorDefaults;
    procedure TestGeneratorDraw;
    procedure TestGeneratorDrawWithBorder;
    procedure TestGeneratorDrawAtOffsetWithBorder;
    procedure TestGeneratorSaveToStream;
    procedure TestGeneratorSaveToStreamWithoutCode;
  end;

implementation

const
  cEccBits: array[TQRErrorLevelCorrection] of Integer = (1, 0, 3, 2);
  cRedColor: TFPColor = (Red: $FFFF; Green: 0; Blue: 0; Alpha: $FFFF);

var
  GExp: array[0..511] of Byte;
  GLog: array[0..255] of Integer;


// Fills the GF(256) tables for the QR code polynomial x^8+x^4+x^3+x^2+1.
procedure InitGaloisField;

var
  I, lValue: Integer;

begin
  lValue := 1;
  for I := 0 to 254 do
    begin
    GExp[I] := lValue;
    GLog[lValue] := I;
    lValue := lValue shl 1;
    if lValue > 255 then
      lValue := lValue xor $11D;
    end;
  for I := 255 to 511 do
    GExp[I] := GExp[I - 255];
end;


// The product of two elements of GF(256).
function GFMul(aA, aB: Byte): Byte;

begin
  if (aA = 0) or (aB = 0) then
    Result := 0
  else
    Result := GExp[GLog[aA] + GLog[aB]];
end;


// The aDegree Reed-Solomon check bytes of aData, generator roots alpha^0 .. alpha^(aDegree-1).
function ReedSolomon(const aData: TBytes; aDegree: Integer): TBytes;

var
  lGen, lNew, lMsg: TBytes;
  I, J: Integer;
  lCoef: Byte;

begin
  SetLength(lGen, 1);
  lGen[0] := 1;
  for I := 0 to aDegree - 1 do
    begin
    SetLength(lNew, Length(lGen) + 1);
    lNew[0] := lGen[0];
    for J := 1 to High(lGen) do
      lNew[J] := lGen[J] xor GFMul(lGen[J - 1], GExp[I]);
    lNew[Length(lGen)] := GFMul(lGen[High(lGen)], GExp[I]);
    lGen := lNew;
    end;
  SetLength(lMsg, Length(aData) + aDegree);
  for I := 0 to High(aData) do
    lMsg[I] := aData[I];
  for I := 0 to High(aData) do
    begin
    lCoef := lMsg[I];
    if lCoef <> 0 then
      for J := 1 to aDegree do
        lMsg[I + J] := lMsg[I + J] xor GFMul(lGen[J], lCoef);
    end;
  Result := Copy(lMsg, Length(aData), aDegree);
end;


// The 15 format bits of ISO/IEC 18004 for an ECC level and a mask number.
function ExpectedFormatBits(aEcl: TQRErrorLevelCorrection; aMask: Integer): Integer;

var
  I, lData, lRem: Integer;

begin
  lData := (cEccBits[aEcl] shl 3) or aMask;
  lRem := lData shl 10;
  for I := 14 downto 10 do
    if (lRem and (1 shl I)) <> 0 then
      lRem := lRem xor ($537 shl (I - 10));
  Result := ((lData shl 10) or lRem) xor $5412;
end;


// The 18 version bits of ISO/IEC 18004 for a version.
function ExpectedVersionBits(aVersion: Integer): Integer;

var
  I, lRem: Integer;

begin
  lRem := aVersion shl 12;
  for I := 17 downto 12 do
    if (lRem and (1 shl I)) <> 0 then
      lRem := lRem xor ($1F25 shl (I - 12));
  Result := (aVersion shl 12) or lRem;
end;


// True if the mask pattern aMask inverts the module at column aX, row aY.
function MaskBit(aMask, aX, aY: Integer): Boolean;

begin
  case aMask of
    0: Result := (aY + aX) mod 2 = 0;
    1: Result := aY mod 2 = 0;
    2: Result := aX mod 3 = 0;
    3: Result := (aY + aX) mod 3 = 0;
    4: Result := ((aY div 2) + (aX div 3)) mod 2 = 0;
    5: Result := (aY * aX) mod 2 + (aY * aX) mod 3 = 0;
    6: Result := ((aY * aX) mod 2 + (aY * aX) mod 3) mod 2 = 0;
    7: Result := ((aY + aX) mod 2 + (aY * aX) mod 3) mod 2 = 0;
  else
    Result := False;
  end;
end;


// The bytes as hexadecimal text.
function BytesToHex(const aBytes: TBytes): String;

var
  I: Integer;

begin
  Result := '';
  for I := 0 to High(aBytes) do
    Result := Result + IntToHex(aBytes[I], 2) + ' ';
end;


// A string of aCount copies of aChar.
function Repeated(aChar: Char; aCount: Integer): String;

begin
  Result := StringOfChar(aChar, aCount);
end;


{ TTestQRCode }

procedure TTestQRCode.SetUp;

begin
  inherited SetUp;
  SetLength(FCode, QRBUFFER_LEN_MAX);
  SetLength(FTemp, QRBUFFER_LEN_MAX);
  FGenerator := TQRCodeGenerator.Create;
end;


procedure TTestQRCode.TearDown;

begin
  FreeAndNil(FGenerator);
  FCode := nil;
  FTemp := nil;
  inherited TearDown;
end;


function TTestQRCode.Encode(const aText: String; aEcl: TQRErrorLevelCorrection; aMinVersion, aMaxVersion: TQRVersion;
  aMask: TQRMask; aBoost: Boolean): Boolean;

begin
  FillChar(FCode[0], Length(FCode), 0);
  FillChar(FTemp[0], Length(FTemp), 0);
  Result := QREncodeText(aText, FTemp, FCode, aEcl, aMinVersion, aMaxVersion, aMask, aBoost);
end;


procedure TTestQRCode.MustEncode(const aText: String; aEcl: TQRErrorLevelCorrection; aMinVersion, aMaxVersion: TQRVersion;
  aMask: TQRMask; aBoost: Boolean);

begin
  AssertTrue('"' + aText + '" can be encoded', Encode(aText, aEcl, aMinVersion, aMaxVersion, aMask, aBoost));
end;


function TTestQRCode.Module(aX, aY: Integer): Boolean;

begin
  Result := QRgetModule(FCode, aX, aY);
end;


function TTestQRCode.Version: Integer;

begin
  Result := (QRgetSize(FCode) - 17) div 4;
end;


function TTestQRCode.FormatBits1: Integer;

var
  I: Integer;

  // Sets bit aBit of the result if the module (aX, aY) is dark.
  procedure Put(aBit, aX, aY: Integer);

  begin
    if Module(aX, aY) then
      I := I or (1 shl aBit);
  end;

var
  J: Integer;

begin
  I := 0;
  for J := 0 to 5 do
    Put(J, 8, J);
  Put(6, 8, 7);
  Put(7, 8, 8);
  Put(8, 7, 8);
  for J := 9 to 14 do
    Put(J, 14 - J, 8);
  Result := I;
end;


function TTestQRCode.FormatBits2: Integer;

var
  J, lSize: Integer;

begin
  Result := 0;
  lSize := QRgetSize(FCode);
  for J := 0 to 7 do
    if Module(lSize - 1 - J, 8) then
      Result := Result or (1 shl J);
  for J := 8 to 14 do
    if Module(8, lSize - 15 + J) then
      Result := Result or (1 shl J);
end;


function TTestQRCode.VersionBits1: Integer;

var
  J, lSize: Integer;

begin
  Result := 0;
  lSize := QRgetSize(FCode);
  for J := 0 to 17 do
    if Module(lSize - 11 + J mod 3, J div 3) then
      Result := Result or (1 shl J);
end;


function TTestQRCode.VersionBits2: Integer;

var
  J, lSize: Integer;

begin
  Result := 0;
  lSize := QRgetSize(FCode);
  for J := 0 to 17 do
    if Module(J div 3, lSize - 11 + J mod 3) then
      Result := Result or (1 shl J);
end;


function TTestQRCode.IsFunctionModule(aX, aY: Integer): Boolean;

var
  lSize, lAlign: Integer;

begin
  lSize := QRgetSize(FCode);
  Result := ((aX < 9) and (aY < 9)) or ((aX >= lSize - 8) and (aY < 9)) or ((aX < 9) and (aY >= lSize - 8))
    or (aX = 6) or (aY = 6);
  if (not Result) and (Version >= 2) then
    begin
    lAlign := lSize - 7;
    Result := (Abs(aX - lAlign) <= 2) and (Abs(aY - lAlign) <= 2);
    end;
end;


function TTestQRCode.ReadCodewords: TBytes;

var
  lSize, lX, lY, lStep, lCol, lMask, lBit: Integer;
  lUp: Boolean;

begin
  Result := nil;
  lSize := QRgetSize(FCode);
  lMask := ((FormatBits1 xor $5412) shr 10) and 7;
  if Version = 1 then
    SetLength(Result, 26)
  else
    SetLength(Result, 44);
  FillChar(Result[0], Length(Result), 0);
  lBit := 0;
  lX := lSize - 1;
  lUp := True;
  while lX > 0 do
    begin
    if lX = 6 then
      Dec(lX);
    for lStep := 0 to lSize - 1 do
      begin
      if lUp then
        lY := lSize - 1 - lStep
      else
        lY := lStep;
      for lCol := 0 to 1 do
        if not IsFunctionModule(lX - lCol, lY) then
          begin
          if (lBit div 8 < Length(Result)) and (Module(lX - lCol, lY) <> MaskBit(lMask, lX - lCol, lY)) then
            Result[lBit div 8] := Result[lBit div 8] or ($80 shr (lBit mod 8));
          Inc(lBit);
          end;
      end;
    lUp := not lUp;
    Dec(lX, 2);
    end;
end;


function TTestQRCode.DataCodewordCount: Integer;

const
  cV1: array[0..3] of Integer = (16, 19, 9, 13);
  cV2: array[0..3] of Integer = (28, 34, 16, 22);

var
  lEcc: Integer;

begin
  lEcc := ((FormatBits1 xor $5412) shr 13) and 3;
  if Version = 1 then
    Result := cV1[lEcc]
  else
    Result := cV2[lEcc];
end;


function TTestQRCode.DecodeData(const aCodewords: TBytes; aCount: Integer): String;

const
  cAlnum = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:';

var
  lPos, lMode, lLen, I, lValue: Integer;

  // The next aNumber bits of the data, most significant first.
  function Bits(aNumber: Integer): Integer;

  var
    J: Integer;

  begin
    Result := 0;
    for J := 1 to aNumber do
      begin
      Result := Result shl 1;
      if (lPos div 8 < aCount) and ((aCodewords[lPos div 8] and ($80 shr (lPos mod 8))) <> 0) then
        Result := Result or 1;
      Inc(lPos);
      end;
  end;

begin
  Result := '';
  lPos := 0;
  while lPos + 4 <= aCount * 8 do
    begin
    lMode := Bits(4);
    case lMode of
      0: Exit;
      1:
        begin
        lLen := Bits(10);
        I := 0;
        while I < lLen do
          begin
          if lLen - I >= 3 then
            begin
            Result := Result + Format('%.3d', [Bits(10)]);
            Inc(I, 3);
            end
          else if lLen - I = 2 then
            begin
            Result := Result + Format('%.2d', [Bits(7)]);
            Inc(I, 2);
            end
          else
            begin
            Result := Result + IntToStr(Bits(4));
            Inc(I);
            end;
          end;
        end;
      2:
        begin
        lLen := Bits(9);
        I := 0;
        while I < lLen do
          if lLen - I >= 2 then
            begin
            lValue := Bits(11);
            Result := Result + cAlnum[lValue div 45 + 1] + cAlnum[lValue mod 45 + 1];
            Inc(I, 2);
            end
          else
            begin
            Result := Result + cAlnum[Bits(6) + 1];
            Inc(I);
            end;
        end;
      4:
        begin
        lLen := Bits(8);
        for I := 1 to lLen do
          Result := Result + Chr(Bits(8));
        end;
    else
      Fail(Format('the data starts with a mode indicator of the standard, not %d', [lMode]));
    end;
    end;
end;


procedure TTestQRCode.CheckDecodes(const aText: String; aEcl: TQRErrorLevelCorrection; aMask: TQRMask);

var
  lWords: TBytes;

begin
  MustEncode(aText, aEcl, 1, 2, aMask, False);
  lWords := ReadCodewords;
  AssertEquals('the symbol of "' + aText + '" decodes to it', aText, DecodeData(lWords, DataCodewordCount));
end;


procedure TTestQRCode.CheckFinder(const aMessage: String; aX, aY: Integer);

var
  I, J, lDist: Integer;

begin
  for J := 0 to 6 do
    for I := 0 to 6 do
      begin
      lDist := Abs(I - 3);
      if Abs(J - 3) > lDist then
        lDist := Abs(J - 3);
      AssertEquals(Format('%s: finder module (%d,%d), dark unless on the ring at distance 2', [aMessage, aX + I, aY + J]),
        lDist <> 2, Module(aX + I, aY + J));
      end;
end;


procedure TTestQRCode.CheckAlignment(const aMessage: String; aX, aY: Integer);

var
  I, J, lDist: Integer;

begin
  for J := -2 to 2 do
    for I := -2 to 2 do
      begin
      lDist := Abs(I);
      if Abs(J) > lDist then
        lDist := Abs(J);
      AssertEquals(Format('%s: alignment module (%d,%d), light only on the ring at distance 1', [aMessage, aX + I, aY + J]),
        lDist <> 1, Module(aX + I, aY + J));
      end;
end;


procedure TTestQRCode.CheckCodewords(const aMessage: String; const aExpected: array of Byte);

var
  lWords: TBytes;
  I: Integer;

begin
  lWords := ReadCodewords;
  for I := 0 to High(aExpected) do
    AssertEquals(Format('%s: codeword %d', [aMessage, I]), aExpected[I], lWords[I]);
end;


function TTestQRCode.ModeIndicator: Integer;

begin
  Result := ReadCodewords[0] shr 4;
end;


procedure TTestQRCode.GenerateTooLong;

begin
  FGenerator.MaxVersion := 1;
  FGenerator.ErrorCorrectionLevel := EccHIGH;
  FGenerator.Generate(Repeated('a', 100));
end;


procedure TTestQRCode.TestSizeOfEachVersion;

var
  lVersion: TQRVersion;

begin
  for lVersion := QRVERSIONMIN to QRVERSIONMAX do
    begin
    MustEncode('1', EccLOW, lVersion, lVersion, mp0, False);
    AssertEquals(Format('version %d has 17+4*%d modules a side', [lVersion, lVersion]), 17 + 4 * lVersion, QRgetSize(FCode));
    end;
end;


procedure TTestQRCode.TestBufferLengthForVersion;

begin
  AssertEquals('version 1 needs ceil(21*21/8)+1 bytes', 57, QRBUFFER_LEN_FOR_VERSION(1));
  AssertEquals('version 40 needs QRBUFFER_LEN_MAX bytes', QRBUFFER_LEN_MAX, QRBUFFER_LEN_FOR_VERSION(40));
end;


procedure TTestQRCode.TestFinderPatterns;

const
  cVersions: array[0..2] of Integer = (1, 2, 7);

var
  I, J, lSize: Integer;

begin
  for I := 0 to High(cVersions) do
    begin
    MustEncode('FINDER', EccMEDIUM, cVersions[I], cVersions[I], mp1, False);
    lSize := QRgetSize(FCode);
    CheckFinder(Format('version %d top left', [cVersions[I]]), 0, 0);
    CheckFinder(Format('version %d top right', [cVersions[I]]), lSize - 7, 0);
    CheckFinder(Format('version %d bottom left', [cVersions[I]]), 0, lSize - 7);
    for J := 0 to 7 do
      begin
      AssertFalse(Format('version %d: the separator right of the top left finder is light at row %d', [cVersions[I], J]), Module(7, J));
      AssertFalse(Format('version %d: the separator below the top left finder is light at column %d', [cVersions[I], J]), Module(J, 7));
      AssertFalse(Format('version %d: the separator left of the top right finder is light at row %d', [cVersions[I], J]),
        Module(lSize - 8, J));
      AssertFalse(Format('version %d: the separator below the top right finder is light', [cVersions[I]]), Module(lSize - 1 - J, 7));
      AssertFalse(Format('version %d: the separator above the bottom left finder is light', [cVersions[I]]), Module(J, lSize - 8));
      AssertFalse(Format('version %d: the separator right of the bottom left finder is light', [cVersions[I]]),
        Module(7, lSize - 1 - J));
      end;
    end;
end;


procedure TTestQRCode.TestTimingPatterns;

const
  cVersions: array[0..2] of Integer = (1, 5, 10);

var
  I, J, lSize: Integer;

begin
  for I := 0 to High(cVersions) do
    begin
    MustEncode('TIMING', EccLOW, cVersions[I], cVersions[I], mp4, False);
    lSize := QRgetSize(FCode);
    for J := 8 to lSize - 9 do
      begin
      AssertEquals(Format('version %d: the horizontal timing pattern on row 6 is dark at even column %d', [cVersions[I], J]),
        not Odd(J), Module(J, 6));
      AssertEquals(Format('version %d: the vertical timing pattern on column 6 is dark at even row %d', [cVersions[I], J]),
        not Odd(J), Module(6, J));
      end;
    end;
end;


procedure TTestQRCode.TestDarkModule;

var
  lVersion: Integer;

begin
  for lVersion := 1 to 10 do
    begin
    MustEncode('DARK', EccQUARTILE, lVersion, lVersion, mp6, False);
    AssertTrue(Format('version %d has the dark module at (8, 4V+9)', [lVersion]), Module(8, 4 * lVersion + 9));
    end;
end;


procedure TTestQRCode.TestAlignmentPatternVersion2;

begin
  MustEncode('ALIGN', EccLOW, 2, 2, mp0, False);
  CheckAlignment('version 2', 18, 18);
end;


procedure TTestQRCode.TestAlignmentPatternsVersion7;

begin
  MustEncode('ALIGN', EccLOW, 7, 7, mp5, False);
  CheckAlignment('version 7 at (22,22)', 22, 22);
  CheckAlignment('version 7 at (38,22)', 38, 22);
  CheckAlignment('version 7 at (22,38)', 22, 38);
  CheckAlignment('version 7 at (38,38)', 38, 38);
  CheckAlignment('version 7 at (6,22)', 6, 22);
  CheckAlignment('version 7 at (22,6)', 22, 6);
end;


procedure TTestQRCode.TestVersionInformation;

var
  lVersion: Integer;

begin
  AssertEquals('the version bits of the test for version 7 are $07C94 (ISO/IEC 18004 table D.1)', $07C94, ExpectedVersionBits(7));
  AssertEquals('the version bits of the test for version 8 are $085BC (ISO/IEC 18004 table D.1)', $085BC, ExpectedVersionBits(8));
  for lVersion := 7 to 40 do
    begin
    MustEncode('V', EccLOW, lVersion, lVersion, mp2, False);
    AssertEquals(Format('version %d: the version block at the top right', [lVersion]), ExpectedVersionBits(lVersion), VersionBits1);
    AssertEquals(Format('version %d: the version block at the bottom left', [lVersion]), ExpectedVersionBits(lVersion), VersionBits2);
    end;
end;


procedure TTestQRCode.TestFormatBitsOfTheTest;

begin
  AssertEquals('the format bits for M and mask 0 are the mask $5412 (101010000010010)', $5412, ExpectedFormatBits(EccMEDIUM, 0));
  AssertEquals('the format bits for L and mask 0 are 111011111000100 (ISO/IEC 18004 table C.1)', $77C4, ExpectedFormatBits(EccLOW, 0));
end;


procedure TTestQRCode.TestFormatInformationForEachLevelAndMask;

var
  lEcl: TQRErrorLevelCorrection;
  lMask: TQRMask;

begin
  for lEcl := Low(TQRErrorLevelCorrection) to High(TQRErrorLevelCorrection) do
    for lMask := mp0 to mp7 do
      begin
      MustEncode('HELLO', lEcl, 1, 1, lMask, False);
      AssertEquals(Format('ECC level %d, mask %d: the first copy of the format bits', [Ord(lEcl), Ord(lMask)]),
        ExpectedFormatBits(lEcl, Ord(lMask)), FormatBits1);
      AssertEquals(Format('ECC level %d, mask %d: the second copy of the format bits', [Ord(lEcl), Ord(lMask)]),
        ExpectedFormatBits(lEcl, Ord(lMask)), FormatBits2);
      end;
end;


procedure TTestQRCode.TestHelloWorldCodewords;

begin
  MustEncode('HELLO WORLD', EccMEDIUM, 1, 1, mp2, False);
  CheckCodewords('"HELLO WORLD" as 1-M: alphanumeric data, pad bytes $EC $11 and 10 ECC codewords',
    [$20, $5B, $0B, $78, $D1, $72, $DC, $4D, $43, $40, $EC, $11, $EC, $11, $EC, $11,
     $C4, $23, $27, $77, $EB, $D7, $E7, $E2, $5D, $17]);
end;


procedure TTestQRCode.TestNumericCodewords;

begin
  MustEncode('01234567', EccMEDIUM, 1, 1, mp0, False);
  CheckCodewords('"01234567" as 1-M (ISO/IEC 18004 annex I): numeric data, pad bytes and 10 ECC codewords',
    [$10, $20, $0C, $56, $61, $80, $EC, $11, $EC, $11, $EC, $11, $EC, $11, $EC, $11,
     $A5, $24, $D4, $C1, $ED, $36, $C7, $87, $2C, $55]);
end;


procedure TTestQRCode.TestErrorCorrectionIsReedSolomon;

const
  cTexts: array[0..3] of String = ('1', 'HELLO', 'hello world', '3141592653589793');

var
  I, lData: Integer;
  lEcl: TQRErrorLevelCorrection;
  lWords, lCheck: TBytes;

begin
  for I := 0 to High(cTexts) do
    for lEcl := Low(TQRErrorLevelCorrection) to High(TQRErrorLevelCorrection) do
      if Encode(cTexts[I], lEcl, 1, 2, mp3, False) then
        begin
        lWords := ReadCodewords;
        lData := DataCodewordCount;
        lCheck := ReedSolomon(Copy(lWords, 0, lData), Length(lWords) - lData);
        AssertEquals(Format('"%s" at ECC level %d: the check codewords are the Reed-Solomon remainder', [cTexts[I], Ord(lEcl)]),
          BytesToHex(lCheck), BytesToHex(Copy(lWords, lData, Length(lWords) - lData)));
        end;
end;


procedure TTestQRCode.TestNumericModeChosen;

begin
  MustEncode('0123456789', EccLOW, 1, 1, mp0, False);
  AssertEquals('digits are encoded in numeric mode (0001)', 1, ModeIndicator);
end;


procedure TTestQRCode.TestAlphanumericModeChosen;

begin
  MustEncode('HELLO $%*+-./: 42', EccLOW, 1, 1, mp0, False);
  AssertEquals('upper case letters, digits and $%*+-./: are encoded in alphanumeric mode (0010)', 2, ModeIndicator);
end;


procedure TTestQRCode.TestByteModeChosen;

begin
  MustEncode('hello', EccLOW, 1, 1, mp0, False);
  AssertEquals('lower case text is encoded in byte mode (0100)', 4, ModeIndicator);
  MustEncode('HELLO!', EccLOW, 1, 1, mp0, False);
  AssertEquals('text with a character outside the alphanumeric set is encoded in byte mode', 4, ModeIndicator);
end;


procedure TTestQRCode.TestDecodesBack;

begin
  CheckDecodes('12345', EccHIGH, mp0);
  CheckDecodes('1', EccLOW, mp1);
  CheckDecodes('98', EccMEDIUM, mp7);
  CheckDecodes('AC-42', EccQUARTILE, mp2);
  CheckDecodes('HELLO WORLD', EccMEDIUM, mp3);
  CheckDecodes('hello, world', EccLOW, mp4);
  CheckDecodes('caf'#$C3#$A9, EccMEDIUM, mp5);
  CheckDecodes('https://www.freepascal.org', EccLOW, mp6);
end;


procedure TTestQRCode.TestEmptyText;

begin
  MustEncode('', EccLOW, 1, 40, mp0, False);
  AssertEquals('an empty text gives a version 1 symbol', 21, QRgetSize(FCode));
  AssertEquals('an empty text decodes to nothing', '', DecodeData(ReadCodewords, DataCodewordCount));
end;


procedure TTestQRCode.TestVersionSelectionNumeric;

begin
  MustEncode(Repeated('7', 41), EccLOW, 1, 40, mp0, False);
  AssertEquals('41 digits fit version 1-L (capacity 41)', 21, QRgetSize(FCode));
  MustEncode(Repeated('7', 42), EccLOW, 1, 40, mp0, False);
  AssertEquals('42 digits need version 2-L', 25, QRgetSize(FCode));
  MustEncode(Repeated('7', 17), EccHIGH, 1, 40, mp0, False);
  AssertEquals('17 digits fit version 1-H (capacity 17)', 21, QRgetSize(FCode));
  MustEncode(Repeated('7', 18), EccHIGH, 1, 40, mp0, False);
  AssertEquals('18 digits need version 2-H', 25, QRgetSize(FCode));
end;


procedure TTestQRCode.TestVersionSelectionAlphanumeric;

begin
  MustEncode(Repeated('Q', 25), EccLOW, 1, 40, mp0, False);
  AssertEquals('25 alphanumeric characters fit version 1-L (capacity 25)', 21, QRgetSize(FCode));
  MustEncode(Repeated('Q', 26), EccLOW, 1, 40, mp0, False);
  AssertEquals('26 alphanumeric characters need version 2-L', 25, QRgetSize(FCode));
  MustEncode(Repeated('Q', 20), EccMEDIUM, 1, 40, mp0, False);
  AssertEquals('20 alphanumeric characters fit version 1-M (capacity 20)', 21, QRgetSize(FCode));
end;


procedure TTestQRCode.TestVersionSelectionBytes;

begin
  MustEncode(Repeated('q', 17), EccLOW, 1, 40, mp0, False);
  AssertEquals('17 bytes fit version 1-L (capacity 17)', 21, QRgetSize(FCode));
  MustEncode(Repeated('q', 18), EccLOW, 1, 40, mp0, False);
  AssertEquals('18 bytes need version 2-L', 25, QRgetSize(FCode));
  MustEncode(Repeated('q', 32), EccLOW, 1, 40, mp0, False);
  AssertEquals('32 bytes fit version 2-L (capacity 32)', 25, QRgetSize(FCode));
  MustEncode(Repeated('q', 33), EccLOW, 1, 40, mp0, False);
  AssertEquals('33 bytes need version 3-L', 29, QRgetSize(FCode));
end;


procedure TTestQRCode.TestMinVersionHonoured;

begin
  MustEncode('1', EccLOW, 5, 40, mp0, False);
  AssertEquals('a minimum version of 5 gives a version 5 symbol', 37, QRgetSize(FCode));
end;


procedure TTestQRCode.TestTooLongFails;

begin
  AssertFalse('18 bytes do not fit version 1-L', Encode(Repeated('q', 18), EccLOW, 1, 1, mp0, False));
  AssertEquals('a failed encoding sets the size byte to 0', 0, FCode[0]);
  AssertFalse('2954 bytes do not fit any version', Encode(Repeated('q', 2954), EccLOW, 1, 40, mp0, False));
  AssertTrue('2953 bytes fit version 40-L', Encode(Repeated('q', 2953), EccLOW, 1, 40, mp0, False));
  AssertEquals('2953 bytes give a version 40 symbol', 177, QRgetSize(FCode));
end;


procedure TTestQRCode.TestBoostErrorCorrection;

begin
  MustEncode('1', EccLOW, 1, 1, mp0, True);
  AssertEquals('with boosting, one digit in version 1 is stored at level H', ExpectedFormatBits(EccHIGH, 0), FormatBits1);
  MustEncode('1', EccLOW, 1, 1, mp0, False);
  AssertEquals('without boosting, level L is kept', ExpectedFormatBits(EccLOW, 0), FormatBits1);
  MustEncode(Repeated('Q', 18), EccLOW, 1, 1, mp0, True);
  AssertEquals('with boosting, 18 alphanumeric characters in version 1 are stored at level M (capacity 20)',
    ExpectedFormatBits(EccMEDIUM, 0), FormatBits1);
end;


procedure TTestQRCode.TestAutomaticMask;

var
  lMask: Integer;
  lFound: Boolean;

begin
  MustEncode('AUTOMATIC MASK', EccMEDIUM, 1, 2, mpAuto, False);
  lFound := False;
  for lMask := 0 to 7 do
    if FormatBits1 = ExpectedFormatBits(EccMEDIUM, lMask) then
      lFound := True;
  AssertTrue('with the automatic mask the format bits name level M and one of the masks 0 to 7', lFound);
  AssertEquals('the symbol with the automatic mask decodes', 'AUTOMATIC MASK', DecodeData(ReadCodewords, DataCodewordCount));
end;


procedure TTestQRCode.TestGetModuleOutOfRange;

begin
  MustEncode('RANGE', EccLOW, 1, 1, mp0, False);
  AssertTrue('the module (0,0) of the finder is dark', QRgetModule(FCode, 0, 0));
  AssertFalse('a module right of the symbol is light', QRgetModule(FCode, 21, 0));
  AssertFalse('a module below the symbol is light', QRgetModule(FCode, 0, 21));
  AssertFalse('a module far outside the symbol is light', QRgetModule(FCode, 1000, 1000));
end;


procedure TTestQRCode.TestIsNumeric;

begin
  AssertTrue('"0123456789" is numeric', QRIsNumeric('0123456789'));
  AssertFalse('"12a" is not numeric', QRIsNumeric('12a'));
  AssertFalse('"1 2" is not numeric', QRIsNumeric('1 2'));
end;


procedure TTestQRCode.TestIsAlphanumeric;

begin
  AssertTrue('upper case letters, digits, space and $%*+-./: are alphanumeric',
    QRIsAlphanumeric('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:'));
  AssertFalse('lower case letters are not alphanumeric', QRIsAlphanumeric('Hello'));
  AssertFalse('"!" is not alphanumeric', QRIsAlphanumeric('HI!'));
end;


procedure TTestQRCode.TestCalcSegmentBufferSize;

begin
  AssertEquals('8 digits need 27 bits, 4 bytes', 4, QRCalcSegmentBufferSize(mNUMERIC, 8));
  AssertEquals('11 alphanumeric characters need 61 bits, 8 bytes', 8, QRCalcSegmentBufferSize(mALPHANUMERIC, 11));
  AssertEquals('5 bytes need 5 bytes', 5, QRCalcSegmentBufferSize(mBYTE, 5));
  AssertEquals('0 characters need 0 bytes', 0, QRCalcSegmentBufferSize(mBYTE, 0));
end;


procedure TTestQRCode.TestMakeNumeric;

var
  lSeg: TQRSegment;

begin
  lSeg := QRMakeNumeric('01234567', FTemp);
  AssertTrue('the numeric segment is in numeric mode', lSeg.mode = mNUMERIC);
  AssertEquals('the numeric segment counts 8 characters', 8, lSeg.numChars);
  AssertEquals('"01234567" takes 10+10+7 bits', 27, lSeg.bitLength);
  AssertEquals('bits 0-7: 012 = 0000001100', $03, lSeg.data[0]);
  AssertEquals('bits 8-15: then 345 = 0101011001', $15, lSeg.data[1]);
  AssertEquals('bits 16-23', $98, lSeg.data[2]);
  AssertEquals('bits 24-26: then 67 = 1000011', $60, lSeg.data[3] and $E0);
end;


procedure TTestQRCode.TestMakeAlphanumeric;

var
  lSeg: TQRSegment;

begin
  lSeg := QRMakeAlphanumeric('AC-42', FTemp);
  AssertTrue('the alphanumeric segment is in alphanumeric mode', lSeg.mode = mALPHANUMERIC);
  AssertEquals('the alphanumeric segment counts 5 characters', 5, lSeg.numChars);
  AssertEquals('"AC-42" takes 11+11+6 bits', 28, lSeg.bitLength);
  AssertEquals('bits 0-7: AC = 462 = 00111001110', $39, lSeg.data[0]);
  AssertEquals('bits 8-15: then -4 = 1849 = 11100111001', $DC, lSeg.data[1]);
  AssertEquals('bits 16-23', $E4, lSeg.data[2]);
  AssertEquals('bits 24-27: then 2 = 000010', $20, lSeg.data[3] and $F0);
end;


procedure TTestQRCode.TestMakeBytes;

var
  lSeg: TQRSegment;
  lData: TQRBuffer;

begin
  SetLength(lData, 3);
  lData[0] := $61;
  lData[1] := $62;
  lData[2] := $FF;
  lSeg := QRmakeBytes(lData, FTemp);
  AssertTrue('the byte segment is in byte mode', lSeg.mode = mBYTE);
  AssertEquals('the byte segment counts 3 bytes', 3, lSeg.numChars);
  AssertEquals('3 bytes take 24 bits', 24, lSeg.bitLength);
  AssertEquals('the byte segment has the bytes: last', $FF, lSeg.data[2]);
end;


procedure TTestQRCode.TestMakeECI;

var
  lSeg: TQRSegment;

begin
  lSeg := QRMakeECI(26, FTemp);
  AssertTrue('the ECI segment is in ECI mode', lSeg.mode = mECI);
  AssertEquals('an ECI designator below 128 takes 8 bits', 8, lSeg.bitLength);
  AssertEquals('the ECI designator 26 (UTF-8) is 00011010', 26, lSeg.data[0]);
  lSeg := QRMakeECI(1000, FTemp);
  AssertEquals('an ECI designator from 128 to 16383 takes 16 bits', 16, lSeg.bitLength);
  AssertEquals('the ECI designator 1000 starts with 10', $83, lSeg.data[0]);
  AssertEquals('the ECI designator 1000 ends with its low byte', $E8, lSeg.data[1]);
end;


procedure TTestQRCode.TestGeneratorDefaults;

begin
  AssertEquals('a new generator has no symbol: size -1', -1, FGenerator.Size);
  AssertTrue('a new generator uses ECC level M', FGenerator.ErrorCorrectionLevel = EccMEDIUM);
  AssertEquals('a new generator starts at version 1', 1, FGenerator.MinVersion);
  AssertEquals('a new generator goes up to version 40', 40, FGenerator.MaxVersion);
  AssertEquals('a new generator has a buffer for version 40', QRBUFFER_LEN_MAX, FGenerator.BufferLength);
end;


procedure TTestQRCode.TestGeneratorBitsMatchModules;

var
  I, J: Integer;

begin
  FGenerator.Generate('GENERATOR');
  AssertEquals('"GENERATOR" at level M fits version 1', 21, FGenerator.Size);
  for J := 0 to FGenerator.Size - 1 do
    for I := 0 to FGenerator.Size - 1 do
      AssertEquals(Format('Bits[%d,%d] is the module of the symbol', [I, J]),
        QRgetModule(FGenerator.Bytes, I, J), FGenerator.Bits[I, J]);
end;


procedure TTestQRCode.TestGeneratorBitsOutOfRange;

begin
  FGenerator.Generate('GENERATOR');
  AssertTrue('Bits[0,1] of the finder is dark', FGenerator.Bits[0, 1]);
  AssertFalse('Bits right of the symbol is light, like QRgetModule', FGenerator.Bits[FGenerator.Size, 0]);
  AssertFalse('Bits below the symbol is light, like QRgetModule', FGenerator.Bits[0, FGenerator.Size]);
end;


procedure TTestQRCode.TestGeneratorNumber;

begin
  FGenerator.Generate(Int64(9876543210));
  FCode := FGenerator.Bytes;
  AssertEquals('a number is encoded in numeric mode', 1, ModeIndicator);
  AssertEquals('a number decodes to its digits', '9876543210', DecodeData(ReadCodewords, DataCodewordCount));
end;


procedure TTestQRCode.TestGeneratorTooLongRaises;

begin
  AssertRaises('a text too long for the versions allowed raises EQRCode', EQRCode, @GenerateTooLong);
end;


{ TTestQRCodeImage }

procedure TTestQRCodeImage.SetUp;

begin
  inherited SetUp;
  SetLength(FCode, QRBUFFER_LEN_MAX);
  FGenerator := TImageQRCodeGenerator.Create;
end;


procedure TTestQRCodeImage.TearDown;

begin
  FreeAndNil(FGenerator);
  FreeAndNil(FImage);
  FCode := nil;
  inherited TearDown;
end;


procedure TTestQRCodeImage.EncodeText(const aText: String);

var
  lTemp: TQRBuffer;

begin
  SetLength(lTemp, QRBUFFER_LEN_MAX);
  AssertTrue('"' + aText + '" can be encoded', QREncodeText(aText, lTemp, FCode, EccMEDIUM, 1, 40, mp3, False));
end;


procedure TTestQRCodeImage.CheckModules(const aMessage: String; aImage: TFPCustomImage; aX, aY, aPixel: Integer; aFromGenerator: Boolean);

var
  lSize, I, J, lPX, lPY: Integer;
  lDark: Boolean;
  lExpected: TFPColor;

begin
  if aFromGenerator then
    lSize := FGenerator.Size
  else
    lSize := QRgetSize(FCode);
  for J := 0 to lSize - 1 do
    for I := 0 to lSize - 1 do
      begin
      if aFromGenerator then
        lDark := QRgetModule(FGenerator.Bytes, I, J)
      else
        lDark := QRgetModule(FCode, I, J);
      if lDark then
        lExpected := colBlack
      else
        lExpected := colWhite;
      for lPY := 0 to aPixel - 1 do
        for lPX := 0 to aPixel - 1 do
          AssertColorsEqual(Format('%s: pixel (%d,%d) of module (%d,%d)', [aMessage, aX + I * aPixel + lPX,
            aY + J * aPixel + lPY, I, J]), lExpected, aImage.Colors[aX + I * aPixel + lPX, aY + J * aPixel + lPY]);
      end;
end;


procedure TTestQRCodeImage.CheckArea(const aMessage: String; aImage: TFPCustomImage; aLeft, aTop, aRight, aBottom: Integer; const aColor: TFPColor);

var
  I, J: Integer;

begin
  for J := aTop to aBottom do
    for I := aLeft to aRight do
      AssertColorsEqual(Format('%s: pixel (%d,%d)', [aMessage, I, J]), aColor, aImage.Colors[I, J]);
end;


procedure TTestQRCodeImage.TestDrawQRCodeOnePixel;

begin
  EncodeText('ONE PIXEL');
  FImage := CreateSolidImage(21, 21, cRedColor);
  DrawQRCode(FImage, FCode, Point(0, 0));
  CheckModules('one pixel per module', FImage, 0, 0, 1, False);
end;


procedure TTestQRCodeImage.TestDrawQRCodeAtOffset;

begin
  EncodeText('OFFSET');
  FImage := CreateSolidImage(21 * 3 + 10, 21 * 3 + 12, cRedColor);
  DrawQRCode(FImage, FCode, Point(4, 7), 3);
  CheckModules('3 pixels per module at (4,7)', FImage, 4, 7, 3, False);
end;


procedure TTestQRCodeImage.TestDrawQRCodeLeavesTheRestAlone;

begin
  EncodeText('OFFSET');
  FImage := CreateSolidImage(21 * 3 + 10, 21 * 3 + 12, cRedColor);
  DrawQRCode(FImage, FCode, Point(4, 7), 3);
  CheckArea('the rows above the symbol are untouched', FImage, 0, 0, FImage.Width - 1, 6, cRedColor);
  CheckArea('the columns left of the symbol are untouched', FImage, 0, 0, 3, FImage.Height - 1, cRedColor);
  CheckArea('the columns right of the symbol are untouched', FImage, 4 + 63, 0, FImage.Width - 1, FImage.Height - 1, cRedColor);
  CheckArea('the rows below the symbol are untouched', FImage, 0, 7 + 63, FImage.Width - 1, FImage.Height - 1, cRedColor);
end;


procedure TTestQRCodeImage.TestGeneratorDefaults;

begin
  AssertEquals('the default pixel size is 2', 2, FGenerator.PixelSize);
  AssertEquals('the default border is 0', 0, FGenerator.Border);
end;


procedure TTestQRCodeImage.TestGeneratorDraw;

begin
  FGenerator.Generate('DRAW');
  FImage := CreateSolidImage(FGenerator.Size * 2, FGenerator.Size * 2, cRedColor);
  FGenerator.Draw(FImage);
  CheckModules('the generator draws 2 pixels per module by default', FImage, 0, 0, 2, True);
end;


procedure TTestQRCodeImage.TestGeneratorDrawWithBorder;

var
  lD: Integer;

begin
  FGenerator.Generate('BORDER');
  FGenerator.PixelSize := 4;
  FGenerator.Border := 6;
  lD := FGenerator.Size * 4;
  FImage := CreateSolidImage(lD + 12, lD + 12, cRedColor);
  FGenerator.Draw(FImage);
  CheckArea('the border above is white', FImage, 0, 0, lD + 11, 5, colWhite);
  CheckArea('the border below is white', FImage, 0, lD + 6, lD + 11, lD + 11, colWhite);
  CheckArea('the border on the left is white', FImage, 0, 0, 5, lD + 11, colWhite);
  CheckArea('the border on the right is white', FImage, lD + 6, 0, lD + 11, lD + 11, colWhite);
  CheckModules('the symbol inside the border', FImage, 6, 6, 4, True);
end;


procedure TTestQRCodeImage.TestGeneratorDrawAtOffsetWithBorder;

var
  lD: Integer;

begin
  FGenerator.Generate('BORDER');
  FGenerator.PixelSize := 3;
  FGenerator.Border := 2;
  lD := FGenerator.Size * 3;
  FImage := CreateSolidImage(lD + 20, lD + 20, cRedColor);
  FGenerator.Draw(FImage, 5, 9);
  CheckArea('the border above is white', FImage, 5, 9, 5 + lD + 3, 10, colWhite);
  CheckArea('the border on the left is white', FImage, 5, 9, 6, 9 + lD + 3, colWhite);
  CheckArea('the border on the right is white', FImage, 5 + lD + 2, 9, 5 + lD + 3, 9 + lD + 3, colWhite);
  CheckArea('the border below is white', FImage, 5, 9 + lD + 2, 5 + lD + 3, 9 + lD + 3, colWhite);
  CheckModules('the symbol inside the border at (5,9)', FImage, 7, 11, 3, True);
  CheckArea('the pixels left of the drawing are untouched', FImage, 0, 0, 4, FImage.Height - 1, cRedColor);
  CheckArea('the pixels above the drawing are untouched', FImage, 0, 0, FImage.Width - 1, 8, cRedColor);
  CheckArea('the pixels right of the drawing are untouched', FImage, 5 + lD + 4, 0, FImage.Width - 1, FImage.Height - 1, cRedColor);
end;


procedure TTestQRCodeImage.TestGeneratorSaveToStream;

var
  lStream: TMemoryStream;
  lWriter: TFPWriterBMP;
  lReader: TFPReaderBMP;
  lD: Integer;

begin
  FGenerator.Generate('SAVE');
  FGenerator.PixelSize := 3;
  FGenerator.Border := 4;
  lD := FGenerator.Size * 3;
  lStream := TMemoryStream.Create;
  lWriter := TFPWriterBMP.Create;
  lReader := TFPReaderBMP.Create;
  try
    AssertTrue('SaveToStream reports success', FGenerator.SaveToStream(lStream, lWriter));
    lStream.Position := 0;
    FImage := TFPMemoryImage.Create(0, 0);
    FImage.LoadFromStream(lStream, lReader);
    AssertEquals('the saved image is the symbol plus the border wide', lD + 8, FImage.Width);
    AssertEquals('the saved image is the symbol plus the border high', lD + 8, FImage.Height);
    CheckArea('the saved border above is white', FImage, 0, 0, lD + 7, 3, colWhite);
    CheckArea('the saved border on the left is white', FImage, 0, 0, 3, lD + 7, colWhite);
    CheckModules('the saved symbol', FImage, 4, 4, 3, True);
  finally
    lReader.Free;
    lWriter.Free;
    lStream.Free;
  end;
end;


procedure TTestQRCodeImage.TestGeneratorSaveToStreamWithoutCode;

var
  lStream: TMemoryStream;
  lWriter: TFPWriterBMP;

begin
  lStream := TMemoryStream.Create;
  lWriter := TFPWriterBMP.Create;
  try
    AssertFalse('SaveToStream without a symbol reports failure', FGenerator.SaveToStream(lStream, lWriter));
    AssertEquals('SaveToStream without a symbol writes nothing', 0, lStream.Size);
  finally
    lWriter.Free;
    lStream.Free;
  end;
end;


initialization
  InitGaloisField;
  RegisterTests('qrcode', [TTestQRCode, TTestQRCodeImage]);
end.
