{
    Tests for the compact image classes of fpimage and the routines that
    choose the smallest of them for an image.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tccompactimg;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests;

type
  TTestCompactImg = class(TTestCase)
  private
    // An image of the class for that descriptor.
    function Make(aGray: Boolean; aDepth: Word; aAlpha: Boolean; aWidth, aHeight: Integer): TFPCustomImage;
    // Checks that the colour stored at (1,1) comes back as expected.
    procedure CheckStore(const aMessage: String; aImage: TFPCustomImage; const aStored, aExpected: TFPColor);
  published
    procedure TestTheClassForEachDescriptor;
    procedure TestEachClassReportsItsDescriptor;
    procedure TestRGBA16KeepsEverything;
    procedure TestRGB16KeepsColorAndIsOpaque;
    procedure TestRGBA8KeepsEightBitColors;
    procedure TestRGB8KeepsEightBitColorsAndIsOpaque;
    procedure TestEightBitStorageTruncates;
    procedure TestGray16KeepsGray;
    procedure TestGray8KeepsEightBitGray;
    procedure TestGrayAlpha16KeepsGrayAndAlpha;
    procedure TestGrayAlpha8KeepsGrayAndAlpha;
    procedure TestGrayImagesStoreTheLumaOfAColor;
    procedure TestANewCompactImageIsBlack;
    procedure TestGrowingKeepsThePixels;
    procedure TestMinimumOfAGrayOpaqueEightBitImage;
    procedure TestMinimumOfAColorImage;
    procedure TestMinimumOfATranslucentImage;
    procedure TestMinimumOfASixteenBitImage;
    procedure TestMinimumOfAnAlreadyMinimalImageIsItself;
    procedure TestMinimumImageKeepsTheColors;
    procedure TestCompatibleImageWithAlpha;
    procedure TestCompatibleImageOfACompactImage;
  end;

implementation

function TTestCompactImg.Make(aGray: Boolean; aDepth: Word; aAlpha: Boolean; aWidth, aHeight: Integer): TFPCustomImage;

begin
  Result := CreateFPCompactImg(GetFPCompactImgDesc(aGray, aDepth, aAlpha), aWidth, aHeight);
end;


procedure TTestCompactImg.CheckStore(const aMessage: String; aImage: TFPCustomImage; const aStored, aExpected: TFPColor);

begin
  try
    aImage.Colors[1, 1] := aStored;
    AssertColorsEqual(aMessage, aExpected, aImage.Colors[1, 1]);
  finally
    aImage.Free;
  end;
end;


procedure TTestCompactImg.TestTheClassForEachDescriptor;

begin
  AssertTrue('gray 8 opaque', GetFPCompactImgClass(GetFPCompactImgDesc(True, 8, False)) = TFPCompactImgGray8Bit);
  AssertTrue('gray 16 opaque', GetFPCompactImgClass(GetFPCompactImgDesc(True, 16, False)) = TFPCompactImgGray16Bit);
  AssertTrue('gray 8 alpha', GetFPCompactImgClass(GetFPCompactImgDesc(True, 8, True)) = TFPCompactImgGrayAlpha8Bit);
  AssertTrue('gray 16 alpha', GetFPCompactImgClass(GetFPCompactImgDesc(True, 16, True)) = TFPCompactImgGrayAlpha16Bit);
  AssertTrue('RGB 8 opaque', GetFPCompactImgClass(GetFPCompactImgDesc(False, 8, False)) = TFPCompactImgRGB8Bit);
  AssertTrue('RGB 16 opaque', GetFPCompactImgClass(GetFPCompactImgDesc(False, 16, False)) = TFPCompactImgRGB16Bit);
  AssertTrue('RGB 8 alpha', GetFPCompactImgClass(GetFPCompactImgDesc(False, 8, True)) = TFPCompactImgRGBA8Bit);
  AssertTrue('RGB 16 alpha', GetFPCompactImgClass(GetFPCompactImgDesc(False, 16, True)) = TFPCompactImgRGBA16Bit);
  AssertTrue('A depth below 8 takes the 8-bit class', GetFPCompactImgClass(GetFPCompactImgDesc(False, 1, False)) = TFPCompactImgRGB8Bit);
end;


procedure TTestCompactImg.TestEachClassReportsItsDescriptor;

var
  lGray, lAlpha: Boolean;
  lDepth: Word;
  lImage: TFPCustomImage;

begin
  for lGray := False to True do
    for lAlpha := False to True do
      begin
      lDepth := 8;
      while lDepth <= 16 do
        begin
        lImage := Make(lGray, lDepth, lAlpha, 2, 2);
        try
          AssertEquals('Gray of the descriptor', lGray, TFPCompactImgBase(lImage).Desc.Gray);
          AssertEquals('Alpha of the descriptor', lAlpha, TFPCompactImgBase(lImage).Desc.HasAlpha);
          AssertEquals('Depth of the descriptor', lDepth, TFPCompactImgBase(lImage).Desc.Depth);
          AssertEquals('Width', 2, lImage.Width);
          AssertEquals('Height', 2, lImage.Height);
        finally
          lImage.Free;
        end;
        Inc(lDepth, 8);
        end;
      end;
end;


procedure TTestCompactImg.TestRGBA16KeepsEverything;

begin
  CheckStore('RGBA16 keeps all 64 bits', Make(False, 16, True, 3, 3),
    FPColor($1234, $5678, $9ABC, $DEF0), FPColor($1234, $5678, $9ABC, $DEF0));
end;


procedure TTestCompactImg.TestRGB16KeepsColorAndIsOpaque;

begin
  CheckStore('RGB16 keeps the colour and reads back opaque', Make(False, 16, False, 3, 3),
    FPColor($1234, $5678, $9ABC, $DEF0), FPColor($1234, $5678, $9ABC, alphaOpaque));
end;


procedure TTestCompactImg.TestRGBA8KeepsEightBitColors;

begin
  CheckStore('RGBA8 keeps 8-bit values', Make(False, 8, True, 3, 3),
    RGB8(1, 128, 255, 77), RGB8(1, 128, 255, 77));
end;


procedure TTestCompactImg.TestRGB8KeepsEightBitColorsAndIsOpaque;

begin
  CheckStore('RGB8 keeps 8-bit values and reads back opaque', Make(False, 8, False, 3, 3),
    RGB8(1, 128, 255, 77), RGB8(1, 128, 255));
end;


procedure TTestCompactImg.TestEightBitStorageTruncates;

begin
  CheckStore('8-bit storage keeps the high byte', Make(False, 8, True, 3, 3),
    FPColor($12FF, $3400, $56AB, $78CD), FPColor($1212, $3434, $5656, $7878));
end;


procedure TTestCompactImg.TestGray16KeepsGray;

begin
  CheckStore('Gray16 keeps a 16-bit gray and reads back opaque', Make(True, 16, False, 3, 3),
    FPColor($4321, $4321, $4321, $1000), FPColor($4321, $4321, $4321, alphaOpaque));
end;


procedure TTestCompactImg.TestGray8KeepsEightBitGray;

begin
  CheckStore('Gray8 keeps an 8-bit gray and reads back opaque', Make(True, 8, False, 3, 3),
    RGB8(99, 99, 99, 10), RGB8(99, 99, 99));
end;


procedure TTestCompactImg.TestGrayAlpha16KeepsGrayAndAlpha;

begin
  CheckStore('GrayAlpha16 keeps gray and alpha', Make(True, 16, True, 3, 3),
    FPColor($4321, $4321, $4321, $1234), FPColor($4321, $4321, $4321, $1234));
end;


procedure TTestCompactImg.TestGrayAlpha8KeepsGrayAndAlpha;

begin
  CheckStore('GrayAlpha8 keeps 8-bit gray and alpha', Make(True, 8, True, 3, 3),
    RGB8(200, 200, 200, 30), RGB8(200, 200, 200, 30));
end;


procedure TTestCompactImg.TestGrayImagesStoreTheLumaOfAColor;

var
  lGray, lAlpha: Boolean;
  lDepth: Word;
  lImage: TFPCustomImage;
  lLuma: Word;

begin
  lGray := True;
  lLuma := CalculateGray(colGreen);
  for lAlpha := False to True do
    begin
    lDepth := 8;
    while lDepth <= 16 do
      begin
      lImage := Make(lGray, lDepth, lAlpha, 2, 2);
      try
        lImage.Colors[0, 0] := colGreen;
        AssertTrue(Format('A gray image (depth %d, alpha %s) stores the luma of pure green',
          [lDepth, BoolToStr(lAlpha, True)]), Abs(Integer(lImage.Colors[0, 0].Red) - lLuma) <= 257);
      finally
        lImage.Free;
      end;
      Inc(lDepth, 8);
      end;
    end;
end;


procedure TTestCompactImg.TestANewCompactImageIsBlack;

var
  lGray, lAlpha: Boolean;
  lDepth: Word;
  lImage: TFPCustomImage;
  lX, lY: Integer;
  lColor: TFPColor;

begin
  for lGray := False to True do
    for lAlpha := False to True do
      begin
      lDepth := 8;
      while lDepth <= 16 do
        begin
        lImage := Make(lGray, lDepth, lAlpha, 16, 16);
        for lY := 0 to 15 do
          for lX := 0 to 15 do
            lImage.Colors[lX, lY] := colWhite;
        lImage.Free;
        lImage := Make(lGray, lDepth, lAlpha, 16, 16);
        try
          for lY := 0 to 15 do
            for lX := 0 to 15 do
              begin
              lColor := lImage.Colors[lX, lY];
              AssertEquals(Format('Pixel (%d,%d) of a new %s image is black', [lX, lY, lImage.ClassName]),
                0, Integer(lColor.Red) + lColor.Green + lColor.Blue);
              end;
        finally
          lImage.Free;
        end;
        Inc(lDepth, 8);
        end;
      end;
end;


procedure TTestCompactImg.TestGrowingKeepsThePixels;

var
  lImage: TFPCustomImage;
  lX, lY: Integer;

begin
  lImage := Make(False, 8, False, 3, 2);
  try
    for lY := 0 to 1 do
      for lX := 0 to 2 do
        lImage.Colors[lX, lY] := RGB8(lX * 50, lY * 50, 7);
    lImage.SetSize(5, 4);
    for lY := 0 to 1 do
      for lX := 0 to 2 do
        AssertColorsEqual(Format('Pixel (%d,%d) survives growing', [lX, lY]),
          RGB8(lX * 50, lY * 50, 7), lImage.Colors[lX, lY]);
  finally
    lImage.Free;
  end;
end;


procedure TTestCompactImg.TestMinimumOfAGrayOpaqueEightBitImage;

var
  lImage: TFPMemoryImage;
  lDesc: TFPCompactImgDesc;

begin
  lImage := CreateGrayImage(8, 8);
  try
    lDesc := GetMinimumPTDesc(lImage);
    AssertTrue('An image of gray pixels is gray', lDesc.Gray);
    AssertFalse('An opaque image needs no alpha', lDesc.HasAlpha);
    AssertEquals('8-bit exact values need depth 8', 8, lDesc.Depth);
  finally
    lImage.Free;
  end;
end;


procedure TTestCompactImg.TestMinimumOfAColorImage;

var
  lImage: TFPMemoryImage;
  lDesc: TFPCompactImgDesc;

begin
  lImage := CreateSolidImage(4, 4, colGray);
  try
    lImage.Colors[3, 3] := RGB8(10, 10, 11);
    lDesc := GetMinimumPTDesc(lImage);
    AssertFalse('One pixel with a different blue makes the image a colour image', lDesc.Gray);
    lImage.Colors[3, 3] := RGB8(10, 11, 10);
    lDesc := GetMinimumPTDesc(lImage);
    AssertFalse('One pixel with a different green makes the image a colour image', lDesc.Gray);
  finally
    lImage.Free;
  end;
end;


procedure TTestCompactImg.TestMinimumOfATranslucentImage;

var
  lImage: TFPMemoryImage;

begin
  lImage := CreateSolidImage(4, 4, colGray);
  try
    lImage.Colors[2, 3] := RGB8(128, 128, 128, 100);
    AssertTrue('One translucent pixel needs alpha', GetMinimumPTDesc(lImage).HasAlpha);
  finally
    lImage.Free;
  end;
end;


procedure TTestCompactImg.TestMinimumOfASixteenBitImage;

var
  lImage: TFPMemoryImage;

begin
  lImage := CreateSolidImage(4, 4, RGB8(128, 128, 128));
  try
    AssertEquals('8-bit exact values need depth 8', 8, GetMinimumPTDesc(lImage).Depth);
    lImage.Colors[1, 2] := FPColor($1280, $1280, $1280);
    AssertEquals('A value whose low byte matters needs depth 16', 16, GetMinimumPTDesc(lImage).Depth);
  finally
    lImage.Free;
  end;
end;


procedure TTestCompactImg.TestMinimumOfAnAlreadyMinimalImageIsItself;

var
  lImage, lResult: TFPCustomImage;

begin
  lImage := Make(True, 8, False, 3, 3);
  lResult := GetMinimumFPCompactImg(lImage, True);
  try
    AssertSame('An image that is already minimal is returned as is', lImage, lResult);
  finally
    lResult.Free;
  end;
end;


procedure TTestCompactImg.TestMinimumImageKeepsTheColors;

var
  lImage: TFPMemoryImage;
  lResult: TFPCustomImage;

begin
  lImage := CreateGradientImage(6, 5);
  try
    lResult := GetMinimumFPCompactImg(lImage, False);
    try
      AssertTrue('The gradient fits an 8-bit RGB image', lResult is TFPCompactImgRGB8Bit);
      AssertImagesEqual('The minimal image has the same colours', lImage, lResult);
    finally
      lResult.Free;
    end;
  finally
    lImage.Free;
  end;
end;


procedure TTestCompactImg.TestCompatibleImageWithAlpha;

var
  lImage: TFPMemoryImage;
  lResult: TFPCustomImage;

begin
  lImage := CreateGrayImage(4, 4);
  try
    lResult := CreateCompatibleFPCompactImgWithAlpha(lImage, 7, 9);
    try
      AssertTrue('The compatible image has alpha', TFPCompactImgBase(lResult).Desc.HasAlpha);
      AssertTrue('and stays gray', TFPCompactImgBase(lResult).Desc.Gray);
      AssertEquals('Width asked for', 7, lResult.Width);
      AssertEquals('Height asked for', 9, lResult.Height);
    finally
      lResult.Free;
    end;
  finally
    lImage.Free;
  end;
end;


procedure TTestCompactImg.TestCompatibleImageOfACompactImage;

var
  lImage, lResult: TFPCustomImage;

begin
  lImage := Make(False, 16, False, 2, 2);
  try
    lResult := CreateCompatibleFPCompactImg(lImage, 3, 3);
    try
      AssertTrue('A compact image gives one of its own class', lResult.ClassType = lImage.ClassType);
    finally
      lResult.Free;
    end;
  finally
    lImage.Free;
  end;
end;


initialization
  RegisterTest('compactimg', TTestCompactImg);
end.
