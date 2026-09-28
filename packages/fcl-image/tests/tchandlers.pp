{
    Tests for the registry of image readers and writers: lookup by extension,
    detection of the format of a stream, and loading and saving by file name.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tchandlers;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, fpcunit, testregistry, fpimage, fpimgtests,
     fpreadbmp, fpwritebmp, fpreadpng, fpwritepng, fpreadjpeg, fpwritejpeg,
     fpreadgif, fpwritegif, fpreadtga, fpwritetga, fpreadtiff, fpwritetiff,
     fpreadpcx, fpwritepcx, fpreadpnm, fpwritepnm, fpreadxpm, fpwritexpm,
     fpreadqoi, fpwriteqoi, fpreadpsd, fpreadxwd, fptiffcmn, fpreadico, fpwriteico, fpreadwebp, fpwritewebp;

type
  TTestHandlers = class(TTestCase)
  private
    FFileName: String;
    procedure LoadGarbage;
    procedure RegisterTwice;
    procedure ReadCutPNGWithoutImage;
    // A file name in the temporary directory with that extension.
    function TempFile(const aExtension: String): String;
    // Writes a small image with the writer to a new stream.
    function WriteSample(aWriterClass: TFPCustomImageWriterClass): TMemoryStream;
    // Checks that a stream written by the writer is detected as the reader's format.
    procedure CheckDetected(const aName: String; aWriterClass: TFPCustomImageWriterClass; aReaderClass: TFPCustomImageReaderClass);
  protected
    procedure TearDown; override;
  published
    procedure TestEveryFormatHasAReader;
    procedure TestEveryWritableFormatHasAWriter;
    procedure TestReaderAndWriterOfAnExtensionShareOneEntry;
    procedure TestExtensionLookupIgnoresCaseAndDot;
    procedure TestLookupByFileName;
    procedure TestUnknownExtensionsGiveNothing;
    procedure TestDefaultExtensionIsTheFirst;
    procedure TestDetectBMP;
    procedure TestDetectPNG;
    procedure TestDetectJPEG;
    procedure TestDetectGIF;
    procedure TestDetectTIFF;
    procedure TestDetectPCX;
    procedure TestDetectPNM;
    procedure TestDetectXPM;
    procedure TestDetectQOI;
    procedure TestDetectTGA;
    procedure TestDetectICO;
    procedure TestDetectCUR;
    procedure TestDetectWebP;
    procedure TestDetectionRestoresThePosition;
    procedure TestTextIsNoImage;
    procedure TestLoadingGarbageRaises;
    procedure TestLoadFromStreamDetectsTheFormat;
    procedure TestReadingWithoutAnImageCreatesOne;
    procedure TestAFailedReadWithoutAnImageFreesIt;
    procedure TestSaveAndLoadByFileName;
    procedure TestSavingToAnUnknownExtensionFails;
    procedure TestLoadingAnUnknownExtensionDetectsTheFormat;
    procedure TestRegisteringATypeTwiceRaises;
    procedure TestUnregisterRemovesAType;
  end;

implementation

procedure TTestHandlers.TearDown;

begin
  if (FFileName <> '') and FileExists(FFileName) then
    DeleteFile(FFileName);
  FFileName := '';
  inherited TearDown;
end;


procedure TTestHandlers.LoadGarbage;

var
  lStream: TMemoryStream;
  lImage: TFPMemoryImage;

begin
  lStream := BytesStream([Ord('n'), Ord('o'), Ord('t'), 32, Ord('a'), 32, Ord('p'), Ord('i'), Ord('c'),
    Ord('t'), Ord('u'), Ord('r'), Ord('e'), 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]);
  lImage := TFPMemoryImage.Create(0, 0);
  try
    lImage.LoadFromStream(lStream);
  finally
    lImage.Free;
    lStream.Free;
  end;
end;


procedure TTestHandlers.RegisterTwice;

begin
  ImageHandlers.RegisterImageHandlers('Test Twice', 'tw1', TFPReaderBMP, TFPWriterBMP);
  try
    ImageHandlers.RegisterImageHandlers('Test Twice', 'tw2', TFPReaderBMP, TFPWriterBMP);
  finally
    ImageHandlers.UnregisterImageHandlers('Test Twice');
  end;
end;


procedure TTestHandlers.ReadCutPNGWithoutImage;

var
  lStream: TMemoryStream;
  lReader: TFPReaderPNG;

begin
  lStream := WriteSample(TFPWriterPNG);
  lReader := TFPReaderPNG.Create;
  try
    lStream.Size := lStream.Size div 2;
    lStream.Position := 0;
    try
      lReader.ImageRead(lStream, nil);
    except
      on EStreamError do ;
      on FPImageException do ;
    end;
  finally
    lReader.Free;
    lStream.Free;
  end;
end;


function TTestHandlers.TempFile(const aExtension: String): String;

begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'fpimgtest-' + IntToStr(GetProcessID) + aExtension;
  FFileName := Result;
end;


function TTestHandlers.WriteSample(aWriterClass: TFPCustomImageWriterClass): TMemoryStream;

var
  lImage: TFPMemoryImage;
  lWriter: TFPCustomImageWriter;

begin
  Result := TMemoryStream.Create;
  lImage := CreateGradientImage(8, 6);
  lWriter := aWriterClass.Create;
  try
    try
      lImage.SaveToStream(Result, lWriter);
      Result.Position := 0;
    except
      Result.Free;
      raise;
    end;
  finally
    lWriter.Free;
    lImage.Free;
  end;
end;


procedure TTestHandlers.CheckDetected(const aName: String; aWriterClass: TFPCustomImageWriterClass; aReaderClass: TFPCustomImageReaderClass);

var
  lStream: TMemoryStream;
  lFound: TFPCustomImageReaderClass;

begin
  lStream := WriteSample(aWriterClass);
  try
    lFound := TFPCustomImage.FindReaderFromStream(lStream);
    AssertNotNull('A ' + aName + ' stream is recognised', lFound);
    AssertEquals('A ' + aName + ' stream is recognised as ' + aName, aReaderClass.ClassName, lFound.ClassName);
  finally
    lStream.Free;
  end;
end;


procedure TTestHandlers.TestEveryFormatHasAReader;

const
  cExtensions: array[0..20] of String = ('bmp', 'png', 'jpg', 'jpeg', 'gif',
    'tga', 'tif', 'tiff', 'pcx', 'pnm', 'pgm', 'pbm', 'ppm', 'xpm', 'qoi',
    'psd', 'pdd', 'xwd', 'ico', 'cur', 'webp');

var
  lExt: String;

begin
  for lExt in cExtensions do
    AssertNotNull('There is a reader for .' + lExt, TFPCustomImage.FindReaderFromExtension(lExt));
end;


procedure TTestHandlers.TestEveryWritableFormatHasAWriter;

const
  cExtensions: array[0..17] of String = ('bmp', 'png', 'jpg', 'jpeg', 'gif',
    'tga', 'tif', 'tiff', 'pcx', 'pnm', 'pgm', 'pbm', 'ppm', 'xpm', 'qoi', 'ico', 'cur', 'webp');

var
  lExt: String;

begin
  for lExt in cExtensions do
    AssertNotNull('There is a writer for .' + lExt, TFPCustomImage.FindWriterFromExtension(lExt));
end;


procedure TTestHandlers.TestReaderAndWriterOfAnExtensionShareOneEntry;

const
  cExtensions: array[0..17] of String = ('bmp', 'png', 'jpg', 'jpeg', 'gif',
    'tga', 'tif', 'tiff', 'pcx', 'pnm', 'pgm', 'pbm', 'ppm', 'xpm', 'qoi', 'ico', 'cur', 'webp');

var
  lExt: String;
  lData: TIHData;

begin
  for lExt in cExtensions do
    begin
    lData := TFPCustomImage.FindHandlerFromExtension(lExt);
    AssertNotNull('There is a handler for .' + lExt, lData);
    AssertNotNull('The handler found for .' + lExt + ' has a reader', lData.Reader);
    AssertNotNull('The handler found for .' + lExt + ' has a writer', lData.Writer);
    end;
end;


procedure TTestHandlers.TestExtensionLookupIgnoresCaseAndDot;

begin
  AssertEquals('Upper case extension', 'TFPReaderPNG', TFPCustomImage.FindReaderFromExtension('PNG').ClassName);
  AssertEquals('Extension with a dot', 'TFPReaderPNG', TFPCustomImage.FindReaderFromExtension('.png').ClassName);
  AssertEquals('Upper case with a dot', 'TFPWriterBMP', TFPCustomImage.FindWriterFromExtension('.BMP').ClassName);
end;


procedure TTestHandlers.TestLookupByFileName;

begin
  AssertEquals('Reader from a file name', 'TFPReaderJPEG',
    TFPCustomImage.FindReaderFromFileName('some' + PathDelim + 'dir.x' + PathDelim + 'photo.JPeg').ClassName);
  AssertEquals('Writer from a file name', 'TFPWriterTiff',
    TFPCustomImage.FindWriterFromFileName('scan.tif').ClassName);
end;


procedure TTestHandlers.TestUnknownExtensionsGiveNothing;

begin
  AssertNull('An unknown extension has no reader', TFPCustomImage.FindReaderFromExtension('docx'));
  AssertNull('An empty extension has no reader', TFPCustomImage.FindReaderFromExtension(''));
  AssertNull('A part of an extension list is no match', TFPCustomImage.FindReaderFromExtension('pe'));
  AssertNull('A file without extension has no writer', TFPCustomImage.FindWriterFromFileName('README'));
end;


procedure TTestHandlers.TestDefaultExtensionIsTheFirst;

begin
  AssertEquals('The default extension of JPEG', 'jpg', ImageHandlers.DefaultExtension['JPEG Graphics']);
  AssertEquals('The extensions of TIFF', 'tif;tiff', ImageHandlers.Extensions[TiffHandlerName]);
end;


procedure TTestHandlers.TestDetectBMP;

begin
  CheckDetected('BMP', TFPWriterBMP, TFPReaderBMP);
end;


procedure TTestHandlers.TestDetectPNG;

begin
  CheckDetected('PNG', TFPWriterPNG, TFPReaderPNG);
end;


procedure TTestHandlers.TestDetectJPEG;

begin
  CheckDetected('JPEG', TFPWriterJPEG, TFPReaderJPEG);
end;


procedure TTestHandlers.TestDetectGIF;

begin
  CheckDetected('GIF', TFPWriterGIF, TFPReaderGif);
end;


procedure TTestHandlers.TestDetectTIFF;

begin
  CheckDetected('TIFF', TFPWriterTiff, TFPReaderTiff);
end;


procedure TTestHandlers.TestDetectPCX;

begin
  CheckDetected('PCX', TFPWriterPCX, TFPReaderPCX);
end;


procedure TTestHandlers.TestDetectPNM;

begin
  CheckDetected('PNM', TFPWriterPPM, TFPReaderPNM);
end;


procedure TTestHandlers.TestDetectXPM;

begin
  CheckDetected('XPM', TFPWriterXPM, TFPReaderXPM);
end;


procedure TTestHandlers.TestDetectQOI;

begin
  CheckDetected('QOI', TFPWriterQoi, TFPReaderQoi);
end;


procedure TTestHandlers.TestDetectTGA;

begin
  CheckDetected('TGA', TFPWriterTarga, TFPReaderTarga);
end;


procedure TTestHandlers.TestDetectICO;

begin
  CheckDetected('ICO', TFPWriterICO, TFPReaderICO);
end;


procedure TTestHandlers.TestDetectCUR;

begin
  CheckDetected('CUR', TFPWriterCUR, TFPReaderCUR);
end;


procedure TTestHandlers.TestDetectWebP;

begin
  CheckDetected('WebP', TFPWriterWebP, TFPReaderWebP);
end;


procedure TTestHandlers.TestDetectionRestoresThePosition;

var
  lStream: TMemoryStream;

begin
  lStream := WriteSample(TFPWriterPNG);
  try
    TFPCustomImage.FindReaderFromStream(lStream);
    AssertEquals('Detection leaves the stream where it was', 0, lStream.Position);
  finally
    lStream.Free;
  end;
end;


procedure TTestHandlers.TestTextIsNoImage;

var
  lStream: TStringStream;

begin
  lStream := TStringStream.Create('This is a plain text file, it holds no image at all.'#10'Second line.'#10);
  try
    AssertNull('Text is recognised by no reader', TFPCustomImage.FindReaderFromStream(lStream));
  finally
    lStream.Free;
  end;
end;


procedure TTestHandlers.TestLoadingGarbageRaises;

begin
  AssertRaises('Loading a stream no reader recognises raises', FPImageException, @LoadGarbage);
end;


procedure TTestHandlers.TestLoadFromStreamDetectsTheFormat;

var
  lStream: TMemoryStream;
  lImage: TFPMemoryImage;

begin
  lStream := WriteSample(TFPWriterBMP);
  lImage := TFPMemoryImage.Create(0, 0);
  try
    lImage.LoadFromStream(lStream);
    AssertEquals('The detected image has its width', 8, lImage.Width);
    AssertEquals('The detected image has its height', 6, lImage.Height);
  finally
    lImage.Free;
    lStream.Free;
  end;
end;


procedure TTestHandlers.TestReadingWithoutAnImageCreatesOne;

var
  lStream: TMemoryStream;
  lReader: TFPReaderPNG;
  lImage: TFPCustomImage;

begin
  lStream := WriteSample(TFPWriterPNG);
  lReader := TFPReaderPNG.Create;
  try
    lImage := lReader.ImageRead(lStream, nil);
    try
      AssertTrue('The reader creates an image of its default class', lImage is TFPMemoryImage);
      AssertEquals('of the size read', 8, lImage.Width);
    finally
      lImage.Free;
    end;
  finally
    lReader.Free;
    lStream.Free;
  end;
end;


procedure TTestHandlers.TestAFailedReadWithoutAnImageFreesIt;

begin
  AssertNoLeak('A read that fails frees the image it created', @ReadCutPNGWithoutImage);
end;


procedure TTestHandlers.TestSaveAndLoadByFileName;

var
  lImage, lRead: TFPMemoryImage;

begin
  lImage := CreateGradientImage(5, 4);
  lRead := TFPMemoryImage.Create(0, 0);
  try
    AssertTrue('Saving by a known extension succeeds', lImage.SaveToFile(TempFile('.png')));
    AssertTrue('The file exists', FileExists(FFileName));
    AssertTrue('Loading by a known extension succeeds', lRead.LoadFromFile(FFileName));
    AssertImagesEqual('The image loaded is the image saved', lImage, lRead);
  finally
    lRead.Free;
    lImage.Free;
  end;
end;


procedure TTestHandlers.TestSavingToAnUnknownExtensionFails;

var
  lImage: TFPMemoryImage;

begin
  lImage := CreateGradientImage(5, 4);
  try
    AssertFalse('Saving by an unknown extension reports failure', lImage.SaveToFile(TempFile('.unknownext')));
    AssertFalse('and writes no file', FileExists(FFileName));
  finally
    lImage.Free;
  end;
end;


procedure TTestHandlers.TestLoadingAnUnknownExtensionDetectsTheFormat;

var
  lImage, lRead: TFPMemoryImage;
  lWriter: TFPWriterPNG;

begin
  lImage := CreateGradientImage(5, 4);
  lRead := TFPMemoryImage.Create(0, 0);
  lWriter := TFPWriterPNG.Create;
  try
    lImage.SaveToFile(TempFile('.dat'), lWriter);
    AssertTrue('Loading a file of unknown extension that holds an image reports success', lRead.LoadFromFile(FFileName));
    AssertImagesEqual('The image is loaded', lImage, lRead);
  finally
    lWriter.Free;
    lRead.Free;
    lImage.Free;
  end;
end;


procedure TTestHandlers.TestRegisteringATypeTwiceRaises;

begin
  AssertRaises('A type name can be registered once', FPImageException, @RegisterTwice);
end;


procedure TTestHandlers.TestUnregisterRemovesAType;

var
  lCount: Integer;

begin
  lCount := ImageHandlers.Count;
  ImageHandlers.RegisterImageHandlers('Test Unregister', 'tu1', TFPReaderBMP, TFPWriterBMP);
  AssertEquals('Registering adds a type', lCount + 1, ImageHandlers.Count);
  AssertNotNull('The new extension is known', TFPCustomImage.FindReaderFromExtension('tu1'));
  ImageHandlers.UnregisterImageHandlers('Test Unregister');
  AssertEquals('Unregistering removes it', lCount, ImageHandlers.Count);
  AssertNull('The extension is no longer known', TFPCustomImage.FindReaderFromExtension('tu1'));
end;


initialization
  RegisterTest('handlers', TTestHandlers);
end.
