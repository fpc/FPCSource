{ Parsed from ITunesLibrary.framework ITLibArtwork.h }
{ Created on Sun Mar 8 1:48:57 2015 }


{$ifdef TYPES}
type
  ITLibArtworkPtr = ^ITLibArtwork;
{$endif}

{$ifdef TYPES}
const
  ITLibArtworkFormatNone = 0;
  ITLibArtworkFormatBitmap = 1;
  ITLibArtworkFormatJPEG = 2;
  ITLibArtworkFormatJPEG2000 = 3;
  ITLibArtworkFormatGIF = 4;
  ITLibArtworkFormatPNG = 5;
  ITLibArtworkFormatBMP = 6;
  ITLibArtworkFormatTIFF = 7;
  ITLibArtworkFormatPICT = 8;

type
  ITLibArtworkFormat = NSUInteger;
  ITLibArtworkFormatPtr = ^ITLibArtworkFormat;
{$endif}

{$ifdef CLASSES}

type
  ITLibArtwork = objcclass external (NSObject)
  private
    _impl: pointer;
    _cachedImage: NSImage;
  public
    function image: NSImage; message 'image';
    function imageData: NSData; message 'imageData';
    function imageDataFormat: ITLibArtworkFormat; message 'imageDataFormat';
  end;
{$endif}

