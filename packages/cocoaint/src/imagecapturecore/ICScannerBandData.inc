{ Parsed from ImageCaptureCore.framework ICScannerBandData.h }


{$ifdef TYPES}
type
  ICScannerBandDataPtr = ^ICScannerBandData;
{$endif}

{$ifdef CLASSES}

type
  ICScannerBandData = objcclass external (NSObject)
  private
    _fullImageWidth: NSUInteger;
    _fullImageHeight: NSUInteger;
    _bitsPerPixel: NSUInteger;
    _bitsPerComponent: NSUInteger;
    _numComponents: NSUInteger;
    _bigEndian: ObjCBOOL;
    _pixelDataType: ICScannerPixelDataType;
    _colorSyncProfilePath: NSString;
    _bytesPerRow: NSUInteger;
    _dataStartRow: NSUInteger;
    _dataNumRows: NSUInteger;
    _dataSize: NSUInteger;
    _dataBuffer: NSData;
  public
    function fullImageWidth: NSUInteger; message 'fullImageWidth';
    function fullImageHeight: NSUInteger; message 'fullImageHeight';
    function bitsPerPixel: NSUInteger; message 'bitsPerPixel';
    function bitsPerComponent: NSUInteger; message 'bitsPerComponent';
    function numComponents: NSUInteger; message 'numComponents';
    function isBigEndian: ObjCBOOL; message 'isBigEndian';
    function pixelDataType: ICScannerPixelDataType; message 'pixelDataType';
    function colorSyncProfilePath: NSString; message 'colorSyncProfilePath';
    function bytesPerRow: NSUInteger; message 'bytesPerRow';
    function dataStartRow: NSUInteger; message 'dataStartRow';
    function dataNumRows: NSUInteger; message 'dataNumRows';
    function dataSize: NSUInteger; message 'dataSize';
    function dataBuffer: NSData; message 'dataBuffer';
  end;
{$endif}

