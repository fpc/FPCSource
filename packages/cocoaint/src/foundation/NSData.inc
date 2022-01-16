{ Parsed from Foundation.framework NSData.h }


{$ifdef TYPES}
type
  NSDataPtr = ^NSData;
  NSMutableDataPtr = ^NSMutableData;
  NSPurgeableDataPtr = ^NSPurgeableData;
{$endif}

{$ifdef TYPES}
type
  NSDataReadingOptions = NSUInteger;
  NSDataReadingOptionsPtr = ^NSDataReadingOptions;

const
  NSDataReadingMappedIfSafe = 1 shl 0;
  NSDataReadingUncached = 1 shl 1;
  NSDataReadingMappedAlways = 1 shl 3 { available in 10_7, 5_0 };
  NSDataReadingMapped = NSDataReadingMappedIfSafe;
  NSMappedRead = NSDataReadingMapped;
  NSUncachedRead = NSDataReadingUncached;

type
  NSDataWritingOptions = NSUInteger;
  NSDataWritingOptionsPtr = ^NSDataWritingOptions;

const
  NSDataWritingAtomic = 1 shl 0;
  NSDataWritingWithoutOverwriting = 1 shl 1 { available in 10_8, 6_0 };
  NSDataWritingFileProtectionNone = $10000000 { available in 4_0 };
  NSDataWritingFileProtectionComplete = $20000000 { available in 4_0 };
  NSDataWritingFileProtectionCompleteUnlessOpen = $30000000 { available in 5_0 };
  NSDataWritingFileProtectionCompleteUntilFirstUserAuthentication = $40000000 { available in 5_0 };
  NSDataWritingFileProtectionMask = $f0000000 { available in 4_0 };
  NSAtomicWrite = NSDataWritingAtomic;

type
  NSDataSearchOptions = NSUInteger;
  NSDataSearchOptionsPtr = ^NSDataSearchOptions;

const
  NSDataSearchBackwards = 1 shl 0;
  NSDataSearchAnchored = 1 shl 1;

type
  NSDataBase64EncodingOptions = NSUInteger;
  NSDataBase64EncodingOptionsPtr = ^NSDataBase64EncodingOptions;

const
  NSDataBase64Encoding64CharacterLineLength = 1 shl 0;
  NSDataBase64Encoding76CharacterLineLength = 1 shl 1;
  NSDataBase64EncodingEndLineWithCarriageReturn = 1 shl 4;
  NSDataBase64EncodingEndLineWithLineFeed = 1 shl 5;

type
  NSDataBase64DecodingOptions = NSUInteger;
  NSDataBase64DecodingOptionsPtr = ^NSDataBase64DecodingOptions;

const
  NSDataBase64DecodingIgnoreUnknownCharacters = 1 shl 0;
{$endif}

{$ifdef CLASSES}

type
  NSData = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSSecureCodingProtocol)
  public
    function length: NSUInteger; message 'length';
    function bytes: pointer; message 'bytes';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSExtendedData = objccategory external (NSData)
    function description: NSString; message 'description';
    procedure getBytes_length (buffer: pointer; length_: NSUInteger); message 'getBytes:length:';
    procedure getBytes_range (buffer: pointer; range: NSRange); message 'getBytes:range:';
    function isEqualToData (other: NSData): ObjCBOOL; message 'isEqualToData:';
    function subdataWithRange (range: NSRange): NSData; message 'subdataWithRange:';
    function writeToFile_atomically (path: NSString; useAuxiliaryFile: ObjCBOOL): ObjCBOOL; message 'writeToFile:atomically:';
    function writeToURL_atomically (url: NSURL; atomically: ObjCBOOL): ObjCBOOL; message 'writeToURL:atomically:';
    function writeToFile_options_error (path: NSString; writeOptionsMask: NSDataWritingOptions; errorPtr: NSErrorPtr): ObjCBOOL; message 'writeToFile:options:error:';
    function writeToURL_options_error (url: NSURL; writeOptionsMask: NSDataWritingOptions; errorPtr: NSErrorPtr): ObjCBOOL; message 'writeToURL:options:error:';
    function rangeOfData_options_range (dataToFind: NSData; mask: NSDataSearchOptions; searchRange: NSRange): NSRange; message 'rangeOfData:options:range:'; { available in 10_6, 4_0 }
    procedure enumerateByteRangesUsingBlock (block: OpaqueCBlock); message 'enumerateByteRangesUsingBlock:'; { available in 10_9, 7_0 }
  end;

type
  NSDataCreation = objccategory external (NSData)
    class function data: instancetype; message 'data';
    class function dataWithBytes_length (bytes_: pointer; length_: NSUInteger): instancetype; message 'dataWithBytes:length:';
    class function dataWithBytesNoCopy_length (bytes_: pointer; length_: NSUInteger): instancetype; message 'dataWithBytesNoCopy:length:';
    class function dataWithBytesNoCopy_length_freeWhenDone (bytes_: pointer; length_: NSUInteger; b: ObjCBOOL): instancetype; message 'dataWithBytesNoCopy:length:freeWhenDone:';
    class function dataWithContentsOfFile_options_error (path: NSString; readOptionsMask: NSDataReadingOptions; errorPtr: NSErrorPtr): instancetype; message 'dataWithContentsOfFile:options:error:';
    class function dataWithContentsOfURL_options_error (url: NSURL; readOptionsMask: NSDataReadingOptions; errorPtr: NSErrorPtr): instancetype; message 'dataWithContentsOfURL:options:error:';
    class function dataWithContentsOfFile (path: NSString): instancetype; message 'dataWithContentsOfFile:';
    class function dataWithContentsOfURL (url: NSURL): instancetype; message 'dataWithContentsOfURL:';
    function initWithBytes_length (bytes_: pointer; length_: NSUInteger): instancetype; message 'initWithBytes:length:';
    function initWithBytesNoCopy_length (bytes_: pointer; length_: NSUInteger): instancetype; message 'initWithBytesNoCopy:length:';
    function initWithBytesNoCopy_length_freeWhenDone (bytes_: pointer; length_: NSUInteger; b: ObjCBOOL): instancetype; message 'initWithBytesNoCopy:length:freeWhenDone:';
    function initWithBytesNoCopy_length_deallocator (bytes_: pointer; length_: NSUInteger; deallocator: OpaqueCBlock): instancetype; message 'initWithBytesNoCopy:length:deallocator:'; { available in 10_9, 7_0 }
    function initWithContentsOfFile_options_error (path: NSString; readOptionsMask: NSDataReadingOptions; errorPtr: NSErrorPtr): instancetype; message 'initWithContentsOfFile:options:error:';
    function initWithContentsOfURL_options_error (url: NSURL; readOptionsMask: NSDataReadingOptions; errorPtr: NSErrorPtr): instancetype; message 'initWithContentsOfURL:options:error:';
    function initWithContentsOfFile (path: NSString): instancetype; message 'initWithContentsOfFile:';
    function initWithContentsOfURL (url: NSURL): instancetype; message 'initWithContentsOfURL:';
    function initWithData (data_: NSData): instancetype; message 'initWithData:';
    class function dataWithData (data_: NSData): instancetype; message 'dataWithData:';
  end;

type
  NSDataBase64Encoding = objccategory external (NSData)
    function initWithBase64EncodedString_options (base64String: NSString; options: NSDataBase64DecodingOptions): instancetype; message 'initWithBase64EncodedString:options:'; { available in 10_9, 7_0 }
    function base64EncodedStringWithOptions (options: NSDataBase64EncodingOptions): NSString; message 'base64EncodedStringWithOptions:'; { available in 10_9, 7_0 }
    function initWithBase64EncodedData_options (base64Data: NSData; options: NSDataBase64DecodingOptions): instancetype; message 'initWithBase64EncodedData:options:'; { available in 10_9, 7_0 }
    function base64EncodedDataWithOptions (options: NSDataBase64EncodingOptions): NSData; message 'base64EncodedDataWithOptions:'; { available in 10_9, 7_0 }
  end;

type
  NSData_NSDeprecated = objccategory external name 'NSDeprecated' (NSData)
    procedure getBytes (buffer: pointer); message 'getBytes:'; deprecated 'in 10_0, 10_10, 2_0, 8_0, "This method is unsafe because it could potentially cause buffer overruns. Use -getBytes:length: instead."';
    class function dataWithContentsOfMappedFile (path: NSString): id; message 'dataWithContentsOfMappedFile:'; deprecated 'in 10_0, 10_10, 2_0, 8_0, "Use +dataWithContentsOfURL:options:error: and NSDataReadingMappedIfSafe or NSDataReadingMappedAlways instead."';
    function initWithContentsOfMappedFile (path: NSString): id; message 'initWithContentsOfMappedFile:'; deprecated 'in 10_0, 10_10, 2_0, 8_0, "Use -initWithContentsOfURL:options:error: and NSDataReadingMappedIfSafe or NSDataReadingMappedAlways instead."';
    function initWithBase64Encoding (base64String: NSString): id; message 'initWithBase64Encoding:'; deprecated 'in 10_6, 10_9, 4_0, 7_0';
    function base64Encoding: NSString; message 'base64Encoding'; deprecated 'in 10_6, 10_9, 4_0, 7_0';
  end;


type
  NSMutableData = objcclass external (NSData)
  public
    function mutableBytes: pointer; message 'mutableBytes';
    procedure setLength(newValue: NSUInteger); message 'setLength:';
    function length: NSUInteger; message 'length';
  end;


type
  NSExtendedMutableData = objccategory external (NSMutableData)
    procedure appendBytes_length (bytes_: pointer; length_: NSUInteger); message 'appendBytes:length:';
    procedure appendData (other: NSData); message 'appendData:';
    procedure increaseLengthBy (extraLength: NSUInteger); message 'increaseLengthBy:';
    procedure replaceBytesInRange_withBytes (range: NSRange; bytes_: pointer); message 'replaceBytesInRange:withBytes:';
    procedure resetBytesInRange (range: NSRange); message 'resetBytesInRange:';
    procedure setData (data: NSData); message 'setData:';
    procedure replaceBytesInRange_withBytes_length (range: NSRange; replacementBytes: pointer; replacementLength: NSUInteger); message 'replaceBytesInRange:withBytes:length:';
  end;

type
  NSMutableDataCreation = objccategory external (NSMutableData)
    class function dataWithCapacity (aNumItems: NSUInteger): instancetype; message 'dataWithCapacity:';
    class function dataWithLength (length_: NSUInteger): instancetype; message 'dataWithLength:';
    function initWithCapacity (capacity: NSUInteger): instancetype; message 'initWithCapacity:';
    function initWithLength (length_: NSUInteger): instancetype; message 'initWithLength:';
  end;


type
  NSPurgeableData = objcclass external (NSMutableData, NSDiscardableContentProtocol)
  private
    _length: NSUInteger;
    _accessCount: cint32;
    _private:array[0..31] of cuint8;
    _reserved: pointer;

    { Adopted protocols }
    function beginContentAccess: ObjCBOOL; message 'beginContentAccess';
    procedure discardContentIfPossible; message 'discardContentIfPossible';
    procedure endContentAccess; message 'endContentAccess';
    function isContentDiscarded: ObjCBOOL; message 'isContentDiscarded';
  end;
{$endif}

