{ Parsed from Foundation.framework NSData.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSDATA_PAS_H}
{$define NSDATA_PAS_H}
type
  NSDataPointer = Pointer;
  NSMutableDataPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSDATA_PAS_T}
{$define NSDATA_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDATA_PAS_R}
{$define NSDATA_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDATA_PAS_F}
{$define NSDATA_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDATA_PAS_T}
{$define NSDATA_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSData = objcclass;
  NSMutableData = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDATA_PAS_C}
{$define NSDATA_PAS_C}

{ NSData }
  NSData = objcclass(NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSData; message 'alloc';

    function length: culong; message 'length';
    function bytes: Pointer; message 'bytes';

    { Category: NSExtendedData }
    function description: NSString; message 'description';
    procedure getBytes(buffer: Pointer); message 'getBytes:';
    procedure getBytes_length(buffer: Pointer; length_: culong); message 'getBytes:length:';
    procedure getBytes_range(buffer: Pointer; range: NSRange); message 'getBytes:range:';
    function isEqualToData(other: NSData): Boolean; message 'isEqualToData:';
    function subdataWithRange(range: NSRange): NSData; message 'subdataWithRange:';
    function writeToFile_atomically(path: NSString; useAuxiliaryFile: Boolean): Boolean; message 'writeToFile:atomically:';
    function writeToURL_atomically(url: NSURL; atomically: Boolean): Boolean; message 'writeToURL:atomically:';
    function writeToFile_options_error(path: NSString; writeOptionsMask: culong; errorPtr: NSErrorPointer): Boolean; message 'writeToFile:options:error:';
    function writeToURL_options_error(url: NSURL; writeOptionsMask: culong; errorPtr: NSErrorPointer): Boolean; message 'writeToURL:options:error:';

    { Category: NSDataCreation }
    class function data: id; message 'data';
    class function dataWithBytes_length(bytes_: Pointer; length_: culong): id; message 'dataWithBytes:length:';
    class function dataWithBytesNoCopy_length(bytes_: Pointer; length_: culong): id; message 'dataWithBytesNoCopy:length:';
    class function dataWithBytesNoCopy_length_freeWhenDone(bytes_: Pointer; length_: culong; b: Boolean): id; message 'dataWithBytesNoCopy:length:freeWhenDone:';
    class function dataWithContentsOfFile_options_error(path: NSString; readOptionsMask: culong; errorPtr: NSErrorPointer): id; message 'dataWithContentsOfFile:options:error:';
    class function dataWithContentsOfURL_options_error(url: NSURL; readOptionsMask: culong; errorPtr: NSErrorPointer): id; message 'dataWithContentsOfURL:options:error:';
    class function dataWithContentsOfFile(path: NSString): id; message 'dataWithContentsOfFile:';
    class function dataWithContentsOfURL(url: NSURL): id; message 'dataWithContentsOfURL:';
    class function dataWithContentsOfMappedFile(path: NSString): id; message 'dataWithContentsOfMappedFile:';
    function initWithBytes_length(bytes_: Pointer; length_: culong): id; message 'initWithBytes:length:';
    function initWithBytesNoCopy_length(bytes_: Pointer; length_: culong): id; message 'initWithBytesNoCopy:length:';
    function initWithBytesNoCopy_length_freeWhenDone(bytes_: Pointer; length_: culong; b: Boolean): id; message 'initWithBytesNoCopy:length:freeWhenDone:';
    function initWithContentsOfFile_options_error(path: NSString; readOptionsMask: culong; errorPtr: NSErrorPointer): id; message 'initWithContentsOfFile:options:error:';
    function initWithContentsOfURL_options_error(url: NSURL; readOptionsMask: culong; errorPtr: NSErrorPointer): id; message 'initWithContentsOfURL:options:error:';
    function initWithContentsOfFile(path: NSString): id; message 'initWithContentsOfFile:';
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';
    function initWithContentsOfMappedFile(path: NSString): id; message 'initWithContentsOfMappedFile:';
    function initWithData(data_: NSData): id; message 'initWithData:';
    class function dataWithData(data_: NSData): id; message 'dataWithData:';
  end; external;

{ NSMutableData }
  NSMutableData = objcclass(NSData)
    
  public
    class function alloc: NSMutableData; message 'alloc';

    function mutableBytes: Pointer; message 'mutableBytes';
    procedure setLength(length_: culong); message 'setLength:';

    { Category: NSExtendedMutableData }
    procedure appendBytes_length(bytes_: Pointer; length_: culong); message 'appendBytes:length:';
    procedure appendData(other: NSData); message 'appendData:';
    procedure increaseLengthBy(extraLength: culong); message 'increaseLengthBy:';
    procedure replaceBytesInRange_withBytes(range: NSRange; bytes_: Pointer); message 'replaceBytesInRange:withBytes:';
    procedure resetBytesInRange(range: NSRange); message 'resetBytesInRange:';
    procedure setData(data_: NSData); message 'setData:';
    procedure replaceBytesInRange_withBytes_length(range: NSRange; replacementBytes: Pointer; replacementLength: culong); message 'replaceBytesInRange:withBytes:length:';

    { Category: NSMutableDataCreation }
    class function dataWithCapacity(aNumItems: culong): id; message 'dataWithCapacity:';
    class function dataWithLength(length_: culong): id; message 'dataWithLength:';
    function initWithCapacity(capacity: culong): id; message 'initWithCapacity:';
    function initWithLength(length_: culong): id; message 'initWithLength:';
  end; external;

{$endif}
{$endif}
