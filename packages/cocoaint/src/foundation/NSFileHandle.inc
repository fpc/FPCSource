{ Parsed from Foundation.framework NSFileHandle.h }


{ Types from NSFileHandleAsynchronousAccess }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  NSFileHandlePtr = ^NSFileHandle;
  NSPipePtr = ^NSPipe;
{$endif}

{$ifdef CLASSES}

type
  NSFileHandle = objcclass external (NSObject, NSSecureCodingProtocol)
  public
    function availableData: NSData; message 'availableData';
    function readDataToEndOfFile: NSData; message 'readDataToEndOfFile';
    function readDataOfLength (length: NSUInteger): NSData; message 'readDataOfLength:';
    procedure writeData (data: NSData); message 'writeData:';
    function offsetInFile: culonglong; message 'offsetInFile';
    function seekToEndOfFile: culonglong; message 'seekToEndOfFile';
    procedure seekToFileOffset (offset: culonglong); message 'seekToFileOffset:';
    procedure truncateFileAtOffset (offset: culonglong); message 'truncateFileAtOffset:';
    procedure synchronizeFile; message 'synchronizeFile';
    procedure closeFile; message 'closeFile';
    function initWithFileDescriptor_closeOnDealloc (fd: cint; closeopt: ObjCBOOL): instancetype; message 'initWithFileDescriptor:closeOnDealloc:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSFileHandleCreation = objccategory external (NSFileHandle)
    class function fileHandleWithStandardInput: NSFileHandle; message 'fileHandleWithStandardInput';
    class function fileHandleWithStandardOutput: NSFileHandle; message 'fileHandleWithStandardOutput';
    class function fileHandleWithStandardError: NSFileHandle; message 'fileHandleWithStandardError';
    class function fileHandleWithNullDevice: NSFileHandle; message 'fileHandleWithNullDevice';
    class function fileHandleForReadingAtPath (path: NSString): instancetype; message 'fileHandleForReadingAtPath:';
    class function fileHandleForWritingAtPath (path: NSString): instancetype; message 'fileHandleForWritingAtPath:';
    class function fileHandleForUpdatingAtPath (path: NSString): instancetype; message 'fileHandleForUpdatingAtPath:';
    class function fileHandleForReadingFromURL_error (url: NSURL; error: NSErrorPtr): instancetype; message 'fileHandleForReadingFromURL:error:'; { available in 10_6, 4_0 }
    class function fileHandleForWritingToURL_error (url: NSURL; error: NSErrorPtr): instancetype; message 'fileHandleForWritingToURL:error:'; { available in 10_6, 4_0 }
    class function fileHandleForUpdatingURL_error (url: NSURL; error: NSErrorPtr): instancetype; message 'fileHandleForUpdatingURL:error:'; { available in 10_6, 4_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSFileHandleOperationException: NSString; cvar; external;
  NSFileHandleReadCompletionNotification: NSString; cvar; external;
  NSFileHandleReadToEndOfFileCompletionNotification: NSString; cvar; external;
  NSFileHandleConnectionAcceptedNotification: NSString; cvar; external;
  NSFileHandleDataAvailableNotification: NSString; cvar; external;
  NSFileHandleNotificationDataItem: NSString; cvar; external;
  NSFileHandleNotificationFileHandleItem: NSString; cvar; external;
  NSFileHandleNotificationMonitorModes: NSString deprecated 'in 10_0, 10_7, 2_0, 5_0'; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSFileHandleAsynchronousAccess = objccategory external (NSFileHandle)
    procedure readInBackgroundAndNotifyForModes (modes: NSArray); message 'readInBackgroundAndNotifyForModes:';
    procedure readInBackgroundAndNotify; message 'readInBackgroundAndNotify';
    procedure readToEndOfFileInBackgroundAndNotifyForModes (modes: NSArray); message 'readToEndOfFileInBackgroundAndNotifyForModes:';
    procedure readToEndOfFileInBackgroundAndNotify; message 'readToEndOfFileInBackgroundAndNotify';
    procedure acceptConnectionInBackgroundAndNotifyForModes (modes: NSArray); message 'acceptConnectionInBackgroundAndNotifyForModes:';
    procedure acceptConnectionInBackgroundAndNotify; message 'acceptConnectionInBackgroundAndNotify';
    procedure waitForDataInBackgroundAndNotifyForModes (modes: NSArray); message 'waitForDataInBackgroundAndNotifyForModes:';
    procedure waitForDataInBackgroundAndNotify; message 'waitForDataInBackgroundAndNotify';
    {$ifdef __BLOCKS__}
    procedure setReadabilityHandler(newValue: OpaqueCBlock); message 'setReadabilityHandler:';
    function readabilityHandler: OpaqueCBlock; message 'readabilityHandler';
    procedure setWriteabilityHandler(newValue: OpaqueCBlock); message 'setWriteabilityHandler:';
    function writeabilityHandler: OpaqueCBlock; message 'writeabilityHandler';
    {$endif}
  end;

type
  NSFileHandlePlatformSpecific = objccategory external (NSFileHandle)
    function initWithFileDescriptor (fd: cint): instancetype; message 'initWithFileDescriptor:';
    function fileDescriptor: cint; message 'fileDescriptor';
  end;


type
  NSPipe = objcclass external (NSObject)
  public
    function fileHandleForReading: NSFileHandle; message 'fileHandleForReading';
    function fileHandleForWriting: NSFileHandle; message 'fileHandleForWriting';
    class function pipe: NSPipe; message 'pipe';
  end;
{$endif}

