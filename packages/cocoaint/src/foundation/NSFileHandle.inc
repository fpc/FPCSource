{ Parsed from Foundation.framework NSFileHandle.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSFILEHANDLE_PAS_T}
{$define NSFILEHANDLE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFILEHANDLE_PAS_R}
{$define NSFILEHANDLE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFILEHANDLE_PAS_F}
{$define NSFILEHANDLE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFILEHANDLE_PAS_S}
{$define NSFILEHANDLE_PAS_S}

{ External string constants }
var
  NSFileHandleOperationException: NSString; cvar; external;
  NSFileHandleReadCompletionNotification: NSString; cvar; external;
  NSFileHandleReadToEndOfFileCompletionNotification: NSString; cvar; external;
  NSFileHandleConnectionAcceptedNotification: NSString; cvar; external;
  NSFileHandleDataAvailableNotification: NSString; cvar; external;
  NSFileHandleNotificationDataItem: NSString; cvar; external;
  NSFileHandleNotificationFileHandleItem: NSString; cvar; external;
  NSFileHandleNotificationMonitorModes: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFileHandle = objcclass;
  NSFileHandlePointer = ^NSFileHandle;
  NSFileHandlePtr = NSFileHandlePointer;
  NSPipe = objcclass;
  NSPipePointer = ^NSPipe;
  NSPipePtr = NSPipePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFILEHANDLE_PAS_C}
{$define NSFILEHANDLE_PAS_C}

{ NSFileHandle }
  NSFileHandle = objcclass(NSObject)
    
  public
    class function alloc: NSFileHandle; message 'alloc';

    function availableData: NSData; message 'availableData';
    function readDataToEndOfFile: NSData; message 'readDataToEndOfFile';
    function readDataOfLength(length: NSUInteger): NSData; message 'readDataOfLength:';
    procedure writeData(data: NSData); message 'writeData:';
    function offsetInFile: culonglong; message 'offsetInFile';
    function seekToEndOfFile: culonglong; message 'seekToEndOfFile';
    procedure seekToFileOffset(offset: culonglong); message 'seekToFileOffset:';
    procedure truncateFileAtOffset(offset: culonglong); message 'truncateFileAtOffset:';
    procedure synchronizeFile; message 'synchronizeFile';
    procedure closeFile; message 'closeFile';
  end; external;

{ NSPipe }
  NSPipe = objcclass(NSObject)
    
  public
    class function alloc: NSPipe; message 'alloc';

    function fileHandleForReading: NSFileHandle; message 'fileHandleForReading';
    function fileHandleForWriting: NSFileHandle; message 'fileHandleForWriting';
    function init: id; message 'init';
    class function pipe: id; message 'pipe';
  end; external;

{ NSFileHandleCreationCategory }
  NSFileHandleCreationCategory = objccategory(NSFileHandle)
    class function fileHandleWithStandardInput: id; message 'fileHandleWithStandardInput';
    class function fileHandleWithStandardOutput: id; message 'fileHandleWithStandardOutput';
    class function fileHandleWithStandardError: id; message 'fileHandleWithStandardError';
    class function fileHandleWithNullDevice: id; message 'fileHandleWithNullDevice';
    class function fileHandleForReadingAtPath(path: NSString): id; message 'fileHandleForReadingAtPath:';
    class function fileHandleForWritingAtPath(path: NSString): id; message 'fileHandleForWritingAtPath:';
    class function fileHandleForUpdatingAtPath(path: NSString): id; message 'fileHandleForUpdatingAtPath:';
    class function fileHandleForReadingFromURL_error(url: NSURL; error: NSErrorPointer): id; message 'fileHandleForReadingFromURL:error:';
    class function fileHandleForWritingToURL_error(url: NSURL; error: NSErrorPointer): id; message 'fileHandleForWritingToURL:error:';
    class function fileHandleForUpdatingURL_error(url: NSURL; error: NSErrorPointer): id; message 'fileHandleForUpdatingURL:error:';
  end; external;

{ NSFileHandleAsynchronousAccessCategory }
  NSFileHandleAsynchronousAccessCategory = objccategory(NSFileHandle)
    procedure readInBackgroundAndNotifyForModes(modes: NSArray); message 'readInBackgroundAndNotifyForModes:';
    procedure readInBackgroundAndNotify; message 'readInBackgroundAndNotify';
    procedure readToEndOfFileInBackgroundAndNotifyForModes(modes: NSArray); message 'readToEndOfFileInBackgroundAndNotifyForModes:';
    procedure readToEndOfFileInBackgroundAndNotify; message 'readToEndOfFileInBackgroundAndNotify';
    procedure acceptConnectionInBackgroundAndNotifyForModes(modes: NSArray); message 'acceptConnectionInBackgroundAndNotifyForModes:';
    procedure acceptConnectionInBackgroundAndNotify; message 'acceptConnectionInBackgroundAndNotify';
    procedure waitForDataInBackgroundAndNotifyForModes(modes: NSArray); message 'waitForDataInBackgroundAndNotifyForModes:';
    procedure waitForDataInBackgroundAndNotify; message 'waitForDataInBackgroundAndNotify';
  end; external;

{ NSFileHandlePlatformSpecificCategory }
  NSFileHandlePlatformSpecificCategory = objccategory(NSFileHandle)
    function initWithFileDescriptor_closeOnDealloc(fd: cint; closeopt: Boolean): id; message 'initWithFileDescriptor:closeOnDealloc:';
    function initWithFileDescriptor(fd: cint): id; message 'initWithFileDescriptor:';
    function fileDescriptor: cint; message 'fileDescriptor';
  end; external;

{$endif}
{$endif}
