{ Parsed from Foundation.framework NSFileHandle.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSFILEHANDLE_PAS_H}
{$define NSFILEHANDLE_PAS_H}
type
  NSFileHandlePointer = Pointer;
  NSPipePointer = Pointer;

{$endif}
{$endif}

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
  NSFileHandleOperationException: NSString; external name '_NSFileHandleOperationException';
  NSFileHandleReadCompletionNotification: NSString; external name '_NSFileHandleReadCompletionNotification';
  NSFileHandleReadToEndOfFileCompletionNotification: NSString; external name '_NSFileHandleReadToEndOfFileCompletionNotification';
  NSFileHandleConnectionAcceptedNotification: NSString; external name '_NSFileHandleConnectionAcceptedNotification';
  NSFileHandleDataAvailableNotification: NSString; external name '_NSFileHandleDataAvailableNotification';
  NSFileHandleNotificationDataItem: NSString; external name '_NSFileHandleNotificationDataItem';
  NSFileHandleNotificationFileHandleItem: NSString; external name '_NSFileHandleNotificationFileHandleItem';
  NSFileHandleNotificationMonitorModes: NSString; external name '_NSFileHandleNotificationMonitorModes';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFileHandle = objcclass;
  NSPipe = objcclass;

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

    { Category: NSFileHandleCreation }
    class function fileHandleWithStandardInput: id; message 'fileHandleWithStandardInput';
    class function fileHandleWithStandardOutput: id; message 'fileHandleWithStandardOutput';
    class function fileHandleWithStandardError: id; message 'fileHandleWithStandardError';
    class function fileHandleWithNullDevice: id; message 'fileHandleWithNullDevice';
    class function fileHandleForReadingAtPath(path: NSString): id; message 'fileHandleForReadingAtPath:';
    class function fileHandleForWritingAtPath(path: NSString): id; message 'fileHandleForWritingAtPath:';
    class function fileHandleForUpdatingAtPath(path: NSString): id; message 'fileHandleForUpdatingAtPath:';

    { Category: NSFileHandleAsynchronousAccess }
    procedure readInBackgroundAndNotifyForModes(modes: NSArray); message 'readInBackgroundAndNotifyForModes:';
    procedure readInBackgroundAndNotify; message 'readInBackgroundAndNotify';
    procedure readToEndOfFileInBackgroundAndNotifyForModes(modes: NSArray); message 'readToEndOfFileInBackgroundAndNotifyForModes:';
    procedure readToEndOfFileInBackgroundAndNotify; message 'readToEndOfFileInBackgroundAndNotify';
    procedure acceptConnectionInBackgroundAndNotifyForModes(modes: NSArray); message 'acceptConnectionInBackgroundAndNotifyForModes:';
    procedure acceptConnectionInBackgroundAndNotify; message 'acceptConnectionInBackgroundAndNotify';
    procedure waitForDataInBackgroundAndNotifyForModes(modes: NSArray); message 'waitForDataInBackgroundAndNotifyForModes:';
    procedure waitForDataInBackgroundAndNotify; message 'waitForDataInBackgroundAndNotify';

    { Category: NSFileHandlePlatformSpecific }
    function initWithFileDescriptor_closeOnDealloc(fd: cint; closeopt: Boolean): id; message 'initWithFileDescriptor:closeOnDealloc:';
    function initWithFileDescriptor(fd: cint): id; message 'initWithFileDescriptor:';
    function fileDescriptor: cint; message 'fileDescriptor';
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

{$endif}
{$endif}
