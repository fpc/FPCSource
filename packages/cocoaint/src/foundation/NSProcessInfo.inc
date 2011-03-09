{ Parsed from Foundation.framework NSProcessInfo.h }

{$ifdef TYPES}
{$ifndef NSPROCESSINFO_PAS_T}
{$define NSPROCESSINFO_PAS_T}

{ Constants }

const
  NSWindowsNTOperatingSystem = 1;
  NSWindows95OperatingSystem = 2;
  NSSolarisOperatingSystem = 3;
  NSHPUXOperatingSystem = 4;
  NSMACHOperatingSystem = 5;
  NSSunOSOperatingSystem = 6;
  NSOSF1OperatingSystem = 7;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPROCESSINFO_PAS_R}
{$define NSPROCESSINFO_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPROCESSINFO_PAS_F}
{$define NSPROCESSINFO_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPROCESSINFO_PAS_S}
{$define NSPROCESSINFO_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSProcessInfo = objcclass;
  NSProcessInfoPointer = ^NSProcessInfo;
  NSProcessInfoPtr = NSProcessInfoPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROCESSINFO_PAS_C}
{$define NSPROCESSINFO_PAS_C}

{ NSProcessInfo }
  NSProcessInfo = objcclass external (NSObject)
  private
    _environment: NSDictionary;
    _arguments: NSArray;
    _hostName: NSString;
    name: NSString;
    reserved: Pointer;
    
  public
    class function processInfo: NSProcessInfo; message 'processInfo';
    function environment: NSDictionary; message 'environment';
    function arguments: NSArray; message 'arguments';
    function hostName: NSString; message 'hostName';
    function processName: NSString; message 'processName';
    function processIdentifier: cint; message 'processIdentifier';
    procedure setProcessName(newName: NSString); message 'setProcessName:';
    function globallyUniqueString: NSString; message 'globallyUniqueString';
    function operatingSystem: NSUInteger; message 'operatingSystem';
    function operatingSystemName: NSString; message 'operatingSystemName';
    function operatingSystemVersionString: NSString; message 'operatingSystemVersionString';
    function processorCount: NSUInteger; message 'processorCount';
    function activeProcessorCount: NSUInteger; message 'activeProcessorCount';
    function physicalMemory: culonglong; message 'physicalMemory';
    function systemUptime: NSTimeInterval; message 'systemUptime';
    procedure disableSuddenTermination; message 'disableSuddenTermination';
    procedure enableSuddenTermination; message 'enableSuddenTermination';
  end;

{$endif}
{$endif}
