{ Parsed from Foundation.framework NSProcessInfo.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPROCESSINFO_PAS_H}
{$define NSPROCESSINFO_PAS_H}
type
  NSProcessInfoPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSPROCESSINFO_PAS_T}
{$define NSPROCESSINFO_PAS_T}

{ Constants }

const
  NSWindowsNTOperatingSystem = 1;
  NSWindows95OperatingSystem = 0;
  NSSolarisOperatingSystem = 1;
  NSHPUXOperatingSystem = 2;
  NSMACHOperatingSystem = 3;
  NSSunOSOperatingSystem = 4;
  NSOSF1OperatingSystem = 5;

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
{$ifndef NSPROCESSINFO_PAS_T}
{$define NSPROCESSINFO_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSProcessInfo = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROCESSINFO_PAS_C}
{$define NSPROCESSINFO_PAS_C}

{ NSProcessInfo }
  NSProcessInfo = objcclass(NSObject)
  private
    _environment: NSDictionary;
    _arguments: NSArray;
    _hostName: NSString;
    _name: NSString;
    _reserved: Pointer;
    
  public
    class function alloc: NSProcessInfo; message 'alloc';

    class function processInfo: NSProcessInfo; message 'processInfo';
    function environment: NSDictionary; message 'environment';
    function arguments: NSArray; message 'arguments';
    function hostName: NSString; message 'hostName';
    function processName: NSString; message 'processName';
    function processIdentifier: cint; message 'processIdentifier';
    procedure setProcessName(newName: NSString); message 'setProcessName:';
    function globallyUniqueString: NSString; message 'globallyUniqueString';
    function operatingSystem: culong; message 'operatingSystem';
    function operatingSystemName: NSString; message 'operatingSystemName';
    function operatingSystemVersionString: NSString; message 'operatingSystemVersionString';
    function processorCount: culong; message 'processorCount';
    function activeProcessorCount: culong; message 'activeProcessorCount';
    function physicalMemory: culonglong; message 'physicalMemory';
  end; external;

{$endif}
{$endif}
