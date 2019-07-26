{ Parsed from CoreMediaIO.framework CMIOHardwareSystem.h }

{$if  not (defined(__CMIOHardwareSystem_h__))}

{$ifdef TYPES}
type
  CMIOHardwarePropertyID = CMIOObjectPropertySelector;
  CMIOHardwarePropertyIDPtr = ^CMIOHardwarePropertyID;

const
  kCMIOSystemObjectClassID = 'asys';
  kCMIOObjectSystemObject = 1;
const
  kCMIOHardwarePropertyProcessIsMaster = 'mast';
  kCMIOHardwarePropertyIsInitingOrExiting = 'inot';
  kCMIOHardwarePropertyDevices = 'dev#';
  kCMIOHardwarePropertyDefaultInputDevice = 'dIn ';
  kCMIOHardwarePropertyDefaultOutputDevice = 'dOut';
  kCMIOHardwarePropertyDeviceForUID = 'duid';
  kCMIOHardwarePropertySleepingIsAllowed = 'slep';
  kCMIOHardwarePropertyUnloadingIsAllowed = 'unld';
  kCMIOHardwarePropertyPlugInForBundleID = 'pibi';
  kCMIOHardwarePropertyUserSessionIsActiveOrHeadless = 'user';
  kCMIOHardwarePropertySuspendedBySystem = 'sbys';
  kCMIOHardwarePropertyAllowScreenCaptureDevices = 'yes ';
{$endif}

{$endif}
