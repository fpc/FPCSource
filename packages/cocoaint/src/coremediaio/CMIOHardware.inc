{ Parsed from CoreMediaIO.framework CMIOHardware.h }

{$if  not (defined(__CMIOHardware_h__))}

{$ifdef TYPES}
const
  kCMIOHardwareNoError = 0;
  kCMIOHardwareNotStoppedError = 'run ';
  kCMIOHardwareNotRunningError = 'stop';
  kCMIOHardwareUnspecifiedError = 'what';
  kCMIOHardwareUnknownPropertyError = 'who?';
  kCMIOHardwareBadPropertySizeError = '!siz';
  kCMIOHardwareIllegalOperationError = 'nope';
  kCMIOHardwareBadObjectError = '!obj';
  kCMIOHardwareBadDeviceError = '!dev';
  kCMIOHardwareBadStreamError = '!str';
  kCMIOHardwareUnsupportedOperationError = 'unop';
  kCMIOHardwareSuspendedBySystemError = 'deny';
  kCMIODeviceUnsupportedFormatError = '!dat';
  kCMIODevicePermissionsError = '!hog';
const
  kCMIOPlugInClassID = 'aplg';
const
  kCMIOPlugInPropertyBundleID = 'piid';
{$endif}

{$endif}
