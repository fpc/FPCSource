{ Parsed from IOBluetooth.framework IOBluetoothUtilities.h }

{$ifdef __OBJC__}
{$ifdef FUNCTIONS}
function IOBluetoothNSStringToDeviceAddress(inNameString: NSString; outDeviceAddress: BluetoothDeviceAddressPtr): IOReturn; cdecl; external;
function IOBluetoothNSStringFromDeviceAddress(deviceAddress: BluetoothDeviceAddressPtr): NSString; cdecl; external;
function IOBluetoothNSStringFromDeviceAddressColon(deviceAddress: BluetoothDeviceAddressPtr): NSString; cdecl; external;
function IOBluetoothIsFileAppleDesignatedPIMData(inFileName: NSString): Boolean; cdecl; external;
function IOBluetoothGetUniqueFileNameAndPath(inName: NSString; inPath: NSString): NSString; cdecl; external;
{$endif}

{$else}
{$ifdef FUNCTIONS}
function IOBluetoothCFStringToDeviceAddress(inNameString: CFStringRef; outDeviceAddress: BluetoothDeviceAddressPtr): IOReturn; cdecl; external;
function IOBluetoothCFStringFromDeviceAddress(deviceAddress: BluetoothDeviceAddressPtr): CFStringRef; cdecl; external;
function IOBluetoothIsFileAppleDesignatedPIMDataAtCFStringPath(inFileName: CFStringRef): Boolean; cdecl; external;
function IOBluetoothGetUniqueFileNameAndWithCFStringPath(inName: CFStringRef; inPath: CFStringRef): CFStringRef; cdecl; external;
function IOBluetoothLaunchHandsFreeAgent(deviceAddressString: CFStringRef): Boolean; cdecl; external;
{$endif}

{$endif}
{$ifdef FUNCTIONS}
function IOBluetoothPackData(ioBuffer: pointer; inFormat: PChar): clong; cdecl; external;
function IOBluetoothPackDataList(ioBuffer: pointer; inFormat: PChar; inArgs: va_list): clong; cdecl; external;
function IOBluetoothUnpackData(inBufferSize: ByteCount; inBuffer: pointer; inFormat: PChar): clong; cdecl; external;
function IOBluetoothUnpackDataList(inBufferSize: ByteCount; inBuffer: pointer; inFormat: PChar; inArgs: va_list): clong; cdecl; external;
function IOBluetoothFindNumberOfRegistryEntriesOfClassName(deviceType: PChar): clong; cdecl; external;
{$endif}

