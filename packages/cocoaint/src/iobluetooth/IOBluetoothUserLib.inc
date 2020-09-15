{ Parsed from IOBluetooth.framework IOBluetoothUserLib.h }

{$ifdef TYPES}
type
  IOBluetoothObjectRef = OpaqueType;
  IOBluetoothDeviceRef = OpaqueType;
  IOBluetoothL2CAPChannelRef = OpaqueType;
  IOBluetoothRFCOMMChannelRef = OpaqueType;
  IOBluetoothSDPServiceRecordRef = OpaqueType;
  IOBluetoothSDPUUIDRef = OpaqueType;
  IOBluetoothSDPDataElementRef = OpaqueType;
  IOBluetoothUserNotificationRef = OpaqueType;
  IOBluetoothObjectID = culong;
  IOBluetoothObjectIDPtr = ^IOBluetoothObjectID;
{$endif}

{$ifdef FUNCTIONS}
function IOBluetoothGetObjectIDFromArguments(argc: cint; argv: PPChar { variable size array of PChar }): IOBluetoothObjectID; cdecl; external; deprecated 'in 10.6 and later';
{$endif}

{$ifdef TYPES}
type
  IOBluetoothDeviceSearchOptions = UInt32;
  IOBluetoothDeviceSearchOptionsPtr = ^IOBluetoothDeviceSearchOptions;

const
  kSearchOptionsNone = 0;
  kSearchOptionsAlwaysStartInquiry = 1 shl 0;
  kSearchOptionsDiscardCachedResults = 1 shl 1;

type
  IOBluetoothDeviceSearchDeviceAttributes = record
    address: BluetoothDeviceAddress;
    name: BluetoothDeviceName;
    serviceClassMajor: BluetoothServiceClassMajor;
    deviceClassMajor: BluetoothDeviceClassMajor;
    deviceClassMinor: BluetoothDeviceClassMinor;
  end;
type
  IOBluetoothDeviceSearchDeviceAttributesPtr = ^IOBluetoothDeviceSearchDeviceAttributes;
type
  IOBluetoothDeviceSearchAttributes = record
    options: IOBluetoothDeviceSearchOptions;
    maxResults: IOItemCount;
    deviceAttributeCount: IOItemCount;
    attributeList: IOBluetoothDeviceSearchDeviceAttributesPtr;
  end;
type
  IOBluetoothDeviceSearchAttributesPtr = ^IOBluetoothDeviceSearchAttributes;

type
  IOBluetoothDeviceSearchTypes = UInt32;
  IOBluetoothDeviceSearchTypesPtr = ^IOBluetoothDeviceSearchTypes;

const
  kIOBluetoothDeviceSearchClassic = 1;
  kIOBluetoothDeviceSearchLE = 2;
{$endif}

{$ifdef FUNCTIONS}
procedure IOBluetoothIgnoreHIDDevice(device: IOBluetoothDeviceRef); cdecl; external;
procedure IOBluetoothRemoveIgnoredHIDDevice(device: IOBluetoothDeviceRef); cdecl; external;
{$endif}

{$ifdef FUNCTIONS}
function IOBluetoothAddServiceDict(serviceDict: CFDictionaryRef; outServiceRecord: IOBluetoothSDPServiceRecordRef): IOReturn; cdecl; external; deprecated 'in 10.9 and later';
function IOBluetoothRemoveServiceWithRecordHandle(serviceRecordHandle: BluetoothSDPServiceRecordHandle): IOReturn; cdecl; external; deprecated 'in 10.9 and later';
{$endif}

{$ifdef TYPES}
const
  kIOBluetoothUserNotificationChannelDirectionAny = 0;
  kIOBluetoothUserNotificationChannelDirectionIncoming = 1;
  kIOBluetoothUserNotificationChannelDirectionOutgoing = 2;
type
  IOBluetoothUserNotificationChannelDirection = clong;

type
  IOBluetoothUserNotificationCallback = procedure (userRefCon: pointer; inRef: IOBluetoothUserNotificationRef; objectRef: IOBluetoothObjectRef); cdecl;
{$endif}

{$ifdef FUNCTIONS}
function IOBluetoothAddSCOAudioDevice(device: IOBluetoothDeviceRef; configDict: CFDictionaryRef): IOReturn; cdecl; external; deprecated 'in 10.9 and later';
function IOBluetoothRemoveSCOAudioDevice(device: IOBluetoothDeviceRef): IOReturn; cdecl; external; deprecated 'in 10.9 and later';
{$endif}

