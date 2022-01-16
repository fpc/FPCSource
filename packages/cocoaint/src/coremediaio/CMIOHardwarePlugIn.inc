{ Parsed from CoreMediaIO.framework CMIOHardwarePlugIn.h }

{$if  not (defined(__CMIOHardwarePlugIn_h__))}

{$ifdef TYPES}
type
  CMIOHardwarePlugInRef = ^CMIOHardwarePlugInInterfacePtr;
  CMIOHardwarePlugInInterfacePtr = ^CMIOHardwarePlugInInterface;
  CMIOHardwarePlugInInterface = record
    _reserved: pointer;
    QueryInterface: function (self_: pointer; uuid: CFUUIDBytes; interface_: pointer): SInt32; cdecl;
    AddRef: function (self_: pointer): UInt32; cdecl;
    Release: function (self_: pointer): UInt32; cdecl;
    Initialize: function (self_: CMIOHardwarePlugInRef): OSStatus; cdecl;
    InitializeWithObjectID: function (self_: CMIOHardwarePlugInRef; objectID: CMIOObjectID): OSStatus; cdecl;
    Teardown: function (self_: CMIOHardwarePlugInRef): OSStatus; cdecl;
    ObjectShow: procedure (self_: CMIOHardwarePlugInRef; objectID: CMIOObjectID); cdecl;
    ObjectHasProperty: function (self_: CMIOHardwarePlugInRef; objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr): Boolean; cdecl;
    ObjectIsPropertySettable: function (self_: CMIOHardwarePlugInRef; objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; isSettable: pObjCBOOL): OSStatus; cdecl;
    ObjectGetPropertyDataSize: function (self_: CMIOHardwarePlugInRef; objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; qualifierDataSize: UInt32; qualifierData: pointer; dataSize: UInt32Ptr): OSStatus; cdecl;
    ObjectGetPropertyData: function (self_: CMIOHardwarePlugInRef; objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; qualifierDataSize: UInt32; qualifierData: pointer; dataSize: UInt32; dataUsed: UInt32Ptr; data: pointer): OSStatus; cdecl;
    ObjectSetPropertyData: function (self_: CMIOHardwarePlugInRef; objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; qualifierDataSize: UInt32; qualifierData: pointer; dataSize: UInt32; data: pointer): OSStatus; cdecl;
    DeviceSuspend: function (self_: CMIOHardwarePlugInRef; device: CMIODeviceID): OSStatus; cdecl;
    DeviceResume: function (self_: CMIOHardwarePlugInRef; device: CMIODeviceID): OSStatus; cdecl;
    DeviceStartStream: function (self_: CMIOHardwarePlugInRef; device: CMIODeviceID; stream: CMIOStreamID): OSStatus; cdecl;
    DeviceStopStream: function (self_: CMIOHardwarePlugInRef; device: CMIODeviceID; stream: CMIOStreamID): OSStatus; cdecl;
    DeviceProcessAVCCommand: function (self_: CMIOHardwarePlugInRef; device: CMIODeviceID; ioAVCCommand: CMIODeviceAVCCommandPtr): OSStatus; cdecl;
    DeviceProcessRS422Command: function (self_: CMIOHardwarePlugInRef; device: CMIODeviceID; ioRS422Command: CMIODeviceRS422CommandPtr): OSStatus; cdecl;
    StreamCopyBufferQueue: function (self_: CMIOHardwarePlugInRef; stream: CMIOStreamID; queueAlteredProc: CMIODeviceStreamQueueAlteredProc; queueAlteredRefCon: pointer; queue: CMSimpleQueueRef): OSStatus; cdecl;
    StreamDeckPlay: function (self_: CMIOHardwarePlugInRef; stream: CMIOStreamID): OSStatus; cdecl;
    StreamDeckStop: function (self_: CMIOHardwarePlugInRef; stream: CMIOStreamID): OSStatus; cdecl;
    StreamDeckJog: function (self_: CMIOHardwarePlugInRef; stream: CMIOStreamID; speed: SInt32): OSStatus; cdecl;
    StreamDeckCueTo: function (self_: CMIOHardwarePlugInRef; stream: CMIOStreamID; frameNumber: Float64; playOnCue: Boolean): OSStatus; cdecl;
  end;
{$endif}

{$ifdef FUNCTIONS}
function CMIOObjectCreate(owningPlugIn: CMIOHardwarePlugInRef; owningObjectID: CMIOObjectID; classID: CMIOClassID; objectID: CMIOObjectIDPtr): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOObjectsPublishedAndDied(owningPlugIn: CMIOHardwarePlugInRef; owningObjectID: CMIOObjectID; numberPublishedCMIOObjects: UInt32; publishedCMIOObjects: CMIOObjectIDPtr { variable size array of CMIOObjectID }; numberDeadCMIOObjects: UInt32; deadCMIOObjects: CMIOObjectIDPtr { variable size array of CMIOObjectID }): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOObjectPropertiesChanged(owningPlugIn: CMIOHardwarePlugInRef; objectID: CMIOObjectID; numberAddresses: UInt32; addresses: CMIOObjectPropertyAddressPtr { variable size array of CMIOObjectPropertyAddress }): OSStatus; cdecl; external; { available in 10.7 and later }
{$endif}

{$endif}
