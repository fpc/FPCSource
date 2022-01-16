{ Parsed from CoreMediaIO.framework CMIOHardwareObject.h }

{$if  not (defined(__CMIOHardwareObject_h__))}

{$ifdef TYPES}
type
  CMIOObjectPropertySelector = UInt32;
  CMIOObjectPropertySelectorPtr = ^CMIOObjectPropertySelector;
  CMIOObjectPropertyScope = UInt32;
  CMIOObjectPropertyScopePtr = ^CMIOObjectPropertyScope;
  CMIOObjectPropertyElement = UInt32;
  CMIOObjectPropertyElementPtr = ^CMIOObjectPropertyElement;

type
  CMIOObjectPropertyAddress = record
    mSelector: CMIOObjectPropertySelector;
    mScope: CMIOObjectPropertyScope;
    mElement: CMIOObjectPropertyElement;
  end;
type
  CMIOObjectPropertyAddressPtr = ^CMIOObjectPropertyAddress;

const
  kCMIOObjectPropertySelectorWildcard = '****';
  kCMIOObjectPropertyScopeWildcard = '****';
  kCMIOObjectPropertyElementWildcard = $FFFFFFFF;

type
  CMIOClassID = UInt32;
  CMIOClassIDPtr = ^CMIOClassID;
  CMIOObjectID = UInt32;
  CMIOObjectIDPtr = ^CMIOObjectID;
  CMIOObjectPropertyListenerProc = function (objectID: CMIOObjectID; numberAddresses: UInt32; addresses: CMIOObjectPropertyAddressPtr { variable size array of CMIOObjectPropertyAddress }; clientData: pointer): OSStatus; cdecl;
  CMIOObjectPropertyListenerBlock = OpaqueCBlock;

const
  kCMIOObjectPropertyScopeGlobal = 'glob';
  kCMIOObjectPropertyElementMaster = 0;
  kCMIOObjectClassID = 'aobj';
  kCMIOObjectClassIDWildcard = '****';
  kCMIOObjectUnknown = 0;
const
  kCMIOObjectPropertyClass = 'clas';
  kCMIOObjectPropertyOwner = 'stdv';
  kCMIOObjectPropertyCreator = 'oplg';
  kCMIOObjectPropertyName = 'lnam';
  kCMIOObjectPropertyManufacturer = 'lmak';
  kCMIOObjectPropertyElementName = 'lchn';
  kCMIOObjectPropertyElementCategoryName = 'lccn';
  kCMIOObjectPropertyElementNumberName = 'lcnn';
  kCMIOObjectPropertyOwnedObjects = 'ownd';
  kCMIOObjectPropertyListenerAdded = 'lisa';
  kCMIOObjectPropertyListenerRemoved = 'lisr';
{$endif}

{$ifdef FUNCTIONS}
procedure CMIOObjectShow(objectID: CMIOObjectID); cdecl; external; { available in 10.7 and later }
function CMIOObjectHasProperty(objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr): Boolean; cdecl; external; { available in 10.7 and later }
function CMIOObjectIsPropertySettable(objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; isSettable: pObjCBOOL): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOObjectGetPropertyDataSize(objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; qualifierDataSize: UInt32; qualifierData: pointer; dataSize: UInt32Ptr): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOObjectGetPropertyData(objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; qualifierDataSize: UInt32; qualifierData: pointer; dataSize: UInt32; dataUsed: UInt32Ptr; data: pointer): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOObjectSetPropertyData(objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; qualifierDataSize: UInt32; qualifierData: pointer; dataSize: UInt32; data: pointer): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOObjectAddPropertyListener(objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; listener: CMIOObjectPropertyListenerProc; clientData: pointer): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOObjectRemovePropertyListener(objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; listener: CMIOObjectPropertyListenerProc; clientData: pointer): OSStatus; cdecl; external; { available in 10.7 and later }
function CMIOObjectAddPropertyListenerBlock(objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; dispatchQueue: dispatch_queue_t; listener: CMIOObjectPropertyListenerBlock): OSStatus; cdecl; external; { available in 10.8 and later }
function CMIOObjectRemovePropertyListenerBlock(objectID: CMIOObjectID; address: CMIOObjectPropertyAddressPtr; dispatchQueue: dispatch_queue_t; listener: CMIOObjectPropertyListenerBlock): OSStatus; cdecl; external; { available in 10.8 and later }
{$endif}

{$endif}
