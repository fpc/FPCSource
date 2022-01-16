{ Parsed from CoreMedia.framework CMMetadata.h }

{$ifndef CMMETADATA_H}

{$ifdef TYPES}
const
  kCMMetadataIdentifierError_AllocationFailed = -16300;
  kCMMetadataIdentifierError_RequiredParameterMissing = -16301;
  kCMMetadataIdentifierError_BadKey = -16302;
  kCMMetadataIdentifierError_BadKeyLength = -16303;
  kCMMetadataIdentifierError_BadKeyType = -16304;
  kCMMetadataIdentifierError_BadNumberKey = -16305;
  kCMMetadataIdentifierError_BadKeySpace = -16306;
  kCMMetadataIdentifierError_BadIdentifier = -16307;
  kCMMetadataIdentifierError_NoKeyValueAvailable = -16308;
const
  kCMMetadataDataTypeRegistryError_AllocationFailed = -16310;
  kCMMetadataDataTypeRegistryError_RequiredParameterMissing = -16311;
  kCMMetadataDataTypeRegistryError_BadDataTypeIdentifier = -16312;
  kCMMetadataDataTypeRegistryError_DataTypeAlreadyRegistered = -16313;
  kCMMetadataDataTypeRegistryError_RequiresConformingBaseType = -16314;
  kCMMetadataDataTypeRegistryError_MultipleConformingBaseTypes = -16315;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMMetadataKeySpace_QuickTimeUserData: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataKeySpace_ISOUserData: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataKeySpace_QuickTimeMetadata: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataKeySpace_iTunes: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataKeySpace_ID3: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataKeySpace_Icy: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataIdentifier_QuickTimeMetadataLocation_ISO6709: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataIdentifier_QuickTimeMetadataDirection_Facing: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataIdentifier_QuickTimeMetadataPreferredAffineTransform: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMMetadataCreateIdentifierForKeyAndKeySpace(allocator: CFAllocatorRef; key: CFTypeRef; keySpace: CFStringRef; identifierOut: CFStringRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataCreateKeyFromIdentifier(allocator: CFAllocatorRef; identifier: CFStringRef; keyOut: CFTypeRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataCreateKeyFromIdentifierAsCFData(allocator: CFAllocatorRef; identifier: CFStringRef; keyOut: CFDataRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataCreateKeySpaceFromIdentifier(allocator: CFAllocatorRef; identifier: CFStringRef; keySpaceOut: CFStringRefPtr): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMMetadataBaseDataType_RawData: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_UTF8: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_UTF16: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_GIF: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_JPEG: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_PNG: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_BMP: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_Float32: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_Float64: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_SInt8: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_SInt16: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_SInt32: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_SInt64: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_UInt8: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_UInt16: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_UInt32: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_UInt64: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_PointF32: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_DimensionsF32: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_RectF32: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataBaseDataType_AffineTransformF64: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataDataType_QuickTimeMetadataLocation_ISO6709: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
  kCMMetadataDataType_QuickTimeMetadataDirection: CFStringRef { available starting in __MAC_10_10,__IPHONE_8_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMMetadataDataTypeRegistryRegisterDataType(dataType: CFStringRef; description: CFStringRef; conformingDataTypes: CFArrayRef): OSStatus; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataDataTypeRegistryDataTypeIsRegistered(dataType: CFStringRef): Boolean; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataDataTypeRegistryGetDataTypeDescription(dataType: CFStringRef): CFStringRef; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataDataTypeRegistryGetConformingDataTypes(dataType: CFStringRef): CFArrayRef; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataDataTypeRegistryDataTypeConformsToDataType(dataType: CFStringRef; conformsToDataType: CFStringRef): Boolean; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataDataTypeRegistryGetBaseDataTypes: CFArrayRef; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataDataTypeRegistryDataTypeIsBaseDataType(dataType: CFStringRef): Boolean; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
function CMMetadataDataTypeRegistryGetBaseDataTypeForConformingDataType(dataType: CFStringRef): CFStringRef; cdecl; external; { available starting in __MAC_10_10,__IPHONE_8_0 }
{$endif}

{$endif}
