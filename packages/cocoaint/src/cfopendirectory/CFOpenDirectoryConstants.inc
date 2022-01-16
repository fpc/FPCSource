{ Parsed from CFOpenDirectory.framework CFOpenDirectoryConstants.h }

{$if  not (defined(__ODCONSTANTS_H))}
{$ifdef EXTERNAL_SYMBOLS}
var
  kODSessionProxyAddress: CFStringRef { available starting in __MAC_10_6, __IPHONE_NA }; cvar; external;
  kODSessionProxyPort: CFStringRef { available starting in __MAC_10_6, __IPHONE_NA }; cvar; external;
  kODSessionProxyUsername: CFStringRef { available starting in __MAC_10_6, __IPHONE_NA }; cvar; external;
  kODSessionProxyPassword: CFStringRef { available starting in __MAC_10_6, __IPHONE_NA }; cvar; external;
  kODModuleConfigOptionQueryTimeout: CFStringRef { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODModuleConfigOptionConnectionSetupTimeout: CFStringRef { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODModuleConfigOptionConnectionIdleDisconnect: CFStringRef { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODModuleConfigOptionPacketSigning: CFStringRef { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODModuleConfigOptionPacketEncryption: CFStringRef { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODModuleConfigOptionManInTheMiddle: CFStringRef { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
{$endif}

{$ifdef TYPES}
const
  kODNodeTypeAuthentication = $2201;
  kODNodeTypeContacts = $2204;
  kODNodeTypeNetwork = $2205;
  kODNodeTypeLocalNodes = $2200;
  kODNodeTypeConfigure = $2202;

type
  ODNodeType = cuint32;
  ODNodeTypePtr = ^ODNodeType;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kODNodeOptionsQuerySkippedSubnode: CFStringRef { available starting in __MAC_10_8, __IPHONE_NA }; cvar; external;
{$endif}

{$ifdef TYPES}
const
  kODMatchAny = $0001;
  kODMatchEqualTo = $2001;
  kODMatchBeginsWith = $2002;
  kODMatchContains = $2004;
  kODMatchEndsWith = $2003;
  kODMatchInsensitiveEqualTo = $2101 { available in __MAC_10_6, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "case matching is defined by attribute schema" };
  kODMatchInsensitiveBeginsWith = $2102 { available in __MAC_10_6, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "case matching is defined by attribute schema" };
  kODMatchInsensitiveContains = $2104 { available in __MAC_10_6, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "case matching is defined by attribute schema" };
  kODMatchInsensitiveEndsWith = $2103 { available in __MAC_10_6, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "case matching is defined by attribute schema" };
  kODMatchGreaterThan = $2006;
  kODMatchLessThan = $2007;

type
  ODMatchType = cuint32;
  ODMatchTypePtr = ^ODMatchType;
{$endif}

{$ifdef __OBJC__}
{$ifdef TYPES}
type
  ODRecordType = ^NSString;
{$endif}

{$else}
{$ifdef TYPES}
type
  ODRecordType = CFStringRef;
  ODRecordTypePtr = ^ODRecordType;
{$endif}

{$endif}

{$ifdef __OBJC__}
{$ifdef TYPES}
type
  ODAttributeType = ^NSString;
{$endif}

{$else}
{$ifdef TYPES}
type
  ODAttributeType = CFStringRef;
  ODAttributeTypePtr = ^ODAttributeType;
{$endif}

{$endif}

{$ifdef __OBJC__}
{$ifdef TYPES}
type
  ODAuthenticationType = ^NSString;
{$endif}

{$else}
{$ifdef TYPES}
type
  ODAuthenticationType = CFStringRef;
  ODAuthenticationTypePtr = ^ODAuthenticationType;
{$endif}

{$endif}

{$ifdef __OBJC_}
{$ifdef TYPES}
type
  ODPolicyType = ^NSString;
{$endif}

{$else}
{$ifdef TYPES}
type
  ODPolicyType = CFStringRef;
  ODPolicyTypePtr = ^ODPolicyType;
{$endif}

{$endif}
{$ifdef TYPES}
type
  _ODRecordType = ODRecordType;
  _ODRecordTypePtr = ^_ODRecordType;
  _ODAttributeType = ODAttributeType;
  _ODAttributeTypePtr = ^_ODAttributeType;
  _ODAuthenticationType = ODAuthenticationType;
  _ODAuthenticationTypePtr = ^_ODAuthenticationType;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kODRecordTypeAttributeTypes: ODRecordType; cvar; external;
  kODRecordTypeAFPServer: ODRecordType; cvar; external;
  kODRecordTypeAliases: ODRecordType; cvar; external;
  kODRecordTypeAugments: ODRecordType; cvar; external;
  kODRecordTypeAutomount: ODRecordType; cvar; external;
  kODRecordTypeAutomountMap: ODRecordType; cvar; external;
  kODRecordTypeAutoServerSetup: ODRecordType; cvar; external;
  kODRecordTypeBootp: ODRecordType; cvar; external;
  kODRecordTypeCertificateAuthorities: ODRecordType; cvar; external;
  kODRecordTypeComputerLists: ODRecordType; cvar; external;
  kODRecordTypeComputerGroups: ODRecordType; cvar; external;
  kODRecordTypeComputers: ODRecordType; cvar; external;
  kODRecordTypeConfiguration: ODRecordType; cvar; external;
  kODRecordTypeEthernets: ODRecordType; cvar; external;
  kODRecordTypeFileMakerServers: ODRecordType; cvar; external;
  kODRecordTypeFTPServer: ODRecordType; cvar; external;
  kODRecordTypeGroups: ODRecordType; cvar; external;
  kODRecordTypeHostServices: ODRecordType; cvar; external;
  kODRecordTypeHosts: ODRecordType; cvar; external;
  kODRecordTypeLDAPServer: ODRecordType; cvar; external;
  kODRecordTypeLocations: ODRecordType; cvar; external;
  kODRecordTypeMounts: ODRecordType; cvar; external;
  kODRecordTypeNFS: ODRecordType; cvar; external;
  kODRecordTypeNetDomains: ODRecordType; cvar; external;
  kODRecordTypeNetGroups: ODRecordType; cvar; external;
  kODRecordTypeNetworks: ODRecordType; cvar; external;
  kODRecordTypePeople: ODRecordType; cvar; external;
  kODRecordTypePresetComputers: ODRecordType; cvar; external;
  kODRecordTypePresetComputerGroups: ODRecordType; cvar; external;
  kODRecordTypePresetComputerLists: ODRecordType; cvar; external;
  kODRecordTypePresetGroups: ODRecordType; cvar; external;
  kODRecordTypePresetUsers: ODRecordType; cvar; external;
  kODRecordTypePrintService: ODRecordType; cvar; external;
  kODRecordTypePrintServiceUser: ODRecordType; cvar; external;
  kODRecordTypePrinters: ODRecordType; cvar; external;
  kODRecordTypeProtocols: ODRecordType; cvar; external;
  kODRecordTypeQTSServer: ODRecordType; cvar; external;
  kODRecordTypeQueryInformation: ODRecordType; cvar; external;
  kODRecordTypeRecordTypes: ODRecordType; cvar; external;
  kODRecordTypeResources: ODRecordType; cvar; external;
  kODRecordTypeRPC: ODRecordType; cvar; external;
  kODRecordTypeSMBServer: ODRecordType; cvar; external;
  kODRecordTypeServer: ODRecordType; cvar; external;
  kODRecordTypeServices: ODRecordType; cvar; external;
  kODRecordTypeSharePoints: ODRecordType; cvar; external;
  kODRecordTypeUsers: ODRecordType; cvar; external;
  kODRecordTypeWebServer: ODRecordType; cvar; external;
  kODAttributeTypeAllAttributes: ODAttributeType; cvar; external;
  kODAttributeTypeStandardOnly: ODAttributeType; cvar; external;
  kODAttributeTypeNativeOnly: ODAttributeType; cvar; external;
  kODAttributeTypeMetaAmbiguousName: ODAttributeType; cvar; external;
  kODAttributeTypeMetaAugmentedAttributes: ODAttributeType; cvar; external;
  kODAttributeTypeMetaRecordName: ODAttributeType; cvar; external;
  kODAttributeTypeAdminLimits: ODAttributeType; cvar; external;
  kODAttributeTypeAltSecurityIdentities: ODAttributeType { available starting in __MAC_10_6, __IPHONE_NA }; cvar; external;
  kODAttributeTypeAuthenticationHint: ODAttributeType; cvar; external;
  kODAttributeTypeAllTypes: ODAttributeType; cvar; external;
  kODAttributeTypeAuthorityRevocationList: ODAttributeType; cvar; external;
  kODAttributeTypeBirthday: ODAttributeType; cvar; external;
  kODAttributeTypeCACertificate: ODAttributeType; cvar; external;
  kODAttributeTypeCapacity: ODAttributeType; cvar; external;
  kODAttributeTypeCertificateRevocationList: ODAttributeType; cvar; external;
  kODAttributeTypeComment: ODAttributeType; cvar; external;
  kODAttributeTypeContactGUID: ODAttributeType; cvar; external;
  kODAttributeTypeContactPerson: ODAttributeType; cvar; external;
  kODAttributeTypeCreationTimestamp: ODAttributeType; cvar; external;
  kODAttributeTypeCrossCertificatePair: ODAttributeType; cvar; external;
  kODAttributeTypeDataStamp: ODAttributeType; cvar; external;
  kODAttributeTypeFullName: ODAttributeType; cvar; external;
  kODAttributeTypeDNSDomain: ODAttributeType; cvar; external;
  kODAttributeTypeDNSNameServer: ODAttributeType; cvar; external;
  kODAttributeTypeENetAddress: ODAttributeType; cvar; external;
  kODAttributeTypeExpire: ODAttributeType; cvar; external;
  kODAttributeTypeFirstName: ODAttributeType; cvar; external;
  kODAttributeTypeGUID: ODAttributeType; cvar; external;
  kODAttributeTypeHardwareUUID: ODAttributeType { available starting in __MAC_10_6, __IPHONE_NA }; cvar; external;
  kODAttributeTypeHomeDirectoryQuota: ODAttributeType; cvar; external;
  kODAttributeTypeHomeDirectorySoftQuota: ODAttributeType; cvar; external;
  kODAttributeTypeHomeLocOwner: ODAttributeType; cvar; external;
  kODAttributeTypeInternetAlias: ODAttributeType; cvar; external;
  kODAttributeTypeKDCConfigData: ODAttributeType; cvar; external;
  kODAttributeTypeKerberosServices: ODAttributeType { available starting in __MAC_10_7, __IPHONE_NA }; cvar; external;
  kODAttributeTypeLastName: ODAttributeType; cvar; external;
  kODAttributeTypeLDAPSearchBaseSuffix: ODAttributeType; cvar; external;
  kODAttributeTypeLocation: ODAttributeType; cvar; external;
  kODAttributeTypeMapGUID: ODAttributeType; cvar; external;
  kODAttributeTypeMCXFlags: ODAttributeType; cvar; external;
  kODAttributeTypeMCXSettings: ODAttributeType; cvar; external;
  kODAttributeTypeMailAttribute: ODAttributeType; cvar; external;
  kODAttributeTypeMetaAutomountMap: ODAttributeType; cvar; external;
  kODAttributeTypeMiddleName: ODAttributeType; cvar; external;
  kODAttributeTypeModificationTimestamp: ODAttributeType; cvar; external;
  kODAttributeTypeNFSHomeDirectory: ODAttributeType; cvar; external;
  kODAttributeTypeNote: ODAttributeType; cvar; external;
  kODAttributeTypeOperatingSystem: ODAttributeType { available starting in __MAC_10_6, __IPHONE_NA }; cvar; external;
  kODAttributeTypeOperatingSystemVersion: ODAttributeType { available starting in __MAC_10_6, __IPHONE_NA }; cvar; external;
  kODAttributeTypeOwner: ODAttributeType; cvar; external;
  kODAttributeTypeOwnerGUID: ODAttributeType; cvar; external;
  kODAttributeTypePassword: ODAttributeType; cvar; external;
  kODAttributeTypePasswordPlus: ODAttributeType; cvar; external;
  kODAttributeTypePasswordPolicyOptions: ODAttributeType; cvar; external;
  kODAttributeTypePasswordServerList: ODAttributeType; cvar; external;
  kODAttributeTypePasswordServerLocation: ODAttributeType; cvar; external;
  kODAttributeTypePicture: ODAttributeType; cvar; external;
  kODAttributeTypePort: ODAttributeType; cvar; external;
  kODAttributeTypePresetUserIsAdmin: ODAttributeType; cvar; external;
  kODAttributeTypePrimaryComputerGUID: ODAttributeType; cvar; external;
  kODAttributeTypePrimaryComputerList: ODAttributeType; cvar; external;
  kODAttributeTypePrimaryGroupID: ODAttributeType; cvar; external;
  kODAttributeTypePrinter1284DeviceID: ODAttributeType; cvar; external;
  kODAttributeTypePrinterLPRHost: ODAttributeType; cvar; external;
  kODAttributeTypePrinterLPRQueue: ODAttributeType; cvar; external;
  kODAttributeTypePrinterMakeAndModel: ODAttributeType; cvar; external;
  kODAttributeTypePrinterType: ODAttributeType; cvar; external;
  kODAttributeTypePrinterURI: ODAttributeType; cvar; external;
  kODAttributeTypePrinterXRISupported: ODAttributeType; cvar; external;
  kODAttributeTypePrintServiceInfoText: ODAttributeType; cvar; external;
  kODAttributeTypePrintServiceInfoXML: ODAttributeType; cvar; external;
  kODAttributeTypePrintServiceUserData: ODAttributeType; cvar; external;
  kODAttributeTypeRealUserID: ODAttributeType; cvar; external;
  kODAttributeTypeRelativeDNPrefix: ODAttributeType; cvar; external;
  kODAttributeTypeSMBAcctFlags: ODAttributeType; cvar; external;
  kODAttributeTypeSMBGroupRID: ODAttributeType; cvar; external;
  kODAttributeTypeSMBHome: ODAttributeType; cvar; external;
  kODAttributeTypeSMBHomeDrive: ODAttributeType; cvar; external;
  kODAttributeTypeSMBKickoffTime: ODAttributeType; cvar; external;
  kODAttributeTypeSMBLogoffTime: ODAttributeType; cvar; external;
  kODAttributeTypeSMBLogonTime: ODAttributeType; cvar; external;
  kODAttributeTypeSMBPrimaryGroupSID: ODAttributeType; cvar; external;
  kODAttributeTypeSMBPWDLastSet: ODAttributeType; cvar; external;
  kODAttributeTypeSMBProfilePath: ODAttributeType; cvar; external;
  kODAttributeTypeSMBRID: ODAttributeType; cvar; external;
  kODAttributeTypeSMBScriptPath: ODAttributeType; cvar; external;
  kODAttributeTypeSMBSID: ODAttributeType; cvar; external;
  kODAttributeTypeSMBUserWorkstations: ODAttributeType; cvar; external;
  kODAttributeTypeServiceType: ODAttributeType; cvar; external;
  kODAttributeTypeSetupAdvertising: ODAttributeType; cvar; external;
  kODAttributeTypeSetupAutoRegister: ODAttributeType; cvar; external;
  kODAttributeTypeSetupLocation: ODAttributeType; cvar; external;
  kODAttributeTypeSetupOccupation: ODAttributeType; cvar; external;
  kODAttributeTypeTimeToLive: ODAttributeType; cvar; external;
  kODAttributeTypeTrustInformation: ODAttributeType; cvar; external;
  kODAttributeTypeUniqueID: ODAttributeType; cvar; external;
  kODAttributeTypeUserCertificate: ODAttributeType; cvar; external;
  kODAttributeTypeUserPKCS12Data: ODAttributeType; cvar; external;
  kODAttributeTypeUserShell: ODAttributeType; cvar; external;
  kODAttributeTypeUserSMIMECertificate: ODAttributeType; cvar; external;
  kODAttributeTypeVFSDumpFreq: ODAttributeType; cvar; external;
  kODAttributeTypeVFSLinkDir: ODAttributeType; cvar; external;
  kODAttributeTypeVFSPassNo: ODAttributeType; cvar; external;
  kODAttributeTypeVFSType: ODAttributeType; cvar; external;
  kODAttributeTypeWeblogURI: ODAttributeType; cvar; external;
  kODAttributeTypeXMLPlist: ODAttributeType; cvar; external;
  kODAttributeTypeProtocolNumber: ODAttributeType; cvar; external;
  kODAttributeTypeRPCNumber: ODAttributeType; cvar; external;
  kODAttributeTypeNetworkNumber: ODAttributeType; cvar; external;
  kODAttributeTypeAccessControlEntry: ODAttributeType; cvar; external;
  kODAttributeTypeAddressLine1: ODAttributeType; cvar; external;
  kODAttributeTypeAddressLine2: ODAttributeType; cvar; external;
  kODAttributeTypeAddressLine3: ODAttributeType; cvar; external;
  kODAttributeTypeAreaCode: ODAttributeType; cvar; external;
  kODAttributeTypeAuthenticationAuthority: ODAttributeType; cvar; external;
  kODAttributeTypeAutomountInformation: ODAttributeType; cvar; external;
  kODAttributeTypeBootParams: ODAttributeType; cvar; external;
  kODAttributeTypeBuilding: ODAttributeType; cvar; external;
  kODAttributeTypeServicesLocator: ODAttributeType; cvar; external;
  kODAttributeTypeCity: ODAttributeType; cvar; external;
  kODAttributeTypeCompany: ODAttributeType; cvar; external;
  kODAttributeTypeComputers: ODAttributeType; cvar; external;
  kODAttributeTypeCountry: ODAttributeType; cvar; external;
  kODAttributeTypeDepartment: ODAttributeType; cvar; external;
  kODAttributeTypeDNSName: ODAttributeType; cvar; external;
  kODAttributeTypeEMailAddress: ODAttributeType; cvar; external;
  kODAttributeTypeEMailContacts: ODAttributeType; cvar; external;
  kODAttributeTypeFaxNumber: ODAttributeType; cvar; external;
  kODAttributeTypeGroup: ODAttributeType; cvar; external;
  kODAttributeTypeGroupMembers: ODAttributeType; cvar; external;
  kODAttributeTypeGroupMembership: ODAttributeType; cvar; external;
  kODAttributeTypeGroupServices: ODAttributeType; cvar; external;
  kODAttributeTypeHomePhoneNumber: ODAttributeType; cvar; external;
  kODAttributeTypeHTML: ODAttributeType; cvar; external;
  kODAttributeTypeHomeDirectory: ODAttributeType; cvar; external;
  kODAttributeTypeIMHandle: ODAttributeType; cvar; external;
  kODAttributeTypeIPAddress: ODAttributeType; cvar; external;
  kODAttributeTypeIPAddressAndENetAddress: ODAttributeType; cvar; external;
  kODAttributeTypeIPv6Address: ODAttributeType; cvar; external;
  kODAttributeTypeJPEGPhoto: ODAttributeType; cvar; external;
  kODAttributeTypeJobTitle: ODAttributeType; cvar; external;
  kODAttributeTypeKDCAuthKey: ODAttributeType; cvar; external;
  kODAttributeTypeKeywords: ODAttributeType; cvar; external;
  kODAttributeTypeLDAPReadReplicas: ODAttributeType; cvar; external;
  kODAttributeTypeLDAPWriteReplicas: ODAttributeType; cvar; external;
  kODAttributeTypeMapCoordinates: ODAttributeType; cvar; external;
  kODAttributeTypeMapURI: ODAttributeType; cvar; external;
  kODAttributeTypeMIME: ODAttributeType; cvar; external;
  kODAttributeTypeMobileNumber: ODAttributeType; cvar; external;
  kODAttributeTypeNestedGroups: ODAttributeType; cvar; external;
  kODAttributeTypeNetGroups: ODAttributeType; cvar; external;
  kODAttributeTypeNickName: ODAttributeType; cvar; external;
  kODAttributeTypeOrganizationInfo: ODAttributeType; cvar; external;
  kODAttributeTypeOrganizationName: ODAttributeType; cvar; external;
  kODAttributeTypePagerNumber: ODAttributeType; cvar; external;
  kODAttributeTypePhoneContacts: ODAttributeType; cvar; external;
  kODAttributeTypePhoneNumber: ODAttributeType; cvar; external;
  kODAttributeTypePGPPublicKey: ODAttributeType; cvar; external;
  kODAttributeTypePostalAddress: ODAttributeType; cvar; external;
  kODAttributeTypePostalAddressContacts: ODAttributeType; cvar; external;
  kODAttributeTypePostalCode: ODAttributeType; cvar; external;
  kODAttributeTypeNamePrefix: ODAttributeType; cvar; external;
  kODAttributeTypeProfiles: ODAttributeType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODAttributeTypeProfilesTimestamp: ODAttributeType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODAttributeTypeProtocols: ODAttributeType; cvar; external;
  kODAttributeTypeRecordName: ODAttributeType; cvar; external;
  kODAttributeTypeRelationships: ODAttributeType; cvar; external;
  kODAttributeTypeResourceInfo: ODAttributeType; cvar; external;
  kODAttributeTypeResourceType: ODAttributeType; cvar; external;
  kODAttributeTypeState: ODAttributeType; cvar; external;
  kODAttributeTypeStreet: ODAttributeType; cvar; external;
  kODAttributeTypeNameSuffix: ODAttributeType; cvar; external;
  kODAttributeTypeURL: ODAttributeType; cvar; external;
  kODAttributeTypeVFSOpts: ODAttributeType; cvar; external;
  kODAttributeTypeAlias: ODAttributeType; cvar; external;
  kODAttributeTypeAuthCredential: ODAttributeType; cvar; external;
  kODAttributeTypeCopyTimestamp: ODAttributeType; cvar; external;
  kODAttributeTypeDateRecordCreated: ODAttributeType; cvar; external;
  kODAttributeTypeKerberosRealm: ODAttributeType; cvar; external;
  kODAttributeTypeNTDomainComputerAccount: ODAttributeType; cvar; external;
  kODAttributeTypeOriginalHomeDirectory: ODAttributeType; cvar; external;
  kODAttributeTypeOriginalNFSHomeDirectory: ODAttributeType; cvar; external;
  kODAttributeTypeOriginalNodeName: ODAttributeType; cvar; external;
  kODAttributeTypePrimaryNTDomain: ODAttributeType; cvar; external;
  kODAttributeTypePwdAgingPolicy: ODAttributeType; cvar; external;
  kODAttributeTypeReadOnlyNode: ODAttributeType; cvar; external;
  kODAttributeTypeTimePackage: ODAttributeType; cvar; external;
  kODAttributeTypeTotalSize: ODAttributeType; cvar; external;
  kODAttributeTypeAuthMethod: ODAttributeType; cvar; external;
  kODAttributeTypeMetaNodeLocation: ODAttributeType; cvar; external;
  kODAttributeTypeNodePath: ODAttributeType; cvar; external;
  kODAttributeTypePlugInInfo: ODAttributeType; cvar; external;
  kODAttributeTypeRecordType: ODAttributeType; cvar; external;
  kODAttributeTypeSchema: ODAttributeType; cvar; external;
  kODAttributeTypeSubNodes: ODAttributeType; cvar; external;
  kODAttributeTypeNetGroupTriplet: ODAttributeType; cvar; external;
  kODAttributeTypeSearchPath: ODAttributeType; cvar; external;
  kODAttributeTypeSearchPolicy: ODAttributeType; cvar; external;
  kODAttributeTypeAutomaticSearchPath: ODAttributeType; cvar; external;
  kODAttributeTypeLocalOnlySearchPath: ODAttributeType; cvar; external;
  kODAttributeTypeCustomSearchPath: ODAttributeType; cvar; external;
  kODAttributeTypeNodeOptions: ODAttributeType { available starting in __MAC_10_7, __IPHONE_NA }; cvar; external;
  kODAttributeTypeNodeSASLRealm: ODAttributeType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODAttributeTypeAdvertisedServices: ODAttributeType { available starting in __MAC_10_7, __IPHONE_NA }; cvar; external;
  kODAttributeTypeLocaleRelay: ODAttributeType { available starting in __MAC_10_7, __IPHONE_NA }; cvar; external;
  kODAttributeTypeLocaleSubnets: ODAttributeType { available starting in __MAC_10_7, __IPHONE_NA }; cvar; external;
  kODAttributeTypeNetworkInterfaces: ODAttributeType { available starting in __MAC_10_7, __IPHONE_NA }; cvar; external;
  kODAttributeTypeParentLocales: ODAttributeType { available starting in __MAC_10_7, __IPHONE_NA }; cvar; external;
  kODAttributeTypePrimaryLocale: ODAttributeType { available starting in __MAC_10_7, __IPHONE_NA }; cvar; external;
  kODAttributeTypeBuildVersion: ODAttributeType; cvar; external;
  kODAttributeTypeConfigAvailable: ODAttributeType; cvar; external;
  kODAttributeTypeConfigFile: ODAttributeType; cvar; external;
  kODAttributeTypeCoreFWVersion: ODAttributeType; cvar; external;
  kODAttributeTypeFunctionalState: ODAttributeType; cvar; external;
  kODAttributeTypeFWVersion: ODAttributeType; cvar; external;
  kODAttributeTypePluginIndex: ODAttributeType; cvar; external;
  kODAttributeTypeNumTableList: ODAttributeType; cvar; external;
  kODAttributeTypeVersion: ODAttributeType; cvar; external;
  kODAttributeTypePIDValue: ODAttributeType; cvar; external;
  kODAttributeTypeProcessName: ODAttributeType; cvar; external;
  kODAttributeTypeTotalRefCount: ODAttributeType; cvar; external;
  kODAttributeTypeDirRefCount: ODAttributeType; cvar; external;
  kODAttributeTypeNodeRefCount: ODAttributeType; cvar; external;
  kODAttributeTypeRecRefCount: ODAttributeType; cvar; external;
  kODAttributeTypeAttrListRefCount: ODAttributeType; cvar; external;
  kODAttributeTypeAttrListValueRefCount: ODAttributeType; cvar; external;
  kODAttributeTypeDirRefs: ODAttributeType; cvar; external;
  kODAttributeTypeNodeRefs: ODAttributeType; cvar; external;
  kODAttributeTypeRecRefs: ODAttributeType; cvar; external;
  kODAttributeTypeAttrListRefs: ODAttributeType; cvar; external;
  kODAttributeTypeAttrListValueRefs: ODAttributeType; cvar; external;
  kODAuthenticationType2WayRandom: ODAuthenticationType; cvar; external;
  kODAuthenticationType2WayRandomChangePasswd: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeAPOP: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeCRAM_MD5: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeChangePasswd: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeClearText: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeCrypt: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeDIGEST_MD5: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeDeleteUser: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeGetEffectivePolicy: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeGetGlobalPolicy: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeGetKerberosPrincipal: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeGetPolicy: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeGetUserData: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeGetUserName: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeKerberosTickets: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeMPPEMasterKeys: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeMSCHAP2: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeNTLMv2: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeNTLMv2WithSessionKey: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeNewUser: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeNewUserWithPolicy: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeNodeNativeClearTextOK: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeNodeNativeNoClearText: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeReadSecureHash: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSMBNTv2UserSessionKey: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSMBWorkstationCredentialSessionKey: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSMB_LM_Key: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSMB_NT_Key: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSMB_NT_UserSessionKey: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSMB_NT_WithUserSessionKey: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSecureHash: ODAuthenticationType deprecated 'in __MAC_10_6, __MAC_10_7, __IPHONE_NA, __IPHONE_NA'; cvar; external;
  kODAuthenticationTypeSetCertificateHashAsCurrent: ODAuthenticationType { available starting in __MAC_10_7, __IPHONE_NA }; cvar; external;
  kODAuthenticationTypeSetGlobalPolicy: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSetLMHash: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSetNTHash: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSetPassword: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSetPasswordAsCurrent: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSetPolicy: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSetPolicyAsCurrent: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSetUserData: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSetUserName: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeSetWorkstationPassword: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeWithAuthorizationRef: ODAuthenticationType; cvar; external;
  kODAuthenticationTypeWriteSecureHash: ODAuthenticationType; cvar; external;
  kODPolicyTypePasswordCannotBeAccountName: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypePasswordChangeRequired: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypePasswordHistory: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypePasswordMinimumNumberOfCharacters: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypePasswordMaximumNumberOfCharacters: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypePasswordMaximumAgeInMinutes: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypePasswordRequiresAlpha: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypePasswordRequiresMixedCase: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypePasswordRequiresNumeric: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypePasswordRequiresSymbol: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypePasswordSelfModification: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypeAccountExpiresOnDate: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypeAccountMaximumFailedLogins: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypeAccountMaximumMinutesUntilDisabled: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypeAccountMinutesUntilFailedLoginReset: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
  kODPolicyTypeAccountMaximumMinutesOfNonUse: ODPolicyType { available starting in __MAC_10_9, __IPHONE_NA }; cvar; external;
{$endif}

{$ifdef TYPES}
const
  kODExpirationTimeExpired = 0;
  kODExpirationTimeNeverExpires = -1;
{$endif}

{$ifdef __OBJC__}
{$ifdef TYPES}
type
  ODPolicyKeyType = ^NSString;
{$endif}

{$else}
{$ifdef TYPES}
type
  ODPolicyKeyType = CFStringRef;
  ODPolicyKeyTypePtr = ^ODPolicyKeyType;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  kODPolicyKeyIdentifier: ODPolicyKeyType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyKeyParameters: ODPolicyKeyType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyKeyContent: ODPolicyKeyType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
{$endif}

{$ifdef __OBJC__}
{$ifdef TYPES}
type
  ODPolicyCategoryType = ^NSString;
{$endif}

{$else}
{$ifdef TYPES}
type
  ODPolicyCategoryType = CFStringRef;
  ODPolicyCategoryTypePtr = ^ODPolicyCategoryType;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  kODPolicyCategoryAuthentication: ODPolicyCategoryType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyCategoryPasswordContent: ODPolicyCategoryType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyCategoryPasswordChange: ODPolicyCategoryType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
{$endif}

{$ifdef __OBJC__}
{$ifdef TYPES}
type
  ODPolicyAttributeType = ^NSString;
{$endif}

{$else}
{$ifdef TYPES}
type
  ODPolicyAttributeType = CFStringRef;
  ODPolicyAttributeTypePtr = ^ODPolicyAttributeType;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  kODPolicyAttributeRecordName: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeRecordType: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributePassword: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributePasswordHashes: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributePasswordHistory: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributePasswordHistoryDepth: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeCurrentDate: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeCurrentTime: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeCurrentTimeOfDay: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeCurrentDayOfWeek: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeFailedAuthentications: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeMaximumFailedAuthentications: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeLastFailedAuthenticationTime: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeLastAuthenticationTime: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeLastPasswordChangeTime: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeNewPasswordRequiredTime: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeCreationTime: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeExpiresEveryNDays: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeEnableOnDate: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeExpiresOnDate: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeEnableOnDayOfWeek: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeExpiresOnDayOfWeek: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeEnableAtTimeOfDay: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeExpiresAtTimeOfDay: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
  kODPolicyAttributeDaysUntilExpiration: ODPolicyAttributeType { available starting in __MAC_10_10, __IPHONE_NA }; cvar; external;
{$endif}

{$ifdef TYPES}
const
  kODErrorSuccess = 0;
  kODErrorSessionLocalOnlyDaemonInUse = 1000;
  kODErrorSessionNormalDaemonInUse = 1001;
  kODErrorSessionDaemonNotRunning = 1002;
  kODErrorSessionDaemonRefused = 1003;
  kODErrorSessionProxyCommunicationError = 1100;
  kODErrorSessionProxyVersionMismatch = 1101;
  kODErrorSessionProxyIPUnreachable = 1102;
  kODErrorSessionProxyUnknownHost = 1103;
  kODErrorNodeUnknownName = 2000;
  kODErrorNodeUnknownType = 2001;
  kODErrorNodeDisabled = 2002;
  kODErrorNodeConnectionFailed = 2100;
  kODErrorNodeUnknownHost = 2200;
  kODErrorQuerySynchronize = 3000;
  kODErrorQueryInvalidMatchType = 3100;
  kODErrorQueryUnsupportedMatchType = 3101;
  kODErrorQueryTimeout = 3102;
  kODErrorRecordReadOnlyNode = 4000;
  kODErrorRecordPermissionError = 4001;
  kODErrorRecordParameterError = 4100;
  kODErrorRecordInvalidType = 4101;
  kODErrorRecordAlreadyExists = 4102;
  kODErrorRecordTypeDisabled = 4103;
  kODErrorRecordNoLongerExists = 4104;
  kODErrorRecordAttributeUnknownType = 4200;
  kODErrorRecordAttributeNotFound = 4201;
  kODErrorRecordAttributeValueSchemaError = 4202;
  kODErrorRecordAttributeValueNotFound = 4203;
  kODErrorCredentialsInvalid = 5000;
  kODErrorCredentialsMethodNotSupported = 5100;
  kODErrorCredentialsNotAuthorized = 5101;
  kODErrorCredentialsParameterError = 5102;
  kODErrorCredentialsOperationFailed = 5103;
  kODErrorCredentialsServerUnreachable = 5200;
  kODErrorCredentialsServerNotFound = 5201;
  kODErrorCredentialsServerError = 5202;
  kODErrorCredentialsServerTimeout = 5203;
  kODErrorCredentialsContactMaster = 5204;
  kODErrorCredentialsServerCommunicationError = 5205;
  kODErrorCredentialsAccountNotFound = 5300;
  kODErrorCredentialsAccountDisabled = 5301;
  kODErrorCredentialsAccountExpired = 5302;
  kODErrorCredentialsAccountInactive = 5303;
  kODErrorCredentialsPasswordExpired = 5400;
  kODErrorCredentialsPasswordChangeRequired = 5401;
  kODErrorCredentialsPasswordQualityFailed = 5402;
  kODErrorCredentialsPasswordTooShort = 5403;
  kODErrorCredentialsPasswordTooLong = 5404;
  kODErrorCredentialsPasswordNeedsLetter = 5405;
  kODErrorCredentialsPasswordNeedsDigit = 5406;
  kODErrorCredentialsPasswordChangeTooSoon = 5407;
  kODErrorCredentialsPasswordUnrecoverable = 5408;
  kODErrorCredentialsInvalidLogonHours = 5500;
  kODErrorCredentialsInvalidComputer = 5501;
  kODErrorPolicyUnsupported = 6000;
  kODErrorPolicyOutOfRange = 6001;
  kODErrorPluginOperationNotSupported = 10000;
  kODErrorPluginError = 10001;
  kODErrorDaemonError = 10002;
  kODErrorPluginOperationTimeout = 10003;
{$endif}

{$endif}
