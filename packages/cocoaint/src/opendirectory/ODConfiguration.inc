{ Parsed from OpenDirectory.framework ODConfiguration.h }


{$ifdef TYPES}
type
  ODConfigurationPtr = ^ODConfiguration;
{$endif}

{$ifdef TYPES}
const
  ODPacketSigningDisabled = 0;
  ODPacketSigningAllow = 1;
  ODPacketSigningRequired = 2;
const
  ODPacketEncryptionDisabled = 0;
  ODPacketEncryptionAllow = 1;
  ODPacketEncryptionRequired = 2;
  ODPacketEncryptionSSL = 3;
{$endif}

{$ifdef CLASSES}

type
  ODConfiguration = objcclass external (NSObject)
  private
    session: ODSession;
    nodeName: NSString;
    comment: NSString;
    defaultModuleEntries: NSArray;
    authenticationModuleEntries: NSArray;
    discoveryModuleEntries: NSArray;
    generalModuleEntries: NSArray;
    defaultMappings: ODMappings;
    templateName: NSString;
    virtualSubnodes: NSArray;
    hideRegistration: ObjCBOOL;
    preferredDestinationHostName: NSString;
    preferredDestinationHostPort: cuint16;
    trustAccount: NSString;
    trustType: NSString;
    trustKerberosPrincipal: NSString;
    trustMetaAccount: NSString;
    trustUsesMutualAuthentication: ObjCBOOL;
    trustUsesKerberosKeytab: ObjCBOOL;
    trustUsesSystemKeychain: ObjCBOOL;
    packetSigning: NSInteger;
    packetEncryption: NSInteger;
    manInTheMiddleProtection: ObjCBOOL;
    queryTimeoutInSeconds: NSInteger;
    connectionSetupTimeoutInSeconds: NSInteger;
    connectionIdleTimeoutInSeconds: NSInteger;
  public
    procedure setNodeName(newValue: NSString); message 'setNodeName:';
    function nodeName_: NSString; message 'nodeName';
    procedure setComment(newValue: NSString); message 'setComment:';
    function comment_: NSString; message 'comment';
    procedure setDefaultMappings(newValue: ODMappings); message 'setDefaultMappings:';
    function defaultMappings_: ODMappings; message 'defaultMappings';
    procedure setTemplateName(newValue: NSString); message 'setTemplateName:';
    function templateName_: NSString; message 'templateName';
    procedure setVirtualSubnodes(newValue: NSArray); message 'setVirtualSubnodes:';
    function virtualSubnodes_: NSArray; message 'virtualSubnodes';
    procedure setHideRegistration(newValue: ObjCBOOL); message 'setHideRegistration:';
    function hideRegistration_: ObjCBOOL; message 'hideRegistration';
    procedure setPreferredDestinationHostName(newValue: NSString); message 'setPreferredDestinationHostName:';
    function preferredDestinationHostName_: NSString; message 'preferredDestinationHostName';
    procedure setPreferredDestinationHostPort(newValue: cuint16); message 'setPreferredDestinationHostPort:';
    function preferredDestinationHostPort_: cuint16; message 'preferredDestinationHostPort';
    function trustAccount_: NSString; message 'trustAccount';
    function trustMetaAccount_: NSString; message 'trustMetaAccount';
    function trustKerberosPrincipal_: NSString; message 'trustKerberosPrincipal';
    function trustType_: NSString; message 'trustType';
    function trustUsesMutualAuthentication_: ObjCBOOL; message 'trustUsesMutualAuthentication';
    function trustUsesKerberosKeytab_: ObjCBOOL; message 'trustUsesKerberosKeytab';
    function trustUsesSystemKeychain_: ObjCBOOL; message 'trustUsesSystemKeychain';
    procedure setPacketSigning(newValue: NSInteger); message 'setPacketSigning:';
    function packetSigning_: NSInteger; message 'packetSigning';
    procedure setPacketEncryption(newValue: NSInteger); message 'setPacketEncryption:';
    function packetEncryption_: NSInteger; message 'packetEncryption';
    procedure setManInTheMiddleProtection(newValue: ObjCBOOL); message 'setManInTheMiddleProtection:';
    function manInTheMiddleProtection_: ObjCBOOL; message 'manInTheMiddleProtection';
    procedure setQueryTimeoutInSeconds(newValue: NSInteger); message 'setQueryTimeoutInSeconds:';
    function queryTimeoutInSeconds_: NSInteger; message 'queryTimeoutInSeconds';
    procedure setConnectionSetupTimeoutInSeconds(newValue: NSInteger); message 'setConnectionSetupTimeoutInSeconds:';
    function connectionSetupTimeoutInSeconds_: NSInteger; message 'connectionSetupTimeoutInSeconds';
    procedure setConnectionIdleTimeoutInSeconds(newValue: NSInteger); message 'setConnectionIdleTimeoutInSeconds:';
    function connectionIdleTimeoutInSeconds_: NSInteger; message 'connectionIdleTimeoutInSeconds';
    procedure setDefaultModuleEntries(newValue: NSArray); message 'setDefaultModuleEntries:';
    function defaultModuleEntries_: NSArray; message 'defaultModuleEntries';
    procedure setAuthenticationModuleEntries(newValue: NSArray); message 'setAuthenticationModuleEntries:';
    function authenticationModuleEntries_: NSArray; message 'authenticationModuleEntries';
    procedure setDiscoveryModuleEntries(newValue: NSArray); message 'setDiscoveryModuleEntries:';
    function discoveryModuleEntries_: NSArray; message 'discoveryModuleEntries';
    procedure setGeneralModuleEntries(newValue: NSArray); message 'setGeneralModuleEntries:';
    function generalModuleEntries_: NSArray; message 'generalModuleEntries';
    class function configuration: instancetype; message 'configuration';
    class function suggestedTrustAccount (hostname: NSString): NSString; message 'suggestedTrustAccount:'; { available in 10_9 }
    class function suggestedTrustPassword (length: size_t): NSString; message 'suggestedTrustPassword:'; { available in 10_9 }
    function saveUsingAuthorization_error (authorization: SFAuthorization; error: NSErrorPtr): ObjCBOOL; message 'saveUsingAuthorization:error:'; { available in 10_9 }
    function addTrustType_trustAccount_trustPassword_username_password_joinExisting_error (trustType__: NSString; account: NSString; accountPassword: NSString; username: NSString; password: NSString; join: ObjCBOOL; error: NSErrorPtr): ObjCBOOL; message 'addTrustType:trustAccount:trustPassword:username:password:joinExisting:error:'; { available in 10_9 }
    function removeTrustUsingUsername_password_deleteTrustAccount_error (username: NSString; password: NSString; deleteAccount: ObjCBOOL; error: NSErrorPtr): ObjCBOOL; message 'removeTrustUsingUsername:password:deleteTrustAccount:error:'; { available in 10_9 }
  end;
{$endif}

