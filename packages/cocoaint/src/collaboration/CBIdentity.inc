{ Parsed from Collaboration.framework CBIdentity.h }


{$ifdef TYPES}
type
  CBIdentityPtr = ^CBIdentity;
  CBUserIdentityPtr = ^CBUserIdentity;
  CBGroupIdentityPtr = ^CBGroupIdentity;
{$endif}

{$ifdef CLASSES}

type
  CBIdentity = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _reserved:array[0..3] of id;
  public
    class function identityWithName_authority (name: NSString; authority: CBIdentityAuthority): CBIdentity; message 'identityWithName:authority:';
    class function identityWithUUIDString_authority (uuid: NSString; authority: CBIdentityAuthority): CBIdentity; message 'identityWithUUIDString:authority:';
    class function identityWithPersistentReference (data: NSData): CBIdentity; message 'identityWithPersistentReference:';
    class function identityWithCSIdentity (csIdentity: CSIdentityRef): CBIdentity; message 'identityWithCSIdentity:';
    function authority: CBIdentityAuthority; message 'authority';
    function UUIDString: NSString; message 'UUIDString';
    function fullName: NSString; message 'fullName';
    function posixName: NSString; message 'posixName';
    function aliases: NSArray; message 'aliases';
    function emailAddress: NSString; message 'emailAddress';
    function image: NSImage; message 'image';
    function persistentReference: NSData; message 'persistentReference';
    function isHidden: ObjCBOOL; message 'isHidden';
    function isMemberOfGroup (group: CBGroupIdentity): ObjCBOOL; message 'isMemberOfGroup:';
    function CSIdentity: CSIdentityRef; message 'CSIdentity';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;

type
  CBUserIdentity = objcclass external (CBIdentity, NSCodingProtocol, NSCopyingProtocol)
  public
    class function userIdentityWithPosixUID_authority (uid: uid_t; authority_: CBIdentityAuthority): CBUserIdentity; message 'userIdentityWithPosixUID:authority:';
    function posixUID: uid_t; message 'posixUID';
    function certificate: SecCertificateRef; message 'certificate';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    function authenticateWithPassword (password: NSString): ObjCBOOL; message 'authenticateWithPassword:';
  end;

type
  CBGroupIdentity = objcclass external (CBIdentity)
  public
    class function groupIdentityWithPosixGID_authority (gid: gid_t; authority_: CBIdentityAuthority): CBGroupIdentity; message 'groupIdentityWithPosixGID:authority:';
    function posixGID: gid_t; message 'posixGID';
    function members: NSArray; message 'members';
  end;
{$endif}

