{ Parsed from SecurityFoundation.framework SFAuthorization.h }


{$ifdef TYPES}
type
  SFAuthorizationPtr = ^SFAuthorization;
{$endif}

{$ifndef _SFAUTHORIZATION_H_}
{$ifdef CLASSES}

type
  SFAuthorization = objcclass external (NSObject, NSCodingProtocol)
  private
    _authorization: AuthorizationRef;
    _reserved: id;
  public
    class function authorization: id; message 'authorization';
    function authorizationRef: AuthorizationRef; message 'authorizationRef';
    class function authorizationWithFlags_rights_environment (flags: AuthorizationFlags; rights: AuthorizationRightsPtr; environment: AuthorizationEnvironmentPtr): id; message 'authorizationWithFlags:rights:environment:';
    function initWithFlags_rights_environment (flags: AuthorizationFlags; rights: AuthorizationRightsPtr; environment: AuthorizationEnvironmentPtr): id; message 'initWithFlags:rights:environment:';
    function init: id; message 'init';
    procedure invalidateCredentials; message 'invalidateCredentials';
    function obtainWithRight_flags_error (rightName: AuthorizationString; flags: AuthorizationFlags; error: NSErrorPtr): ObjCBOOL; message 'obtainWithRight:flags:error:';
    function obtainWithRights_flags_environment_authorizedRights_error (rights: AuthorizationRightsPtr; flags: AuthorizationFlags; environment: AuthorizationEnvironmentPtr; authorizedRights: AuthorizationRightsPtrPtr; error: NSErrorPtr): ObjCBOOL; message 'obtainWithRights:flags:environment:authorizedRights:error:';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  SFAuthorizationDeprecated = objccategory external (SFAuthorization)
    function permitWithRights_flags_environment_authorizedRights (rights: AuthorizationRightsPtr; flags: AuthorizationFlags; environment: AuthorizationEnvironmentPtr; authorizedRights: AuthorizationRightsPtr): OSStatus; message 'permitWithRights:flags:environment:authorizedRights:'; deprecated 'in 10.5 and later';
    function permitWithRight_flags (rightName: AuthorizationString; flags: AuthorizationFlags): OSStatus; message 'permitWithRight:flags:'; deprecated 'in 10.5 and later';
  end;
{$endif}

{$endif}
