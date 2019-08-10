{ Parsed from OpenDirectory.framework ODSession.h }


{$ifdef TYPES}
type
  ODSessionPtr = ^ODSession;
{$endif}

{$ifdef CLASSES}

type
  ODSession = objcclass external (NSObject)
  private
    _internal: pointer;
  public
    class function defaultSession: ODSession; message 'defaultSession'; { available in 10_6, NA }
    class function sessionWithOptions_error (inOptions: NSDictionary; outError: NSErrorPtr): instancetype; message 'sessionWithOptions:error:'; { available in 10_6, NA }
    function initWithOptions_error (inOptions: NSDictionary; outError: NSErrorPtr): instancetype; message 'initWithOptions:error:'; { available in 10_6, NA }
    function nodeNamesAndReturnError (outError: NSErrorPtr): NSArray; message 'nodeNamesAndReturnError:'; { available in 10_6, NA }
    function configurationTemplateNames: NSArray; message 'configurationTemplateNames';
    function mappingTemplateNames: NSArray; message 'mappingTemplateNames';
    function configurationAuthorizationAllowingUserInteraction_error (allowInteraction: ObjCBOOL; error: NSErrorPtr): SFAuthorization; message 'configurationAuthorizationAllowingUserInteraction:error:'; { available in 10_9 }
    function configurationForNodename (nodename: NSString): ODConfiguration; message 'configurationForNodename:'; { available in 10_9 }
    function addConfiguration_authorization_error (configuration: ODConfiguration; authorization: SFAuthorization; error: NSErrorPtr): ObjCBOOL; message 'addConfiguration:authorization:error:'; { available in 10_9 }
    function deleteConfiguration_authorization_error (configuration: ODConfiguration; authorization: SFAuthorization; error: NSErrorPtr): ObjCBOOL; message 'deleteConfiguration:authorization:error:'; { available in 10_9 }
    function deleteConfigurationWithNodename_authorization_error (nodename: NSString; authorization: SFAuthorization; error: NSErrorPtr): ObjCBOOL; message 'deleteConfigurationWithNodename:authorization:error:'; { available in 10_9 }
  end;
{$endif}

