{ Parsed from OpenDirectory.framework ODNode.h }


{$ifdef TYPES}
type
  ODNodePtr = ^ODNode;
{$endif}

{$ifdef CLASSES}

type
  ODNode = objcclass external (NSObject)
  private
    _internal: pointer;
  public
    class function nodeWithSession_type_error (inSession: ODSession; inType: ODNodeType; outError: NSErrorPtr): instancetype; message 'nodeWithSession:type:error:'; { available in 10_6, NA }
    class function nodeWithSession_name_error (inSession: ODSession; inName: NSString; outError: NSErrorPtr): instancetype; message 'nodeWithSession:name:error:'; { available in 10_6, NA }
    function initWithSession_type_error (inSession: ODSession; inType: ODNodeType; outError: NSErrorPtr): instancetype; message 'initWithSession:type:error:'; { available in 10_6, NA }
    function initWithSession_name_error (inSession: ODSession; inName: NSString; outError: NSErrorPtr): instancetype; message 'initWithSession:name:error:'; { available in 10_6, NA }
    function subnodeNamesAndReturnError (outError: NSErrorPtr): NSArray; message 'subnodeNamesAndReturnError:'; { available in 10_6, NA }
    function unreachableSubnodeNamesAndReturnError (outError: NSErrorPtr): NSArray; message 'unreachableSubnodeNamesAndReturnError:'; { available in 10_6, NA }
    function nodeName: NSString; message 'nodeName';
    function nodeDetailsForKeys_error (inKeys: NSArray; outError: NSErrorPtr): NSDictionary; message 'nodeDetailsForKeys:error:'; { available in 10_6, NA }
    function supportedRecordTypesAndReturnError (outError: NSErrorPtr): NSArray; message 'supportedRecordTypesAndReturnError:'; { available in 10_6, NA }
    function supportedAttributesForRecordType_error (inRecordType: ODRecordType; outError: NSErrorPtr): NSArray; message 'supportedAttributesForRecordType:error:'; { available in 10_6, NA }
    function setCredentialsWithRecordType_recordName_password_error (inRecordType: ODRecordType; inRecordName: NSString; inPassword: NSString; outError: NSErrorPtr): ObjCBOOL; message 'setCredentialsWithRecordType:recordName:password:error:'; { available in 10_6, NA }
    function setCredentialsWithRecordType_authenticationType_authenticationItems_continueItems_context_error (inRecordType: ODRecordType; inType: ODAuthenticationType; inItems: NSArray; outItems: NSArrayPtr; outContext: idPtr; outError: NSErrorPtr): ObjCBOOL; message 'setCredentialsWithRecordType:authenticationType:authenticationItems:continueItems:context:error:'; { available in 10_6, NA }
    function setCredentialsUsingKerberosCache_error (inCacheName: NSString; outError: NSErrorPtr): ObjCBOOL; message 'setCredentialsUsingKerberosCache:error:'; { available in 10_6, NA }
    function createRecordWithRecordType_name_attributes_error (inRecordType: ODRecordType; inRecordName: NSString; inAttributes: NSDictionary; outError: NSErrorPtr): ODRecord; message 'createRecordWithRecordType:name:attributes:error:'; { available in 10_6, NA }
    function recordWithRecordType_name_attributes_error (inRecordType: ODRecordType; inRecordName: NSString; inAttributes: id; outError: NSErrorPtr): ODRecord; message 'recordWithRecordType:name:attributes:error:'; { available in 10_6, NA }
    function customCall_sendData_error (inCustomCode: NSInteger; inSendData: NSData; outError: NSErrorPtr): NSData; message 'customCall:sendData:error:'; { available in 10_6, NA }
    function customFunction_payload_error (function_: NSString; payload: id; error: NSErrorPtr): id; message 'customFunction:payload:error:'; { available in 10_9, NA }
    function configuration: ODConfiguration; message 'configuration';
    function policiesAndReturnError (error: NSErrorPtr): NSDictionary; message 'policiesAndReturnError:'; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use accountPoliciesAndReturnError:" }
    function supportedPoliciesAndReturnError (error: NSErrorPtr): NSDictionary; message 'supportedPoliciesAndReturnError:'; deprecated 'in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA';
    function setPolicies_error (policies: NSDictionary; error: NSErrorPtr): ObjCBOOL; message 'setPolicies:error:'; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use setAccountPolicies:error:" }
    function setPolicy_value_error (policy: ODPolicyType; value: id; error: NSErrorPtr): ObjCBOOL; message 'setPolicy:value:error:'; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use addAccountPolicy:toCategory:error:" }
    function removePolicy_error (policy: ODPolicyType; error: NSErrorPtr): ObjCBOOL; message 'removePolicy:error:'; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use removeAccountPolicy:fromCategory:error:" }
    function addAccountPolicy_toCategory_error (policy: NSDictionary; category: ODPolicyCategoryType; error: NSErrorPtr): ObjCBOOL; message 'addAccountPolicy:toCategory:error:'; { available starting in __MAC_10_10, __IPHONE_NA }
    function removeAccountPolicy_fromCategory_error (policy: NSDictionary; category: ODPolicyCategoryType; error: NSErrorPtr): ObjCBOOL; message 'removeAccountPolicy:fromCategory:error:'; { available starting in __MAC_10_10, __IPHONE_NA }
    function setAccountPolicies_error (policies: NSDictionary; error: NSErrorPtr): ObjCBOOL; message 'setAccountPolicies:error:'; { available starting in __MAC_10_10, __IPHONE_NA }
    function accountPoliciesAndReturnError (error: NSErrorPtr): NSDictionary; message 'accountPoliciesAndReturnError:'; { available starting in __MAC_10_10, __IPHONE_NA }
    function passwordContentCheck_forRecordName_error (password: NSString; recordName: NSString; error: NSErrorPtr): ObjCBOOL; message 'passwordContentCheck:forRecordName:error:'; { available starting in __MAC_10_10, __IPHONE_NA }
  end;
{$endif}

