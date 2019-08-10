{ Parsed from CloudKit.framework CKDiscoveredUserInfo.h }


{$ifdef TYPES}
type
  CKDiscoveredUserInfoPtr = ^CKDiscoveredUserInfo;
{$endif}

{$ifdef CLASSES}

type
  CKDiscoveredUserInfo = objcclass external (NSObject)
  public
    function init: instancetype; message 'init';
    function userRecordID: CKRecordID; message 'userRecordID';
    function firstName: NSString; message 'firstName';
    function lastName: NSString; message 'lastName';
  end;
{$endif}

