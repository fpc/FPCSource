{ Parsed from CloudKit.framework CKContainer.h }


{$ifdef TYPES}
type
  CKContainerPtr = ^CKContainer;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CKOwnerDefaultName: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  CKContainer = objcclass external (NSObject)
  public
    function init: instancetype; message 'init';
    class function defaultContainer: CKContainer; message 'defaultContainer';
    class function containerWithIdentifier (containerIdentifier: NSString): CKContainer; message 'containerWithIdentifier:';
    function containerIdentifier: NSString; message 'containerIdentifier';
    procedure addOperation (operation: CKOperation); message 'addOperation:';
  end;


type
  Database = objccategory external (CKContainer)
    function privateCloudDatabase: CKDatabase; message 'privateCloudDatabase';
    function publicCloudDatabase: CKDatabase; message 'publicCloudDatabase';
  end;
{$endif}

{$ifdef TYPES}
type
  CKAccountStatus = NSInteger;
  CKAccountStatusPtr = ^CKAccountStatus;

const
  CKAccountStatusCouldNotDetermine = 0;
  CKAccountStatusAvailable = 1;
  CKAccountStatusRestricted = 2;
  CKAccountStatusNoAccount = 3;
{$endif}

{$ifdef CLASSES}

type
  AccountStatus = objccategory external (CKContainer)
    procedure accountStatusWithCompletionHandler (completionHandler: OpaqueCBlock); message 'accountStatusWithCompletionHandler:';
  end;
{$endif}

{$ifdef TYPES}
type
  CKApplicationPermissions = NSUInteger;
  CKApplicationPermissionsPtr = ^CKApplicationPermissions;

const
  CKApplicationPermissionUserDiscoverability = 1 shl 0;

type
  CKApplicationPermissionStatus = NSInteger;
  CKApplicationPermissionStatusPtr = ^CKApplicationPermissionStatus;

const
  CKApplicationPermissionStatusInitialState = 0;
  CKApplicationPermissionStatusCouldNotComplete = 1;
  CKApplicationPermissionStatusDenied = 2;
  CKApplicationPermissionStatusGranted = 3;

type
  CKApplicationPermissionBlock = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  ApplicationPermission = objccategory external (CKContainer)
    procedure statusForApplicationPermission_completionHandler (applicationPermission: CKApplicationPermissions; completionHandler: CKApplicationPermissionBlock); message 'statusForApplicationPermission:completionHandler:';
    procedure requestApplicationPermission_completionHandler (applicationPermission: CKApplicationPermissions; completionHandler: CKApplicationPermissionBlock); message 'requestApplicationPermission:completionHandler:';
  end;

type
  UserRecords = objccategory external (CKContainer)
    procedure fetchUserRecordIDWithCompletionHandler (completionHandler: OpaqueCBlock); message 'fetchUserRecordIDWithCompletionHandler:';
    procedure discoverAllContactUserInfosWithCompletionHandler (completionHandler: OpaqueCBlock); message 'discoverAllContactUserInfosWithCompletionHandler:';
    procedure discoverUserInfoWithEmailAddress_completionHandler (email: NSString; completionHandler: OpaqueCBlock); message 'discoverUserInfoWithEmailAddress:completionHandler:';
    procedure discoverUserInfoWithUserRecordID_completionHandler (userRecordID: CKRecordID; completionHandler: OpaqueCBlock); message 'discoverUserInfoWithUserRecordID:completionHandler:';
  end;
{$endif}

