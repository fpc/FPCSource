{ Parsed from CoreData.framework NSMergePolicy.h }


{$ifdef TYPES}
type
  NSMergeConflictPtr = ^NSMergeConflict;
  NSMergePolicyPtr = ^NSMergePolicy;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSErrorMergePolicy: id { available in 10_4, 3_0 }; cvar; external;
  NSMergeByPropertyStoreTrumpMergePolicy: id { available in 10_4, 3_0 }; cvar; external;
  NSMergeByPropertyObjectTrumpMergePolicy: id { available in 10_4, 3_0 }; cvar; external;
  NSOverwriteMergePolicy: id { available in 10_4, 3_0 }; cvar; external;
  NSRollbackMergePolicy: id { available in 10_4, 3_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSMergePolicyType = NSUInteger;
  NSMergePolicyTypePtr = ^NSMergePolicyType;

const
  NSErrorMergePolicyType = $00;
  NSMergeByPropertyStoreTrumpMergePolicyType = $01;
  NSMergeByPropertyObjectTrumpMergePolicyType = $02;
  NSOverwriteMergePolicyType = $03;
  NSRollbackMergePolicyType = $04;
{$endif}

{$ifdef CLASSES}

type
  NSMergeConflict = objcclass external (NSObject)
  private
    _source: id;
    _snapshot1: id;
    _snapshot2: id;
    _snapshot3: id;
    _newVersion: NSUInteger;
    _oldVersion: NSUInteger;
  public
    function sourceObject: NSManagedObject; message 'sourceObject';
    function objectSnapshot: NSDictionary; message 'objectSnapshot';
    function cachedSnapshot: NSDictionary; message 'cachedSnapshot';
    function persistedSnapshot: NSDictionary; message 'persistedSnapshot';
    function newVersionNumber: NSUInteger; message 'newVersionNumber';
    function oldVersionNumber: NSUInteger; message 'oldVersionNumber';
    function initWithSource_newVersion_oldVersion_cachedSnapshot_persistedSnapshot (srcObject: NSManagedObject; newvers: NSUInteger; oldvers: NSUInteger; cachesnap: NSDictionary; persnap: NSDictionary): instancetype; message 'initWithSource:newVersion:oldVersion:cachedSnapshot:persistedSnapshot:'; { NS_DESIGNATED_INITIALIZER }
  end;

type
  NSMergePolicy = objcclass external (NSObject)
  private
    _type: NSUInteger;
    _reserved2: pointer;
    _reserved3: pointer;
  public
    function mergeType: NSMergePolicyType; message 'mergeType';
    function initWithMergeType (ty: NSMergePolicyType): id; message 'initWithMergeType:'; { NS_DESIGNATED_INITIALIZER }
    function resolveConflicts_error (list: NSArray; error: NSErrorPtr): ObjCBOOL; message 'resolveConflicts:error:';
  end;
{$endif}

