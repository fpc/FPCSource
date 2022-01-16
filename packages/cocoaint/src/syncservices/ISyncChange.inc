{ Parsed from SyncServices.framework ISyncChange.h }


{$ifdef TYPES}
type
  ISyncChangePtr = ^ISyncChange;
{$endif}

{$ifdef TYPES}
type
  ISyncChangeType = cint deprecated 'in 10_4, 10_7';
  ISyncChangeTypePtr = ^ISyncChangeType deprecated 'in 10_4, 10_7';

const
  ISyncChangeTypeNone = 0;
  ISyncChangeTypeAdd = 1;
  ISyncChangeTypeModify = 2;
  ISyncChangeTypeDelete = 3;
{$endif}

{$ifdef CLASSES}

type
  ISyncChange = objcclass external (NSObject)
  public
    class function changeWithType_recordIdentifier_changes (type_: ISyncChangeType; recordIdentifier: NSString; changes: NSArray): id; message 'changeWithType:recordIdentifier:changes:'; deprecated 'in 10_4, 10_7';
    function initWithChangeType_recordIdentifier_changes (type_: ISyncChangeType; recordIdentifier: NSString; changes: NSArray): id; message 'initWithChangeType:recordIdentifier:changes:'; deprecated 'in 10_4, 10_7';
    function type_: ISyncChangeType; message 'type'; deprecated 'in 10_4, 10_7';
    function recordIdentifier: NSString; message 'recordIdentifier'; deprecated 'in 10_4, 10_7';
    function record_: NSDictionary; message 'record'; deprecated 'in 10_4, 10_7';
    function changes: NSArray; message 'changes'; deprecated 'in 10_4, 10_7';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ISyncChangePropertyActionKey: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncChangePropertySet: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncChangePropertyClear: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncChangePropertyNameKey: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncChangePropertyValueKey: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncChangePropertyValueIsDefaultKey: NSString deprecated 'in 10_4, 10_7'; cvar; external;
{$endif}

