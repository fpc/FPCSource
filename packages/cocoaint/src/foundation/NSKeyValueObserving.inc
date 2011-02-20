{ Parsed from Foundation.framework NSKeyValueObserving.h }

{$ifdef TYPES}
{$ifndef NSKEYVALUEOBSERVING_PAS_T}
{$define NSKEYVALUEOBSERVING_PAS_T}

{ Constants }

const
  NSKeyValueObservingOptionNew = $01;
  NSKeyValueObservingOptionOld = $02;
  NSKeyValueObservingOptionInitial = $04;
  NSKeyValueObservingOptionPrior = $08;

const
  NSKeyValueChangeSetting = 1;
  NSKeyValueChangeInsertion = 2;
  NSKeyValueChangeRemoval = 3;
  NSKeyValueChangeReplacement = 4;

const
  NSKeyValueUnionSetMutation = 1;
  NSKeyValueMinusSetMutation = 2;
  NSKeyValueIntersectSetMutation = 3;
  NSKeyValueSetSetMutation = 4;

{ Types }
type
  NSKeyValueObservingOptions = NSUInteger;
  NSKeyValueObservingOptionsPtr = ^NSKeyValueObservingOptions;
  NSKeyValueChange = NSUInteger;
  NSKeyValueChangePtr = ^NSKeyValueChange;
  NSKeyValueSetMutationKind = NSUInteger;
  NSKeyValueSetMutationKindPtr = ^NSKeyValueSetMutationKind;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSKEYVALUEOBSERVING_PAS_R}
{$define NSKEYVALUEOBSERVING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSKEYVALUEOBSERVING_PAS_F}
{$define NSKEYVALUEOBSERVING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSKEYVALUEOBSERVING_PAS_S}
{$define NSKEYVALUEOBSERVING_PAS_S}

{ External string constants }
var
  NSKeyValueChangeKindKey: NSString; cvar; external;
  NSKeyValueChangeNewKey: NSString; cvar; external;
  NSKeyValueChangeOldKey: NSString; cvar; external;
  NSKeyValueChangeIndexesKey: NSString; cvar; external;
  NSKeyValueChangeNotificationIsPriorKey: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSKEYVALUEOBSERVING_PAS_C}
{$define NSKEYVALUEOBSERVING_PAS_C}

{ NSKeyValueObservingCategory }
  NSKeyValueObservingCategory = objccategory external (NSObject)
    procedure observeValueForKeyPath_ofObject_change_context(keyPath: NSString; object_: id; change: NSDictionary; context: Pointer); message 'observeValueForKeyPath:ofObject:change:context:';
  end;

{ NSKeyValueObserverRegistrationCategory }
  NSKeyValueObserverRegistrationCategory = objccategory external (NSObject)
    procedure addObserver_forKeyPath_options_context(observer: NSObject; keyPath: NSString; options: NSKeyValueObservingOptions; context: Pointer); message 'addObserver:forKeyPath:options:context:';
    procedure removeObserver_forKeyPath(observer: NSObject; keyPath: NSString); message 'removeObserver:forKeyPath:';
  end;

{ NSKeyValueObserverRegistration_NSArrayCategory }
  NSKeyValueObserverRegistration_NSArrayCategory = objccategory external name 'NSKeyValueObserverRegistration' (NSArray)
    procedure addObserver_toObjectsAtIndexes_forKeyPath_options_context(observer: NSObject; indexes: NSIndexSet; keyPath: NSString; options: NSKeyValueObservingOptions; context: Pointer); message 'addObserver:toObjectsAtIndexes:forKeyPath:options:context:';
    procedure removeObserver_fromObjectsAtIndexes_forKeyPath(observer: NSObject; indexes: NSIndexSet; keyPath: NSString); message 'removeObserver:fromObjectsAtIndexes:forKeyPath:';
    procedure addObserver_forKeyPath_options_context(observer: NSObject; keyPath: NSString; options: NSKeyValueObservingOptions; context: Pointer); message 'addObserver:forKeyPath:options:context:';
    procedure removeObserver_forKeyPath(observer: NSObject; keyPath: NSString); message 'removeObserver:forKeyPath:';
  end;

{ NSKeyValueObserverRegistration_NSSetCategory }
  NSKeyValueObserverRegistration_NSSetCategory = objccategory external name 'NSKeyValueObserverRegistration' (NSSet)
    procedure addObserver_forKeyPath_options_context(observer: NSObject; keyPath: NSString; options: NSKeyValueObservingOptions; context: Pointer); message 'addObserver:forKeyPath:options:context:';
    procedure removeObserver_forKeyPath(observer: NSObject; keyPath: NSString); message 'removeObserver:forKeyPath:';
  end;

{ NSKeyValueObserverNotificationCategory }
  NSKeyValueObserverNotificationCategory = objccategory external (NSObject)
    procedure willChangeValueForKey(key: NSString); message 'willChangeValueForKey:';
    procedure didChangeValueForKey(key: NSString); message 'didChangeValueForKey:';
    procedure willChange_valuesAtIndexes_forKey(changeKind: NSKeyValueChange; indexes: NSIndexSet; key: NSString); message 'willChange:valuesAtIndexes:forKey:';
    procedure didChange_valuesAtIndexes_forKey(changeKind: NSKeyValueChange; indexes: NSIndexSet; key: NSString); message 'didChange:valuesAtIndexes:forKey:';
    procedure willChangeValueForKey_withSetMutation_usingObjects(key: NSString; mutationKind: NSKeyValueSetMutationKind; objects: NSSet); message 'willChangeValueForKey:withSetMutation:usingObjects:';
    procedure didChangeValueForKey_withSetMutation_usingObjects(key: NSString; mutationKind: NSKeyValueSetMutationKind; objects: NSSet); message 'didChangeValueForKey:withSetMutation:usingObjects:';
  end;

{ NSKeyValueObservingCustomizationCategory }
  NSKeyValueObservingCustomizationCategory = objccategory external (NSObject)
    class function keyPathsForValuesAffectingValueForKey(key: NSString): NSSet; message 'keyPathsForValuesAffectingValueForKey:';
    class function automaticallyNotifiesObserversForKey(key: NSString): Boolean; message 'automaticallyNotifiesObserversForKey:';
    procedure setObservationInfo(observationInfo: Pointer); message 'setObservationInfo:';
    function observationInfo: Pointer; message 'observationInfo';
  end;

{ NSDeprecatedKeyValueObservingCustomizationCategory }
  NSDeprecatedKeyValueObservingCustomizationCategory = objccategory external (NSObject)
    class procedure setKeys_triggerChangeNotificationsForDependentKey(keys: NSArray; dependentKey: NSString); message 'setKeys:triggerChangeNotificationsForDependentKey:'; deprecated 'in Mac OS X 10.5 and later';
  end;

{$endif}
{$endif}
