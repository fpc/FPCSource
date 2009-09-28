{ Parsed from Foundation.framework NSObject.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.3 - Fri Sep 18 9:46:45 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSOBJECT_PAS_H}
{$define NSOBJECT_PAS_H}
type
  NSObjectPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSOBJECT_PAS_T}
{$define NSOBJECT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSOBJECT_PAS_R}
{$define NSOBJECT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSOBJECT_PAS_F}
{$define NSOBJECT_PAS_F}

{ Functions }
function NSAllocateObject(aClass: Pobjc_class; extraBytes: NSUInteger; zonePointer: NSZone): id; cdecl; external name 'NSAllocateObject';
procedure NSDeallocateObject(object_: id); cdecl; external name 'NSDeallocateObject';
function NSCopyObject(object_: id; extraBytes: NSUInteger; zonePointer: NSZone): id; cdecl; external name 'NSCopyObject';
function NSShouldRetainWithZone(anObject: id; requestedZonePointer: NSZone): Boolean; cdecl; external name 'NSShouldRetainWithZone';
procedure NSIncrementExtraRefCount(object_: id); cdecl; external name 'NSIncrementExtraRefCount';
function NSDecrementExtraRefCountWasZero(object_: id): Boolean; cdecl; external name 'NSDecrementExtraRefCountWasZero';
function NSExtraRefCount(object_: id): NSUInteger; cdecl; external name 'NSExtraRefCount';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOBJECT_PAS_T}
{$define NSOBJECT_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSObjectProtocol = objcprotocol;
  NSCopyingProtocol = objcprotocol;
  NSMutableCopyingProtocol = objcprotocol;
  NSCodingProtocol = objcprotocol;
  NSObject = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOBJECT_PAS_C}
{$define NSOBJECT_PAS_C}

{ NSObject }
  NSObject = objcclass(NSObject, NSObjectProtocol)
  private
    _isa: Pobjc_class;
    
  public
    class function alloc: NSObject; message 'alloc';

    class procedure load; message 'load';
    class procedure initialize; message 'initialize';
    function init: id; message 'init';
    class function new_: id; message 'new';
    class function allocWithZone(zone_: NSZonePointer): id; message 'allocWithZone:';
    class function alloc: id; message 'alloc';
    procedure dealloc; message 'dealloc';
    function copy: id; message 'copy';
    function mutableCopy: id; message 'mutableCopy';
    class function copyWithZone(zone_: NSZonePointer): id; message 'copyWithZone:';
    class function mutableCopyWithZone(zone_: NSZonePointer): id; message 'mutableCopyWithZone:';
    class function superclass: Pobjc_class; message 'superclass';
    class function class_: Pobjc_class; message 'class';
    class function instancesRespondToSelector(aSelector: SEL): Boolean; message 'instancesRespondToSelector:';
    class function conformsToProtocol(protocol: objc_protocol): Boolean; message 'conformsToProtocol:';
    function methodForSelector(aSelector: SEL): IMP; message 'methodForSelector:';
    class function instanceMethodForSelector(aSelector: SEL): IMP; message 'instanceMethodForSelector:';
    procedure doesNotRecognizeSelector(aSelector: SEL); message 'doesNotRecognizeSelector:';
    procedure forwardInvocation(anInvocation: NSInvocation); message 'forwardInvocation:';
    function methodSignatureForSelector(aSelector: SEL): NSMethodSignature; message 'methodSignatureForSelector:';
    class function instanceMethodSignatureForSelector(aSelector: SEL): NSMethodSignature; message 'instanceMethodSignatureForSelector:';
    class function description: NSString; message 'description';
    class function isSubclassOfClass(aClass: Pobjc_class): Boolean; message 'isSubclassOfClass:';
    class function resolveClassMethod(sel: SEL): Boolean; message 'resolveClassMethod:';
    class function resolveInstanceMethod(sel: SEL): Boolean; message 'resolveInstanceMethod:';

    { Category: NSCoderMethods }
    class function version: NSInteger; message 'version';
    class procedure setVersion(aVersion: NSInteger); message 'setVersion:';
    function classForCoder: Pobjc_class; message 'classForCoder';
    function replacementObjectForCoder(aCoder: NSCoder): id; message 'replacementObjectForCoder:';
    function awakeAfterUsingCoder(aDecoder: NSCoder): id; message 'awakeAfterUsingCoder:';

    { Category: NSDeprecatedMethods }
    class procedure poseAsClass(aClass: Pobjc_class); message 'poseAsClass:';

    { Category: NSClassDescriptionPrimitives }
    function classDescription: NSClassDescription; message 'classDescription';
    function attributeKeys: NSArray; message 'attributeKeys';
    function toOneRelationshipKeys: NSArray; message 'toOneRelationshipKeys';
    function toManyRelationshipKeys: NSArray; message 'toManyRelationshipKeys';
    function inverseForRelationshipKey(relationshipKey: NSString): NSString; message 'inverseForRelationshipKey:';

    { Category: NSArchiverCallback }
    function classForArchiver: Pobjc_class; message 'classForArchiver';
    function replacementObjectForArchiver(archiver: NSArchiver): id; message 'replacementObjectForArchiver:';

    { Category: NSErrorRecoveryAttempting }
    procedure attemptRecoveryFromError_optionIndex_delegate_didRecoverSelector_contextInfo(error: NSError; recoveryOptionIndex: NSUInteger; delegate: id; didRecoverSelector: SEL; contextInfo: Pointer); message 'attemptRecoveryFromError:optionIndex:delegate:didRecoverSelector:contextInfo:';
    function attemptRecoveryFromError_optionIndex(error: NSError; recoveryOptionIndex: NSUInteger): Boolean; message 'attemptRecoveryFromError:optionIndex:';

    { Category: NSKeyValueCoding }
    class function accessInstanceVariablesDirectly: Boolean; message 'accessInstanceVariablesDirectly';
    function valueForKey(key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';
    function validateValue_forKey_error(ioValue: id; inKey: NSString; outError: NSErrorPointer): Boolean; message 'validateValue:forKey:error:';
    function mutableArrayValueForKey(key: NSString): NSMutableArray; message 'mutableArrayValueForKey:';
    function mutableSetValueForKey(key: NSString): NSMutableSet; message 'mutableSetValueForKey:';
    function valueForKeyPath(keyPath: NSString): id; message 'valueForKeyPath:';
    procedure setValue_forKeyPath(value: id; keyPath: NSString); message 'setValue:forKeyPath:';
    function validateValue_forKeyPath_error(ioValue: id; inKeyPath: NSString; outError: NSErrorPointer): Boolean; message 'validateValue:forKeyPath:error:';
    function mutableArrayValueForKeyPath(keyPath: NSString): NSMutableArray; message 'mutableArrayValueForKeyPath:';
    function mutableSetValueForKeyPath(keyPath: NSString): NSMutableSet; message 'mutableSetValueForKeyPath:';
    function valueForUndefinedKey(key: NSString): id; message 'valueForUndefinedKey:';
    procedure setValue_forUndefinedKey(value: id; key: NSString); message 'setValue:forUndefinedKey:';
    procedure setNilValueForKey(key: NSString); message 'setNilValueForKey:';
    function dictionaryWithValuesForKeys(keys: NSArray): NSDictionary; message 'dictionaryWithValuesForKeys:';
    procedure setValuesForKeysWithDictionary(keyedValues: NSDictionary); message 'setValuesForKeysWithDictionary:';

    { Category: NSDeprecatedKeyValueCoding }
    class function useStoredAccessor: Boolean; message 'useStoredAccessor';
    function storedValueForKey(key: NSString): id; message 'storedValueForKey:';
    procedure takeStoredValue_forKey(value: id; key: NSString); message 'takeStoredValue:forKey:';
    procedure takeValue_forKey(value: id; key: NSString); message 'takeValue:forKey:';
    procedure takeValue_forKeyPath(value: id; keyPath: NSString); message 'takeValue:forKeyPath:';
    function handleQueryWithUnboundKey(key: NSString): id; message 'handleQueryWithUnboundKey:';
    procedure handleTakeValue_forUnboundKey(value: id; key: NSString); message 'handleTakeValue:forUnboundKey:';
    procedure unableToSetNilForKey(key: NSString); message 'unableToSetNilForKey:';
    function valuesForKeys(keys: NSArray): NSDictionary; message 'valuesForKeys:';
    procedure takeValuesFromDictionary(properties: NSDictionary); message 'takeValuesFromDictionary:';

    { Category: NSKeyValueObserving }
    procedure observeValueForKeyPath_ofObject_change_context(keyPath: NSString; object_: id; change: NSDictionary; context: Pointer); message 'observeValueForKeyPath:ofObject:change:context:';

    { Category: NSKeyValueObserverRegistration }
    procedure addObserver_forKeyPath_options_context(observer: NSObject; keyPath: NSString; options: NSKeyValueObservingOptions; context: Pointer); message 'addObserver:forKeyPath:options:context:';
    procedure removeObserver_forKeyPath(observer: NSObject; keyPath: NSString); message 'removeObserver:forKeyPath:';

    { Category: NSKeyValueObservingCustomization }
    class function keyPathsForValuesAffectingValueForKey(key: NSString): NSSet; message 'keyPathsForValuesAffectingValueForKey:';
    class function automaticallyNotifiesObserversForKey(key: NSString): Boolean; message 'automaticallyNotifiesObserversForKey:';
    procedure setObservationInfo(observationInfo: Pointer); message 'setObservationInfo:';

    { Category: NSDeprecatedKeyValueObservingCustomization }
    class procedure setKeys_triggerChangeNotificationsForDependentKey(keys: NSArray; dependentKey: NSString); message 'setKeys:triggerChangeNotificationsForDependentKey:';

    { Category: NSKeyedArchiverObjectSubstitution }
    function classForKeyedArchiver: Pobjc_class; message 'classForKeyedArchiver';
    function replacementObjectForKeyedArchiver(archiver: NSKeyedArchiver): id; message 'replacementObjectForKeyedArchiver:';
    class function classFallbacksForKeyedArchiver: NSArray; message 'classFallbacksForKeyedArchiver';

    { Category: NSKeyedUnarchiverObjectSubstitution }
    class function classForKeyedUnarchiver: Pobjc_class; message 'classForKeyedUnarchiver';

    { Category: NSDistributedObjects }
    function classForPortCoder: Pobjc_class; message 'classForPortCoder';
    function replacementObjectForPortCoder(coder: NSPortCoder): id; message 'replacementObjectForPortCoder:';

    { Category: NSDelayedPerforming }
    procedure performSelector_withObject_afterDelay_inModes(aSelector: SEL; anArgument: id; delay: NSTimeInterval; modes: NSArray); message 'performSelector:withObject:afterDelay:inModes:';
    procedure performSelector_withObject_afterDelay(aSelector: SEL; anArgument: id; delay: NSTimeInterval); message 'performSelector:withObject:afterDelay:';
    class procedure cancelPreviousPerformRequestsWithTarget_selector_object(aTarget: id; aSelector: SEL; anArgument: id); message 'cancelPreviousPerformRequestsWithTarget:selector:object:';
    class procedure cancelPreviousPerformRequestsWithTarget(aTarget: id); message 'cancelPreviousPerformRequestsWithTarget:';

    { Category: NSThreadPerformAdditions }
    procedure performSelectorOnMainThread_withObject_waitUntilDone_modes(aSelector: SEL; arg: id; wait: Boolean; array_: NSArray); message 'performSelectorOnMainThread:withObject:waitUntilDone:modes:';
    procedure performSelectorOnMainThread_withObject_waitUntilDone(aSelector: SEL; arg: id; wait: Boolean); message 'performSelectorOnMainThread:withObject:waitUntilDone:';
    procedure performSelector_onThread_withObject_waitUntilDone_modes(aSelector: SEL; thr: NSThread; arg: id; wait: Boolean; array_: NSArray); message 'performSelector:onThread:withObject:waitUntilDone:modes:';
    procedure performSelector_onThread_withObject_waitUntilDone(aSelector: SEL; thr: NSThread; arg: id; wait: Boolean); message 'performSelector:onThread:withObject:waitUntilDone:';
    procedure performSelectorInBackground_withObject(aSelector: SEL; arg: id); message 'performSelectorInBackground:withObject:';

    { Category: NSScriptKeyValueCoding }
    function valueAtIndex_inPropertyWithKey(index: NSUInteger; key: NSString): id; message 'valueAtIndex:inPropertyWithKey:';
    function valueWithName_inPropertyWithKey(name: NSString; key: NSString): id; message 'valueWithName:inPropertyWithKey:';
    function valueWithUniqueID_inPropertyWithKey(uniqueID: id; key: NSString): id; message 'valueWithUniqueID:inPropertyWithKey:';
    procedure insertValue_atIndex_inPropertyWithKey(value: id; index: NSUInteger; key: NSString); message 'insertValue:atIndex:inPropertyWithKey:';
    procedure removeValueAtIndex_fromPropertyWithKey(index: NSUInteger; key: NSString); message 'removeValueAtIndex:fromPropertyWithKey:';
    procedure replaceValueAtIndex_inPropertyWithKey_withValue(index: NSUInteger; key: NSString; value: id); message 'replaceValueAtIndex:inPropertyWithKey:withValue:';
    procedure insertValue_inPropertyWithKey(value: id; key: NSString); message 'insertValue:inPropertyWithKey:';
    function coerceValue_forKey(value: id; key: NSString): id; message 'coerceValue:forKey:';

    { Category: NSComparisonMethods }
    function isEqualTo(object_: id): Boolean; message 'isEqualTo:';
    function isLessThanOrEqualTo(object_: id): Boolean; message 'isLessThanOrEqualTo:';
    function isLessThan(object_: id): Boolean; message 'isLessThan:';
    function isGreaterThanOrEqualTo(object_: id): Boolean; message 'isGreaterThanOrEqualTo:';
    function isGreaterThan(object_: id): Boolean; message 'isGreaterThan:';
    function isNotEqualTo(object_: id): Boolean; message 'isNotEqualTo:';
    function doesContain(object_: id): Boolean; message 'doesContain:';
    function isLike(object_: NSString): Boolean; message 'isLike:';
    function isCaseInsensitiveLike(object_: NSString): Boolean; message 'isCaseInsensitiveLike:';

    { Category: NSAccessibility }
    function accessibilityAttributeNames: NSArray; message 'accessibilityAttributeNames';
    function accessibilityAttributeValue(attribute: NSString): id; message 'accessibilityAttributeValue:';
    function accessibilityIsAttributeSettable(attribute: NSString): Boolean; message 'accessibilityIsAttributeSettable:';
    procedure accessibilitySetValue_forAttribute(value: id; attribute: NSString); message 'accessibilitySetValue:forAttribute:';
    function accessibilityParameterizedAttributeNames: NSArray; message 'accessibilityParameterizedAttributeNames';
    function accessibilityAttributeValue_forParameter(attribute: NSString; parameter: id): id; message 'accessibilityAttributeValue:forParameter:';
    function accessibilityActionNames: NSArray; message 'accessibilityActionNames';
    function accessibilityActionDescription(action: NSString): NSString; message 'accessibilityActionDescription:';
    procedure accessibilityPerformAction(action: NSString); message 'accessibilityPerformAction:';
    function accessibilityIsIgnored: Boolean; message 'accessibilityIsIgnored';
    function accessibilityHitTest(point: NSPoint): id; message 'accessibilityHitTest:';
    function accessibilityFocusedUIElement: id; message 'accessibilityFocusedUIElement';

    { Category: NSAccessibilityAdditions }
    function accessibilitySetOverrideValue_forAttribute(value: id; attribute: NSString): Boolean; message 'accessibilitySetOverrideValue:forAttribute:';

    { Category: NSServicesRequests }
    function writeSelectionToPasteboard_types(pboard: NSPasteboard; types: NSArray): Boolean; message 'writeSelectionToPasteboard:types:';
    function readSelectionFromPasteboard(pboard: NSPasteboard): Boolean; message 'readSelectionFromPasteboard:';

    { Category: NSKeyValueBindingCreation }
    class procedure exposeBinding(binding: NSString); message 'exposeBinding:';
    function exposedBindings: NSArray; message 'exposedBindings';
    function valueClassForBinding(binding: NSString): Pobjc_class; message 'valueClassForBinding:';
    procedure bind_toObject_withKeyPath_options(binding: NSString; observable: id; keyPath: NSString; options: NSDictionary); message 'bind:toObject:withKeyPath:options:';
    procedure unbind(binding: NSString); message 'unbind:';
    function infoForBinding(binding: NSString): NSDictionary; message 'infoForBinding:';
    function optionDescriptionsForBinding(aBinding: NSString): NSArray; message 'optionDescriptionsForBinding:';

    { Category: NSPlaceholders }
    class procedure setDefaultPlaceholder_forMarker_withBinding(placeholder: id; marker: id; binding: NSString); message 'setDefaultPlaceholder:forMarker:withBinding:';
    class function defaultPlaceholderForMarker_withBinding(marker: id; binding: NSString): id; message 'defaultPlaceholderForMarker:withBinding:';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSOBJECT_PAS_P}
{$define NSOBJECT_PAS_P}
  
{ NSObject Protocol }
  NSObjectProtocol = objcprotocol
    function isEqual(object_: id): Boolean; message 'isEqual:';
    function hash: NSUInteger; message 'hash';
    function superclass: Pobjc_class; message 'superclass';
    function class_: Pobjc_class; message 'class';
    function self_: id; message 'self';
    function zone_: NSZone; message 'zone';
    function performSelector(aSelector: SEL): id; message 'performSelector:';
    function performSelector_withObject(aSelector: SEL; object_: id): id; message 'performSelector:withObject:';
    function performSelector_withObject_withObject(aSelector: SEL; object_: id; object_1: id): id; message 'performSelector:withObject:withObject:';
    function isProxy: Boolean; message 'isProxy';
    function isKindOfClass(aClass: Pobjc_class): Boolean; message 'isKindOfClass:';
    function isMemberOfClass(aClass: Pobjc_class): Boolean; message 'isMemberOfClass:';
    function conformsToProtocol(aProtocol: objc_protocol): Boolean; message 'conformsToProtocol:';
    function respondsToSelector(aSelector: SEL): Boolean; message 'respondsToSelector:';
    function retain: id; message 'retain';
    function release: oneway void; message 'release';
    function autorelease: id; message 'autorelease';
    function retainCount: NSUInteger; message 'retainCount';
    function description: NSString; message 'description';
  end; external name 'NSObject';
  
{ NSCopying Protocol }
  NSCopyingProtocol = objcprotocol
    function copyWithZone(zone_: NSZonePointer): id; message 'copyWithZone:';
  end; external name 'NSCopying';
  
{ NSMutableCopying Protocol }
  NSMutableCopyingProtocol = objcprotocol
    function mutableCopyWithZone(zone_: NSZonePointer): id; message 'mutableCopyWithZone:';
  end; external name 'NSMutableCopying';
  
{ NSCoding Protocol }
  NSCodingProtocol = objcprotocol
    procedure encodeWithCoder(aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder(aDecoder: NSCoder): id; message 'initWithCoder:';
  end; external name 'NSCoding';
{$endif}
{$endif}
