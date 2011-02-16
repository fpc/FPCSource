{ Parsed from Foundation.framework NSScriptClassDescription.h }

{$ifdef TYPES}
{$ifndef NSSCRIPTCLASSDESCRIPTION_PAS_T}
{$define NSSCRIPTCLASSDESCRIPTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCRIPTCLASSDESCRIPTION_PAS_R}
{$define NSSCRIPTCLASSDESCRIPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCRIPTCLASSDESCRIPTION_PAS_F}
{$define NSSCRIPTCLASSDESCRIPTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCRIPTCLASSDESCRIPTION_PAS_S}
{$define NSSCRIPTCLASSDESCRIPTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptClassDescription = objcclass;
  NSScriptClassDescriptionPointer = ^NSScriptClassDescription;
  NSScriptClassDescriptionPtr = NSScriptClassDescriptionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTCLASSDESCRIPTION_PAS_C}
{$define NSSCRIPTCLASSDESCRIPTION_PAS_C}

{ NSScriptClassDescription }
  NSScriptClassDescription = objcclass external (NSClassDescription)
  private
    _suiteName: NSString;
    _objcClassName: NSString;
    _appleEventCode: FourCharCode;
    _superclassNameOrDescription: NSObject;
    _attributeDescriptions: NSArray;
    _toOneRelationshipDescriptions: NSArray;
    _toManyRelationshipDescriptions: NSArray;
    _commandMethodSelectorsByName: NSDictionary;
    _moreVars: id;
    
  public
    class function classDescriptionForClass(aClass: Pobjc_class): NSScriptClassDescription; message 'classDescriptionForClass:';
    function initWithSuiteName_className_dictionary(suiteName_: NSString; className_: NSString; classDeclaration: NSDictionary): id; message 'initWithSuiteName:className:dictionary:';
    function suiteName: NSString; message 'suiteName';
    function className: NSString; message 'className';
    function implementationClassName: NSString; message 'implementationClassName';
    function superclassDescription: NSScriptClassDescription; message 'superclassDescription';
    function appleEventCode: FourCharCode; message 'appleEventCode';
    function matchesAppleEventCode(appleEventCode_: FourCharCode): Boolean; message 'matchesAppleEventCode:';
    function supportsCommand(commandDescription: NSScriptCommandDescription): Boolean; message 'supportsCommand:';
    function selectorForCommand(commandDescription: NSScriptCommandDescription): SEL; message 'selectorForCommand:';
    function typeForKey(key: NSString): NSString; message 'typeForKey:';
    function classDescriptionForKey(key: NSString): NSScriptClassDescription; message 'classDescriptionForKey:';
    function appleEventCodeForKey(key: NSString): FourCharCode; message 'appleEventCodeForKey:';
    function keyWithAppleEventCode(appleEventCode_: FourCharCode): NSString; message 'keyWithAppleEventCode:';
    function defaultSubcontainerAttributeKey: NSString; message 'defaultSubcontainerAttributeKey';
    function isLocationRequiredToCreateForKey(toManyRelationshipKey: NSString): Boolean; message 'isLocationRequiredToCreateForKey:';
    function hasPropertyForKey(key: NSString): Boolean; message 'hasPropertyForKey:';
    function hasOrderedToManyRelationshipForKey(key: NSString): Boolean; message 'hasOrderedToManyRelationshipForKey:';
    function hasReadablePropertyForKey(key: NSString): Boolean; message 'hasReadablePropertyForKey:';
    function hasWritablePropertyForKey(key: NSString): Boolean; message 'hasWritablePropertyForKey:';
  end;

{ NSDeprecated_NSScriptClassDescriptionCategory }
  NSDeprecated_NSScriptClassDescriptionCategory = objccategory external name 'NSDeprecated' (NSScriptClassDescription)
    function isReadOnlyKey(key: NSString): Boolean; message 'isReadOnlyKey:'; deprecated 'in Mac OS X 10.5 and later';
  end;

{ NSScriptClassDescription_NSObjectCategory }
  NSScriptClassDescription_NSObjectCategory = objccategory external name 'NSScriptClassDescription' (NSObject)
    function classCode: FourCharCode; message 'classCode';
    function className: NSString; message 'className';
  end;

{$endif}
{$endif}
