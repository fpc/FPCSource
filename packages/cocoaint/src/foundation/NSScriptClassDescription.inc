{ Parsed from Foundation.framework NSScriptClassDescription.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTCLASSDESCRIPTION_PAS_C}
{$define NSSCRIPTCLASSDESCRIPTION_PAS_C}

{ NSScriptClassDescription }
  NSScriptClassDescription = objcclass(NSClassDescription)
  private
    __suiteName: NSString;
    __objcClassName: NSString;
    __appleEventCode: FourCharCode;
    __superclassNameOrDescription: NSObject;
    __attributeDescriptions: NSArray;
    __toOneRelationshipDescriptions: NSArray;
    __toManyRelationshipDescriptions: NSArray;
    __commandMethodSelectorsByName: NSDictionary;
    __moreVars: id;
    
  public
    class function alloc: NSScriptClassDescription; message 'alloc';

    class function classDescriptionForClass(aClass: Pobjc_class): NSScriptClassDescription; message 'classDescriptionForClass:';
    function initWithSuiteName_className_dictionary(suiteName_: NSString; className__: NSString; classDeclaration: NSDictionary): id; message 'initWithSuiteName:className:dictionary:';
    function suiteName: NSString; message 'suiteName';
    function className_: NSString; message 'className';
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

    { Category: NSDeprecated }
    function isReadOnlyKey(key: NSString): Boolean; message 'isReadOnlyKey:';
  end; external;

{$endif}
{$endif}
