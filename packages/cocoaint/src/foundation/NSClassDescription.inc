{ Parsed from Foundation.framework NSClassDescription.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSCLASSDESCRIPTION_PAS_T}
{$define NSCLASSDESCRIPTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCLASSDESCRIPTION_PAS_R}
{$define NSCLASSDESCRIPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCLASSDESCRIPTION_PAS_F}
{$define NSCLASSDESCRIPTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCLASSDESCRIPTION_PAS_S}
{$define NSCLASSDESCRIPTION_PAS_S}

{ External string constants }
var
  NSClassDescriptionNeededForClassNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSClassDescription = objcclass;
  NSClassDescriptionPointer = ^NSClassDescription;
  NSClassDescriptionPtr = NSClassDescriptionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCLASSDESCRIPTION_PAS_C}
{$define NSCLASSDESCRIPTION_PAS_C}

{ NSClassDescription }
  NSClassDescription = objcclass(NSObject)
    
  public
    class function alloc: NSClassDescription; message 'alloc';

    class procedure registerClassDescription_forClass(description_: NSClassDescription; aClass: Pobjc_class); message 'registerClassDescription:forClass:';
    class procedure invalidateClassDescriptionCache; message 'invalidateClassDescriptionCache';
    class function classDescriptionForClass(aClass: Pobjc_class): NSClassDescription; message 'classDescriptionForClass:';
    function attributeKeys: NSArray; message 'attributeKeys';
    function toOneRelationshipKeys: NSArray; message 'toOneRelationshipKeys';
    function toManyRelationshipKeys: NSArray; message 'toManyRelationshipKeys';
    function inverseForRelationshipKey(relationshipKey: NSString): NSString; message 'inverseForRelationshipKey:';
  end; external;

{ NSClassDescriptionPrimitivesCategory }
  NSClassDescriptionPrimitivesCategory = objccategory(NSObject)
    function classDescription: NSClassDescription; message 'classDescription';
    function attributeKeys: NSArray; message 'attributeKeys';
    function toOneRelationshipKeys: NSArray; message 'toOneRelationshipKeys';
    function toManyRelationshipKeys: NSArray; message 'toManyRelationshipKeys';
    function inverseForRelationshipKey(relationshipKey: NSString): NSString; message 'inverseForRelationshipKey:';
  end; external;

{$endif}
{$endif}
