{ Parsed from Foundation.framework NSClassDescription.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCLASSDESCRIPTION_PAS_H}
{$define NSCLASSDESCRIPTION_PAS_H}
type
  NSClassDescriptionPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCLASSDESCRIPTION_PAS_T}
{$define NSCLASSDESCRIPTION_PAS_T}

{ CFString constants }
var
  NSClassDescriptionNeededForClassNotification: CFStringRef; external name '_NSClassDescriptionNeededForClassNotification';

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
{$ifndef NSCLASSDESCRIPTION_PAS_T}
{$define NSCLASSDESCRIPTION_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSClassDescription = objcclass;

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

{$endif}
{$endif}
