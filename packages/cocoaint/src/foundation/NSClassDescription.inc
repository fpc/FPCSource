{ Parsed from Foundation.framework NSClassDescription.h }


{$ifdef TYPES}
type
  NSClassDescriptionPtr = ^NSClassDescription;
{$endif}

{$ifdef CLASSES}

type
  NSClassDescription = objcclass external (NSObject)
  public
    class procedure registerClassDescription_forClass (description: NSClassDescription; aClass: pobjc_class); message 'registerClassDescription:forClass:';
    class procedure invalidateClassDescriptionCache; message 'invalidateClassDescriptionCache';
    class function classDescriptionForClass (aClass: pobjc_class): NSClassDescription; message 'classDescriptionForClass:';
    function attributeKeys: NSArray; message 'attributeKeys';
    function toOneRelationshipKeys: NSArray; message 'toOneRelationshipKeys';
    function toManyRelationshipKeys: NSArray; message 'toManyRelationshipKeys';
    function inverseForRelationshipKey (relationshipKey: NSString): NSString; message 'inverseForRelationshipKey:';
  end;


type
  NSClassDescriptionPrimitives = objccategory external (NSObject)
    function classDescription: NSClassDescription; message 'classDescription';
    function attributeKeys: NSArray; message 'attributeKeys';
    function toOneRelationshipKeys: NSArray; message 'toOneRelationshipKeys';
    function toManyRelationshipKeys: NSArray; message 'toManyRelationshipKeys';
    function inverseForRelationshipKey (relationshipKey: NSString): NSString; message 'inverseForRelationshipKey:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSClassDescriptionNeededForClassNotification: NSString; cvar; external;
{$endif}

