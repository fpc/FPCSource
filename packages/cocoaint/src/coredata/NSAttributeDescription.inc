{ Parsed from CoreData.framework NSAttributeDescription.h }


{$ifdef TYPES}
type
  NSAttributeDescriptionPtr = ^NSAttributeDescription;
{$endif}

{$ifdef TYPES}
type
  NSAttributeType = NSUInteger;
  NSAttributeTypePtr = ^NSAttributeType;

const
  NSUndefinedAttributeType = 0;
  NSInteger16AttributeType = 100;
  NSInteger32AttributeType = 200;
  NSInteger64AttributeType = 300;
  NSDecimalAttributeType = 400;
  NSDoubleAttributeType = 500;
  NSFloatAttributeType = 600;
  NSStringAttributeType = 700;
  NSBooleanAttributeType = 800;
  NSDateAttributeType = 900;
  NSBinaryDataAttributeType = 1000;
  NSTransformableAttributeType = 1800 { available in 10_5,3_0 };
  NSObjectIDAttributeType = 2000 { available in 10_6,3_0 };
{$endif}

{$ifdef CLASSES}

type
  NSAttributeDescription = objcclass external (NSPropertyDescription)
  private
    _attributeValueClass: pobjc_class;
    _valueTransformerName: NSString;
    _type: NSAttributeType;
    _attributeValueClassName: NSString;
    __attributeDescriptionFlags: bitpacked record
      case byte of
        0: (_anonBitField___attributeDescriptionFlags0: cuint);
        1: (
          _hasMaxValueInExtraIvars: 0..1;
          _hasMinValueInExtraIvars: 0..1;
          _storeBinaryDataExternally: 0..1;
          _reservedAttributeDescription: 0..((1 shl 29)-1);
        );
      end;
    _defaultValue: id;
  public
    procedure setAttributeType(newValue: NSAttributeType); message 'setAttributeType:';
    function attributeType: NSAttributeType; message 'attributeType';
    procedure setAttributeValueClassName(newValue: NSString); message 'setAttributeValueClassName:';
    function attributeValueClassName: NSString; message 'attributeValueClassName';
    procedure setDefaultValue(newValue: id); message 'setDefaultValue:';
    function defaultValue: id; message 'defaultValue';
    function versionHash: NSData; message 'versionHash';
    procedure setValueTransformerName(newValue: NSString); message 'setValueTransformerName:';
    function valueTransformerName: NSString; message 'valueTransformerName';
    procedure setAllowsExternalBinaryDataStorage(newValue: ObjCBOOL); message 'setAllowsExternalBinaryDataStorage:';
    function allowsExternalBinaryDataStorage: ObjCBOOL; message 'allowsExternalBinaryDataStorage';
  end;
{$endif}

