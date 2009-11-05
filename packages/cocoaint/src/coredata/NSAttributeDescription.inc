{ Parsed from Coredata.framework NSAttributeDescription.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSATTRIBUTEDESCRIPTION_PAS_T}
{$define NSATTRIBUTEDESCRIPTION_PAS_T}

{ Constants }

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
  NSTransformableAttributeType = 1800;

{ Types }
type
  NSAttributeType = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSATTRIBUTEDESCRIPTION_PAS_R}
{$define NSATTRIBUTEDESCRIPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSATTRIBUTEDESCRIPTION_PAS_F}
{$define NSATTRIBUTEDESCRIPTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSATTRIBUTEDESCRIPTION_PAS_S}
{$define NSATTRIBUTEDESCRIPTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAttributeDescription = objcclass;
  NSAttributeDescriptionPointer = ^NSAttributeDescription;

{$endif}

{$ifdef CLASSES}
{$ifndef NSATTRIBUTEDESCRIPTION_PAS_C}
{$define NSATTRIBUTEDESCRIPTION_PAS_C}

{ NSAttributeDescription }
  NSAttributeDescription = objcclass(NSPropertyDescription)
  private
    __attributeValueClass: Pobjc_class;
    __valueTransformerName: NSString;
    __type: NSAttributeType;
    __attributeValueClassName: NSString;
    __attributeDescriptionFlags: bitpacked record
        _useForLocking: 0..1;
        _reservedAttributeDescription: 0..((1 shl 31)-1);
      end;
    __defaultValue: id;
    
  public
    class function alloc: NSAttributeDescription; message 'alloc';

    function attributeType: NSAttributeType; message 'attributeType';
    procedure setAttributeType(type_: NSAttributeType); message 'setAttributeType:';
    function attributeValueClassName: NSString; message 'attributeValueClassName';
    function defaultValue: id; message 'defaultValue';
    procedure setDefaultValue(value: id); message 'setDefaultValue:';
    procedure setAttributeValueClassName(className_: NSString); message 'setAttributeValueClassName:';
    function versionHash: NSData; message 'versionHash';
    function valueTransformerName: NSString; message 'valueTransformerName';
    procedure setValueTransformerName(string_: NSString); message 'setValueTransformerName:';
  end; external;

{$endif}
{$endif}
