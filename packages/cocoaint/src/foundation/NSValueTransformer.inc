{ Parsed from Foundation.framework NSValueTransformer.h }


{$ifdef TYPES}
type
  NSValueTransformerPtr = ^NSValueTransformer;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSNegateBooleanTransformerName: NSString { available in 10_3, 3_0 }; cvar; external;
  NSIsNilTransformerName: NSString { available in 10_3, 3_0 }; cvar; external;
  NSIsNotNilTransformerName: NSString { available in 10_3, 3_0 }; cvar; external;
  NSUnarchiveFromDataTransformerName: NSString { available in 10_3, 3_0 }; cvar; external;
  NSKeyedUnarchiveFromDataTransformerName: NSString { available in 10_5, 3_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSValueTransformer = objcclass external (NSObject)
  private
  public
    class procedure setValueTransformer_forName (transformer: NSValueTransformer; name: NSString); message 'setValueTransformer:forName:';
    class function valueTransformerForName (name: NSString): NSValueTransformer; message 'valueTransformerForName:';
    class function valueTransformerNames: NSArray; message 'valueTransformerNames';
    class function transformedValueClass: pobjc_class; message 'transformedValueClass';
    class function allowsReverseTransformation: ObjCBOOL; message 'allowsReverseTransformation';
    function transformedValue (value: id): id; message 'transformedValue:';
    function reverseTransformedValue (value: id): id; message 'reverseTransformedValue:';
  end;
{$endif}

