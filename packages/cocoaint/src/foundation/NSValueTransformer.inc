{ Parsed from Foundation.framework NSValueTransformer.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSVALUETRANSFORMER_PAS_T}
{$define NSVALUETRANSFORMER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSVALUETRANSFORMER_PAS_R}
{$define NSVALUETRANSFORMER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSVALUETRANSFORMER_PAS_F}
{$define NSVALUETRANSFORMER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSVALUETRANSFORMER_PAS_S}
{$define NSVALUETRANSFORMER_PAS_S}

{ External string constants }
var
  NSNegateBooleanTransformerName: NSString; cvar; external;
  NSIsNilTransformerName: NSString; cvar; external;
  NSIsNotNilTransformerName: NSString; cvar; external;
  NSUnarchiveFromDataTransformerName: NSString; cvar; external;
  NSKeyedUnarchiveFromDataTransformerName: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSValueTransformer = objcclass;
  NSValueTransformerPointer = ^NSValueTransformer;
  NSValueTransformerPtr = NSValueTransformerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSVALUETRANSFORMER_PAS_C}
{$define NSVALUETRANSFORMER_PAS_C}

{ NSValueTransformer }
  NSValueTransformer = objcclass(NSObject)
    
  public
    class function alloc: NSValueTransformer; message 'alloc';

    class procedure setValueTransformer_forName(transformer: NSValueTransformer; name: NSString); message 'setValueTransformer:forName:';
    class function valueTransformerForName(name: NSString): NSValueTransformer; message 'valueTransformerForName:';
    class function valueTransformerNames: NSArray; message 'valueTransformerNames';
    class function transformedValueClass: Pobjc_class; message 'transformedValueClass';
    class function allowsReverseTransformation: Boolean; message 'allowsReverseTransformation';
    function transformedValue(value: id): id; message 'transformedValue:';
    function reverseTransformedValue(value: id): id; message 'reverseTransformedValue:';
  end; external;

{$endif}
{$endif}
