{ Parsed from Foundation.framework NSValueTransformer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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
  NSNegateBooleanTransformerName: NSString; external name '_NSNegateBooleanTransformerName';
  NSIsNilTransformerName: NSString; external name '_NSIsNilTransformerName';
  NSIsNotNilTransformerName: NSString; external name '_NSIsNotNilTransformerName';
  NSUnarchiveFromDataTransformerName: NSString; external name '_NSUnarchiveFromDataTransformerName';
  NSKeyedUnarchiveFromDataTransformerName: NSString; external name '_NSKeyedUnarchiveFromDataTransformerName';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSValueTransformer = objcclass;
  NSValueTransformerPointer = ^NSValueTransformer;

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
