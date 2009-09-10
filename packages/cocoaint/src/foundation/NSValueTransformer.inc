{ Parsed from Foundation.framework NSValueTransformer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSVALUETRANSFORMER_PAS_H}
{$define NSVALUETRANSFORMER_PAS_H}
type
  NSValueTransformerPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSVALUETRANSFORMER_PAS_T}
{$define NSVALUETRANSFORMER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSValueTransformer = objcclass;

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
