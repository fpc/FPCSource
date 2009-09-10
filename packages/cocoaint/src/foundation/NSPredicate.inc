{ Parsed from Foundation.framework NSPredicate.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPREDICATE_PAS_H}
{$define NSPREDICATE_PAS_H}
type
  NSPredicatePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSPREDICATE_PAS_T}
{$define NSPREDICATE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPREDICATE_PAS_R}
{$define NSPREDICATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPREDICATE_PAS_F}
{$define NSPREDICATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPREDICATE_PAS_T}
{$define NSPREDICATE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPredicate = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPREDICATE_PAS_C}
{$define NSPREDICATE_PAS_C}

{ NSPredicate }
  NSPredicate = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __reserved: Pointer;
    
  public
    class function alloc: NSPredicate; message 'alloc';

    class function predicateWithFormat_argumentArray(predicateFormat_: NSString; arguments: NSArray): NSPredicate; message 'predicateWithFormat:argumentArray:';
    class function predicateWithFormat(predicateFormat_: NSString): NSPredicate; message 'predicateWithFormat:';
    class function predicateWithFormat_arguments(predicateFormat_: NSString; argList: va_list): NSPredicate; message 'predicateWithFormat:arguments:';
    class function predicateWithValue(value: Boolean): NSPredicate; message 'predicateWithValue:';
    function predicateFormat: NSString; message 'predicateFormat';
    function predicateWithSubstitutionVariables(variables: NSDictionary): NSPredicate; message 'predicateWithSubstitutionVariables:';
    function evaluateWithObject(object_: id): Boolean; message 'evaluateWithObject:';
    function evaluateWithObject_substitutionVariables(object_: id; bindings: NSDictionary): Boolean; message 'evaluateWithObject:substitutionVariables:';
  end; external;

{$endif}
{$endif}
