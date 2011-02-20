{ Parsed from Foundation.framework NSPredicate.h }

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
{$ifndef NSPREDICATE_PAS_S}
{$define NSPREDICATE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPredicate = objcclass;
  NSPredicatePointer = ^NSPredicate;
  NSPredicatePtr = NSPredicatePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPREDICATE_PAS_C}
{$define NSPREDICATE_PAS_C}

{ NSPredicate }
  NSPredicate = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _reserved: Pointer;
    
  public
    class function predicateWithFormat_argumentArray(predicateFormat_: NSString; arguments: NSArray): NSPredicate; message 'predicateWithFormat:argumentArray:';
    class function predicateWithFormat(firstKey: id): NSPredicate; varargs; message 'predicateWithFormat:';
    class function predicateWithFormat_arguments(predicateFormat_: NSString; argList: va_list): NSPredicate; message 'predicateWithFormat:arguments:';
    class function predicateWithValue(value: Boolean): NSPredicate; message 'predicateWithValue:';
    function predicateFormat: NSString; message 'predicateFormat';
    function predicateWithSubstitutionVariables(variables: NSDictionary): NSPredicate; message 'predicateWithSubstitutionVariables:';
    function evaluateWithObject(object_: id): Boolean; message 'evaluateWithObject:';
    function evaluateWithObject_substitutionVariables(object_: id; bindings: NSDictionary): Boolean; message 'evaluateWithObject:substitutionVariables:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{ NSPredicateSupportCategory }
  NSPredicateSupportCategory = objccategory external (NSArray)
    function filteredArrayUsingPredicate(predicate: NSPredicate): NSArray; message 'filteredArrayUsingPredicate:';
  end;

{ NSPredicateSupport_NSMutableArrayCategory }
  NSPredicateSupport_NSMutableArrayCategory = objccategory external name 'NSPredicateSupport' (NSMutableArray)
    procedure filterUsingPredicate(predicate: NSPredicate); message 'filterUsingPredicate:';
  end;

{ NSPredicateSupport_NSSetCategory }
  NSPredicateSupport_NSSetCategory = objccategory external name 'NSPredicateSupport' (NSSet)
    function filteredSetUsingPredicate(predicate: NSPredicate): NSSet; message 'filteredSetUsingPredicate:';
  end;

{ NSPredicateSupport_NSMutableSetCategory }
  NSPredicateSupport_NSMutableSetCategory = objccategory external name 'NSPredicateSupport' (NSMutableSet)
    procedure filterUsingPredicate(predicate: NSPredicate); message 'filterUsingPredicate:';
  end;

{$endif}
{$endif}
