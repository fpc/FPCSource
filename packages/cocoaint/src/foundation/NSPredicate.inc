{ Parsed from Foundation.framework NSPredicate.h }


{$ifdef TYPES}
type
  NSPredicatePtr = ^NSPredicate;
{$endif}

{$ifdef CLASSES}

type
  NSPredicate = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  private
    _predicateFlags: bitpacked record
      case byte of
        0: (_anonBitField__predicateFlags0: cuint);
        1: (
          _evaluationBlocked: 0..1;
          _reservedPredicateFlags: 0..((1 shl 31)-1);
        );
      end;
    {$ifdef __LP64__}
    reserved: cuint32;
    {$endif}
  public
    class function predicateWithFormat_argumentArray (predicateFormat: NSString; arguments: NSArray): NSPredicate; message 'predicateWithFormat:argumentArray:';
    class function predicateWithFormat (predicateFormat: NSString): NSPredicate; varargs; message 'predicateWithFormat:';
    class function predicateWithFormat_arguments (predicateFormat: NSString; argList: va_list): NSPredicate; message 'predicateWithFormat:arguments:';
    class function predicateFromMetadataQueryString (queryString: NSString): NSPredicate; message 'predicateFromMetadataQueryString:'; { available in 10_9 }
    class function predicateWithValue (value: ObjCBOOL): NSPredicate; message 'predicateWithValue:';
    class function predicateWithBlock (block: OpaqueCBlock): NSPredicate; message 'predicateWithBlock:'; { available in 10_6, 4_0 }
    function predicateFormat: NSString; message 'predicateFormat';
    function predicateWithSubstitutionVariables (variables: NSDictionary): instancetype; message 'predicateWithSubstitutionVariables:';
    function evaluateWithObject (object_: id): ObjCBOOL; message 'evaluateWithObject:';
    function evaluateWithObject_substitutionVariables (object_: id; bindings: NSDictionary): ObjCBOOL; message 'evaluateWithObject:substitutionVariables:'; { available in 10_5, 3_0 }
    procedure allowEvaluation; message 'allowEvaluation'; { available in 10_9, 7_0 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSArray_NSPredicateSupport = objccategory external name 'NSPredicateSupport' (NSArray)
    function filteredArrayUsingPredicate (predicate: NSPredicate): NSArray; message 'filteredArrayUsingPredicate:';
  end;

type
  NSMutableArray_NSPredicateSupport = objccategory external name 'NSPredicateSupport' (NSMutableArray)
    procedure filterUsingPredicate (predicate: NSPredicate); message 'filterUsingPredicate:';
  end;

type
  NSSet_NSPredicateSupport = objccategory external name 'NSPredicateSupport' (NSSet)
    function filteredSetUsingPredicate (predicate: NSPredicate): NSSet; message 'filteredSetUsingPredicate:'; { available in 10_5, 3_0 }
  end;

type
  NSMutableSet_NSPredicateSupport = objccategory external name 'NSPredicateSupport' (NSMutableSet)
    procedure filterUsingPredicate (predicate: NSPredicate); message 'filterUsingPredicate:'; { available in 10_5, 3_0 }
  end;

type
  NSOrderedSet_NSPredicateSupport = objccategory external name 'NSPredicateSupport' (NSOrderedSet)
    function filteredOrderedSetUsingPredicate (p: NSPredicate): NSOrderedSet; message 'filteredOrderedSetUsingPredicate:'; { available in 10_7, 5_0 }
  end;

type
  NSPredicateSupport = objccategory external (NSMutableOrderedSet)
    procedure filterUsingPredicate (p: NSPredicate); message 'filterUsingPredicate:'; { available in 10_7, 5_0 }
  end;
{$endif}

