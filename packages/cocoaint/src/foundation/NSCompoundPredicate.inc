{ Parsed from Foundation.framework NSCompoundPredicate.h }


{$ifdef TYPES}
type
  NSCompoundPredicatePtr = ^NSCompoundPredicate;
{$endif}

{$ifdef TYPES}
type
  NSCompoundPredicateType = NSUInteger;
  NSCompoundPredicateTypePtr = ^NSCompoundPredicateType;

const
  NSNotPredicateType = 0;
  NSAndPredicateType = 1;
  NSOrPredicateType = 2;
{$endif}

{$ifdef CLASSES}

type
  NSCompoundPredicate = objcclass external (NSPredicate)
  private
    _reserved2: pointer;
    _type: NSUInteger;
    _subpredicates: NSArray;
  public
    function initWithType_subpredicates (type_: NSCompoundPredicateType; subpredicates: NSArray): instancetype; message 'initWithType:subpredicates:';
    function compoundPredicateType: NSCompoundPredicateType; message 'compoundPredicateType';
    function subpredicates: NSArray; message 'subpredicates';
    class function andPredicateWithSubpredicates (subpredicates_: NSArray): NSCompoundPredicate; message 'andPredicateWithSubpredicates:';
    class function orPredicateWithSubpredicates (subpredicates_: NSArray): NSCompoundPredicate; message 'orPredicateWithSubpredicates:';
    class function notPredicateWithSubpredicate (predicate: NSPredicate): NSCompoundPredicate; message 'notPredicateWithSubpredicate:';
  end;
{$endif}

