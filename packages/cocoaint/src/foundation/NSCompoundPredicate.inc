{ Parsed from Foundation.framework NSCompoundPredicate.h }

{$ifdef TYPES}
{$ifndef NSCOMPOUNDPREDICATE_PAS_T}
{$define NSCOMPOUNDPREDICATE_PAS_T}

{ Constants }

const
  NSNotPredicateType = 0;
  NSAndPredicateType = 1;
  NSOrPredicateType = 2;

{ Types }
type
  NSCompoundPredicateType = NSUInteger;
  NSCompoundPredicateTypePtr = ^NSCompoundPredicateType;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOMPOUNDPREDICATE_PAS_R}
{$define NSCOMPOUNDPREDICATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOMPOUNDPREDICATE_PAS_F}
{$define NSCOMPOUNDPREDICATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOMPOUNDPREDICATE_PAS_S}
{$define NSCOMPOUNDPREDICATE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCompoundPredicate = objcclass;
  NSCompoundPredicatePointer = ^NSCompoundPredicate;
  NSCompoundPredicatePtr = NSCompoundPredicatePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOMPOUNDPREDICATE_PAS_C}
{$define NSCOMPOUNDPREDICATE_PAS_C}

{ NSCompoundPredicate }
  NSCompoundPredicate = objcclass external (NSPredicate)
  private
    _reserved2: Pointer;
    _type: NSUInteger;
    _subpredicates: NSArray;
    
  public
    function initWithType_subpredicates(type_: NSCompoundPredicateType; subpredicates_: NSArray): id; message 'initWithType:subpredicates:';
    function compoundPredicateType: NSCompoundPredicateType; message 'compoundPredicateType';
    function subpredicates: NSArray; message 'subpredicates';
    class function andPredicateWithSubpredicates(subpredicates_: NSArray): NSPredicate; message 'andPredicateWithSubpredicates:';
    class function orPredicateWithSubpredicates(subpredicates_: NSArray): NSPredicate; message 'orPredicateWithSubpredicates:';
    class function notPredicateWithSubpredicate(predicate: NSPredicate): NSPredicate; message 'notPredicateWithSubpredicate:';
  end;

{$endif}
{$endif}
