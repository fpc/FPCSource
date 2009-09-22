{ Parsed from Foundation.framework NSCompoundPredicate.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCOMPOUNDPREDICATE_PAS_H}
{$define NSCOMPOUNDPREDICATE_PAS_H}
type
  NSCompoundPredicatePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCOMPOUNDPREDICATE_PAS_T}
{$define NSCOMPOUNDPREDICATE_PAS_T}

{ Constants }

const
  NSNotPredicateType = 0;
  NSAndPredicateType = 0;
  NSOrPredicateType = 1;

{ Types }
type
  NSCompoundPredicateType = NSUInteger;

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
{$ifndef NSCOMPOUNDPREDICATE_PAS_T}
{$define NSCOMPOUNDPREDICATE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCompoundPredicate = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOMPOUNDPREDICATE_PAS_C}
{$define NSCOMPOUNDPREDICATE_PAS_C}

{ NSCompoundPredicate }
  NSCompoundPredicate = objcclass(NSPredicate)
  private
    __reserved2: Pointer;
    __type: NSUInteger;
    __subpredicates: NSArray;
    
  public
    class function alloc: NSCompoundPredicate; message 'alloc';

    function initWithType_subpredicates(type_: NSCompoundPredicateType; subpredicates_: NSArray): id; message 'initWithType:subpredicates:';
    function compoundPredicateType: NSCompoundPredicateType; message 'compoundPredicateType';
    function subpredicates: NSArray; message 'subpredicates';
    class function andPredicateWithSubpredicates(subpredicates_: NSArray): NSPredicate; message 'andPredicateWithSubpredicates:';
    class function orPredicateWithSubpredicates(subpredicates_: NSArray): NSPredicate; message 'orPredicateWithSubpredicates:';
    class function notPredicateWithSubpredicate(predicate: NSPredicate): NSPredicate; message 'notPredicateWithSubpredicate:';
  end; external;

{$endif}
{$endif}
