{ Parsed from Foundation.framework NSComparisonPredicate.h }

{$ifdef TYPES}
{$ifndef NSCOMPARISONPREDICATE_PAS_T}
{$define NSCOMPARISONPREDICATE_PAS_T}

{ Constants }

const
  NSCaseInsensitivePredicateOption = $01;
  NSDiacriticInsensitivePredicateOption = $02;

const
  NSDirectPredicateModifier = 0;
  NSAllPredicateModifier = 1;
  NSAnyPredicateModifier = 2;

const
  NSLessThanPredicateOperatorType = 0;
  NSLessThanOrEqualToPredicateOperatorType = 1;
  NSGreaterThanPredicateOperatorType = 2;
  NSGreaterThanOrEqualToPredicateOperatorType = 3;
  NSEqualToPredicateOperatorType = 4;
  NSNotEqualToPredicateOperatorType = 5;
  NSMatchesPredicateOperatorType = 6;
  NSLikePredicateOperatorType = 7;
  NSBeginsWithPredicateOperatorType = 8;
  NSEndsWithPredicateOperatorType = 9;
  NSInPredicateOperatorType = 10;
  NSCustomSelectorPredicateOperatorType = 11;
  NSContainsPredicateOperatorType = 99;
  NSBetweenPredicateOperatorType = 100;

{ Types }
type
  NSComparisonPredicateModifier = NSUInteger;
  NSComparisonPredicateModifierPtr = ^NSComparisonPredicateModifier;
  NSPredicateOperatorType = NSUInteger;
  NSPredicateOperatorTypePtr = ^NSPredicateOperatorType;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOMPARISONPREDICATE_PAS_R}
{$define NSCOMPARISONPREDICATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOMPARISONPREDICATE_PAS_F}
{$define NSCOMPARISONPREDICATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOMPARISONPREDICATE_PAS_S}
{$define NSCOMPARISONPREDICATE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSComparisonPredicate = objcclass;
  NSComparisonPredicatePointer = ^NSComparisonPredicate;
  NSComparisonPredicatePtr = NSComparisonPredicatePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOMPARISONPREDICATE_PAS_C}
{$define NSCOMPARISONPREDICATE_PAS_C}

{ NSComparisonPredicate }
  NSComparisonPredicate = objcclass external (NSPredicate)
  private
    _reserved2: Pointer;
    _predicateOperator: NSPredicateOperator;
    _lhs: NSExpression;
    _rhs: NSExpression;
    
  public
    class function predicateWithLeftExpression_rightExpression_modifier_type_options(lhs: NSExpression; rhs: NSExpression; modifier: NSComparisonPredicateModifier; type_: NSPredicateOperatorType; options_: NSUInteger): NSPredicate; message 'predicateWithLeftExpression:rightExpression:modifier:type:options:';
    class function predicateWithLeftExpression_rightExpression_customSelector(lhs: NSExpression; rhs: NSExpression; selector: SEL): NSPredicate; message 'predicateWithLeftExpression:rightExpression:customSelector:';
    function initWithLeftExpression_rightExpression_modifier_type_options(lhs: NSExpression; rhs: NSExpression; modifier: NSComparisonPredicateModifier; type_: NSPredicateOperatorType; options_: NSUInteger): id; message 'initWithLeftExpression:rightExpression:modifier:type:options:';
    function initWithLeftExpression_rightExpression_customSelector(lhs: NSExpression; rhs: NSExpression; selector: SEL): id; message 'initWithLeftExpression:rightExpression:customSelector:';
    function predicateOperatorType: NSPredicateOperatorType; message 'predicateOperatorType';
    function comparisonPredicateModifier: NSComparisonPredicateModifier; message 'comparisonPredicateModifier';
    function leftExpression: NSExpression; message 'leftExpression';
    function rightExpression: NSExpression; message 'rightExpression';
    function customSelector: SEL; message 'customSelector';
    function options: NSUInteger; message 'options';
  end;

{$endif}
{$endif}
