{ Parsed from Foundation.framework NSKeyValueObserving.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSKEYVALUEOBSERVING_PAS_T}
{$define NSKEYVALUEOBSERVING_PAS_T}

{ Constants }

const
  NSKeyValueObservingOptionNew = $01;
  NSKeyValueObservingOptionOld = $02;
  NSKeyValueObservingOptionInitial = $04;
  NSKeyValueObservingOptionPrior = $08;

const
  NSKeyValueChangeSetting = 1;
  NSKeyValueChangeInsertion = 2;
  NSKeyValueChangeRemoval = 3;
  NSKeyValueChangeReplacement = 4;

const
  NSKeyValueUnionSetMutation = 1;
  NSKeyValueMinusSetMutation = 2;
  NSKeyValueIntersectSetMutation = 3;
  NSKeyValueSetSetMutation = 4;

{ Types }
type
  NSKeyValueObservingOptions = NSUInteger;
  NSKeyValueChange = NSUInteger;
  NSKeyValueSetMutationKind = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSKEYVALUEOBSERVING_PAS_R}
{$define NSKEYVALUEOBSERVING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSKEYVALUEOBSERVING_PAS_F}
{$define NSKEYVALUEOBSERVING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSKEYVALUEOBSERVING_PAS_S}
{$define NSKEYVALUEOBSERVING_PAS_S}

{ External string constants }
var
  NSKeyValueChangeKindKey: NSString; external name '_NSKeyValueChangeKindKey';
  NSKeyValueChangeNewKey: NSString; external name '_NSKeyValueChangeNewKey';
  NSKeyValueChangeOldKey: NSString; external name '_NSKeyValueChangeOldKey';
  NSKeyValueChangeIndexesKey: NSString; external name '_NSKeyValueChangeIndexesKey';
  NSKeyValueChangeNotificationIsPriorKey: NSString; external name '_NSKeyValueChangeNotificationIsPriorKey';

{$endif}
{$endif}
