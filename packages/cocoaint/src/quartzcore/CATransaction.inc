{ Parsed from Quartzcore.framework CATransaction.h }

{$ifdef TYPES}
{$ifndef CATRANSACTION_PAS_T}
{$define CATRANSACTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CATRANSACTION_PAS_R}
{$define CATRANSACTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CATRANSACTION_PAS_F}
{$define CATRANSACTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CATRANSACTION_PAS_S}
{$define CATRANSACTION_PAS_S}

{ External string constants }
var
  kCATransactionAnimationDuration: NSString; cvar; external;
  kCATransactionDisableActions: NSString; cvar; external;
  kCATransactionAnimationTimingFunction: NSString; cvar; external;
  kCATransactionCompletionBlock: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CATransaction = objcclass;
  CATransactionPointer = ^CATransaction;
  CATransactionPtr = CATransactionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CATRANSACTION_PAS_C}
{$define CATRANSACTION_PAS_C}

{ CATransaction }
  CATransaction = objcclass external (NSObject)
    
  public
    class procedure begin_; message 'begin';
    class procedure commit; message 'commit';
    class procedure flush; message 'flush';
    class procedure lock; message 'lock';
    class procedure unlock; message 'unlock';
    class function animationDuration: CFTimeInterval; message 'animationDuration';
    class procedure setAnimationDuration(dur: CFTimeInterval); message 'setAnimationDuration:';
    class function animationTimingFunction: CAMediaTimingFunction; message 'animationTimingFunction';
    class procedure setAnimationTimingFunction(function_: CAMediaTimingFunction); message 'setAnimationTimingFunction:';
    class function disableActions: Boolean; message 'disableActions';
    class procedure setDisableActions(flag: Boolean); message 'setDisableActions:';
    class function valueForKey(key: NSString): id; message 'valueForKey:';
    class procedure setValue_forKey(anObject: id; key: NSString); message 'setValue:forKey:';
  end;

{$endif}
{$endif}
