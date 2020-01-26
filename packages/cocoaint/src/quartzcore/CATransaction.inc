{ Parsed from QuartzCore.framework CATransaction.h }


{ Types from CATransaction }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  CATransactionPtr = ^CATransaction;
{$endif}

{$ifdef CLASSES}

type
  CATransaction = objcclass external (NSObject)
  public
    class procedure begin_; message 'begin';
    class procedure commit; message 'commit';
    class procedure flush; message 'flush';
    class procedure lock; message 'lock';
    class procedure unlock; message 'unlock';
    class function animationDuration: CFTimeInterval; message 'animationDuration';
    class procedure setAnimationDuration (dur: CFTimeInterval); message 'setAnimationDuration:';
    class function animationTimingFunction: CAMediaTimingFunction; message 'animationTimingFunction';
    class procedure setAnimationTimingFunction (function_: CAMediaTimingFunction); message 'setAnimationTimingFunction:';
    class function disableActions: ObjCBOOL; message 'disableActions';
    class procedure setDisableActions (flag: ObjCBOOL); message 'setDisableActions:';
    {$if defined(__BLOCKS__)}
    class function (void))completionBlock: void (^; message '(void))completionBlock';
    class procedure setCompletionBlock (block: OpaqueCBlock); message 'setCompletionBlock:';
    {$endif}
    class function valueForKey (key: NSString): id; message 'valueForKey:';
    class procedure setValue_forKey (anObject: id; key: NSString); message 'setValue:forKey:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCATransactionAnimationDuration: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCATransactionDisableActions: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCATransactionAnimationTimingFunction: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCATransactionCompletionBlock: NSString { available starting in __MAC_10_6, __IPHONE_4_0 }; cvar; external;
{$endif}

