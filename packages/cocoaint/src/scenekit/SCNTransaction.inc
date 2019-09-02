{ Parsed from SceneKit.framework SCNTransaction.h }


{ Types from SCNTransaction }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  SCNTransactionPtr = ^SCNTransaction;
{$endif}

{$ifdef CLASSES}

type
  SCNTransaction = objcclass external (NSObject)
  public
    class procedure begin_; message 'begin';
    class procedure commit; message 'commit';
    class procedure flush; message 'flush';
    class procedure lock; message 'lock';
    class procedure unlock; message 'unlock';
    class function animationDuration: CFTimeInterval; message 'animationDuration';
    class procedure setAnimationDuration (duration: CFTimeInterval); message 'setAnimationDuration:';
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

