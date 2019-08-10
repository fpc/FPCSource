{ Parsed from AppKit.framework NSAnimationContext.h }


{$ifdef TYPES}
type
  NSAnimationContextPtr = ^NSAnimationContext;
{$endif}

{$ifdef CLASSES}

type
  NSAnimationContext = objcclass external (NSObject)
  private
    {$if  not defined(__LP64__)}
    _duration: NSTimeInterval;
    _reserved:array[0..3] of id;
    _anonStruct_NSAnimationContext0: record
      case byte of
        0: (anonBitField_NSAnimationContext0: cuint);
        1: (data: bitpacked record
          _allowsImplicitAnimation: 0..1;
          _isImplicitContext: 0..1;
          __reserved: 0..((1 shl 30)-1);
        end;
      );
          end;
    {$endif}
  public
    class procedure runAnimationGroup_completionHandler (changes: OpaqueCBlock; completionHandler: OpaqueCBlock); message 'runAnimationGroup:completionHandler:'; { available in 10_7 }
    class procedure beginGrouping; message 'beginGrouping';
    class procedure endGrouping; message 'endGrouping';
    class function currentContext: NSAnimationContext; message 'currentContext';
    procedure setDuration(newValue: NSTimeInterval); message 'setDuration:';
    function duration: NSTimeInterval; message 'duration';
    procedure setTimingFunction(newValue: CAMediaTimingFunction); message 'setTimingFunction:';
    function timingFunction: CAMediaTimingFunction; message 'timingFunction';
    procedure setCompletionHandler(newValue: OpaqueCBlock); message 'setCompletionHandler:';
    function completionHandler: OpaqueCBlock; message 'completionHandler';
    procedure setAllowsImplicitAnimation(newValue: ObjCBOOL); message 'setAllowsImplicitAnimation:';
    function allowsImplicitAnimation: ObjCBOOL; message 'allowsImplicitAnimation';
  end;
{$endif}

