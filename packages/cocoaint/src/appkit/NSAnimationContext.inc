{ Parsed from Appkit.framework NSAnimationContext.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSANIMATIONCONTEXT_PAS_H}
{$define NSANIMATIONCONTEXT_PAS_H}
type
  NSAnimationContextPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSANIMATIONCONTEXT_PAS_T}
{$define NSANIMATIONCONTEXT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSANIMATIONCONTEXT_PAS_R}
{$define NSANIMATIONCONTEXT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSANIMATIONCONTEXT_PAS_F}
{$define NSANIMATIONCONTEXT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSANIMATIONCONTEXT_PAS_T}
{$define NSANIMATIONCONTEXT_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAnimationContext = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSANIMATIONCONTEXT_PAS_C}
{$define NSANIMATIONCONTEXT_PAS_C}

{ NSAnimationContext }
  NSAnimationContext = objcclass(NSObject)
  private
    __duration: NSTimeInterval;
        _reserved: array[0..4] of id;
    
  public
    class function alloc: NSAnimationContext; message 'alloc';

    class procedure beginGrouping; message 'beginGrouping';
    class procedure endGrouping; message 'endGrouping';
    class function currentContext: NSAnimationContext; message 'currentContext';
    procedure setDuration(duration_: NSTimeInterval); message 'setDuration:';
    function duration: NSTimeInterval; message 'duration';
  end; external;

{$endif}
{$endif}
