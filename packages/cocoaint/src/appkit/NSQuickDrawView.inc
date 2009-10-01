{ Parsed from Appkit.framework NSQuickDrawView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSQUICKDRAWVIEW_PAS_T}
{$define NSQUICKDRAWVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSQUICKDRAWVIEW_PAS_R}
{$define NSQUICKDRAWVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSQUICKDRAWVIEW_PAS_F}
{$define NSQUICKDRAWVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSQUICKDRAWVIEW_PAS_S}
{$define NSQUICKDRAWVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSQuickDrawView = objcclass;
  NSQuickDrawViewPointer = ^NSQuickDrawView;

{$endif}

{$ifdef CLASSES}
{$ifndef NSQUICKDRAWVIEW_PAS_C}
{$define NSQUICKDRAWVIEW_PAS_C}

{ NSQuickDrawView }
  NSQuickDrawView = objcclass(NSView)
  private
    __qdPort: Pointer;
    __savePort: Pointer;
    __synchToView: Boolean;
    
  public
    class function alloc: NSQuickDrawView; message 'alloc';

    function qdPort: Pointer; message 'qdPort';
  end; external;

{$endif}
{$endif}
