{ Parsed from Appkit.framework NSQuickDrawView.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


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
  NSQuickDrawViewPtr = NSQuickDrawViewPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSQUICKDRAWVIEW_PAS_C}
{$define NSQUICKDRAWVIEW_PAS_C}

{ NSQuickDrawView }
  NSQuickDrawView = objcclass(NSView)
  private
    _qdPort: Pointer;
    _savePort: Pointer;
    _synchToView: Boolean;
    
  public
    class function alloc: NSQuickDrawView; message 'alloc';

    function qdPort: Pointer; message 'qdPort'; deprecated 'in Mac OS X 10.4 and later';
  end; external;

{$endif}
{$endif}
