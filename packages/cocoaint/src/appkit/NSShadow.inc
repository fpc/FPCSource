{ Parsed from Appkit.framework NSShadow.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSSHADOW_PAS_T}
{$define NSSHADOW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSHADOW_PAS_R}
{$define NSSHADOW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSHADOW_PAS_F}
{$define NSSHADOW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSHADOW_PAS_S}
{$define NSSHADOW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSShadow = objcclass;
  NSShadowPointer = ^NSShadow;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSHADOW_PAS_C}
{$define NSSHADOW_PAS_C}

{ NSShadow }
  NSShadow = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __shadowFlags: NSUInteger;
    __shadowOffset: NSSize;
    __shadowBlurRadius: CGFloat;
    __shadowColor: NSColor;
        _reservedFloat: array[0..2] of CGFloat;
    __reserved: Pointer;
    
  public
    class function alloc: NSShadow; message 'alloc';

    function init: id; message 'init';
    function shadowOffset: NSSize; message 'shadowOffset';
    procedure setShadowOffset(offset: NSSize); message 'setShadowOffset:';
    function shadowBlurRadius: CGFloat; message 'shadowBlurRadius';
    procedure setShadowBlurRadius(val: CGFloat); message 'setShadowBlurRadius:';
    function shadowColor: NSColor; message 'shadowColor';
    procedure setShadowColor(color: NSColor); message 'setShadowColor:';
    procedure set_; message 'set';
  end; external;

{$endif}
{$endif}
