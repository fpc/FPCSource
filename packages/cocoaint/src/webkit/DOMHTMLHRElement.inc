{ Parsed from Webkit.framework DOMHTMLHRElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLHRELEMENT_PAS_T}
{$define DOMHTMLHRELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLHRELEMENT_PAS_R}
{$define DOMHTMLHRELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLHRELEMENT_PAS_F}
{$define DOMHTMLHRELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLHRELEMENT_PAS_S}
{$define DOMHTMLHRELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLHRElement = objcclass;
  DOMHTMLHRElementPointer = ^DOMHTMLHRElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLHRELEMENT_PAS_C}
{$define DOMHTMLHRELEMENT_PAS_C}

{ DOMHTMLHRElement }
  DOMHTMLHRElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLHRElement; message 'alloc';

    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setSize (newValue: NSString); message 'setSize:';
    function size: NSString; message 'size';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
  end; external;

{$endif}
{$endif}
