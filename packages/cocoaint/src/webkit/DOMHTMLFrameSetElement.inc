{ Parsed from Webkit.framework DOMHTMLFrameSetElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLFRAMESETELEMENT_PAS_T}
{$define DOMHTMLFRAMESETELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLFRAMESETELEMENT_PAS_R}
{$define DOMHTMLFRAMESETELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLFRAMESETELEMENT_PAS_F}
{$define DOMHTMLFRAMESETELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLFRAMESETELEMENT_PAS_S}
{$define DOMHTMLFRAMESETELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLFrameSetElement = objcclass;
  DOMHTMLFrameSetElementPointer = ^DOMHTMLFrameSetElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLFRAMESETELEMENT_PAS_C}
{$define DOMHTMLFRAMESETELEMENT_PAS_C}

{ DOMHTMLFrameSetElement }
  DOMHTMLFrameSetElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLFrameSetElement; message 'alloc';

    procedure setCols (newValue: NSString); message 'setCols:';
    function cols: NSString; message 'cols';
    procedure setRows (newValue: NSString); message 'setRows:';
    function rows: NSString; message 'rows';
  end; external;

{$endif}
{$endif}
