{ Parsed from Webkit.framework DOMHTMLFrameSetElement.h }

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
  DOMHTMLFrameSetElementPtr = DOMHTMLFrameSetElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLFRAMESETELEMENT_PAS_C}
{$define DOMHTMLFRAMESETELEMENT_PAS_C}

{ DOMHTMLFrameSetElement }
  DOMHTMLFrameSetElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setCols (newValue: NSString); message 'setCols:';
    function cols: NSString; message 'cols';
    procedure setRows (newValue: NSString); message 'setRows:';
    function rows: NSString; message 'rows';
  end;

{$endif}
{$endif}
