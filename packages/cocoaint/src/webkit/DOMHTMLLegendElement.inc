{ Parsed from Webkit.framework DOMHTMLLegendElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLLEGENDELEMENT_PAS_T}
{$define DOMHTMLLEGENDELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLLEGENDELEMENT_PAS_R}
{$define DOMHTMLLEGENDELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLLEGENDELEMENT_PAS_F}
{$define DOMHTMLLEGENDELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLLEGENDELEMENT_PAS_S}
{$define DOMHTMLLEGENDELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLLegendElement = objcclass;
  DOMHTMLLegendElementPointer = ^DOMHTMLLegendElement;
  DOMHTMLLegendElementPtr = DOMHTMLLegendElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLLEGENDELEMENT_PAS_C}
{$define DOMHTMLLEGENDELEMENT_PAS_C}

{ DOMHTMLLegendElement }
  DOMHTMLLegendElement = objcclass external (DOMHTMLElement)
    
  public
    function form: DOMHTMLFormElement; message 'form';
    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end;

{$endif}
{$endif}
