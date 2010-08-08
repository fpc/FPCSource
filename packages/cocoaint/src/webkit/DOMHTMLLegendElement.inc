{ Parsed from Webkit.framework DOMHTMLLegendElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLLEGENDELEMENT_PAS_C}
{$define DOMHTMLLEGENDELEMENT_PAS_C}

{ DOMHTMLLegendElement }
  DOMHTMLLegendElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLLegendElement; message 'alloc';

    function form: DOMHTMLFormElement; message 'form';
    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end; external;

{$endif}
{$endif}
