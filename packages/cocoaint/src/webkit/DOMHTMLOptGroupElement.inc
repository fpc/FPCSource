{ Parsed from Webkit.framework DOMHTMLOptGroupElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLOPTGROUPELEMENT_PAS_T}
{$define DOMHTMLOPTGROUPELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLOPTGROUPELEMENT_PAS_R}
{$define DOMHTMLOPTGROUPELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLOPTGROUPELEMENT_PAS_F}
{$define DOMHTMLOPTGROUPELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLOPTGROUPELEMENT_PAS_S}
{$define DOMHTMLOPTGROUPELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLOptGroupElement = objcclass;
  DOMHTMLOptGroupElementPointer = ^DOMHTMLOptGroupElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLOPTGROUPELEMENT_PAS_C}
{$define DOMHTMLOPTGROUPELEMENT_PAS_C}

{ DOMHTMLOptGroupElement }
  DOMHTMLOptGroupElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLOptGroupElement; message 'alloc';

    procedure setLabel (newValue: NSString); message 'setLabel:';
    function label_: NSString; message 'label_';
  end; external;

{$endif}
{$endif}
