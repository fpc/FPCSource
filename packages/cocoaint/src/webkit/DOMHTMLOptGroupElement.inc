{ Parsed from Webkit.framework DOMHTMLOptGroupElement.h }

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
  DOMHTMLOptGroupElementPtr = DOMHTMLOptGroupElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLOPTGROUPELEMENT_PAS_C}
{$define DOMHTMLOPTGROUPELEMENT_PAS_C}

{ DOMHTMLOptGroupElement }
  DOMHTMLOptGroupElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setDisabled (newValue: Boolean); message 'setDisabled:';
    function disabled: Boolean; message 'disabled';
    procedure setLabel (newValue: NSString); message 'setLabel:';
    function label_: NSString; message 'label_';
  end;

{$endif}
{$endif}
