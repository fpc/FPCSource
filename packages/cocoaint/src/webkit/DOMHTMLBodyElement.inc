{ Parsed from Webkit.framework DOMHTMLBodyElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLBODYELEMENT_PAS_T}
{$define DOMHTMLBODYELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLBODYELEMENT_PAS_R}
{$define DOMHTMLBODYELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLBODYELEMENT_PAS_F}
{$define DOMHTMLBODYELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLBODYELEMENT_PAS_S}
{$define DOMHTMLBODYELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLBodyElement = objcclass;
  DOMHTMLBodyElementPointer = ^DOMHTMLBodyElement;
  DOMHTMLBodyElementPtr = DOMHTMLBodyElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLBODYELEMENT_PAS_C}
{$define DOMHTMLBODYELEMENT_PAS_C}

{ DOMHTMLBodyElement }
  DOMHTMLBodyElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setALink (newValue: NSString); message 'setALink:';
    function aLink: NSString; message 'aLink';
    procedure setBackground (newValue: NSString); message 'setBackground:';
    function background: NSString; message 'background';
    procedure setBgColor (newValue: NSString); message 'setBgColor:';
    function bgColor: NSString; message 'bgColor';
    procedure setLink (newValue: NSString); message 'setLink:';
    function link: NSString; message 'link';
    procedure setText (newValue: NSString); message 'setText:';
    function text: NSString; message 'text';
    procedure setVLink (newValue: NSString); message 'setVLink:';
    function vLink: NSString; message 'vLink';
  end;

{$endif}
{$endif}
