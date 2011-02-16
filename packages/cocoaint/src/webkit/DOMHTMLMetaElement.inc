{ Parsed from Webkit.framework DOMHTMLMetaElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLMETAELEMENT_PAS_T}
{$define DOMHTMLMETAELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLMETAELEMENT_PAS_R}
{$define DOMHTMLMETAELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLMETAELEMENT_PAS_F}
{$define DOMHTMLMETAELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLMETAELEMENT_PAS_S}
{$define DOMHTMLMETAELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLMetaElement = objcclass;
  DOMHTMLMetaElementPointer = ^DOMHTMLMetaElement;
  DOMHTMLMetaElementPtr = DOMHTMLMetaElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLMETAELEMENT_PAS_C}
{$define DOMHTMLMETAELEMENT_PAS_C}

{ DOMHTMLMetaElement }
  DOMHTMLMetaElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setContent (newValue: NSString); message 'setContent:';
    function content: NSString; message 'content';
    procedure setHttpEquiv (newValue: NSString); message 'setHttpEquiv:';
    function httpEquiv: NSString; message 'httpEquiv';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setScheme (newValue: NSString); message 'setScheme:';
    function scheme: NSString; message 'scheme';
  end;

{$endif}
{$endif}
