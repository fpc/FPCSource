{ Parsed from Webkit.framework DOMHTMLBodyElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLBODYELEMENT_PAS_C}
{$define DOMHTMLBODYELEMENT_PAS_C}

{ DOMHTMLBodyElement }
  DOMHTMLBodyElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLBodyElement; message 'alloc';

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
  end; external;

{$endif}
{$endif}
