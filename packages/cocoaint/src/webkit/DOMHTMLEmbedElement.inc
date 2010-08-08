{ Parsed from Webkit.framework DOMHTMLEmbedElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLEMBEDELEMENT_PAS_T}
{$define DOMHTMLEMBEDELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLEMBEDELEMENT_PAS_R}
{$define DOMHTMLEMBEDELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLEMBEDELEMENT_PAS_F}
{$define DOMHTMLEMBEDELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLEMBEDELEMENT_PAS_S}
{$define DOMHTMLEMBEDELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLEmbedElement = objcclass;
  DOMHTMLEmbedElementPointer = ^DOMHTMLEmbedElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLEMBEDELEMENT_PAS_C}
{$define DOMHTMLEMBEDELEMENT_PAS_C}

{ DOMHTMLEmbedElement }
  DOMHTMLEmbedElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLEmbedElement; message 'alloc';

    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
  end; external;

{$endif}
{$endif}
