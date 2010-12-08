{ Parsed from Webkit.framework DOMHTMLEmbedElement.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMHTMLEmbedElementPtr = DOMHTMLEmbedElementPointer;

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
    procedure setHeight (newValue: cint); message 'setHeight:';
    function height: cint; message 'height';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    procedure setWidth (newValue: cint); message 'setWidth:';
    function width: cint; message 'width';
  end; external;

{$endif}
{$endif}
