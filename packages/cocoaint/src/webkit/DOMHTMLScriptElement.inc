{ Parsed from Webkit.framework DOMHTMLScriptElement.h }
{ Version: 2.1.5 - Fri Feb 11 21:46:20 CET 2011 }


{$ifdef TYPES}
{$ifndef DOMHTMLSCRIPTELEMENT_PAS_T}
{$define DOMHTMLSCRIPTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLSCRIPTELEMENT_PAS_R}
{$define DOMHTMLSCRIPTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLSCRIPTELEMENT_PAS_F}
{$define DOMHTMLSCRIPTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLSCRIPTELEMENT_PAS_S}
{$define DOMHTMLSCRIPTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLScriptElement = objcclass;
  DOMHTMLScriptElementPointer = ^DOMHTMLScriptElement;
  DOMHTMLScriptElementPtr = DOMHTMLScriptElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLSCRIPTELEMENT_PAS_C}
{$define DOMHTMLSCRIPTELEMENT_PAS_C}

{ DOMHTMLScriptElement }
  DOMHTMLScriptElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setText (newValue: NSString); message 'setText:';
    function text: NSString; message 'text';
    procedure setHtmlFor (newValue: NSString); message 'setHtmlFor:';
    function htmlFor: NSString; message 'htmlFor';
    procedure setEvent (newValue: NSString); message 'setEvent:';
    function event: NSString; message 'event';
    procedure setCharset (newValue: NSString); message 'setCharset:';
    function charset: NSString; message 'charset';
    procedure setDefer (newValue: Boolean); message 'setDefer:';
    function defer: Boolean; message 'defer';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
  end;

{$endif}
{$endif}
