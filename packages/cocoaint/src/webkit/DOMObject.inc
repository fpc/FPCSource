{ Parsed from Webkit.framework DOMObject.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMOBJECT_PAS_T}
{$define DOMOBJECT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMOBJECT_PAS_R}
{$define DOMOBJECT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMOBJECT_PAS_F}
{$define DOMOBJECT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMOBJECT_PAS_S}
{$define DOMOBJECT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMObject = objcclass;
  DOMObjectPointer = ^DOMObject;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMOBJECT_PAS_C}
{$define DOMOBJECT_PAS_C}

{ DOMObject }
  DOMObject = objcclass(WebScriptObject, NSCopyingProtocol)
  private
    __internal: DOMObjectInternal;
    
  public
    class function alloc: DOMObject; message 'alloc';

    { Category: DOMLinkStyle }
    function sheet: DOMStyleSheet; message 'sheet';
  end; external;

{$endif}
{$endif}
