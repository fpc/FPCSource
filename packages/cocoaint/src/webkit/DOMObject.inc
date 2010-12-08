{ Parsed from Webkit.framework DOMObject.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMOBJECT_PAS_T}
{$define DOMOBJECT_PAS_T}

{ Types }
type
  DOMObjectInternal = record end;
  DOMObjectInternalPtr = ^DOMObjectInternal;

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
  DOMObjectPtr = DOMObjectPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMOBJECT_PAS_C}
{$define DOMOBJECT_PAS_C}

{ DOMObject }
  DOMObject = objcclass(WebScriptObject, NSCopyingProtocol)
  private
    _internal: DOMObjectInternalPtr;
    
  public
    class function alloc: DOMObject; message 'alloc';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end; external;

{ DOMLinkStyleCategory }
  DOMLinkStyleCategory = objccategory(DOMObject)
    function sheet: DOMStyleSheet; message 'sheet';
  end; external;

{$endif}
{$endif}
