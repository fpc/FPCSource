{ Parsed from Webkit.framework DOMObject.h }

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
  DOMObject = objcclass external (WebScriptObject, NSCopyingProtocol)
  private
    _internal: DOMObjectInternalPtr;
    
  public

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{ DOMLinkStyleCategory }
  DOMLinkStyleCategory = objccategory external (DOMObject)
    function sheet: DOMStyleSheet; message 'sheet';
  end;

{$endif}
{$endif}
