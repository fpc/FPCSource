{ Parsed from Webkit.framework DOMEntity.h }

{$ifdef TYPES}
{$ifndef DOMENTITY_PAS_T}
{$define DOMENTITY_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMENTITY_PAS_R}
{$define DOMENTITY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMENTITY_PAS_F}
{$define DOMENTITY_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMENTITY_PAS_S}
{$define DOMENTITY_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMEntity = objcclass;
  DOMEntityPointer = ^DOMEntity;
  DOMEntityPtr = DOMEntityPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMENTITY_PAS_C}
{$define DOMENTITY_PAS_C}

{ DOMEntity }
  DOMEntity = objcclass external (DOMNode)
    
  public
    function publicId: NSString; message 'publicId';
    function systemId: NSString; message 'systemId';
    function notationName: NSString; message 'notationName';
  end;

{$endif}
{$endif}
