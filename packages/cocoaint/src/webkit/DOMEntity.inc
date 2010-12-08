{ Parsed from Webkit.framework DOMEntity.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMEntity = objcclass(DOMNode)
    
  public
    class function alloc: DOMEntity; message 'alloc';

    function publicId: NSString; message 'publicId';
    function systemId: NSString; message 'systemId';
    function notationName: NSString; message 'notationName';
  end; external;

{$endif}
{$endif}
