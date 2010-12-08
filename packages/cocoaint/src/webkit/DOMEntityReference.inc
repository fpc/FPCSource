{ Parsed from Webkit.framework DOMEntityReference.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMENTITYREFERENCE_PAS_T}
{$define DOMENTITYREFERENCE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMENTITYREFERENCE_PAS_R}
{$define DOMENTITYREFERENCE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMENTITYREFERENCE_PAS_F}
{$define DOMENTITYREFERENCE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMENTITYREFERENCE_PAS_S}
{$define DOMENTITYREFERENCE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMEntityReference = objcclass;
  DOMEntityReferencePointer = ^DOMEntityReference;
  DOMEntityReferencePtr = DOMEntityReferencePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMENTITYREFERENCE_PAS_C}
{$define DOMENTITYREFERENCE_PAS_C}

{ DOMEntityReference }
  DOMEntityReference = objcclass(DOMNode)
    
  public
    class function alloc: DOMEntityReference; message 'alloc';
  end; external;

{$endif}
{$endif}
