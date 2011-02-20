{ Parsed from Webkit.framework DOMEntityReference.h }

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
  DOMEntityReference = objcclass external (DOMNode)
    
  public
  end;

{$endif}
{$endif}
