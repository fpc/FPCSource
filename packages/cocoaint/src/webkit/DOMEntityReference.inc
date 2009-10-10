{ Parsed from Webkit.framework DOMEntityReference.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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
