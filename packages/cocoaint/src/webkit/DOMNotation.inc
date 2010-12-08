{ Parsed from Webkit.framework DOMNotation.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMNOTATION_PAS_T}
{$define DOMNOTATION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMNOTATION_PAS_R}
{$define DOMNOTATION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMNOTATION_PAS_F}
{$define DOMNOTATION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMNOTATION_PAS_S}
{$define DOMNOTATION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMNotation = objcclass;
  DOMNotationPointer = ^DOMNotation;
  DOMNotationPtr = DOMNotationPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMNOTATION_PAS_C}
{$define DOMNOTATION_PAS_C}

{ DOMNotation }
  DOMNotation = objcclass(DOMNode)
    
  public
    class function alloc: DOMNotation; message 'alloc';

    function publicId: NSString; message 'publicId';
    function systemId: NSString; message 'systemId';
  end; external;

{$endif}
{$endif}
