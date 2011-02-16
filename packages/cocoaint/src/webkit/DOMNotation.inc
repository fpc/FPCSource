{ Parsed from Webkit.framework DOMNotation.h }

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
  DOMNotation = objcclass external (DOMNode)
    
  public
    function publicId: NSString; message 'publicId';
    function systemId: NSString; message 'systemId';
  end;

{$endif}
{$endif}
