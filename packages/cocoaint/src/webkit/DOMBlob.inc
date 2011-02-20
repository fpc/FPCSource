{ Parsed from Webkit.framework DOMBlob.h }

{$ifdef TYPES}
{$ifndef DOMBLOB_PAS_T}
{$define DOMBLOB_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMBLOB_PAS_R}
{$define DOMBLOB_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMBLOB_PAS_F}
{$define DOMBLOB_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMBLOB_PAS_S}
{$define DOMBLOB_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMBlob = objcclass;
  DOMBlobPointer = ^DOMBlob;
  DOMBlobPtr = DOMBlobPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMBLOB_PAS_C}
{$define DOMBLOB_PAS_C}

{ DOMBlob }
  DOMBlob = objcclass external (DOMObject)
    
  public
    function size: cuint; message 'size';
  end;

{$endif}
{$endif}
