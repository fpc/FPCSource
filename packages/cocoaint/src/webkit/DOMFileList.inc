{ Parsed from Webkit.framework DOMFileList.h }

{$ifdef TYPES}
{$ifndef DOMFILELIST_PAS_T}
{$define DOMFILELIST_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMFILELIST_PAS_R}
{$define DOMFILELIST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMFILELIST_PAS_F}
{$define DOMFILELIST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMFILELIST_PAS_S}
{$define DOMFILELIST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMFileList = objcclass;
  DOMFileListPointer = ^DOMFileList;
  DOMFileListPtr = DOMFileListPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMFILELIST_PAS_C}
{$define DOMFILELIST_PAS_C}

{ DOMFileList }
  DOMFileList = objcclass external (DOMObject)
    
  public
    function length: cuint; message 'length';
    function item(index: cuint): DOMFile; message 'item:';
  end;

{$endif}
{$endif}
