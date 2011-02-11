{ Parsed from Webkit.framework DOMFile.h }
{ Version: 2.1.5 - Fri Feb 11 21:46:20 CET 2011 }


{$ifdef TYPES}
{$ifndef DOMFILE_PAS_T}
{$define DOMFILE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMFILE_PAS_R}
{$define DOMFILE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMFILE_PAS_F}
{$define DOMFILE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMFILE_PAS_S}
{$define DOMFILE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMFile = objcclass;
  DOMFilePointer = ^DOMFile;
  DOMFilePtr = DOMFilePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMFILE_PAS_C}
{$define DOMFILE_PAS_C}

{ DOMFile }
  DOMFile = objcclass external (DOMBlob)
    
  public
    function name: NSString; message 'name';
    function fileName: NSString; message 'fileName';
    function fileSize: cuint; message 'fileSize';
  end;

{$endif}
{$endif}
