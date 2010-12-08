{ Parsed from Webkit.framework DOMFile.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMFile = objcclass(DOMObject)
    
  public
    class function alloc: DOMFile; message 'alloc';

    function fileName: NSString; message 'fileName';
    function fileSize: cuint; message 'fileSize';
  end; external;

{$endif}
{$endif}
