{ Parsed from Webkit.framework DOMDocumentFragment.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMDOCUMENTFRAGMENT_PAS_T}
{$define DOMDOCUMENTFRAGMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMDOCUMENTFRAGMENT_PAS_R}
{$define DOMDOCUMENTFRAGMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMDOCUMENTFRAGMENT_PAS_F}
{$define DOMDOCUMENTFRAGMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMDOCUMENTFRAGMENT_PAS_S}
{$define DOMDOCUMENTFRAGMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMDocumentFragment = objcclass;
  DOMDocumentFragmentPointer = ^DOMDocumentFragment;
  DOMDocumentFragmentPtr = DOMDocumentFragmentPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMDOCUMENTFRAGMENT_PAS_C}
{$define DOMDOCUMENTFRAGMENT_PAS_C}

{ DOMDocumentFragment }
  DOMDocumentFragment = objcclass(DOMNode)
    
  public
    class function alloc: DOMDocumentFragment; message 'alloc';
  end; external;

{$endif}
{$endif}
