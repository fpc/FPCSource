{ Parsed from Webkit.framework DOMDocumentFragment.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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
