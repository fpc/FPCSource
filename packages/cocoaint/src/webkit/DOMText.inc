{ Parsed from Webkit.framework DOMText.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMTEXT_PAS_T}
{$define DOMTEXT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMTEXT_PAS_R}
{$define DOMTEXT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMTEXT_PAS_F}
{$define DOMTEXT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMTEXT_PAS_S}
{$define DOMTEXT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMText = objcclass;
  DOMTextPointer = ^DOMText;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMTEXT_PAS_C}
{$define DOMTEXT_PAS_C}

{ DOMText }
  DOMText = objcclass(DOMCharacterData)
    
  public
    class function alloc: DOMText; message 'alloc';

    function splitText(offset: cuint): DOMText; message 'splitText:';
  end; external;

{$endif}
{$endif}
