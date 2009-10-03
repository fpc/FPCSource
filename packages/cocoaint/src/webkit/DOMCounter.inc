{ Parsed from Webkit.framework DOMCounter.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMCOUNTER_PAS_T}
{$define DOMCOUNTER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCOUNTER_PAS_R}
{$define DOMCOUNTER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCOUNTER_PAS_F}
{$define DOMCOUNTER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCOUNTER_PAS_S}
{$define DOMCOUNTER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCounter = objcclass;
  DOMCounterPointer = ^DOMCounter;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCOUNTER_PAS_C}
{$define DOMCOUNTER_PAS_C}

{ DOMCounter }
  DOMCounter = objcclass(DOMObject)
    
  public
    class function alloc: DOMCounter; message 'alloc';

    function identifier: NSString; message 'identifier';
    function listStyle: NSString; message 'listStyle';
    function separator: NSString; message 'separator';
  end; external;

{$endif}
{$endif}
