{ Parsed from Webkit.framework DOMCounter.h }
{ Version: 2.1.4 - Sun Jan 2 15:08:41 CET 2011 }


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
  DOMCounterPtr = DOMCounterPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCOUNTER_PAS_C}
{$define DOMCOUNTER_PAS_C}

{ DOMCounter }
  DOMCounter = objcclass external (DOMObject)
    
  public
    function identifier: NSString; message 'identifier';
    function listStyle: NSString; message 'listStyle';
    function separator: NSString; message 'separator';
  end;

{$endif}
{$endif}
