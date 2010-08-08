{ Parsed from Webkit.framework DOMCDATASection.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMCDATASECTION_PAS_T}
{$define DOMCDATASECTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCDATASECTION_PAS_R}
{$define DOMCDATASECTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCDATASECTION_PAS_F}
{$define DOMCDATASECTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCDATASECTION_PAS_S}
{$define DOMCDATASECTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCDATASection = objcclass;
  DOMCDATASectionPointer = ^DOMCDATASection;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCDATASECTION_PAS_C}
{$define DOMCDATASECTION_PAS_C}

{ DOMCDATASection }
  DOMCDATASection = objcclass(DOMText)
    
  public
    class function alloc: DOMCDATASection; message 'alloc';
  end; external;

{$endif}
{$endif}
