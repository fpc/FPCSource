{ Parsed from Webkit.framework DOMAbstractView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMABSTRACTVIEW_PAS_T}
{$define DOMABSTRACTVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMABSTRACTVIEW_PAS_R}
{$define DOMABSTRACTVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMABSTRACTVIEW_PAS_F}
{$define DOMABSTRACTVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMABSTRACTVIEW_PAS_S}
{$define DOMABSTRACTVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMAbstractView = objcclass;
  DOMAbstractViewPointer = ^DOMAbstractView;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMABSTRACTVIEW_PAS_C}
{$define DOMABSTRACTVIEW_PAS_C}

{ DOMAbstractView }
  DOMAbstractView = objcclass(DOMObject)
    
  public
    class function alloc: DOMAbstractView; message 'alloc';

    function document: DOMDocument; message 'document';
  end; external;

{$endif}
{$endif}
