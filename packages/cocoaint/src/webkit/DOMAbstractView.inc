{ Parsed from Webkit.framework DOMAbstractView.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMAbstractViewPtr = DOMAbstractViewPointer;

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
