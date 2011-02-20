{ Parsed from Webkit.framework DOMAbstractView.h }

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
  DOMAbstractView = objcclass external (DOMObject)
    
  public
    function document: DOMDocument; message 'document';
  end;

{$endif}
{$endif}
