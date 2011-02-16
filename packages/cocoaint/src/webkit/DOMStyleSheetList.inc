{ Parsed from Webkit.framework DOMStyleSheetList.h }

{$ifdef TYPES}
{$ifndef DOMSTYLESHEETLIST_PAS_T}
{$define DOMSTYLESHEETLIST_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMSTYLESHEETLIST_PAS_R}
{$define DOMSTYLESHEETLIST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMSTYLESHEETLIST_PAS_F}
{$define DOMSTYLESHEETLIST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMSTYLESHEETLIST_PAS_S}
{$define DOMSTYLESHEETLIST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMStyleSheetList = objcclass;
  DOMStyleSheetListPointer = ^DOMStyleSheetList;
  DOMStyleSheetListPtr = DOMStyleSheetListPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMSTYLESHEETLIST_PAS_C}
{$define DOMSTYLESHEETLIST_PAS_C}

{ DOMStyleSheetList }
  DOMStyleSheetList = objcclass external (DOMObject)
    
  public
    function length: cuint; message 'length';
    function item(index: cuint): DOMStyleSheet; message 'item:';
  end;

{$endif}
{$endif}
