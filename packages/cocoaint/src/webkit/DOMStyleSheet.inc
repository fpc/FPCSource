{ Parsed from Webkit.framework DOMStyleSheet.h }

{$ifdef TYPES}
{$ifndef DOMSTYLESHEET_PAS_T}
{$define DOMSTYLESHEET_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMSTYLESHEET_PAS_R}
{$define DOMSTYLESHEET_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMSTYLESHEET_PAS_F}
{$define DOMSTYLESHEET_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMSTYLESHEET_PAS_S}
{$define DOMSTYLESHEET_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMStyleSheet = objcclass;
  DOMStyleSheetPointer = ^DOMStyleSheet;
  DOMStyleSheetPtr = DOMStyleSheetPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMSTYLESHEET_PAS_C}
{$define DOMSTYLESHEET_PAS_C}

{ DOMStyleSheet }
  DOMStyleSheet = objcclass external (DOMObject)
    
  public
    function type_: NSString; message 'type_';
    procedure setDisabled (newValue: Boolean); message 'setDisabled:';
    function disabled: Boolean; message 'disabled';
    function ownerNode: DOMNode; message 'ownerNode';
    function parentStyleSheet: DOMStyleSheet; message 'parentStyleSheet';
    function href: NSString; message 'href';
    function title: NSString; message 'title';
    function media: DOMMediaList; message 'media';
  end;

{$endif}
{$endif}
