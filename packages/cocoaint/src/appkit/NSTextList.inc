{ Parsed from Appkit.framework NSTextList.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSTEXTLIST_PAS_T}
{$define NSTEXTLIST_PAS_T}

{ Constants }

const
  NSTextListPrependEnclosingMarker = 1  shl  0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTLIST_PAS_R}
{$define NSTEXTLIST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTLIST_PAS_F}
{$define NSTEXTLIST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTLIST_PAS_S}
{$define NSTEXTLIST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextList = objcclass;
  NSTextListPointer = ^NSTextList;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTLIST_PAS_C}
{$define NSTEXTLIST_PAS_C}

{ NSTextList }
  NSTextList = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __markerFormat: NSString;
    __listFlags: NSUInteger;
    __listPrimary: Pointer;
    __listSecondary: Pointer;
    
  public
    class function alloc: NSTextList; message 'alloc';

    function initWithMarkerFormat_options(format: NSString; mask: NSUInteger): id; message 'initWithMarkerFormat:options:';
    function markerFormat: NSString; message 'markerFormat';
    function listOptions: NSUInteger; message 'listOptions';
    function markerForItemNumber(itemNum: NSInteger): NSString; message 'markerForItemNumber:';
  end; external;

{$endif}
{$endif}
