{ Parsed from Appkit.framework NSUserInterfaceItemSearching.h }
{ Version: 2.1.4 - Sun Jan 2 15:08:20 CET 2011 }


{$ifdef TYPES}
{$ifndef NSUSERINTERFACEITEMSEARCHING_PAS_T}
{$define NSUSERINTERFACEITEMSEARCHING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSUSERINTERFACEITEMSEARCHING_PAS_R}
{$define NSUSERINTERFACEITEMSEARCHING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSUSERINTERFACEITEMSEARCHING_PAS_F}
{$define NSUSERINTERFACEITEMSEARCHING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSUSERINTERFACEITEMSEARCHING_PAS_S}
{$define NSUSERINTERFACEITEMSEARCHING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSUserInterfaceItemSearchingProtocol = objcprotocol;

{$endif}

{$ifdef CLASSES}
{$ifndef NSUSERINTERFACEITEMSEARCHING_PAS_C}
{$define NSUSERINTERFACEITEMSEARCHING_PAS_C}

{ NSUserInterfaceItemSearchingCategory }
  NSUserInterfaceItemSearchingCategory = objccategory external (NSApplication)
    procedure registerUserInterfaceItemSearchHandler(handler: id); message 'registerUserInterfaceItemSearchHandler:';
    procedure unregisterUserInterfaceItemSearchHandler(handler: id); message 'unregisterUserInterfaceItemSearchHandler:';
    function searchString_inUserInterfaceItemString_searchRange_foundRange(searchString: NSString; stringToSearch: NSString; searchRange: NSRange; foundRange: NSRangePtr): Boolean; message 'searchString:inUserInterfaceItemString:searchRange:foundRange:';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSUSERINTERFACEITEMSEARCHING_PAS_P}
{$define NSUSERINTERFACEITEMSEARCHING_PAS_P}
  
{ NSUserInterfaceItemSearching Protocol }
  NSUserInterfaceItemSearchingProtocol = objcprotocol external name 'NSUserInterfaceItemSearching'
    function localizedTitlesForItem(item: id): NSArray; message 'localizedTitlesForItem:';
    procedure performActionForItem(item: id); message 'performActionForItem:';
    procedure showAllHelpTopicsForSearchString(searchString: NSString); message 'showAllHelpTopicsForSearchString:';
  end;
{$endif}
{$endif}
