{ Parsed from Appkit.framework NSOpenPanel.h }

{$ifdef TYPES}
{$ifndef NSOPENPANEL_PAS_T}
{$define NSOPENPANEL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSOPENPANEL_PAS_R}
{$define NSOPENPANEL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSOPENPANEL_PAS_F}
{$define NSOPENPANEL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOPENPANEL_PAS_S}
{$define NSOPENPANEL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSOpenPanel = objcclass;
  NSOpenPanelPointer = ^NSOpenPanel;
  NSOpenPanelPtr = NSOpenPanelPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOPENPANEL_PAS_C}
{$define NSOPENPANEL_PAS_C}

{ NSOpenPanel }
  NSOpenPanel = objcclass external (NSSavePanel)
  private
    _reservedOpenPanel: array[0..(4)-1] of char;
    _privateOpenPanel: Pointer;
    
  public
    class function openPanel: NSOpenPanel; message 'openPanel';
    function URLs: NSArray; message 'URLs';
    function resolvesAliases: ObjCBOOL; message 'resolvesAliases';
    procedure setResolvesAliases(flag: ObjCBOOL); message 'setResolvesAliases:';
    function canChooseDirectories: ObjCBOOL; message 'canChooseDirectories';
    procedure setCanChooseDirectories(flag: ObjCBOOL); message 'setCanChooseDirectories:';
    function allowsMultipleSelection: ObjCBOOL; message 'allowsMultipleSelection';
    procedure setAllowsMultipleSelection(flag: ObjCBOOL); message 'setAllowsMultipleSelection:';
    function canChooseFiles: ObjCBOOL; message 'canChooseFiles';
    procedure setCanChooseFiles(flag: ObjCBOOL); message 'setCanChooseFiles:';
  end;

{ NSDeprecated_NSOpenPanelCategory }
  NSDeprecated_NSOpenPanelCategory = objccategory external name 'NSDeprecated' (NSOpenPanel)
    function filenames: NSArray; message 'filenames';
    procedure beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo(path: NSString; name: NSString; fileTypes: NSArray; docWindow: NSWindow; delegate_: id; didEndSelector: SEL; contextInfo: Pointer); message 'beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:';
    procedure beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfo(path: NSString; name: NSString; fileTypes: NSArray; delegate_: id; didEndSelector: SEL; contextInfo: Pointer); message 'beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:';
    function runModalForDirectory_file_types(path: NSString; name: NSString; fileTypes: NSArray): NSInteger; message 'runModalForDirectory:file:types:';
    function runModalForTypes(fileTypes: NSArray): NSInteger; message 'runModalForTypes:';
  end;

{$endif}
{$endif}
