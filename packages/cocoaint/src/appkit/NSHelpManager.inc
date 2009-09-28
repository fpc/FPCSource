{ Parsed from Appkit.framework NSHelpManager.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSHELPMANAGER_PAS_H}
{$define NSHELPMANAGER_PAS_H}
type
  NSHelpManagerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSHELPMANAGER_PAS_T}
{$define NSHELPMANAGER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSHELPMANAGER_PAS_R}
{$define NSHELPMANAGER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSHELPMANAGER_PAS_F}
{$define NSHELPMANAGER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSHELPMANAGER_PAS_S}
{$define NSHELPMANAGER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSHelpManager = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSHELPMANAGER_PAS_C}
{$define NSHELPMANAGER_PAS_C}

{ NSHelpManager }
  NSHelpManager = objcclass(NSObject)
  private
    __helpMapTable: NSMapTable;
    __keyMapTable: NSMapTable;
    __bundleMapTable: NSMapTable;
    __helpWindow: NSWindow;
    __shadowWindow: NSWindow;
    __evtWindow: NSWindow;
    __helpBundle: NSBundle;
    
  public
    class function alloc: NSHelpManager; message 'alloc';

    class function sharedHelpManager: NSHelpManager; message 'sharedHelpManager';
    class procedure setContextHelpModeActive(active: Boolean); message 'setContextHelpModeActive:';
    class function isContextHelpModeActive: Boolean; message 'isContextHelpModeActive';
    procedure setContextHelp_forObject(attrString: NSAttributedString; object_: id); message 'setContextHelp:forObject:';
    procedure removeContextHelpForObject(object_: id); message 'removeContextHelpForObject:';
    function contextHelpForObject(object_: id): NSAttributedString; message 'contextHelpForObject:';
    function showContextHelpForObject_locationHint(object_: id; pt: NSPoint): Boolean; message 'showContextHelpForObject:locationHint:';
    procedure openHelpAnchor_inBook(anchor: NSString; book: NSString); message 'openHelpAnchor:inBook:';
    procedure findString_inBook(query: NSString; book: NSString); message 'findString:inBook:';
  end; external;

{$endif}
{$endif}
