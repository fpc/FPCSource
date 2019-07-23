{ Parsed from Appkit.framework NSWindowScripting.h }

{$ifdef TYPES}
{$ifndef NSWINDOWSCRIPTING_PAS_T}
{$define NSWINDOWSCRIPTING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSWINDOWSCRIPTING_PAS_R}
{$define NSWINDOWSCRIPTING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSWINDOWSCRIPTING_PAS_F}
{$define NSWINDOWSCRIPTING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSWINDOWSCRIPTING_PAS_S}
{$define NSWINDOWSCRIPTING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSWINDOWSCRIPTING_PAS_C}
{$define NSWINDOWSCRIPTING_PAS_C}

{ NSScripting_NSWindowCategory }
  NSScripting_NSWindowCategory = objccategory external name 'NSScripting' (NSWindow)
    function hasCloseBox: ObjCBOOL; message 'hasCloseBox';
    function hasTitleBar: ObjCBOOL; message 'hasTitleBar';
    function isFloatingPanel: ObjCBOOL; message 'isFloatingPanel';
    function isMiniaturizable: ObjCBOOL; message 'isMiniaturizable';
    function isModalPanel: ObjCBOOL; message 'isModalPanel';
    function isResizable: ObjCBOOL; message 'isResizable';
    function isZoomable: ObjCBOOL; message 'isZoomable';
    function orderedIndex: NSInteger; message 'orderedIndex';
    procedure setIsMiniaturized(flag: ObjCBOOL); message 'setIsMiniaturized:';
    procedure setIsVisible(flag: ObjCBOOL); message 'setIsVisible:';
    procedure setIsZoomed(flag: ObjCBOOL); message 'setIsZoomed:';
    procedure setOrderedIndex(index: NSInteger); message 'setOrderedIndex:';
    function handleCloseScriptCommand(command: NSCloseCommand): id; message 'handleCloseScriptCommand:';
    function handlePrintScriptCommand(command: NSScriptCommand): id; message 'handlePrintScriptCommand:';
    function handleSaveScriptCommand(command: NSScriptCommand): id; message 'handleSaveScriptCommand:';
  end;

{$endif}
{$endif}
