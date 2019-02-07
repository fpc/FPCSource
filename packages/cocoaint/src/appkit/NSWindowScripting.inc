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
    function hasCloseBox: BOOL; message 'hasCloseBox';
    function hasTitleBar: BOOL; message 'hasTitleBar';
    function isFloatingPanel: BOOL; message 'isFloatingPanel';
    function isMiniaturizable: BOOL; message 'isMiniaturizable';
    function isModalPanel: BOOL; message 'isModalPanel';
    function isResizable: BOOL; message 'isResizable';
    function isZoomable: BOOL; message 'isZoomable';
    function orderedIndex: NSInteger; message 'orderedIndex';
    procedure setIsMiniaturized(flag: BOOL); message 'setIsMiniaturized:';
    procedure setIsVisible(flag: BOOL); message 'setIsVisible:';
    procedure setIsZoomed(flag: BOOL); message 'setIsZoomed:';
    procedure setOrderedIndex(index: NSInteger); message 'setOrderedIndex:';
    function handleCloseScriptCommand(command: NSCloseCommand): id; message 'handleCloseScriptCommand:';
    function handlePrintScriptCommand(command: NSScriptCommand): id; message 'handlePrintScriptCommand:';
    function handleSaveScriptCommand(command: NSScriptCommand): id; message 'handleSaveScriptCommand:';
  end;

{$endif}
{$endif}
