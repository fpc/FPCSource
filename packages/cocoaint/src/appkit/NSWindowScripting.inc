{ Parsed from AppKit.framework NSWindowScripting.h }

{$ifdef CLASSES}

type
  NSWindow_NSScripting = objccategory external name 'NSScripting' (NSWindow)
    function hasCloseBox: ObjCBOOL; message 'hasCloseBox';
    function hasTitleBar: ObjCBOOL; message 'hasTitleBar';
    function isFloatingPanel: ObjCBOOL; message 'isFloatingPanel';
    function isMiniaturizable: ObjCBOOL; message 'isMiniaturizable';
    function isModalPanel: ObjCBOOL; message 'isModalPanel';
    function isResizable: ObjCBOOL; message 'isResizable';
    function isZoomable: ObjCBOOL; message 'isZoomable';
    procedure setOrderedIndex(newValue: NSInteger); message 'setOrderedIndex:';
    function orderedIndex: NSInteger; message 'orderedIndex';
    procedure setIsMiniaturized (flag: ObjCBOOL); message 'setIsMiniaturized:';
    procedure setIsVisible (flag: ObjCBOOL); message 'setIsVisible:';
    procedure setIsZoomed (flag: ObjCBOOL); message 'setIsZoomed:';
    function handleCloseScriptCommand (command: NSCloseCommand): id; message 'handleCloseScriptCommand:';
    function handlePrintScriptCommand (command: NSScriptCommand): id; message 'handlePrintScriptCommand:';
    function handleSaveScriptCommand (command: NSScriptCommand): id; message 'handleSaveScriptCommand:';
  end;
{$endif}

