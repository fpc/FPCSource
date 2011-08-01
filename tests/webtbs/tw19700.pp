{ %target=darwin }
{ %norun }

{$mode objfpc}
{$modeswitch objectivec1}

program Main;
uses
    CocoaAll;

type
    TSomeView = objcclass (NSView)
        function canvasPointFromEvent (theEvent: NSEvent): NSPoint; message 'canvasPointFromEvent:';
    end;

function TSomeView.canvasPointFromEvent (theEvent: NSEvent): NSPoint;
begin
    result := convertPoint_fromView(theEvent.locationInWindow, nil);
end;

var
    argc: LongInt;
    argv: PPChar;
begin
    NSApplicationMain(argc, argv);
end.
