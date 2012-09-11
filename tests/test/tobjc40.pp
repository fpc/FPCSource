{ %target=darwin }
{ %opt=norun }

{$mode objfpc}
{$modeswitch objectivec1}
program Main;

type
	NSView = objcclass(NSObject)
		procedure setNeedsDisplay (flag: boolean); message 'setNeedsDisplay:';
	end;

type
	NSViewUtilities = objccategory (NSView)
		procedure setNeedsDisplay; message 'setNeedsDisplay'; overload;
	end;

procedure NSView.setNeedsDisplay (flag: boolean);
begin
end;

procedure NSViewUtilities.setNeedsDisplay;
begin
	setNeedsDisplay(true);
end;

begin
end.

