{ %norun }

{$mode objfpc}
{$modeswitch objectivec1}

unit tobjc37;
interface
uses
	CocoaAll;

type
	MyWindow = objcclass(NSWindow)
		procedure awakeFromNib; override;
	end;
	
	
implementation

type
	MyView = objcclass(NSView)
		procedure awakeFromNib; override;
	end;

procedure MyWindow.awakeFromNib;
begin
end;

procedure MyView.awakeFromNib;
begin
end;

procedure test;
var
  w: mywindow;
  v: myview;
begin
  w:=mywindow.alloc.init;
  w.awakefromnib;
  w.release;
  v:=myview.alloc.init;
  v.awakefromnib;
  v.release;
end;
	
end.
