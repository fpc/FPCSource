{$MODE MACPAS}
{$APPTYPE GUI}

program HelloMac;

{Demo of a simple program which uses the classic Macintosh Toolbox.
 It also shows how to include a mac style resource file.}

USES
	MacOS;
	
{$R HelloMac.r}

var
	window: WindowPtr;

begin
	InitGraf(@qd.thePort);
	InitFonts;
	InitWindows;
	InitMenus;
	TEInit;
	InitDialogs(nil);
	InitCursor;

	window:= GetNewWindow(128, nil, WindowPtr(-1));
	if window <> nil then
		begin
			SetPort(window);
			MoveTo(20,20);
			DrawString('Hello Free Pascal on the Mac :-)');

			repeat
			until Button;
		end;
		
end.
