program ConsoleWindows;

{$mode objfpc}

uses
  ctypes, nds9;

const border =  '------------' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '|          |' +
                '------------';

var
  touch: touchPosition;
	left: pPrintConsole;
	right: PrintConsole;
  keys: integer;
begin

	left := consoleDemoInit();
	right := left^;

	consoleSetWindow(left, 15,1,12,16);
	consoleSetWindow(@right, 1,1,12,16);

	consoleSelect(left);
	iprintf(border);
	consoleSelect(@right);
	iprintf(border);

	consoleSetWindow(left, 2,2,10,14);
	consoleSetWindow(@right,16,2,10,14);

	while true do
	begin
		scanKeys();
		keys := keysHeld();

		if (keys and KEY_TOUCH) <> 0 then
		begin
			touchRead(touch);

			if (touch.px < 128) then
				consoleSelect(left)
			else
				consoleSelect(@right);

			iprintf(#10'T: %i', touch.px);
		end;

		swiWaitForVBlank();
	end;

end.
