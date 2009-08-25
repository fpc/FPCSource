program AnsiConsole;

{$mode objfpc}

uses
  ctypes, nds9;

begin

  consoleDemoInit();

  // ansi escape sequence to clear screen and home cursor
  // #27 + [line;columnH
  iprintf(#27 + '[2J');

  // ansi escape sequence to set print co-ordinates
  // #27 + [line;columnH
  iprintf(#27 + '[10;10H' + 'Hello World!');

  // ansi escape sequence to move cursor up
  // #27 + [linesA
  iprintf(#27 + '[10A' + 'Line 0');

  // ansi escape sequence to move cursor left
  // #27 + [columnsD
  iprintf(#27 + '[28D' + 'Column 0');

  // ansi escape sequence to move cursor down
  // #27 + [linesB
  iprintf(#27 + '[19B' + 'Line 19');

  // ansi escape sequence to move cursor right
  // #27 + [columnsC
  iprintf(#27 + '[5C' + 'Column 20');

  while true do
    swiWaitForVBlank();

end.
