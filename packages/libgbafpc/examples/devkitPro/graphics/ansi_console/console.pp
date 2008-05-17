program console;

uses
  gba;

begin
  // the vblank interrupt must be enabled for VBlankIntrWait() to work
  // since the default dispatcher handles the bios flags no vblank handler
  // is required
  irqInit();
  
  irqEnable(IRQ_VBLANK);
  
  // initialise the console
  // setting NULL & 0 for the font address & size uses the default font
  // The font should be a complete 1bit 8x8 ASCII font
  consoleInit(  0,		// charbase
                4,		// mapbase
                0,		// background number
                nil,	// font
                0, 		// font size
                15		// 16 color palette
             );

  // set the screen colors, color 0 is the background color
  // the foreground color is index 1 of the selected 16 color palette
  BG_COLORS[0] := RGB8(58,110,165);
  BG_COLORS[241] := RGB5(31,31,31);
  
  SetMode(MODE_0 or BG0_ON);
  
  // ansi escape sequence to clear screen and home cursor
  // /x1b[line;columnH
  iprintf(#27'[2J');
  
  // ansi escape sequence to set print co-ordinates
  // /x1b[line;columnH
  iprintf(#27'[10;10H' + 'Hello World!');
  
  // ansi escape sequence to move cursor up
  // /x1b[linesA
  iprintf(#27'[10A' + 'Line 0');
  
  // ansi escape sequence to move cursor left
  // /x1b[columnsD
  iprintf(#27'[28D' + 'Column 0');
  
  // ansi escape sequence to move cursor down
  // /x1b[linesB
  iprintf(#27'[19B' + 'Line 19');
  
  // ansi escape sequence to move cursor right
  // /x1b[columnsC
  iprintf(#27'[5C' + 'Column 20');
  
  while true do
    VBlankIntrWait();
  
end.

