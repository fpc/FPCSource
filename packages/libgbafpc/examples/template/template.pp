program template;

uses
  ctypes, gba;

begin

  // the vblank interrupt must be enabled for VBlankIntrWait() to work
  // since the default dispatcher handles the bios flags no vblank handler
  // is required
  irqInit();
  irqEnable(IRQ_VBLANK);

  consoleDemoInit();

  // ansi escape sequence to set print co-ordinates
  // /x1b[line;columnH
  iprintf(#$1b'[10;10HHello World!'#10);

  while true do
    VBlankIntrWait();

end.
