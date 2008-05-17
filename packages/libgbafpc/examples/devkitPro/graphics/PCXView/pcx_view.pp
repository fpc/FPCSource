program pcx_view;

uses
  ctypes, gba;

{$l data/splash.pcx.o}

var
  PaletteBuffer: array [0..255] of cuint16;
  frame: cuint;

  splash_pcx_end: array [0..0] of cuint8; cvar; external;
  splash_pcx: array [0..0] of cuint8; cvar; external;
  splash_pcx_size: array [0..0] of cuint32; cvar; external;


procedure VblankInterrupt();
begin
  frame := frame + 1;
  scanKeys();
end;



begin
  // Set up the interrupt handlers
  irqInit();
  irqSet(IRQ_VBLANK, @VblankInterrupt);
  
  // Enable Vblank Interrupt to allow VblankIntrWait
  irqEnable(IRQ_VBLANK);
  
  // Allow Interrupts
  REG_IME^ := 1;
  
  SetMode(MODE_4 or BG2_ON);		// screen mode & background to display
  
  DecodePCX(@splash_pcx, pcuint16(VRAM), @PaletteBuffer);
  
  FadeToPalette(PaletteBuffer, 60);
  
  while true do
    VBlankIntrWait();
end.


