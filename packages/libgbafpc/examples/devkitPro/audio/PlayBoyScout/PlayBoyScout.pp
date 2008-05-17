program PlayBoyScout;

{$l data/ScoutSplash.pcx.o}
{$l data/tune.bgf.o}

uses
  ctypes, gba;
  

var
  ScoutSplash_pcx: array [0..0] of cuint8; cvar; external;
  ScoutSplash_pcx_size: array [0..0] of cuint32; cvar; external;
  ScoutSplash_pcx_end: array [0..0] of cuint8; cvar; external;
  tune_bgf: array [0..0] of cuint8; cvar; external;
  tune_bgf_size: array [0..0] of cuint32; cvar; external;
  tune_bgf_end: array [0..0] of cuint8; cvar; external;


  PaletteBuffer: array [0..255] of u16;
  frame: cuint;
	nBSSongSize: cuint;

procedure VblankInterrupt();
begin
  BoyScoutUpdateSong();
  frame := frame + 1;
end;


begin
	// Set up the interrupt handlers
	irqInit();

	// Initialize BoyScout
	BoyScoutInitialize();

	// Get needed song memory
	nBSSongSize := BoyScoutGetNeededSongMemory(tune_bgf);

	// Allocate and set BoyScout memory area
	BoyScoutSetMemoryArea(u32(GetMem(nBSSongSize)));

	// Open song
	BoyScoutOpenSong(tune_bgf);

	// Play song and loop
	BoyScoutPlaySong(1);

	irqSet(IRQ_VBLANK, @VblankInterrupt);

	// Enable Vblank Interrupt to allow VblankIntrWait
	irqEnable(IRQ_VBLANK);
	// Allow Interrupts
	REG_IME^ := 1;

	SetMode( MODE_4 or BG2_ON );		// screen mode & background to display

	DecodePCX(@ScoutSplash_pcx, pu16(VRAM), PaletteBuffer);

	FadeToPalette( PaletteBuffer, 60);


	while true do
		VBlankIntrWait();

	// This part will never be reached but just for completion
	// Free memory
	free(@BoyScoutGetMemoryArea);


end.


