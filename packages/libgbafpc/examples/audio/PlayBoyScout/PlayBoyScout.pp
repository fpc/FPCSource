program PlayBoyScout;

{$l build/ScoutSplash.pcx.o}
{$l build/tune.bgf.o}

uses
  ctypes, gba;

{$include inc/ScoutSplash.pcx.inc}
{$include inc/tune.bgf.inc}


var
  PaletteBuffer: array [0..255] of cuint16;
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
	BoyScoutSetMemoryArea(cuint32(GetMem(nBSSongSize)));

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

	DecodePCX(@ScoutSplash_pcx, pcuint16(VRAM), PaletteBuffer);

	FadeToPalette( PaletteBuffer, 60);


	while true do
		VBlankIntrWait();

	// This part will never be reached but just for completion
	// Free memory
	free(@BoyScoutGetMemoryArea);


end.


