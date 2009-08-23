program BasicSound;
{$L build/soundbank.bin.o}

{$mode objfpc}

uses
  ctypes, nds9, maxmod9;

const
  SFX_AMBULANCE = 0;
  SFX_BOOM = 1;
  MOD_FLATOUTLIES = 0;
  MSL_NSONGS = 1;
  MSL_NSAMPS = 33;
  MSL_BANKSIZE = 34;

var
  soundbank_bin_end: array [0..0] of cuint8; cvar; external;
  soundbank_bin: array [0..0] of cuint8; cvar; external;
  soundbank_bin_size: cuint32; cvar; external;

  ambulance, boom: mm_sound_effect; 

  amb: mm_sfxhand;
	keys_pressed, keys_released: integer;

begin
  consoleDemoInit();

  mmInitDefaultMem(mm_addr(@soundbank_bin));
  
  // load the module
  mmLoad(MOD_FLATOUTLIES);
  
  // load sound effects
  mmLoadEffect(SFX_AMBULANCE);
  mmLoadEffect(SFX_BOOM);
  
  // Start playing module
  mmStart(MOD_FLATOUTLIES, MM_PLAY_LOOP);

  with ambulance do
  begin
    id := SFX_AMBULANCE;
    rate := trunc(1.0 * (1 shl 10));
    handle := 0;
    volume := 255;
    panning := 0;
  end;

  with boom do
  begin
    id := SFX_BOOM; 
    rate := trunc(1.0 * (1 shl 10));
    handle := 0;
    volume := 255;
    panning := 255;
  end;

	// ansi escape sequence to clear screen and home cursor
	// /x1b[line;columnH
	iprintf(#$1b'[2J');

	// ansi escape sequence to set print co-ordinates
	// /x1b[line;columnH
	iprintf(#$1b'[0;8HMaxMod Audio demo');
	iprintf(#$1b'[3;0HHold A for ambulance sound');
	iprintf(#$1b'[4;0HPress B for boom sound');
	
	// sound effect handle (for cancelling it later)
	amb := 0;

  while true do
	begin
		swiWaitForVBlank();
		scanKeys();

		keys_pressed := keysDown();
		keys_released := keysUp();

		// Play looping ambulance sound effect out of left speaker if A button is pressed
		if ( keys_pressed and KEY_A ) <> 0 then
		begin
    	amb := mmEffectEx(@ambulance);
    end;
    
		// stop ambulance sound when A button is released
		if ( keys_released and KEY_A ) <> 0 then
		begin
			mmEffectCancel(amb);
    end;

		// Play explosion sound effect out of right speaker if B button is pressed
		if ( keys_pressed and KEY_B ) <> 0 then
		begin
			mmEffectEx(@boom);
		end;
  end;
end.
