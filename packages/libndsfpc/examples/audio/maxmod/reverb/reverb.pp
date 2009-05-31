program reverb;
{$L build/soundbank.bin.o}

{$mode objfpc}

uses
  ctypes, nds9, maxmod9;

var
  soundbank_bin_end: array [0..0] of cuint8; cvar; external;
  soundbank_bin: array [0..0] of cuint8; cvar; external;
  soundbank_bin_size: cuint32; cvar; external;

const
  SFX_DAM	= 0;
  MOD_TEMPEST_ZEN_BOWLING	= 0;
  MSL_NSONGS	= 1;
  MSL_NSAMPS	= 31;
  MSL_BANKSIZE	= 32;

  SCREEN_TEXT = #10 +

  ' Maxmod Reverb Example'#10#10 + 

  ' Press A to toggle reverb. '#10#10;

	rv_delay_left = 500;		// milliseconds
	rv_delay_right = 520;
	rv_rate = 32768;			// Hertz
	rv_format = 16;				// 16-bit

procedure setupReverb();
var
  rv_buffer_left,
	 rv_buffer_right: pointer;
	rv_size_left, 
   rv_size_right: cint;
	config: mm_reverb_cfg;
begin
	//---------------------------------------------------------
	// Enable reverb system
	//---------------------------------------------------------
	mmReverbEnable();
	
	//---------------------------------------------------------
	// Calculate amount required and allocate memory
	// for both reverb channels
	//---------------------------------------------------------
	rv_size_left := mmReverbBufferSize( rv_format, rv_rate, rv_delay_left );
	rv_size_right := mmReverbBufferSize( rv_format, rv_rate, rv_delay_right );
	
	GetMem(rv_buffer_left, rv_size_left * 4);
	GetMem(rv_buffer_right, rv_size_right * 4);
	
	//---------------------------------------------------------
	// Configure reverb settings
	//---------------------------------------------------------
	
	config.flags := MMRF_MEMORY or MMRF_DELAY or MMRF_FEEDBACK or
				   MMRF_PANNING or MMRF_DRYLEFT or MMRF_DRYRIGHT or
				   MMRF_RATE or MMRF_16BITLEFT or MMRF_16BITRIGHT or
				   MMRF_INVERSEPAN or MMRF_BOTH;

	config.memory := rv_buffer_left;		// Set memory target (for left)
	config.delay := rv_size_left;		// Set delay (for left)
	config.feedback := 1024;				// Set feedback to 50% (for both)
	config.panning := 0;					// Set panning to 0% (and inversed (100%) for right channel)
	config.rate := 16777216 div rv_rate;	// Set sampling rate for both channels
	mmReverbConfigure(@config);		// Run configuration...

	//---------------------------------------------------------
	// Configure remaining parameters (right channel memory
	// and delay)
	//---------------------------------------------------------
	config.flags := MMRF_MEMORY or MMRF_DELAY or MMRF_RIGHT;
	config.delay := rv_size_right;
	config.memory := rv_buffer_right;
	mmReverbConfigure( @config );
	
end;

var
  reverb_is_started: cint = 0;
  keys: cint;


begin
	//---------------------------------------------------------
	// setup console
	//---------------------------------------------------------
	consoleDemoInit();
	
	// give it a dark blue backdrop
	BG_PALETTE_SUB[0] := RGB15( 0, 0, 13 );

	//---------------------------------------------------------
	// init maxmod with default settings
	//---------------------------------------------------------
	mmInitDefaultMem(mm_addr(@soundbank_bin));
	
	//---------------------------------------------------------
	// setup and configure the reverb system
	//---------------------------------------------------------
	setupReverb();
	
	//---------------------------------------------------------
	// load and play test module
	//---------------------------------------------------------
	mmLoad( MOD_TEMPEST_ZEN_BOWLING );
	mmStart( MOD_TEMPEST_ZEN_BOWLING, MM_PLAY_LOOP );
	
	//---------------------------------------------------------
	// display screen info
	//---------------------------------------------------------
	iprintf(SCREEN_TEXT);
	
	//---------------------------------------------------------
	// main loop
	//---------------------------------------------------------
	while true do
	begin
		//-----------------------------------------------------
		// get new keypad input
		//-----------------------------------------------------
		scanKeys();
		keys := keysDown();

		//-----------------------------------------------------
		// A: toggle reverb
		//-----------------------------------------------------
		if( keys and KEY_A ) <> 0 then
		begin
			if (reverb_is_started) = 0 then
			begin
				// start reverb
				mmReverbStart( MMRC_BOTH );
				
				// lower module volume & tempo
				// this creates a cool atmosphere
				mmSetModuleVolume( 550 );
				mmSetModuleTempo( 800 );
				
				reverb_is_started := 1;
			end else 
			begin
				// stop reverb and restore volume and tempo
				mmReverbStop( MMRC_BOTH );
				mmSetModuleVolume( $400 );
				mmSetModuleTempo( $400 );
				
				reverb_is_started := 0;
			end;
		end;
		
		//-----------------------------------------------------
		// wait until next frame
		//-----------------------------------------------------
		swiWaitForVBlank();
	end;
	
end.
