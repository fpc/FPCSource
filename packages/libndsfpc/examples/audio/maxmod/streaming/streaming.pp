program Streaming;

{$mode objfpc}

uses
  ctypes, nds9, maxmod9;
  

var
  sine: cint;  // sine position
  lfo: cint;   // LFO position

const
  // waveform base frequency
  sine_freq = 500;
  
  // LFO frequency
  lfo_freq = 3;
  
  // LFO output shift amount
  lfo_shift = 4;
  
  // blue backdrop
  bg_colour = 13 shl 10;
  
  // red cpu usage
  cpu_colour = 31;


function on_stream_request(aLength: mm_word; aDest: mm_addr; aFormat: mm_stream_formats): mm_word; 
var
  target: pcint16;
  len: cint;
  sample: cint;
begin	
	target := aDest;
	
	//------------------------------------------------------------
	// synthensize a sine wave with an LFO applied to the pitch
	// the stereo data is interleaved
	//------------------------------------------------------------
	len := aLength;
  while len <> 0 do
  begin
		sample := sinLerp(sine);
		
		// output sample for left
		target^ := sample;
		inc(target);
   
		// output inverted sample for right
		target^ := -sample;
		inc(target);
   
		sine := sine + sine_freq + (sinLerp(lfo) shr lfo_shift);
		lfo := (lfo + lfo_freq);
  
    dec(len);
  end;
	
	result := aLength;
end;

var
  sys: mm_ds_system;
  mystream: mm_stream;
begin	

	//----------------------------------------------------------------
	// print out some stuff
	//----------------------------------------------------------------
	consoleDemoInit();
	iprintf( #10'    Maxmod Streaming Example   '#10);

	//----------------------------------------------------------------
	// initialize maxmod without any soundbank (unusual setup)
	//----------------------------------------------------------------
	sys.mod_count 			:= 0;
	sys.samp_count			:= 0;
	sys.mem_bank			:= nil;
	sys.fifo_channel		:= FIFO_MAXMOD;
	mmInit( @sys );
	
	//----------------------------------------------------------------
	// open stream
	//----------------------------------------------------------------
	
	mystream.sampling_rate	:= 25000;					// sampling rate = 25khz
	mystream.buffer_length	:= 1200;						// buffer length = 1200 samples
	mystream.callback		:= @on_stream_request;		// set callback function
	mystream.format			:= MM_STREAM_16BIT_STEREO;	// format = stereo 16-bit
	mystream.timer			:= MM_TIMER0;				// use hardware timer 0
	mystream.manual			:= 1;						// use manual filling
	mmStreamOpen( @mystream );
		
	//----------------------------------------------------------------
	// when using 'automatic' filling, your callback will be triggered
	// every time half of the wave buffer is processed.
	//
	// so: 
	// 25000 (rate)
	// ----- = ~21 Hz for a full pass, and ~42hz for half pass
	// 1200  (length)
	//----------------------------------------------------------------
	// with 'manual' filling, you must call mmStreamUpdate
	// periodically (and often enough to avoid buffer underruns)
	//----------------------------------------------------------------
	
	SetYtrigger( 0 );
	irqEnable( IRQ_VCOUNT );
	
	while true do
	begin
		// wait until line 0
		swiIntrWait( 0, IRQ_VCOUNT);
		
		// update stream
		mmStreamUpdate();
		
		// restore backdrop (some lines were drawn with another colour to show cpu usage)
		BG_PALETTE_SUB[0] := bg_colour;
		
		// wait until next frame
		swiWaitForVBlank();
		
		// set backdrop to show cpu usage
		BG_PALETTE_SUB[0] := cpu_colour;
	end;
		
end.
