program micrecord;

{$mode objfpc}

uses
  ctypes, nds9, maxmod9;

const
  //the record sample rate
  sample_rate = 8000;

var
  //buffer to hold sound data for playback
  sound_buffer: pcuint16 = nil;

  //buffer which is written to by the arm7
  mic_buffer: pcuint16 = nil;

  //the length of the current data
  data_length: cuint32 = 0;

  //enough hold 5 seconds of 16bit audio
  sound_buffer_size: cuint32 = sample_rate * 2 * 5;

  //the mic buffer sent to the arm7 is a double buffer
  //every time it is half full the arm7 signals us so we can read the
  //data.  I want the buffer to swap about once per frame so i chose a
  //buffer size large enough to hold two frames of 16bit mic data
  mic_buffer_size: cuint32 = sample_rate * 2 div 30;


//mic stream handler
procedure micHandler(data: pointer; length: cint);
begin
	if (sound_buffer = nil) or (data_length > sound_buffer_size) then 
    exit;
	
	DC_InvalidateRange(data, length);

	dmaCopy(data, pcuint8(sound_buffer) + data_length, length);
	
	data_length := data_length + length;

	iprintf('.');
	
end;

procedure rec();
begin
	data_length := 0;
	soundMicRecord(mic_buffer, mic_buffer_size, MicFormat_12Bit, sample_rate, @micHandler);
end;

procedure play();
begin
	soundMicOff();
	soundEnable();
	iprintf('data length: %i'#10, data_length);
	soundPlaySample(sound_buffer, SoundFormat_16Bit, data_length, sample_rate, 127, 64, false, 0);
end;

var
	key: cint;
	recording: cbool = false;
begin
	getmem(sound_buffer, sound_buffer_size);

	getmem(mic_buffer, mic_buffer_size);

	consoleDemoInit();

	iprintf('Press A to record / play'#10);

	while true do 
	begin
		scanKeys();
		key := keysDown();

		if(key and KEY_A )<> 0 then
		begin
			if recording then 
			begin
        play();
			 iprintf('playing');
      end else
      begin 
        rec();
        iprintf('recording');
			end;
      recording := not recording;
		end;

		swiWaitForVBlank();

	end;
end.
