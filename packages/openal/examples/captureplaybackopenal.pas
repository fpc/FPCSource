program CaptureAndPlayback;
{$mode objfpc}

uses
  sysutils, openal;

const
  Seconds = 5;                            //- We'll record for 5 seconds
  Frequency = 8000;                       //- Recording a frequency of 8000
  Format = AL_FORMAT_MONO16;              //- Recording 16-bit mono
  BufferSize = (Frequency*2)*(Seconds+1); //- (frequency * 2bytes(16-bit)) * seconds

var
  pCaptureDevice: pALCDevice;                  //- Device used to capture audio
  pPlaybackDevice: pALCDevice;                 //- Device used to playback audio
  pPlaybackContext: pALCContext;               //- Playback context
  pPlaybackSource: ALuint;                     //- Source for playback (in 3D sound would be located)
  CaptureBuffer: array[0..BufferSize] of ALubyte; //- Capture buffer external from openAL, sized as calculated above for 5 second recording
  PlayBuffer: ALInt;                           //- openAL internal playback buffer

  //- These two are used to control when to begin/end recording and playback
  Samples: ALInt;                //- count of the number of samples recorded
  PlayState: ALInt;              //- playback state

begin

  //- Find out which extensions are supported and print them (could error check for capture extension here)
  writeln('OpenAL Extensions = ',PChar(alGetString(AL_EXTENSIONS)));

  //- Print device specifiers for default devices
  writeln('ALC_DEFAULT_DEVICE_SPECIFIER = ',PChar(alcGetString(nil, ALC_DEFAULT_DEVICE_SPECIFIER )));
  writeln('ALC_CAPTURE_DEVICE_SPECIFIER = ',PChar(alcGetString(nil, ALC_CAPTURE_DEVICE_SPECIFIER )));

  //- Setup the input capture device (default device)
  writeln('Setting up alcCaptureOpenDevice to use default device');
  pcaptureDevice:=alcCaptureOpenDevice(nil, Frequency, Format, BufferSize);
  if pcaptureDevice=nil then begin
    raise exception.create('Capture device is nil!');
    exit;
  end;

  //- Setup the output player device (default device)
  writeln('Setting up alcOpenDevice to use default device');
  pPlaybackDevice:=alcOpenDevice(nil);
  if pPlaybackDevice=nil then
    raise exception.create('Playback device is nil!');

  //- Setup the output context, not sure why a context is needed, it just is ok?
  writeln('Setting up alcCreateContext');
  pPlaybackContext:=alcCreateContext(pPlaybackDevice,nil);
  writeln('Making the playback context the current context (alcMakeContextCurrent)');
  alcMakeContextCurrent(pPlaybackContext);

  // Generate Buffer(s) for playback
  alGetError(); // clear error code
  alGenBuffers( 1, @PlayBuffer );
  if alGetError() <> AL_NO_ERROR then
    raise exception.create('Ack!! Error creating playback buffer(s)!');

  // Generate Playback Sources - single source, not adjusting locational information for 3D sound
  writeln('Setting up playback source (alGenSources)');
  alGenSources(1, @pPlaybackSource);
  if alGetError() <> AL_NO_ERROR then
    raise exception.create('Ack an error creating a playback source!');


  //===========================================================================
  // Here's where we do the recording bit :)
  //===========================================================================

  //- Start capturing data
  alcCaptureStart(PCaptureDevice);
  repeat
    alcGetIntegerv(pCaptureDevice, ALC_CAPTURE_SAMPLES, ALsizei(sizeof(ALint)), @samples);
    Writeln(IntToStr(samples)+'/'+IntToStr(Seconds*Frequency)+' samples');
  until samples>=seconds*frequency;

  //- Capture the samples into our capture buffer
  alcCaptureSamples(pCaptureDevice, @CaptureBuffer, samples);

  //- Done recording
  alcCaptureStop(pCaptureDevice);


  //===========================================================================
  // Here's where we do the playback bit :)
  //===========================================================================

  //- Load up the playback buffer from our capture buffer
  alBufferData( PlayBuffer, Format, @CaptureBuffer, Samples*2, Frequency);

  //- Queue the buffer for playback
  alSourcei( pPlaybackSource, AL_BUFFER, PlayBuffer );

  //- Play the sound
  alSourcePlay(ALuint(pPlaybackSource));

  //- Wait for the player to stop
  repeat
    alGetSourcei( pPlaybackSource, AL_SOURCE_STATE, PlayState);
  until (PlayState <> AL_INITIAL) and (PlayState <> AL_PLAYING);


  //===========================================================================
  
  //- Shutdown/Clean up the playback stuff
  pPlaybackContext:=alcGetCurrentContext();
  pPlaybackDevice:=alcGetContextsDevice(pPlaybackContext);
  alcMakeContextCurrent(nil);
  alcDestroyContext(pPlaybackContext);
  alcCloseDevice(pPlaybackDevice);

  //- Shutdown/Clean up the capture stuff
  alcCaptureStop( pCaptureDevice );
  alcCaptureCloseDevice( pCaptureDevice );
end.
