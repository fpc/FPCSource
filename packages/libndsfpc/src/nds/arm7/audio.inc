{$ifndef ARM7}
  {$error Audio is only available on the ARM7}
{$endif ARM7}


{$ifdef NDS_INTERFACE}
function SOUND_VOL(n: cint): cint; inline;	
function SOUND_FREQ(n: cint): cint; inline;	

const
  SOUND_ENABLE        = (1 shl 15);
  SOUND_REPEAT        = (1 shl 27);
  SOUND_ONE_SHOT      = (1 shl 28);
  SOUND_FORMAT_16BIT  = (1 shl 29);
  SOUND_FORMAT_8BIT   = (0 shl 29);
  SOUND_FORMAT_PSG    = (3 shl 29);
  SOUND_FORMAT_ADPCM  = (2 shl 29);
  
function SOUND_PAN(n: cint): cint; inline;

const
  SCHANNEL_ENABLE = (1 shl 31);

  REG_MASTER_VOLUME:  pcuint8 = pointer($04000500);
  REG_SOUNDCNT     :  pcuint16 = pointer($04000500);
  REG_SOUNDBIAS    :  pcuint32 = pointer($04000504);


function SCHANNEL_CR(n: cint): pcuint32; inline; 
function SCHANNEL_VOL(n: cint): pcuint8; inline;				
function SCHANNEL_PAN(n: cint): pcuint8; inline;				 
function SCHANNEL_SOURCE(n: cint): pcuint32; inline;			 
function SCHANNEL_TIMER(n: cint): pcuint16; inline;
function SCHANNEL_REPEAT_POINT(n: cint): pcuint16; inline;	 
function SCHANNEL_LENGTH(n: cint): pcuint32; inline;			 

const
  REG_SNDCAP0CNT    : pcuint8 = pointer($04000508);
  REG_SNDCAP1CNT    : pcuint8 = pointer($04000509);
 
  REG_SNDCAP0DAD  : pcuint32 = pointer($04000510);
  REG_SNDCAP0LEN  : pcuint16 = pointer($04000514);
  REG_SNDCAP1DAD  : pcuint32 = pointer($04000518);
  REG_SNDCAP1LEN  : pcuint16 = pointer($0400051C);

type
  MIC_BUF_SWAP_CB = procedure(completedBuffer: pcuint8; length: cint);


//---------------------------------------------------------------------------------
// DSi Registers
//---------------------------------------------------------------------------------
const
  REG_SNDEXTCNT : pcuint16 = pointer($04004700);
  REG_MICCNT    : pcuint16 = pointer($04004600);
  REG_MICDATA   : pcuint32 = pointer($04004604);

function SNDEXTCNT_RATIO(n: cint): cint32; inline;
const  
  SNDEXTCNT_FREQ_32KHZ  = (0 shl 13); // output freq 32.73kHz
  SNDEXTCNT_FREQ_47KHZ  = (1 shl 13); // output freq 47.61kHz
  SNDEXTCNT_MUTE        = (1 shl 14);
  SNDEXTCNT_ENABLE      = (1 shl 15);

function MICCNT_FORMAT(n: cint): cint; inline;
function MICCNT_FREQ_DIV(n: cint): cint; inline;

const
  MICCNT_EMPTY        = (1 shl 8);
  MICCNT_NOT_EMPTY    = (1 shl 9);
  MICCNT_MORE_DATA    = (1 shl 10);
  MICCNT_OVERRUN      = (1 shl 11);
  MICCNT_CLEAR_FIFO	  = (1 shl 12);
  MICCNT_ENABLE_IRQ   = (1 shl 13);
  MICCNT_ENABLE_IRQ2  = (1 shl 14);
  MICCNT_ENABLE       = (1 shl 15);



function micReadData8(): cuint8; cdecl; external;
function micReadData12(): cuint16; cdecl; external;

procedure micStartRecording(buffer: pcuint8; length, freq, timer: cint; eightBitSample: cbool; bufferSwapCallback: MIC_BUF_SWAP_CB); cdecl; external;

function micStopRecording(): cint; cdecl; external;

procedure micTimerHandler(); cdecl; external;

procedure micSetAmp(control, gain: cuint8); cdecl; external;

procedure micOn(); inline; 
procedure micOff(); inline; 

procedure installSoundFIFO(); cdecl; external;
{$endif NDS_INTERFACE}


{$ifdef NDS_IMPLEMENTATION}
function SOUND_VOL(n: cint): cint; inline;	
begin
  SOUND_VOL := (n);
end;

function SOUND_FREQ(n: cint): cint; inline;	
begin
  SOUND_FREQ := ((-$1000000 div (n)));
end;

function SOUND_PAN(n: cint): cint; inline;
begin
  SOUND_PAN := ((n) shl 16);
end;


function SCHANNEL_CR(n: cint): pcuint32; inline; 
begin
  SCHANNEL_CR := pcuint32($04000400 + ((n) shl 4));
end;

function SCHANNEL_VOL(n: cint): pcuint8; inline;				
begin
  SCHANNEL_VOL := pcuint8($04000400 + ((n) shl 4));
end;

function SCHANNEL_PAN(n: cint): pcuint8; inline;				 
begin
  SCHANNEL_PAN := pcuint8($04000402 + ((n) shl 4));
end;

function SCHANNEL_SOURCE(n: cint): pcuint32; inline;			 
begin
  SCHANNEL_SOURCE := pcuint32($04000404 + ((n) shl 4));
end;

function SCHANNEL_TIMER(n: cint): pcuint16; inline;
begin
  SCHANNEL_TIMER := pcuint16($04000408 + ((n) shl 4));
end;

function SCHANNEL_REPEAT_POINT(n: cint): pcuint16; inline;	 
begin
  SCHANNEL_REPEAT_POINT := pcuint16($0400040A + ((n) shl 4));
end;

function SCHANNEL_LENGTH(n: cint): pcuint32; inline;			 
begin
  SCHANNEL_LENGTH := pcuint32($0400040C + ((n) shl 4));
end;

procedure micOn(); inline; 
begin
  micSetAmp(PM_AMP_ON, PM_GAIN_160);
end;

procedure micOff(); inline; 
begin
  micSetAmp(PM_AMP_OFF, 0);
end;

function SNDEXTCNT_RATIO(n: cint): cint;  inline;
begin
  SNDEXTCNT_RATIO := (n and $F);
end;

function MICCNT_FORMAT(n: cint): cint; inline;
begin
  MICCNT_FORMAT := (n and 3); // unknown, always set to '2'
end;

function MICCNT_FREQ_DIV(n: cint): cint; inline;
begin
  MICCNT_FREQ_DIV := (n and 3) shl 2; // F/(n+1) where F is SNDEXTCNT output freq
end;
{$endif NDS_IMPLEMENTATION}
