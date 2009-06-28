(*
  $Id$
  ------------------------------------------------------------------------------
  	Copyright (C) 2005
  		Jason Rogers (dovoto)
  		Dave Murphy (WinterMute)
  	
  	This software is provided 'as-is', without any express or implied
  	warranty.  In no event will the authors be held liable for any
  	damages arising from the use of this software.
  	
  	Permission is granted to anyone to use this software for any
  	purpose, including commercial applications, and to alter it and
  	redistribute it freely, subject to the following restrictions:
  	
  	1.	The origin of this software must not be misrepresented; you
  		must not claim that you wrote the original software. If you use
  		this software in a product, an acknowledgment in the product
  		documentation would be appreciated but is not required.
  	
  	2.	Altered source versions must be plainly marked as such, and
  		must not be misrepresented as being the original software.
  	
  	3.	This notice may not be removed or altered from any source
  		distribution.
  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

*)


{$ifdef NDS_INTERFACE}
 
type
  FifoChannels = integer;
const
  FIFO_PM     : FifoChannels = 0;
  FIFO_SOUND  : FifoChannels = 1;
  FIFO_SYSTEM : FifoChannels = 2;
  FIFO_RSDV_01: FifoChannels = 3;
  FIFO_RSVD_02: FifoChannels = 4;
  FIFO_RSVD_03: FifoChannels = 5;
  FIFO_DSWIFI : FifoChannels = 6;
  FIFO_MAXMOD : FifoChannels = 7;
  FIFO_USER_01: FifoChannels = 8;
  FIFO_USER_02: FifoChannels = 9;
  FIFO_USER_03: FifoChannels = 10;	    
  FIFO_USER_04: FifoChannels = 11;	    
  FIFO_USER_05: FifoChannels = 12;	    
  FIFO_USER_06: FifoChannels = 13;	    
  FIFO_USER_07: FifoChannels = 14;	    
  FIFO_USER_08: FifoChannels = 15;
  
  
type
  FifoSoundCommand = integer;
const
  SOUND_SET_PAN         : FifoSoundCommand = (0 shl 20);
  SOUND_SET_VOLUME      : FifoSoundCommand = (1 shl 20);
  SOUND_SET_FREQ        : FifoSoundCommand = (2 shl 20);
  SOUND_MASTER_ENABLE   : FifoSoundCommand = (3 shl 20);
  SOUND_MASTER_DISABLE  : FifoSoundCommand = (4 shl 20);
  SOUND_PAUSE           : FifoSoundCommand = (5 shl 20);
  SOUND_RESUME          : FifoSoundCommand = (6 shl 20);
  SOUND_KILL            : FifoSoundCommand = (7 shl 20);
  SOUND_SET_MASTER_VOL  : FifoSoundCommand = (8 shl 20);
  MIC_STOP              : FifoSoundCommand = (9 shl 20);

type
  FifoSystemCommands = integer;
const
  SYS_REQ_TOUCH : FifoSystemCommands = 0;
  SYS_REQ_KEYS  : FifoSystemCommands = 1;
  SYS_REQ_TIME  : FifoSystemCommands = 2;

type
  FifoPMCommands = integer;
const
  PM_REQ_ON             : FifoPMCommands = (1 shl 16);
  PM_REQ_OFF            : FifoPMCommands = (2 shl 16);
  PM_REQ_LED            : FifoPMCommands = (3 shl 16);
  PM_REQ_SLEEP          : FifoPMCommands = (4 shl 16);
  PM_REQ_SLEEP_DISABLE  : FifoPMCommands = (5 shl 16);
  PM_REQ_SLEEP_ENABLE   : FifoPMCommands = (6 shl 16);
	
type
  FifoWifiCommands = integer;
const
  WIFI_ENABLE   : FifoWifiCommands = 0;
  WIFI_DISABLE  : FifoWifiCommands = 1;
  WIFI_SYNC     : FifoWifiCommands = 2;

type
  PM_LedBlinkMode = integer;
const
  PM_LED_ON: PM_LedBlinkMode = (0 shl 4);
  PM_LED_SLEEP: PM_LedBlinkMode = (1 shl 4);
  PM_LED_BLINK: PM_LedBlinkMode = (3 shl 4);

const
  FIFO_CHANNEL_BITS = 4;
  FIFO_MAX_DATA_BYTES = 128;

{$ifdef ARM9}
  FIFO_BUFFER_ENTRIES = 512;
{$else ARM9} // ARM7
  FIFO_BUFFER_ENTRIES = 256;
{$endif ARM9}

var
  fifo_buffer: array [0..FIFO_BUFFER_ENTRIES*2-1] of cuint32; cvar; external;


{$ifdef FIFO_RIGOROUS_ERROR_CHECKING}
	function fifoError(value: pchar; array of const): integer; cdecl; external;// expected to be defined externally.
{$endif}

type  // FUNCTION(...):POINTER or PROCEDURE?!?
  FifoAddressHandlerFunc = function(address, userdata: pointer): pointer;
  FifoValue32HandlerFunc = function(value32: cuint32; userdata: pointer): pointer;
  FifoDatamsgHandlerFunc = function(num_bytes: integer; userdata: pointer): pointer;

function fifoInit(): cbool; cdecl; external;
function fifoSendAddress(channel: integer; address: pointer): cbool; cdecl; external;
function fifoSendValue32(channel: integer; value32: cuint32): cbool; cdecl; external;

function fifoSendDatamsg(channel, num_bytes: integer; data_array: pcuint8): cbool; cdecl; external;

function fifoSetAddressHandler(channel: integer; newhandler: FifoAddressHandlerFunc; userdata: pointer): cbool; cdecl; external;
function fifoSetValue32Handler(channel: integer; newhandler: FifoValue32HandlerFunc; userdata: pointer): cbool; cdecl; external;
function fifoSetDatamsgHandler(channel: integer; newhandler: FifoDatamsgHandlerFunc; userdata: pointer): cbool; cdecl; external;

procedure fifoForceAddressHandler(channel: integer; newhandler: FifoAddressHandlerFunc; userdata: pointer); cdecl; external;
procedure fifoForceValue32Handler(channel: integer; newhandler: FifoValue32HandlerFunc; userdata: pointer); cdecl; external;
procedure fifoForceDatamsgHandler(channel: integer; newhandler: FifoDatamsgHandlerFunc; userdata: pointer); cdecl; external;

function fifoCheckAddress(channel: integer): cbool; cdecl; external;
function fifoCheckValue32(channel: integer): cbool; cdecl; external;
function fifoCheckDatamsg(channel: integer): cbool; cdecl; external;

function fifoCheckDatamsgLength(channel: integer): cint; cdecl; external;

function fifoCountDatamsg(channel: integer): cint; cdecl; external;

function fifoGetAddress(channel: integer): pointer; cdecl; external;

function fifoGetValue32(channel: integer): cuint32; cdecl; external;


function fifoGetDatamsg(channel, buffersize: cint; destbuffer: pcuint8): cint; cdecl; external;

function fifoGetBufferUsage(): cint; cdecl; external;

function fifoGetLostWordCount(): cint; cdecl; external;



procedure fifoInternalSendInterrupt(); cdecl; external;
procedure fifoInternalRecvInterrupt(); cdecl; external;
procedure fifoInternalProtect(); cdecl; external;
procedure fifoInternalUnprotect(); cdecl; external;
function fifoInternalSend(firstword: cuint32; extrawordcount: cint; wordlist: pcuint32): cbool; cdecl; external;
procedure fifoInternalBeginSend(); cdecl; external;
function fifoInternalRecvPeek(): cuint32; cdecl; external;
function fifoInternalRecvPopFirst(): cuint32; cdecl; external;
function fifoInternalRecvWordCount(): cint; cdecl; external;
function fifoInternalRecvPushBack(word: cuint32): cbool; cdecl; external;
function fifoInternalSendEmpty(): cbool; cdecl; external;
function fifoInternalSendDeque(): cuint32; cdecl; external;
function fifoInternalSendEnqueue(word: cuint32): cbool; cdecl; external;
function fifoInternalAllocFreeBlock(): cint; cdecl; external;
procedure fifoInternalFreeBlock(blockid: cint); cdecl; external;
function fifoInternalFreeCheck(num_words: cint): cbool; cdecl; external;




// And some aspects of the configuration can't be changed
//  please don't edit below this line.

const
  FIFO_ADDRESSDATA_SHIFT = 0;
  FIFO_MINADDRESSDATABITS = 22;
  FIFO_ADDRESSDATA_MASK = $003FFFFF;
  FIFO_ADDRESSBASE = $02000000;
  FIFO_ADDRESSCOMPATIBLE = $FFC00000;

  FIFO_NUM_CHANNELS = (1 shl FIFO_CHANNEL_BITS);
  FIFO_CHANNEL_SHIFT = (32 - FIFO_CHANNEL_BITS);
  FIFO_CHANNEL_MASK = ((1 shl FIFO_CHANNEL_BITS) - 1);

  FIFO_ADDRESSBIT_SHIFT = (FIFO_CHANNEL_SHIFT - 1);
  FIFO_ADDRESSBIT = (1 shl FIFO_ADDRESSBIT_SHIFT);

  FIFO_IMMEDIATEBIT_SHIFT = (FIFO_CHANNEL_SHIFT - 2);
  FIFO_IMMEDIATEBIT = (1 shl FIFO_IMMEDIATEBIT_SHIFT);

  FIFO_EXTRABIT_SHIFT = (FIFO_CHANNEL_SHIFT - 3);
  FIFO_EXTRABIT = (1 shl FIFO_EXTRABIT_SHIFT);


  FIFO_VALUE32_MASK = (FIFO_EXTRABIT - 1);


  FIFO_BUFFER_TERMINATE = $FFFF;
  FIFO_BUFFER_NEXTMASK = $FFFF;


{ $IF (FIFO_MINADDRESSDATABITS + FIFO_CHANNEL_BITS + 1) > 32}
  { $ERROR Too many channel bits - control word isn't big enough for address packet}
{ $ENDIF}




function FIFO_PACK_ADDRESS(channel, address: cint): cint; inline;
function FIFO_VALUE32_NEEDEXTRA(value32: cint32): boolean; inline;
function FIFO_PACK_VALUE32(channel, value32: cint): cint; inline; 
function FIFO_PACK_VALUE32_EXTRA(channel: cint): cint; inline;
function FIFO_IS_ADDRESS_COMPATIBLE(address: cint): boolean; inline;
function FIFO_PACK_DATAMSG_HEADER(channel, numwords: cint): cint; inline;
function FIFO_IS_ADDRESS(dataword: cint): boolean; inline; 
function FIFO_IS_VALUE32(dataword: cint): boolean; inline;
function FIFO_IS_DATA(dataword: cint): boolean; inline;
function FIFO_UNPACK_CHANNEL(dataword: cint): cint; inline;
function FIFO_UNPACK_ADDRESS(dataword: cint): pointer; inline;
function FIFO_UNPACK_VALUE32_NEEDEXTRA(dataword: cint): boolean; inline;
function FIFO_UNPACK_VALUE32_NOEXTRA(dataword: cint): cint; inline;
function FIFO_UNPACK_DATALENGTH(dataword: cint): cint; inline;

const
  FIFO_BUFFERCONTROL_UNUSED = 0;
  FIFO_BUFFERCONTROL_SENDWORD = 1;
  FIFO_BUFFERCONTROL_RECVWORD = 2;
  FIFO_BUFFERCONTROL_ADDRESS = 3;
  FIFO_BUFFERCONTROL_VALUE32 = 4;
  FIFO_BUFFERCONTROL_DATASTART = 5;
  FIFO_BUFFERCONTROL_DATA = 6;

function FIFO_BUFFER_DATA(index: cint): cint; inline; 
function FIFO_BUFFER_DATA_BYTE(index, byteindex: cint): pcuint8; inline;
function FIFO_BUFFER_GETNEXT(index: cint): cint; inline; 
function FIFO_BUFFER_GETCONTROL(index: cint): cint; inline; 
function FIFO_BUFFER_GETEXTRA(index: cint): cint; inline; 
procedure FIFO_BUFFER_SETCONTROL(index,next,control,extra: cint); inline; 
procedure FIFO_BUFFER_SETNEXT(index, next: cint); inline;


{$endif NDS_INTERFACE}



{$ifdef NDS_IMPLEMENTATION}

function FIFO_PACK_ADDRESS(channel, address: cint): cint; inline;
begin
  result := ((channel) shl FIFO_CHANNEL_SHIFT) or 
            FIFO_ADDRESSBIT or 
            ((address shr FIFO_ADDRESSDATA_SHIFT) and FIFO_ADDRESSDATA_MASK);
end; 

function FIFO_VALUE32_NEEDEXTRA(value32: cint32): boolean; inline;
begin
  result :=  (value32 and (not FIFO_VALUE32_MASK)) <> 0; 
end;

function FIFO_PACK_VALUE32(channel, value32: cint): cint; inline; 
begin
  result := (channel shl FIFO_CHANNEL_SHIFT) or
            FIFO_IMMEDIATEBIT or 
            (value32 and FIFO_VALUE32_MASK);
end; 

function FIFO_PACK_VALUE32_EXTRA(channel: cint): cint; inline;
begin
  result :=  (channel shl FIFO_CHANNEL_SHIFT) or FIFO_IMMEDIATEBIT or FIFO_EXTRABIT;
end;

function FIFO_IS_ADDRESS_COMPATIBLE(address: cint): boolean; inline;
begin
  result := (address and FIFO_ADDRESSCOMPATIBLE) = FIFO_ADDRESSBASE;
end;

function FIFO_PACK_DATAMSG_HEADER(channel, numwords: cint): cint; inline;
begin
  result :=  (channel shl FIFO_CHANNEL_SHIFT) or (numwords and FIFO_VALUE32_MASK);
end;

function FIFO_IS_ADDRESS(dataword: cint): boolean; inline; 
begin
  result := (dataword and FIFO_ADDRESSBIT) <> 0;
end;

function FIFO_IS_VALUE32(dataword: cint): boolean; inline;
begin
  result := ((dataword and FIFO_ADDRESSBIT) = 0) and ((dataword and FIFO_IMMEDIATEBIT) <> 0);
end;

function FIFO_IS_DATA(dataword: cint): boolean; inline;
begin
  result := (dataword and (FIFO_ADDRESSBIT or FIFO_IMMEDIATEBIT)) = 0;
end;

function FIFO_UNPACK_CHANNEL(dataword: cint): cint; inline;
begin
  result := (dataword shr FIFO_CHANNEL_SHIFT) and FIFO_CHANNEL_MASK;
end;

function FIFO_UNPACK_ADDRESS(dataword: cint): pointer; inline;
begin
  result := pointer(((dataword and FIFO_ADDRESSDATA_MASK) shl FIFO_ADDRESSDATA_SHIFT) or FIFO_ADDRESSBASE);
end; 

function FIFO_UNPACK_VALUE32_NEEDEXTRA(dataword: cint): boolean; inline;
begin
  result := (dataword and FIFO_EXTRABIT) <> 0;
end;

function FIFO_UNPACK_VALUE32_NOEXTRA(dataword: cint): cint; inline;
begin
  result := (dataword and FIFO_VALUE32_MASK);
end;

function FIFO_UNPACK_DATALENGTH(dataword: cint): cint; inline;
begin
  result := (dataword and FIFO_VALUE32_MASK);
end;

function FIFO_BUFFER_DATA(index: cint): cint; inline; 
begin
  result := fifo_buffer[(index)*2+1];
end;

function FIFO_BUFFER_DATA_BYTE(index, byteindex: cint): pcuint8; inline;
begin
// ((u8*)(&fifo_buffer[(index)*2+1]))[(byteindex)]
/////////  result := pcuint8(pointer(fifo_buffer[index*2+1]))[byteindex];
end;


function FIFO_BUFFER_GETNEXT(index: cint): cint; inline; 
begin
  result := (fifo_buffer[(index)*2] and FIFO_BUFFER_NEXTMASK);
end;

function FIFO_BUFFER_GETCONTROL(index: cint): cint; inline; 
begin
  result := (fifo_buffer[(index)*2] shr 28);
end;

function FIFO_BUFFER_GETEXTRA(index: cint): cint; inline; 
begin
  result := ((fifo_buffer[(index)*2] shr 16) and $FFF);
end;

procedure FIFO_BUFFER_SETCONTROL(index,next,control,extra: cint); inline; 
begin
  fifo_buffer[(index)*2] := (next and FIFO_BUFFER_NEXTMASK) or (control shl 28) or ((extra and $FFF) shl 16);
end;  
  
procedure FIFO_BUFFER_SETNEXT(index, next: cint); inline;
begin
  fifo_buffer[(index)*2] :=  (next and FIFO_BUFFER_NEXTMASK) or (fifo_buffer[(index)*2] and (not FIFO_BUFFER_NEXTMASK)); 
end;

{$endif NDS_IMPLEMENTATION}
