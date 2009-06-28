(*
  $Id$
  ---------------------------------------------------------------------------------

	Sound Functions

	Copyright (C) 2008
		Jason Rogers (Dovoto)

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


---------------------------------------------------------------------------------*)

{$ifdef NDS_INTERFACE}

type
  FifoMessageType = integer;
const
  SOUND_PLAY_MESSAGE      : FifoMessageType = $1234;
  SOUND_PSG_MESSAGE       : FifoMessageType = $1235;
  SOUND_NOISE_MESSAGE     : FifoMessageType = $1236;
  MIC_RECORD_MESSAGE      : FifoMessageType = $1237;
  MIC_BUFFER_FULL_MESSAGE : FifoMessageType = $1238;
  SYS_INPUT_MESSAGE       : FifoMessageType = $1239;

type
{$PACKRECORDS 4} 
  FifoMessage = record
    _type: cuint16;
    empty: array[0..31] of cuint8; 
  end;
  TFifoMessage = FifoMessage;
  PFifoMessage = ^FifoMessage;

  SoundPlayMsg = record
    _type: cuint16; 	
    data: pointer;
    loopPoint: cuint16;
    dataSize: cuint16;
    freq: cuint16;
    volume: cuint8;
    pan: cuint8;
    loop: cbool;
    format: cuint8;
  end;
  TSoundPlayMsg = SoundPlayMsg;
  PSoundPlayMsg = ^SoundPlayMsg;

  SoundPsgMsg = record
    _type: cuint16;
    freq: cuint16;	
    dutyCycle: cuint8;
    volume: cuint8;
    pan: cuint8;
  end;
  TSoundPsgMsg = SoundPsgMsg;
  PSoundPsgMsg = ^SoundPsgMsg; 

  MicRecordMsg = record
    _type: cuint16;
    buffer: pointer;
    bufferLength: cuint32;
    freq: cuint16;
    format: cuint8;
  end;
  TMicRecordMsg = MicRecordMsg;
  PMicRecordMsg = ^MicRecordMsg;

  MicBufferFullMsg = record
    _type: cuint16;
    buffer: pointer;
    length: cuint32;
  end;
  TMicBufferFullMsg = MicBufferFullMsg;
  PMicBufferFullMsg = ^MicBufferFullMsg; 


  SystemInputMsg = record
    _type: cuint16;
    touch: touchPosition;
    keys: cuint16;
  end;
  TSystemInputMsg = SystemInputMsg; 
  PSystemInputMsg = ^SystemInputMsg;
  
{$PACKRECORDS C}   
{$endif NDS_INTERFACE}

