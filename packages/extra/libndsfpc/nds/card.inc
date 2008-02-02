(*
  $Id: card.inc 25 2007-12-10 21:06:46Z p4p3r0 $
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
const  
  // Card bus
  CARD_CR1       : pcuint16 = pointer($040001A0);
  CARD_CR1H      : pcuint8  = pointer($040001A1);
  CARD_EEPDATA   : pcuint8  = pointer($040001A2);
  CARD_CR2       : pcuint32 = pointer($040001A4);
  CARD_COMMAND   : pcuint8  = pointer($040001A8);

  CARD_DATA_RD   : pcuint32 = pointer($04100010);

  CARD_1B0       : pcuint32 = pointer($040001B0);
  CARD_1B4       : pcuint32 = pointer($040001B4);
  CARD_1B8       : pcuint16 = pointer($040001B8);
  CARD_1BA       : pcuint16 = pointer($040001BA);


  CARD_CR1_ENABLE = $80;  // in byte 1, i.e. 0x8000
  CARD_CR1_IRQ    = $40;  // in byte 1, i.e. 0x4000


// CARD_CR2 register:
  CARD_ACTIVATE   = (1 shl 31);  // when writing, get the ball rolling
  // 1<<30
  CARD_nRESET     = (1 shl 29);  // value on the /reset pin (1 = high out, not a reset state, 0 = low out = in reset)
  CARD_28         = (1 shl 28);  // when writing
  CARD_27         = (1 shl 27);  // when writing
  CARD_26         = (1 shl 26); 
  CARD_22         = (1 shl 22); 
  CARD_19         = (1 shl 19); 
  CARD_ENCRYPTED  = (1 shl 14);  // when writing, this command should be encrypted
  CARD_13         = (1 shl 13);  // when writing
  CARD_4          = (1 shl 4);   // when writing
  
  // 3 bits in b10..b8 indicate something
  // read bits
  CARD_BUSY       = (1 shl 31);  // when reading, still expecting incomming data?
  CARD_DATA_READY = (1 shl 23);  // when reading, CARD_DATA_RD or CARD_DATA has another word of data and is good to go



procedure cardWriteCommand(const command: pcuint8); cdecl; external;

procedure cardPolledTransfer(flags: cuint32;  destination: pcuint32;  length: cuint32; const command: pcuint8); cdecl; external; 
procedure cardStartTransfer(const command: pcuint8;  destination: pcuint32;  channel: cint;  flags: cuint32); cdecl; external;
function cardWriteAndRead(const command: pcuint8;  flags: cuint32): cuint32; cdecl; external;
procedure cardRead00(address: cuint32;  destination: pcuint32;  length: cuint32;  flags: cuint32); cdecl; external; 
procedure cardReadHeader(header: pcuint8); cdecl; external;
function cardReadID(flags: cuint32): cint; cdecl; external;
procedure cardReadEeprom(address: cuint32;  data: pcuint8;  length: cuint32;  addrtype: cuint32); cdecl; external; 
procedure cardWriteEeprom(address: cuint32;  data: pcuint8;  length: cuint32;  addrtype: cuint32); cdecl; external; 

function cardEepromReadID(i: cuint8): cuint8; cdecl; external;
function cardEepromCommand(command: cuint8; address: cuint32): cuint8; cdecl; external;
function cardEepromGetType({void}): cint; cdecl; external;
function cardEepromGetSize(): cuint32; cdecl; external;
procedure cardEepromChipErase({void}); cdecl; external;
procedure cardEepromSectorErase(address: cuint32); cdecl; external;
{$endif NDS_INTERFACE}

