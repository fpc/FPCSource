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
  CARD_WR         = (1 shl 30);
  CARD_nRESET     = (1 shl 29);  // value on the /reset pin (1 = high out, not a reset state, 0 = low out = in reset)
  CARD_SEC_LARGE  = (1 shl 28);  // when writing
  CARD_CLK_SLOW   = (1 shl 27);  // when writing

  CARD_SEC_CMD    = (1 shl 22);             // The command transfer will be hardware encrypted (KEY2)
  CARD_SEC_SEED   = (1 shl 15);           // Apply encryption (KEY2) seed to hardware registers
  CARD_SEC_EN     = (1 shl 14);           // Security enable
  CARD_SEC_DAT    = (1 shl 13);           // The data transfer will be hardware encrypted (KEY2)


function CARD_BLK_SIZE(n: cint): cint; inline;
function CARD_DELAY2(n: cint): cint; inline;
function CARD_DELAY1(n: cint): cint; inline;



const  
  // 3 bits in b10..b8 indicate something
  // read bits
  CARD_BUSY       = (1 shl 31);  // when reading, still expecting incomming data?
  CARD_DATA_READY = (1 shl 23);  // when reading, CARD_DATA_RD or CARD_DATA has another word of data and is good to go

  // Card commands
  CARD_CMD_DUMMY          = $9F;
  CARD_CMD_HEADER_READ    = $00;
  CARD_CMD_HEADER_CHIPID  = $90;
  CARD_CMD_ACTIVATE_BF    = $3C; // Go into blowfish (KEY1) encryption mode
  CARD_CMD_ACTIVATE_SEC   = $40; // Go into hardware (KEY2) encryption mode
  CARD_CMD_SECURE_CHIPID  = $10;
  CARD_CMD_SECURE_READ    = $20;
  CARD_CMD_DISABLE_SEC    = $60; // Leave hardware (KEY2) encryption mode
  CARD_CMD_DATA_MODE      = $A0;
  CARD_CMD_DATA_READ      = $B7;
  CARD_CMD_DATA_CHIPID    = $B8;




procedure cardWriteCommand(const command: pcuint8); cdecl; external;
procedure cardPolledTransfer(flags: cuint32;  destination: pcuint32;  length: cuint32; const command: pcuint8); cdecl; external; 
procedure cardStartTransfer(const command: pcuint8;  destination: pcuint32;  channel: cint;  flags: cuint32); cdecl; external;
function cardWriteAndRead(const command: pcuint8;  flags: cuint32): cuint32; cdecl; external;
procedure cardParamCommand(command: cuint8; parameter, flags: cuint32; destination: pcuint32; length: cuint32); cdecl; external;

procedure cardReadHeader(header: pcuint8); cdecl; external;
function cardReadID(flags: cuint32): cuint32; cdecl; external;



procedure cardReadEeprom(address: cuint32;  data: pcuint8;  length: cuint32;  addrtype: cuint32); cdecl; external; 
procedure cardWriteEeprom(address: cuint32;  data: pcuint8;  length: cuint32;  addrtype: cuint32); cdecl; external; 

function cardEepromReadID(i: cuint8): cuint8; cdecl; external;

function cardEepromCommand(command: cuint8; address: cuint32): cuint8; cdecl; external;

function cardEepromGetType({void}): cint; cdecl; external;
function cardEepromGetSize(): cuint32; cdecl; external;
procedure cardEepromChipErase({void}); cdecl; external;
procedure cardEepromSectorErase(address: cuint32); cdecl; external;

{$endif NDS_INTERFACE}


{$ifdef NDS_IMPLEMENTATION}

function CARD_BLK_SIZE(n: cint): cint; inline;
begin
  result := (n and $7) shl 24;
end;

function CARD_DELAY2(n: cint): cint; inline;
begin
  result := (n and $3F) shl 16;
end;

function CARD_DELAY1(n: cint): cint; inline;
begin
  result := (n and $1FFF);
end;

{$endif NDS_IMPLEMENTATION}
