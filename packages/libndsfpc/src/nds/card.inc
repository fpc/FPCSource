{$ifdef NDS_INTERFACE}
const  
  // Card bus
  CARD_COMMAND   : pcuint8  = pointer($040001A8);
  REG_ROMCTRL    : pcuint32 = pointer($040001A4);
  REG_AUXSPICNT  : pcuint16 = pointer($040001A0);
  REG_AUXSPICNTH : pcuint8  = pointer($040001A1);
  REG_AUXSPIDATA : pcuint8  = pointer($040001A2);
  CARD_DATA_RD   : pcuint32 = pointer($04100010);

  CARD_1B0       : pcuint32 = pointer($040001B0);
  CARD_1B4       : pcuint32 = pointer($040001B4);
  CARD_1B8       : pcuint16 = pointer($040001B8);
  CARD_1BA       : pcuint16 = pointer($040001BA);

  CARD_CR1_ENABLE = $80;  // in byte 1, i.e. 0x8000
  CARD_CR1_IRQ    = $40;  // in byte 1, i.e. 0x4000

  // SPI EEPROM COMMANDS

  SPI_EEPROM_WRSR   = $01;
  SPI_EEPROM_PP     = $02;	// Page Program
  SPI_EEPROM_READ   = $03;
  SPI_EEPROM_WRDI   = $04;  // Write disable
  SPI_EEPROM_RDSR   = $05;  // Read status register
  SPI_EEPROM_WREN   = $06;  // Write enable
  SPI_EEPROM_PW     = $0a;	// Page Write
  SPI_EEPROM_FAST   = $0b;	// Fast Read
  SPI_EEPROM_RDID   = $9f;
  SPI_EEPROM_RDP    = $ab;  // Release from deep power down
  SPI_EEPROM_DPD    = $b9;  // Deep power down


// REG_ROMCTRL:
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


  // REG_AUXSPICNT
  CARD_ENABLE     = (1 shl 15);
  CARD_SPI_ENABLE = (1 shl 13);
  CARD_SPI_BUSY   = (1 shl 7);
  CARD_SPI_HOLD   = (1 shl 6);

  CARD_SPICNTH_ENABLE = (1 shl 7);  // in byte 1, i.e. 0x8000
  CARD_SPICNTH_IRQ    = (1 shl 6);  // in byte 1, i.e. 0x4000




procedure cardWriteCommand(const command: pcuint8); cdecl; external;
procedure cardPolledTransfer(flags: cuint32;  destination: pcuint32;  length: cuint32; const command: pcuint8); cdecl; external; 
procedure cardStartTransfer(const command: pcuint8;  destination: pcuint32;  channel: cint;  flags: cuint32); cdecl; external;
function cardWriteAndRead(const command: pcuint8;  flags: cuint32): cuint32; cdecl; external;
procedure cardParamCommand(command: cuint8; parameter, flags: cuint32; destination: pcuint32; length: cuint32); cdecl; external;

procedure cardReadHeader(header: pcuint8); cdecl; external;
function cardReadID(flags: cuint32): cuint32; cdecl; external;
procedure cardReset(); cdecl; external;

procedure eepromWaitBusy(); inline;

procedure cardReadEeprom(address: cuint32;  data: pcuint8;  length: cuint32;  addrtype: cuint32); cdecl; external; 
procedure cardWriteEeprom(address: cuint32;  data: pcuint8;  length: cuint32;  addrtype: cuint32); cdecl; external; 

function cardEepromReadID(): cuint32; cdecl; external;

function cardEepromCommand(command: cuint8): cuint8; cdecl; external;

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

procedure eepromWaitBusy(); inline;
begin
  while (REG_AUXSPICNT^ and CARD_SPI_BUSY) <> 0 do;
end;

{$endif NDS_IMPLEMENTATION}
