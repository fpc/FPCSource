{$mode objfpc}{$H+}
{$packrecords 1}
unit ps1CDRom;
interface
uses ps1System;

type
  // CD-ROM XA sector header structure
  TCDROMXAHeader = packed record
    file_: Byte;
    channel: Byte;
    submode: Byte;
    codingInfo: Byte;
  end;

  // CD-ROM drive data types
  TCDROMMSF = packed record
    minute: Byte;
    second: Byte;
    frame: Byte;
  end;

  TCDROMGetlocLResult = packed record
    absoluteMSF: TCDROMMSF;
    mode: Byte;
    header: TCDROMXAHeader;
  end;

  TCDROMGetlocPResult = packed record
    track: Byte;
    index: Byte;
    relativeMSF: TCDROMMSF;
    absoluteMSF: TCDROMMSF;
  end;

  TCDROMGetIDResult = packed record
    status: Byte;
    flag: Byte;
    typ: Byte;
    atip: Byte;
    license: array[0..3] of Char;
  end;

  TCDROMReportPacket = packed record
    status: Byte;
    track: Byte;
    index: Byte;
    msf: TCDROMMSF;
    peak: Word;
  end;

// -----------------------------------------------------------------------------
// Constant definitions (replacing all C enums with Pascal const integers)
// -----------------------------------------------------------------------------

const
  // CDROMXASubmodeFlag
  CDROM_XA_SM_END_OF_RECORD = 1 shl 0;
  CDROM_XA_SM_TYPE_VIDEO    = 1 shl 1;
  CDROM_XA_SM_TYPE_AUDIO    = 1 shl 2;
  CDROM_XA_SM_TYPE_DATA     = 1 shl 3;
  CDROM_XA_SM_TRIGGER       = 1 shl 4;
  CDROM_XA_SM_FORM2         = 1 shl 5;
  CDROM_XA_SM_REAL_TIME     = 1 shl 6;
  CDROM_XA_SM_END_OF_FILE   = 1 shl 7;

  // CDROMXACodingInfoFlag
  CDROM_XA_CI_STEREO              = 1 shl 0;
  CDROM_XA_CI_SAMPLE_RATE_BITMASK = 1 shl 2;
  CDROM_XA_CI_SAMPLE_RATE_18900   = 0 shl 2;
  CDROM_XA_CI_SAMPLE_RATE_37800   = 1 shl 2;
  CDROM_XA_CI_BITS_BITMASK        = 1 shl 4;
  CDROM_XA_CI_BITS_4              = 0 shl 4;
  CDROM_XA_CI_BITS_8              = 1 shl 4;
  CDROM_XA_CI_EMPHASIS            = 1 shl 6;

  // CDROMCommand
  CDROM_CMD_NOP        = $01;
  CDROM_CMD_SETLOC     = $02;
  CDROM_CMD_PLAY       = $03;
  CDROM_CMD_FORWARD    = $04;
  CDROM_CMD_BACKWARD   = $05;
  CDROM_CMD_READ_N     = $06;
  CDROM_CMD_STANDBY    = $07;
  CDROM_CMD_STOP       = $08;
  CDROM_CMD_PAUSE      = $09;
  CDROM_CMD_INIT       = $0a;
  CDROM_CMD_MUTE       = $0b;
  CDROM_CMD_DEMUTE     = $0c;
  CDROM_CMD_SETFILTER  = $0d;
  CDROM_CMD_SETMODE    = $0e;
  CDROM_CMD_GETPARAM   = $0f;
  CDROM_CMD_GETLOC_L   = $10;
  CDROM_CMD_GETLOC_P   = $11;
  CDROM_CMD_SETSESSION = $12;
  CDROM_CMD_GET_TN     = $13;
  CDROM_CMD_GET_TD     = $14;
  CDROM_CMD_SEEK_L     = $15;
  CDROM_CMD_SEEK_P     = $16;
  CDROM_CMD_TEST       = $19;
  CDROM_CMD_GET_ID     = $1a;
  CDROM_CMD_READ_S     = $1b;
  CDROM_CMD_RESET      = $1c;
  CDROM_CMD_GET_Q      = $1d; // Versions 0xc1 and later only
  CDROM_CMD_READ_TOC   = $1e; // Versions 0xc1 and later only
  CDROM_CMD_UNLOCK0    = $50;
  CDROM_CMD_UNLOCK1    = $51;
  CDROM_CMD_UNLOCK2    = $52;
  CDROM_CMD_UNLOCK3    = $53;
  CDROM_CMD_UNLOCK4    = $54;
  CDROM_CMD_UNLOCK5    = $55;
  CDROM_CMD_UNLOCK6    = $56;
  CDROM_CMD_LOCK       = $57;

  // CDROMTestCommand
  CDROM_TEST_READ_ID              = $04;
  CDROM_TEST_GET_ID_COUNTERS      = $05;
  CDROM_TEST_GET_VERSION          = $20;
  CDROM_TEST_GET_SWITCHES         = $21;
  CDROM_TEST_GET_REGION           = $22;
  CDROM_TEST_GET_SERVO_TYPE       = $23;
  CDROM_TEST_GET_DSP_TYPE         = $24;
  CDROM_TEST_GET_DECODER_TYPE     = $25;
  CDROM_TEST_DSP_CMD              = $50;
  CDROM_TEST_DSP_CMD_RESP         = $51;
  CDROM_TEST_MCU_PEEK             = $60;
  CDROM_TEST_DECODER_GET_REG      = $71;
  CDROM_TEST_DECODER_SET_REG      = $72;
  CDROM_TEST_DECODER_GET_SRAM_PTR = $75;
  CDROM_TEST_DECODER_SET_SRAM_PTR = $76;

  // CDROMIRQType
  CDROM_IRQ_NONE        = 0;
  CDROM_IRQ_DATA_READY  = 1;
  CDROM_IRQ_COMPLETE    = 2;
  CDROM_IRQ_ACKNOWLEDGE = 3;
  CDROM_IRQ_DATA_END    = 4;
  CDROM_IRQ_ERROR       = 5;

  // CDROMCommandStatusFlag
  CDROM_CMD_STAT_ERROR      = 1 shl 0;
  CDROM_CMD_STAT_SPINDLE_ON = 1 shl 1;
  CDROM_CMD_STAT_SEEK_ERROR = 1 shl 2;
  CDROM_CMD_STAT_ID_ERROR   = 1 shl 3;
  CDROM_CMD_STAT_LID_OPEN   = 1 shl 4;
  CDROM_CMD_STAT_READING    = 1 shl 5;
  CDROM_CMD_STAT_SEEKING    = 1 shl 6;
  CDROM_CMD_STAT_PLAYING    = 1 shl 7;

  // CDROMCommandErrorFlag
  CDROM_CMD_ERR_SEEK_FAILED         = 1 shl 2;
  CDROM_CMD_ERR_LID_OPENED          = 1 shl 3;
  CDROM_CMD_ERR_INVALID_PARAM_VALUE = 1 shl 4;
  CDROM_CMD_ERR_INVALID_PARAM_COUNT = 1 shl 5;
  CDROM_CMD_ERR_INVALID_COMMAND     = 1 shl 6;
  CDROM_CMD_ERR_NO_DISC             = 1 shl 7;

  // CDROMModeFlag
  CDROM_MODE_CDDA         = 1 shl 0;
  CDROM_MODE_AUTO_PAUSE   = 1 shl 1;
  CDROM_MODE_CDDA_REPORT  = 1 shl 2;
  CDROM_MODE_XA_FILTER    = 1 shl 3;
  CDROM_MODE_SIZE_BITMASK = 3 shl 4;
  CDROM_MODE_SIZE_2048    = 0 shl 4;
  CDROM_MODE_SIZE_2340    = 2 shl 4;
  CDROM_MODE_XA_ADPCM     = 1 shl 6;
  CDROM_MODE_SPEED_1X     = 0 shl 7;
  CDROM_MODE_SPEED_2X     = 1 shl 7;

// -----------------------------------------------------------------------------
// Inline helper functions
// -----------------------------------------------------------------------------

function cdrom_encodeBCD(value: Byte): Byte; inline;
function cdrom_decodeBCD(value: Byte): Byte; inline;
procedure cdrom_convertLBAToMSF(var msf: TCDROMMSF; lba: Cardinal); inline;
function cdrom_convertMSFToLBA(const msf: TCDROMMSF): Cardinal; inline;



var
  cdromWaitingForInt3: boolean;
  cdromWaitingForInt4: boolean;
  cdromWaitingForInt5: boolean;

  cdromReadDataPtr: pointer;
  cdromReadDataSectorSize: dword;
  cdromReadDataNumSectors: dword;
  cdromReadDone : Boolean;

  cdromResponse: array[0..15] of byte;
  cdromRespLength: byte;
  cdromStatus: byte;

  cdromIsReading: Boolean;

function CDROM_BUSY: boolean;

procedure initCDROM;
procedure issueCDROMCommand(cmd: byte; arg: pbyte; argLength: dword);

procedure waitForINT3;

procedure startCDROMRead(lba: dword; ptr: pointer; numSectors, sectorSize: dword; doubleSpeed, wait: Boolean);

procedure cdromINT1;
procedure cdromINT2;
procedure cdromINT3;
procedure cdromINT4;
procedure cdromINT5;

procedure HandleCDROMIRQx;

var
  rootDirLBA : dword;

type
  TDirectoryEntry = record
    lba      : dword;
    length   : dword;
    fileSize : array [0..1] of dword;
    name     : array [0..254] of char;
  end;

  TEntries = array of TDirectoryEntry;

  TFile = record
    name : string;
    size : dword;
    data : pointer;
    lba : dword;    
  end;


procedure initFileSystem;

function ParseDirRecord(data: pointer; offset: dword; var recordLength: Byte; var directoryEntry: TDirectoryEntry): Integer;
procedure pareseDir(lba: dword; var entrys: TEntries);
function getLBA(const name: string; const entries: TEntries; var info: TDirectoryEntry): longint;

function getFileLBA(const name: string; startLBA: longint; var fileInfo: TDirectoryEntry): longint;

procedure printEntries(const entries: TEntries);

function getFileInfo(const name: string; var fileInfo: TFile): boolean;
procedure loadFile(var theFile: TFile);


implementation

function cdrom_encodeBCD(value: Byte): Byte; inline;
begin
  Result := value + (value div 10) * 6;
end;

function cdrom_decodeBCD(value: Byte): Byte; inline;
begin
  Result := value - ((value shr 4) * 6);
end;

procedure cdrom_convertLBAToMSF(var msf: TCDROMMSF; lba: Cardinal); inline;
begin

  lba := lba + 150; // Skip lead-in area (LBA 0 is always at 00:02:00)

  msf.minute := cdrom_encodeBCD(lba div (75 * 60));
  msf.second := cdrom_encodeBCD((lba div 75) mod 60);
  msf.frame  := cdrom_encodeBCD(lba mod 75);

end;

function cdrom_convertMSFToLBA(const msf: TCDROMMSF): Cardinal; inline;
begin

  Result :=
      cdrom_decodeBCD(msf.minute) * (75 * 60) +
      cdrom_decodeBCD(msf.second) * 75 +
      cdrom_decodeBCD(msf.frame) - 150;

end;



procedure HandleCDROMIRQx;
var
  irqType: Byte;

begin

    CDROM_ADDRESS:= 1;

    irqType:= CDROM_HINTSTS and (CDROM_HINT_INT0 or CDROM_HINT_INT1 or CDROM_HINT_INT2);

    // If a new sector is available, request a sector buffer read.
    if irqType = CDROM_IRQ_DATA_READY then begin
        CDROM_ADDRESS:= 0;
        CDROM_HCHPCTL:= 0;
        CDROM_HCHPCTL:= CDROM_HCHPCTL_BFRD;
    end;

    CDROM_ADDRESS:= 1;
    CDROM_HCLRCTL:= CDROM_HCLRCTL_CLRINT0 or CDROM_HCLRCTL_CLRINT1 or CDROM_HCLRCTL_CLRINT2;
    CDROM_HCLRCTL:= CDROM_HCLRCTL_CLRPRM; // Clear parameter buffer
    delayMicroseconds(3);

    cdromRespLength:= 0;

    while (CDROM_HSTS and CDROM_HSTS_RSLRRDY) <> 0 do begin
        cdromResponse[cdromRespLength]:= CDROM_RESULT;
        inc(cdromRespLength);
    end;

  case irqType of
    CDROM_IRQ_DATA_READY      : cdromINT1;
    CDROM_IRQ_COMPLETE        : cdromINT2;
    CDROM_IRQ_ACKNOWLEDGE     : cdromINT3;
    CDROM_IRQ_DATA_END        : cdromINT4;
    CDROM_IRQ_ERROR           : cdromINT5;
  end;
  
end;



function CDROM_BUSY: boolean;
begin
  Result:= (CDROM_HSTS and CDROM_HSTS_BUSYSTS) <> 0;
end;


procedure initCDROM;
begin

  HandleCDROMIRQ:= @HandleCDROMIRQx;


  // Configure the bus
  BIU_DEV5_CTRL:= $00020943;

  // Enable DMA for the CD-ROM channel
  DMA_DPCR:= DMA_DPCR or DMA_DPCR_CH_ENABLE(DMA_CDROM);

  // Select CD-ROM register set 1
  CDROM_ADDRESS:= 1;

  // Acknowledge all IRQs
  CDROM_HCLRCTL:= CDROM_HCLRCTL_CLRINT0 or CDROM_HCLRCTL_CLRINT1 or CDROM_HCLRCTL_CLRINT2;

  // Enable all IRQs
  CDROM_HINTMSK_W:= CDROM_HCLRCTL_CLRINT0 or CDROM_HCLRCTL_CLRINT1 or CDROM_HCLRCTL_CLRINT2;

  // Deselect register set 1
  CDROM_ADDRESS:= 0;

  // Clear pending requests
  CDROM_HCHPCTL:= 0;

  // Select register set 2 (left audio)
  CDROM_ADDRESS:= 2;
  CDROM_ATV0:= 128; // SPU left channel
  CDROM_ATV1:= 0;

  // Select register set 3 (right audio)
  CDROM_ADDRESS:= 3;
  CDROM_ATV2:= 128; // SPU right channel
  CDROM_ATV3:= 0;

  // Apply changes to audio channel
  CDROM_ADPCTL:= CDROM_ADPCTL_CHNGATV;

end;


procedure issueCDROMCommand(cmd: byte; arg: pbyte; argLength: dword);
begin
  
  // Set all waiting flags
  cdromReadDone:= false;
  cdromWaitingForInt3:= true;
  cdromWaitingForInt4:= true;
  cdromWaitingForInt5:= true;

  while CDROM_BUSY do ;

  // Clear parameter buffer
  CDROM_ADDRESS:= 1;
  CDROM_HCLRCTL:= CDROM_HCLRCTL_CLRPRM;
  delayMicroseconds(3);

  while CDROM_BUSY do ;  

  // Send parameters
  CDROM_ADDRESS:= 0;
  while argLength > 0 do begin
    CDROM_PARAMETER:= arg^;
    Inc(arg);
    Dec(argLength);
  end;

  CDROM_COMMAND:= cmd;

end;


procedure waitForINT3;
begin
  // spin until either Int3 or Int5 flag is cleared
  while cdromWaitingForInt3 and cdromWaitingForInt5 do ;
end;


procedure startCDROMRead(lba: dword; ptr: Pointer; numSectors, sectorSize: dword; doubleSpeed, wait: Boolean);
var
  mode : byte;
  msf : TCDROMMSF;

begin
  
  cdromIsReading:= true;

  cdromReadDataPtr        := ptr;
  cdromReadDataNumSectors := numSectors;
  cdromReadDataSectorSize := sectorSize;


  mode:= 0;
  if sectorSize = 2340 then mode:= mode or CDROM_MODE_SIZE_2340;
  if doubleSpeed then mode:= mode or CDROM_MODE_SPEED_2X;


  cdrom_convertLBAToMSF(msf, lba);
  issueCDROMCommand(CDROM_CMD_SETMODE, @mode, 1);
  waitForINT3;

  issueCDROMCommand(CDROM_CMD_SETLOC, @msf, 3);
  waitForINT3;

  issueCDROMCommand(CDROM_CMD_READ_N, nil, 0);
  waitForINT3;

  if wait then while not cdromReadDone do ;

end;


procedure cdromINT1;
begin

  DMA_MADR_Set(DMA_CDROM, dword(cdromReadDataPtr));
  DMA_BCR_Set (DMA_CDROM, cdromReadDataSectorSize div 4);
  DMA_CHCR_Set(DMA_CDROM, DMA_CHCR_ENABLE or DMA_CHCR_TRIGGER);

  while (DMA_CHCR(DMA_CDROM) and DMA_CHCR_ENABLE) <> 0 do ; // spin until DMA finished
  
 // Advance pointer now that DMA has finished
  cdromReadDataPtr:= pointer(cdromReadDataPtr + cdromReadDataSectorSize);

  Dec(cdromReadDataNumSectors);
  if cdromReadDataNumSectors <= 0 then begin
    cdromIsReading:= False;
    issueCDROMCommand(CDROM_CMD_PAUSE, nil, 0);
  end;

end;


procedure cdromINT2;
begin

  if not cdromIsReading then cdromReadDone:= true;

end;


procedure cdromINT3;
begin

  cdromStatus:= cdromResponse[0];
  cdromWaitingForInt3:= False;

end;


procedure cdromINT4;
begin
  cdromWaitingForInt4:= False;
end;


procedure cdromINT5;
var 
  i: integer;

begin

  writeln('TRACK ERROR: HSTS=', CDROM_HSTS);

  // read response FIFO to cdromResponse
  cdromRespLength:= 0;
  while (CDROM_HSTS and CDROM_HSTS_RSLRRDY) <> 0 do begin
    cdromResponse[cdromRespLength]:= CDROM_RESULT;
    Inc(cdromRespLength);
    if cdromRespLength >= length(cdromResponse) then break;
  end;

  write('Response: ');
  for i := 0 to cdromRespLength - 1 do write(PrintHexValue(cdromResponse[i]),' ');

  writeln;
  cdromWaitingForInt5:= False;

end;



function Int32_LM(const A: array of Byte; StartIndex: SizeInt): dword;
begin
  // little-endian 4-byte read: LSB first
  Result :=
      Cardinal(A[StartIndex + 0])         or
     (Cardinal(A[StartIndex + 1]) shl 8 ) or
     (Cardinal(A[StartIndex + 2]) shl 16) or
     (Cardinal(A[StartIndex + 3]) shl 24);
end;

// Returns 1 if it is the end of the directory list.
function ParseDirRecord(data: pointer; offset: dword; var recordLength: Byte; var directoryEntry: TDirectoryEntry): Integer;
var
  dataSector : array [0..2047] of byte;

begin

  result:= 0;

  Move(pointer(data + offset)^, dataSector[0], 2048);

  recordLength      := dataSector[0];

  directoryEntry.lba := Int32_LM(dataSector, 2);
  directoryEntry.length := Int32_LM(dataSector, 10);

  if recordLength < 1 then exit(1); // End of list

  if dataSector[33] = $00 then begin
    directoryEntry.name[0] := '.';
    directoryEntry.name[1] := #0;
    Exit; // Working Dir
  end;

  if dataSector[33] = $01 then begin
    directoryEntry.name[0] := '.';
    directoryEntry.name[1] := '.';
    directoryEntry.name[2] := #0;
    Exit; // Parent Dir
  end;

  Move(dataSector[33], directoryEntry.name[0], dataSector[32]);
  directoryEntry.name[dataSector[32]] := #0;

  directoryEntry.fileSize[0] :=
    (Cardinal(dataSector[10]) shl 24) or
    (Cardinal(dataSector[11]) shl 16) or
    (Cardinal(dataSector[12]) shl  8) or
    (Cardinal(dataSector[13]));

  directoryEntry.fileSize[1] :=
    (Cardinal(dataSector[14]) shl 24) or
    (Cardinal(dataSector[15]) shl 16) or
    (Cardinal(dataSector[16]) shl  8) or
    (Cardinal(dataSector[17]));

end;


procedure initFileSystem;
var
  buffer    : array [0..2048] of Byte;

begin

  // Read the Primary Volume Descriptor (sector 16)
  startCDROMRead(16, @buffer, 1, 2048, false, true);

  rootDirLBA:= Int32_LM(buffer, 158);

end;


procedure pareseDir(lba: dword; var entrys: TEntries);
var
  offset : dword;
  recLen : byte;
  dirData : pointer;
  entry : TDirectoryEntry;

begin

  getmem(dirData, 2048);
  fillchar(dirData^, sizeof(2048), 0);

  startCDROMRead(lba, dirData, 1, 2048, false, True);


  setlength(entrys, 0);
  offset := 0;
  while offset < 2048 do begin

    if ParseDirRecord(dirData, offset, recLen, entry) <> 0 then break;
    Inc(offset, recLen);

    setlength(entrys, length(entrys) + 1);

    entrys[length(entrys) - 1]:= entry;
  
  end;

  freemem(dirData);

end;


procedure printEntries(const entries: TEntries);
var
  i : dword;
begin

  for i:= 0 to length(entries) - 1 do begin
    Writeln('Entry name: ', Entries[i].name, ' LBA=', entries[i].lba);
  end;

end;


function getLBA(const name: string; const entries: TEntries; var info: TDirectoryEntry): longint;
var
  i : dword;
begin

  result:= -1;

  for i:= 0 to length(entries) - 1 do
    if Entries[i].name = name then begin
      info:= entries[i];
      exit(entries[i].lba);
    end;

end;


type
    TStringArray = array of string;

function Split(const s: string; sep: Char): TStringArray;
var
  p, start, count, i: dword;

begin
  if length(s) = 0 then exit;
  count:= 0;
  start:= 1;
  SetLength(Result, 0);
  p:= 1;
  repeat

    if s[p] = sep then begin

      SetLength(Result, count + 1);
      Result[count]:= '';
 
      i:= start;
      repeat
        Result[count]:= Result[count] + s[i];
        inc(i);
      until (s[i] = sep) or (i = length(s)) or (i = p);
      if s[i] = sep then Result[count]:= Result[count] + sep; 
      start:= p + 1;

      Inc(count);

    end;

    inc(p);

  until p = length(s);

  if (start <> p) and (start <> (length(s))) then begin
      SetLength(Result, count + 1);
      Result[count]:= '';
      i:= start;
      
      repeat
        Result[count]:= Result[count] + s[i];
        inc(i);
      until (i = (length(s) + 1)) or (s[i] = sep);

  end;


end;


function getFileLBA(const name: string; startLBA: longint; var fileInfo: TDirectoryEntry): longint;
var
  entries : TEntries;
  stringArray: TStringArray;
  lba : longint;
  curname : string;
  i : dword;

begin

  result:= -1;
  if startLBA = -1 then exit;
  
  pareseDir(startLBA, entries);

  stringArray:= Split(name, '/');

  curname:= stringArray[0];

  if curname[length(curname)] = '/' then setlength(curname, length(curname) - 1);

  if length(curname) > 0 then begin

      if curname = name then begin
        result:= getLBA(name, entries, fileInfo);
        exit;
      end;
        

      lba:= getLBA(curname, entries, fileInfo);

      if lba <> -1 then begin
        curname:= '';
        if length(stringArray) > 1 then begin
          for i:= 1 to length(stringArray) - 1 do curname:= curname + stringArray[i];
        end else curname:= stringArray[0];

        result:= getFileLBA(curname, lba, fileInfo);          
        
      end;
  end;


end;


function getFileInfo(const name: string; var fileInfo: TFile): boolean;
var
  entry : TDirectoryEntry;

begin

  result:= false;

  if getFileLBA(name, rootDirLBA, entry) <> -1 then begin

    fileInfo.name:= entry.name;
    fileInfo.lba:= entry.lba;
    fileInfo.size:= entry.fileSize[1];
    fileInfo.data:= nil;
    result:= true;

  end;

end;


procedure loadFile(var theFile: TFile);
var
  numSelectors : dword;

begin

  numSelectors:= (theFile.size + 2048 - 1) div 2048;
  getmem(theFile.data, numSelectors * 2048);

  startCDROMRead(theFile.lba, theFile.data, numSelectors, 2048, true, true);

  ReAllocMem(theFile.data, theFile.size);

end;

end.
