unit scsidefs;

interface

const

{ ---------------------------------------------------------------------
  TARGET STATUS VALUES
  ---------------------------------------------------------------------}

  STATUS_GOOD = $00;     // Status Good
  STATUS_CHKCOND = $02;  // Check Condition
  STATUS_CONDMET = $04;  // Condition Met
  STATUS_BUSY = $08;     // Busy
  STATUS_INTERM = $10;   // Intermediate
  STATUS_INTCDMET = $14; // Intermediate-condition met
  STATUS_RESCONF = $18;  // Reservation conflict
  STATUS_COMTERM = $22;  // Command Terminated
  STATUS_QFULL = $28;    // Queue full

{ ---------------------------------------------------------------------
  SCSI MISCELLANEOUS EQUATES
  ---------------------------------------------------------------------}

  MAXLUN = 7;         // Maximum Logical Unit Id
  MAXTARG = 7;        // Maximum Target Id
  MAX_SCSI_LUNS = 64; // Maximum Number of SCSI LUNs
  MAX_NUM_HA = 8;     // Maximum Number of SCSI HA's

{ ---------------------------------------------------------------------
  SCSI COMMAND OPCODES
  ---------------------------------------------------------------------}

{ ---------------------------------------------------------------------
  Commands for all Device Types
  ---------------------------------------------------------------------}

  SCSI_CHANGE_DEF = $40; // Change Definition (Optional)
  SCSI_COMPARE = $39;    // Compare (O)
  SCSI_COPY = $18;       // Copy (O)
  SCSI_COP_VERIFY = $3A; // Copy and Verify (O)
  SCSI_INQUIRY = $12;    // Inquiry (MANDATORY)
  SCSI_LOG_SELECT = $4C; // Log Select (O)
  SCSI_LOG_SENSE = $4D;  // Log Sense (O)
  SCSI_MODE_SEL6 = $15;  // Mode Select 6-byte (Device Specific)
  SCSI_MODE_SEL10 = $55; // Mode Select 10-byte (Device Specific)
  SCSI_MODE_SEN6 = $1A;  // Mode Sense 6-byte (Device Specific)
  SCSI_MODE_SEN10 = $5A; // Mode Sense 10-byte (Device Specific)
  SCSI_READ_BUFF = $3C;  // Read Buffer (O)
  SCSI_REQ_SENSE = $03;  // Request Sense (MANDATORY)
  SCSI_SEND_DIAG = $1D;  // Send Diagnostic (O)
  SCSI_TST_U_RDY = $00;  // Test Unit Ready (MANDATORY)
  SCSI_WRITE_BUFF = $3B; // Write Buffer (O)

{ ---------------------------------------------------------------------
  Commands Unique to Direct Access Devices
  ---------------------------------------------------------------------}

  SCSI_FORMAT = $04;     // Format Unit (MANDATORY)
  SCSI_LCK_UN_CAC = $36; // Lock Unlock Cache (O)
  SCSI_PREFETCH = $34;   // Prefetch (O)
  SCSI_MED_REMOVL = $1E; // Prevent/Allow medium Removal (O)
  SCSI_READ6 = $08;      // Read 6-byte (MANDATORY)
  SCSI_READ10 = $28;     // Read 10-byte (MANDATORY)
  SCSI_RD_CAPAC = $25;   // Read Capacity (MANDATORY)
  SCSI_RD_DEFECT = $37;  // Read Defect Data (O)
  SCSI_READ_LONG = $3E;  // Read Long (O)
  SCSI_REASS_BLK = $07;  // Reassign Blocks (O)
  SCSI_RCV_DIAG = $1C;   // Receive Diagnostic Results (O)
  SCSI_RELEASE = $17;    // Release Unit (MANDATORY)
  SCSI_REZERO = $01;     // Rezero Unit (O)
  SCSI_SRCH_DAT_E = $31; // Search Data Equal (O)
  SCSI_SRCH_DAT_H = $30; // Search Data High (O)
  SCSI_SRCH_DAT_L = $32; // Search Data Low (O)
  SCSI_SEEK6 = $0B;      // Seek 6-Byte (O)
  SCSI_SEEK10 = $2B;     // Seek 10-Byte (O)
  SCSI_SET_LIMIT = $33;  // Set Limits (O)
  SCSI_START_STP = $1B;  // Start/Stop Unit (O)
  SCSI_SYNC_CACHE = $35; // Synchronize Cache (O)
  SCSI_VERIFY = $2F;     // Verify (O)
  SCSI_WRITE6 = $0A;     // Write 6-Byte (MANDATORY)
  SCSI_WRITE10 = $2A;    // Write 10-Byte (MANDATORY)
  SCSI_WRT_VERIFY = $2E; // Write and Verify (O)
  SCSI_WRITE_LONG = $3F; // Write Long (O)
  SCSI_WRITE_SAME = $41; // Write Same (O)

{ ---------------------------------------------------------------------
  Commands Unique to Sequential Access Devices
  ---------------------------------------------------------------------}

  SCSI_ERASE = $19;      // Erase (MANDATORY)
  SCSI_LOAD_UN = $1B;    // Load/Unload (O)
  SCSI_LOCATE = $2B;     // Locate (O)
  SCSI_RD_BLK_LIM = $05; // Read Block Limits (MANDATORY)
  SCSI_READ_POS = $34;   // Read Position (O)
  SCSI_READ_REV = $0F;   // Read Reverse (O)
  SCSI_REC_BF_DAT = $14; // Recover Buffer Data (O)
  SCSI_RESERVE = $16;    // Reserve Unit (MANDATORY)
  SCSI_REWIND = $01;     // Rewind (MANDATORY)
  SCSI_SPACE = $11;      // Space (MANDATORY)
  SCSI_VERIFY_T = $13;   // Verify (Tape) (O)
  SCSI_WRT_FILE = $10;   // Write Filemarks (MANDATORY)

{ ---------------------------------------------------------------------
  Commands Unique to Printer Devices
  ---------------------------------------------------------------------}

  SCSI_PRINT = $0A;      // Print (MANDATORY)
  SCSI_SLEW_PNT = $0B;   // Slew and Print (O)
  SCSI_STOP_PNT = $1B;   // Stop Print (O)
  SCSI_SYNC_BUFF = $10;  // Synchronize Buffer (O)

{ ---------------------------------------------------------------------
  Commands Unique to Processor Devices
  ---------------------------------------------------------------------}

  SCSI_RECEIVE = $08;    // Receive (O)
  SCSI_SEND = $0A;       // Send (O)

{ ---------------------------------------------------------------------
  Commands Unique to Write-Once Devices
  ---------------------------------------------------------------------}

  SCSI_MEDIUM_SCN = $38; // Medium Scan (O)
  SCSI_SRCHDATE10 = $31; // Search Data Equal 10-Byte (O)
  SCSI_SRCHDATE12 = $B1; // Search Data Equal 12-Byte (O)
  SCSI_SRCHDATH10 = $30; // Search Data High 10-Byte (O)
  SCSI_SRCHDATH12 = $B0; // Search Data High 12-Byte (O)
  SCSI_SRCHDATL10 = $32; // Search Data Low 10-Byte (O)
  SCSI_SRCHDATL12 = $B2; // Search Data Low 12-Byte (O)
  SCSI_SET_LIM_10 = $33; // Set Limits 10-Byte (O)
  SCSI_SET_LIM_12 = $B3; // Set Limits 10-Byte (O)
  SCSI_VERIFY10 = $2F;   // Verify 10-Byte (O)
  SCSI_VERIFY12 = $AF;   // Verify 12-Byte (O)
  SCSI_WRITE12 = $AA;    // Write 12-Byte (O)
  SCSI_WRT_VER10 = $2E;  // Write and Verify 10-Byte (O)
  SCSI_WRT_VER12 = $AE;  // Write and Verify 12-Byte (O)

{ ---------------------------------------------------------------------
  Commands Unique to CD-ROM Devices
  ---------------------------------------------------------------------}

  SCSI_PLAYAUD_10  = $45; // Play Audio 10-Byte (O)
  SCSI_PLAYAUD_12  = $A5; // Play Audio 12-Byte 12-Byte (O)
  SCSI_PLAYAUDMSF  = $47; // Play Audio MSF (O)
  SCSI_PLAYA_TKIN  = $48; // Play Audio Track/Index (O)
  SCSI_PLYTKREL10  = $49; // Play Track Relative 10-Byte (O)
  SCSI_PLYTKREL12  = $A9; // Play Track Relative 12-Byte (O)
  SCSI_READCDCAP   = $25; // Read CD-ROM Capacity (MANDATORY)
  SCSI_READHEADER  = $44; // Read Header (O)
  SCSI_SUBCHANNEL  = $42; // Read Subchannel (O)
  SCSI_READ_TOC    = $43; // Read TOC (O)
  SCSI_STOP        = $4E;
  SCSI_PAUSERESUME = $4B;

{ ---------------------------------------------------------------------
  Commands Unique to Scanner Devices
  ---------------------------------------------------------------------}

  SCSI_GETDBSTAT  = $34; // Get Data Buffer Status (O)
  SCSI_GETWINDOW  = $25; // Get Window (O)
  SCSI_OBJECTPOS  = $31; // Object Postion (O)
  SCSI_SCAN       = $1B; // Scan (O)
  SCSI_SETWINDOW  = $24; // Set Window (MANDATORY)

{ ---------------------------------------------------------------------
  Commands Unique to Optical Memory Devices
  ---------------------------------------------------------------------}

  SCSI_UpdateBlk = $3D; // Update Block (O)

{ ---------------------------------------------------------------------
  Commands Unique to Optical Medium Changer Devices
  ---------------------------------------------------------------------}

  SCSI_EXCHMEDIUM = $A6; // Exchange Medium (O)
  SCSI_INITELSTAT = $07; // Initialize Element Status (O)
  SCSI_POSTOELEM  = $2B; // Position to Element (O)
  SCSI_REQ_VE_ADD = $B5; // Request Volume Element Address (O)
  SCSI_SENDVOLTAG = $B6; // Send Volume Tag (O)

{ ---------------------------------------------------------------------
  Commands Unique to Optical Communication Devices
  ---------------------------------------------------------------------}

  SCSI_GET_MSG_6 = $08;  // Get Message 6-Byte (MANDATORY)
  SCSI_GET_MSG_10 = $28; // Get Message 10-Byte (O)
  SCSI_GET_MSG_12 = $A8; // Get Message 12-Byte (O)
  SCSI_SND_MSG_6 = $0A;  // Send Message 6-Byte (MANDATORY)
  SCSI_SND_MSG_10 = $2A; // Send Message 10-Byte (O)
  SCSI_SND_MSG_12 = $AA; // Send Message 12-Byte (O)

{ ---------------------------------------------------------------------
  Request Sense Data Format
  ---------------------------------------------------------------------}

type

  SENSE_DATA_FMT = record
    ErrorCode, // Error Code (70H or 71H)
    SegmentNum, // Number of current segment descriptor
    SenseKey, // Sense Key(See bit definitions too)
    InfoByte0, // Information MSB
    InfoByte1, // Information MID
    InfoByte2, // Information MID
    InfoByte3, // Information LSB
    AddSenLen, // Additional Sense Length
    ComSpecInf0, // Command Specific Information MSB
    ComSpecInf1, // Command Specific Information MID
    ComSpecInf2, // Command Specific Information MID
    ComSpecInf3, // Command Specific Information LSB
    AddSenseCode, // Additional Sense Code
    AddSenQual, // Additional Sense Code Qualifier
    FieldRepUCode, // Field Replaceable Unit Code
    SenKeySpec15, // Sense Key Specific 15th byte
    SenKeySpec16, // Sense Key Specific 16th byte
    SenKeySpec17, // Sense Key Specific 17th byte
    AddSenseBytes : BYTE; // Additional Sense Bytes
  end;

  TSENSE_DATA_FMT = SENSE_DATA_FMT;
  PSENSE_DATA_FMT = ^SENSE_DATA_FMT;

{ ---------------------------------------------------------------------
  REQUEST SENSE ERROR CODE
  ---------------------------------------------------------------------}

const

  SERROR_CURRENT = $70; // Current Errors
  SERROR_DEFERED = $71; // Deferred Errors

{ ---------------------------------------------------------------------
  REQUEST SENSE BIT DEFINITIONS
  ---------------------------------------------------------------------}

  SENSE_VALID   = $80; // Byte 0 Bit 7
  SENSE_FILEMRK = $80; // Byte 2 Bit 7
  SENSE_EOM     = $40; // Byte 2 Bit 6
  SENSE_ILI     = $20; // Byte 2 Bit 5

{ ---------------------------------------------------------------------
  REQUEST SENSE SENSE KEY DEFINITIONS
  ---------------------------------------------------------------------}

  KEY_NOSENSE    = $00; // No Sense
  KEY_RECERROR   = $01; // Recovered Error
  KEY_NOTREADY   = $02; // Not Ready
  KEY_MEDIUMERR  = $03; // Medium Error
  KEY_HARDERROR  = $04; // Hardware Error
  KEY_ILLGLREQ   = $05; // Illegal Request
  KEY_UNITATT    = $06; // Unit Attention
  KEY_DATAPROT   = $07; // Data Protect
  KEY_BLANKCHK   = $08; // Blank Check
  KEY_VENDSPEC   = $09; // Vendor Specific
  KEY_COPYABORT  = $0A; // Copy Abort
  KEY_EQUAL      = $0C; // Equal (Search)
  KEY_VOLOVRFLW  = $0D; // Volume Overflow
  KEY_MISCOMP    = $0E; // Miscompare (Search)
  KEY_RESERVED   = $0F; // Reserved

{ ---------------------------------------------------------------------
  PERIPHERAL DEVICE TYPE DEFINITIONS
  ---------------------------------------------------------------------}

  DTYPE_DASD    = $00; // Disk Device
  DTYPE_SEQD    = $01; // Tape Device
  DTYPE_PRNT    = $02; // Printer
  DTYPE_PROC    = $03; // Processor
  DTYPE_WORM    = $04; // Write-once read-multiple
  DTYPE_CROM    = $05; // CD-ROM device
  DTYPE_CDROM   = DTYPE_CROM;
  DTYPE_SCAN    = $06; // Scanner device
  DTYPE_OPTI    = $07; // Optical memory device
  DTYPE_JUKE    = $08; // Medium Changer device
  DTYPE_COMM    = $09; // Communications device
  DTYPE_RESL    = $0A; // Reserved (low)
  DTYPE_RESH    = $1E; // Reserved (high)
  DTYPE_UNKNOWN = $1F; // Unknown or no device type

{ ---------------------------------------------------------------------
  ANSI APPROVED VERSION DEFINITIONS
  ---------------------------------------------------------------------}

  ANSI_MAYBE = $0; // Device may or may not be ANSI approved stand
  ANSI_SCSI1 = $1; // Device complies to ANSI X3.131-1986 (SCSI-1)
  ANSI_SCSI2 = $2; // Device complies to SCSI-2
  ANSI_RESLO = $3; // Reserved (low)
  ANSI_RESHI = $7; // Reserved (high)

implementation

end.
