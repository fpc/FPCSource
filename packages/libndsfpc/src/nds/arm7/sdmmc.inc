{$ifdef NDS_INTERFACE}

{$define DATA32_SUPPORT}

const
  SDMMC_BASE = $04004800;

  REG_SDCMD       = $00;
  REG_SDPORTSEL   = $02;
  REG_SDCMDARG    = $04;
  REG_SDCMDARG0   = $04;
  REG_SDCMDARG1  	= $06;
  REG_SDSTOP      = $08;
  REG_SDRESP      = $0c;
  REG_SDBLKCOUNT  = $0a;
  
  REG_SDRESP0     = $0c;
  REG_SDRESP1     = $0e;
  REG_SDRESP2     = $10;
  REG_SDRESP3     = $12;
  REG_SDRESP4     = $14;
  REG_SDRESP5     = $16;
  REG_SDRESP6     = $18;
  REG_SDRESP7     = $1a;

  REG_SDSTATUS0	  = $1c;
  REG_SDSTATUS1	  = $1e;

  REG_SDIRMASK0   = $20;
  REG_SDIRMASK1   = $22;
  REG_SDCLKCTL    = $24;
  
  REG_SDBLKLEN    = $26;
  REG_SDOPT       = $28;
  REG_SDFIFO      = $30;

  REG_SDDATACTL   = $d8;
  REG_SDRESET     = $e0;
  REG_SDPROTECTED = $f6;

  REG_SDDATACTL32  = $100;
  REG_SDBLKLEN32   = $104;
  REG_SDBLKCOUNT32 = $108;
  REG_SDFIFO32     = $10C;

  REG_CLK_AND_WAIT_CTL = $138;
  REG_RESET_SDIO       = $1e0;
  
  TMIO_STAT0_CMDRESPEND    = $0001;
  TMIO_STAT0_DATAEND       = $0004;
  TMIO_STAT0_CARD_REMOVE   = $0008;
  TMIO_STAT0_CARD_INSERT   = $0010;
  TMIO_STAT0_SIGSTATE      = $0020;
  TMIO_STAT0_WRPROTECT     = $0080;
  TMIO_STAT0_CARD_REMOVE_A = $0100;
  TMIO_STAT0_CARD_INSERT_A = $0200;
  TMIO_STAT0_SIGSTATE_A    = $0400;

  TMIO_STAT1_CMD_IDX_ERR   = $0001;
  TMIO_STAT1_CRCFAIL       = $0002;
  TMIO_STAT1_STOPBIT_ERR   = $0004;
  TMIO_STAT1_DATATIMEOUT   = $0008;
  TMIO_STAT1_RXOVERFLOW    = $0010;
  TMIO_STAT1_TXUNDERRUN    = $0020;
  TMIO_STAT1_CMDTIMEOUT    = $0040;
  TMIO_STAT1_RXRDY         = $0100;
  TMIO_STAT1_TXRQ          = $0200;
  TMIO_STAT1_ILL_FUNC      = $2000;
  TMIO_STAT1_CMD_BUSY      = $4000;
  TMIO_STAT1_ILL_ACCESS    = $8000;
  
//Comes from TWLSDK mongoose.tef DWARF info
  SDMC_NORMAL              = $00000000;
  SDMC_ERR_COMMAND         = $00000001;
  SDMC_ERR_CRC             = $00000002;
  SDMC_ERR_END             = $00000004;
  SDMC_ERR_TIMEOUT         = $00000008;
  SDMC_ERR_FIFO_OVF        = $00000010;
  SDMC_ERR_FIFO_UDF        = $00000020;
  SDMC_ERR_WP              = $00000040;
  SDMC_ERR_ABORT           = $00000080;
  SDMC_ERR_FPGA_TIMEOUT    = $00000100;
  SDMC_ERR_PARAM           = $00000200;
  SDMC_ERR_R1_STATUS       = $00000800;
  SDMC_ERR_NUM_WR_SECTORS  = $00001000;
  SDMC_ERR_RESET           = $00002000;
  SDMC_ERR_ILA             = $00004000;
  SDMC_ERR_INFO_DETECT     = $00008000;

  SDMC_STAT_ERR_UNKNOWN    = $00080000;
  SDMC_STAT_ERR_CC         = $00100000;
  SDMC_STAT_ERR_ECC_FAILED = $00200000;
  SDMC_STAT_ERR_CRC        = $00800000;
  SDMC_STAT_ERR_OTHER      = $f9c70008;

  TMIO_MASK_ALL            = $837f031d;

  TMIO_MASK_GW      = (TMIO_STAT1_ILL_ACCESS or TMIO_STAT1_CMDTIMEOUT or 
	                     TMIO_STAT1_TXUNDERRUN or TMIO_STAT1_RXOVERFLOW or 
											 TMIO_STAT1_DATATIMEOUT or TMIO_STAT1_STOPBIT_ERR or 
											 TMIO_STAT1_CRCFAIL or TMIO_STAT1_CMD_IDX_ERR);

  TMIO_MASK_READOP  = (TMIO_STAT1_RXRDY or TMIO_STAT0_DATAEND); // TMIO_STAT1_DATAEND ???
  TMIO_MASK_WRITEOP = (TMIO_STAT1_TXRQ or TMIO_STAT0_DATAEND);  // TMIO_STAT1_DATAEND ???
  
type
	TMMCDevice = packed record
    data: pcuint8;
    size: cuint32;
    error: cuint32;
    stat0: cuint16;
    stat1: cuint16;
    ret: array[0..3] of cuint32;
    initarg: cuint32;
    isSDHC: cuint32;
    clk: cuint32;
    SDOPT: cuint32;
    devicenumber: cuint32;
    total_size: cuint32;
    res: cuint32;
  end;
  mmcdevice = TMMCDevice;  
  

procedure sdmmc_controller_init(); cdecl; external;
procedure sdmmc_initirq(); cdecl; external;
function sdmmc_cardinserted(): cint; cdecl; external;

function sdmmc_sdcard_init(): cint; cdecl; external;
function sdmmc_sdcard_readsectors(sector_no: cuint32; numsectors: cuint32; _out: pointer): cint; cdecl; external;
function sdmmc_sdcard_writesectors(sector_no: cuint32; numsectors: cuint32; _in: pointer): cint; cdecl; external;


var
  sdmmc_curdevice: cint; cvar; external;
  sdmmc_cid: pcuint32; cvar; external;

function sdmmc_read16(reg: cuint16): cuint16; inline;
procedure sdmmc_write16(reg, val: cuint16); inline;
function sdmmc_read32(reg: cuint16): cuint32; inline;
procedure sdmmc_write32(reg: cuint16; val: cuint32); inline;
procedure sdmmc_mask16(reg, clear, _set: cuint16); inline;
procedure setckl(data: cuint32); inline;


{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function sdmmc_read16(reg: cuint16): cuint16; inline;
begin
  sdmmc_read16 := pcuint16(SDMMC_BASE + reg)^;
end;

procedure sdmmc_write16(reg, val: cuint16); inline;
begin
	pcuint16(SDMMC_BASE + reg)^ := val;
end;

function sdmmc_read32(reg: cuint16): cuint32; inline;
begin
  sdmmc_read32 := pcuint32(SDMMC_BASE + reg)^;
end;

procedure sdmmc_write32(reg: cuint16; val: cuint32); inline;
begin
  pcuint32(SDMMC_BASE + reg)^ := val;
end;

procedure sdmmc_mask16(reg, clear, _set: cuint16); inline;
var
  val: cuint16;
begin
	val := sdmmc_read16(reg);
	val := val and not clear;
	val := val or _set;
	sdmmc_write16(reg, val);
end;

procedure setckl(data: cuint32); inline;
begin
  sdmmc_mask16(REG_SDCLKCTL, $100, 0);
  sdmmc_mask16(REG_SDCLKCTL, $2FF, data and $2FF);
  sdmmc_mask16(REG_SDCLKCTL, $0, $100);
end;

{$endif NDS_IMPLEMENTATION}

