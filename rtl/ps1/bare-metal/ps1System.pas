{$MODE OBJFPC} {$H+}
{$LINK sysintern.o}
{$ALIGN 4}
unit ps1System;
interface
uses ps1COP0;

{ --- Constants ------------------------------------------------------------ }

const
  F_CPU      : dword = 33868800;
  F_GPU_NTSC : dword = 53693175;
  F_GPU_PAL  : dword = 53203425;

{ Base addresses (use as plain constants) }
const
  DEV0_BASE  = $BF000000;
  EXP1_BASE  = $BF000000;
  CACHE_BASE = $9F800000; // Cannot be accessed from KSEG1
  IO_BASE    = $BF801000;
  EXP2_BASE  = $BF802000;
  EXP3_BASE  = $BFA00000;
  DEV2_BASE  = $BFC00000;
  CPU_BASE   = $FFFE0000;

{ --- Bus interface -------------------------------------------------------- }

const
  BIU_CTRL_WRITE_DELAY_BITMASK = 15 shl  0;
  BIU_CTRL_READ_DELAY_BITMASK  = 15 shl  4;
  BIU_CTRL_RECOVERY            =  1 shl  8;
  BIU_CTRL_HOLD                =  1 shl  9;
  BIU_CTRL_FLOAT               =  1 shl 10;
  BIU_CTRL_PRESTROBE           =  1 shl 11;
  BIU_CTRL_WIDTH_8             =  0 shl 12;
  BIU_CTRL_WIDTH_16            =  1 shl 12;
  BIU_CTRL_AUTO_INCR           =  1 shl 13;
  BIU_CTRL_SIZE_BITMASK        = 31 shl 16;
  BIU_CTRL_DMA_DELAY_BITMASK   = 15 shl 24;
  BIU_CTRL_ADDR_ERROR          =  1 shl 28;
  BIU_CTRL_DMA_DELAY           =  1 shl 29;
  BIU_CTRL_DMA32               =  1 shl 30;
  BIU_CTRL_WAIT                =  1 shl 31;

var
  BIU_DEV0_ADDR: LongWord absolute (IO_BASE or $000); // PIO/573
  BIU_EXP2_ADDR: LongWord absolute (IO_BASE or $004); // PIO/debug
  BIU_DEV0_CTRL: LongWord absolute (IO_BASE or $008); // PIO/573
  BIU_EXP3_CTRL: LongWord absolute (IO_BASE or $00C); // PIO/debug
  BIU_DEV2_CTRL: LongWord absolute (IO_BASE or $010); // BIOS ROM
  BIU_DEV4_CTRL: LongWord absolute (IO_BASE or $014); // SPU
  BIU_DEV5_CTRL: LongWord absolute (IO_BASE or $018); // CD-ROM
  BIU_EXP2_CTRL: LongWord absolute (IO_BASE or $01C); // PIO/debug
  BIU_COM_DELAY: LongWord absolute (IO_BASE or $020);


{ --- Serial interfaces ---------------------------------------------------- }

const
  SIO_STAT_TX_NOT_FULL   = 1 shl 0;
  SIO_STAT_RX_NOT_EMPTY  = 1 shl 1;
  SIO_STAT_TX_EMPTY      = 1 shl 2;
  SIO_STAT_RX_PARITY_ERR = 1 shl 3;
  SIO_STAT_RX_OVERRUN    = 1 shl 4; // SIO1 only
  SIO_STAT_RX_STOP_ERR   = 1 shl 5; // SIO1 only
  SIO_STAT_RX_INVERT     = 1 shl 6; // SIO1 only
  SIO_STAT_DSR           = 1 shl 7; // DSR is /ACK on SIO0
  SIO_STAT_CTS           = 1 shl 8; // SIO1 only
  SIO_STAT_IRQ           = 1 shl 9;

  SIO_MODE_BAUD_BITMASK   = 3 shl 0;
  SIO_MODE_BAUD_DIV1      = 1 shl 0;
  SIO_MODE_BAUD_DIV16     = 2 shl 0;
  SIO_MODE_BAUD_DIV64     = 3 shl 0;
  SIO_MODE_DATA_BITMASK   = 3 shl 2;
  SIO_MODE_DATA_5         = 0 shl 2;
  SIO_MODE_DATA_6         = 1 shl 2;
  SIO_MODE_DATA_7         = 2 shl 2;
  SIO_MODE_DATA_8         = 3 shl 2;
  SIO_MODE_PARITY_BITMASK = 3 shl 4;
  SIO_MODE_PARITY_NONE    = 0 shl 4;
  SIO_MODE_PARITY_EVEN    = 1 shl 4;
  SIO_MODE_PARITY_ODD     = 3 shl 4;
  SIO_MODE_STOP_BITMASK   = 3 shl 6; // SIO1 only
  SIO_MODE_STOP_1         = 1 shl 6; // SIO1 only
  SIO_MODE_STOP_1_5       = 2 shl 6; // SIO1 only
  SIO_MODE_STOP_2         = 3 shl 6; // SIO1 only
  SIO_MODE_SCK_INVERT     = 1 shl 8; // SIO0 only

  SIO_CTRL_TX_ENABLE      = 1 shl  0;
  SIO_CTRL_DTR            = 1 shl  1; // DTR is /CS on SIO0
  SIO_CTRL_RX_ENABLE      = 1 shl  2;
  SIO_CTRL_TX_INVERT      = 1 shl  3; // SIO1 only
  SIO_CTRL_ACKNOWLEDGE    = 1 shl  4;
  SIO_CTRL_RTS            = 1 shl  5; // SIO1 only
  SIO_CTRL_RESET          = 1 shl  6;
  SIO_CTRL_TX_IRQ_ENABLE  = 1 shl 10;
  SIO_CTRL_RX_IRQ_ENABLE  = 1 shl 11;
  SIO_CTRL_DSR_IRQ_ENABLE = 1 shl 12; // DSR is /ACK on SIO0
  SIO_CTRL_CS_PORT_1      = 0 shl 13; // SIO0 only
  SIO_CTRL_CS_PORT_2      = 1 shl 13; // SIO0 only

function SIO_DATA(N: dword): Byte; inline; // 8-bit access (see note)
function SIO_STAT(N: dword): Word; inline;
function SIO_MODE(N: dword): Word; inline;
function SIO_CTRL(N: dword): Word; inline;
function SIO_BAUD(N: dword): Word; inline;
procedure SIO_DATA_Set(N: dword; value: byte); inline;
procedure SIO_STAT_Set(N: dword; value: word); inline;
procedure SIO_MODE_Set(N: dword; value: word); inline;
procedure SIO_CTRL_Set(N: dword; value: word); inline;
procedure SIO_BAUD_Set(N: dword; value: word); inline;


{ --- DRAM controller ------------------------------------------------------ }

const
  DRAM_CTRL_UNKNOWN     = 1 shl  3;
  DRAM_CTRL_FETCH_DELAY = 1 shl  7;
  DRAM_CTRL_SIZE_MUL1   = 0 shl  9;
  DRAM_CTRL_SIZE_MUL4   = 1 shl  9;
  DRAM_CTRL_COUNT_1     = 0 shl 10; // 1 DRAM bank (single RAS)
  DRAM_CTRL_COUNT_2     = 1 shl 10; // 2 DRAM banks (dual RAS)
  DRAM_CTRL_SIZE_1MB    = 0 shl 11; // 1MB chips (4MB with MUL4)
  DRAM_CTRL_SIZE_2MB    = 1 shl 11; // 2MB chips (8MB with MUL4)

var
  DRAM_CTRL: LongWord absolute (IO_BASE or $060);

{ --- IRQ controller ------------------------------------------------------- }

{ Use numeric constants (not an enum) to allow aliasing (GUN/PIO = 10) }
const
  IRQ_VSYNC  =  0;
  IRQ_GPU    =  1;
  IRQ_CDROM  =  2;
  IRQ_DMA    =  3;
  IRQ_TIMER0 =  4;
  IRQ_TIMER1 =  5;
  IRQ_TIMER2 =  6;
  IRQ_SIO0   =  7;
  IRQ_SIO1   =  8;
  IRQ_SPU    =  9;
  IRQ_GUN    = 10;
  IRQ_PIO    = 10;

var
  IRQ_STAT: Word absolute (IO_BASE + $070);
  IRQ_MASK: Word absolute (IO_BASE + $074);

{ --- DMA ------------------------------------------------------------------ }

const
  DMA_MDEC_IN  = 0;
  DMA_MDEC_OUT = 1;
  DMA_GPU      = 2;
  DMA_CDROM    = 3;
  DMA_SPU      = 4;
  DMA_PIO      = 5;
  DMA_OTC      = 6;

  DMA_CHCR_READ             = 0 shl  0;
  DMA_CHCR_WRITE            = 1 shl  0;
  DMA_CHCR_REVERSE          = 1 shl  1;
  DMA_CHCR_CHOPPING         = 1 shl  8;
  DMA_CHCR_MODE_BITMASK     = 3 shl  9;
  DMA_CHCR_MODE_BURST       = 0 shl  9;
  DMA_CHCR_MODE_SLICE       = 1 shl  9;
  DMA_CHCR_MODE_LIST        = 2 shl  9;
  DMA_CHCR_DMA_TIME_BITMASK = 7 shl 16;
  DMA_CHCR_CPU_TIME_BITMASK = 7 shl 20;
  DMA_CHCR_ENABLE           = 1 shl 24;
  DMA_CHCR_TRIGGER          = 1 shl 28;
  DMA_CHCR_PAUSE            = 1 shl 29;  // Burst mode only

function DMA_DPCR_CH_PRIORITY_BITMASK(N: dword): dword; inline;
function DMA_DPCR_CH_PRIORITY(N, priority: dword): dword; inline;
function DMA_DPCR_CH_ENABLE(N: dword): dword; inline;

const
  DMA_DICR_CH_MODE_BITMASK   = $7F shl  0;
  DMA_DICR_BUS_ERROR         =   1 shl 15;
  DMA_DICR_CH_ENABLE_BITMASK = $7F shl 16;
  DMA_DICR_IRQ_ENABLE        =   1 shl 23;
  DMA_DICR_CH_STAT_BITMASK   = $7F shl 24;
  DMA_DICR_IRQ               =   1 shl 31;

function DMA_DICR_CH_MODE(N: dword): dword;   inline;
function DMA_DICR_CH_ENABLE(N: dword): dword; inline;
function DMA_DICR_CH_STAT(N: dword): dword;   inline;

function DMA_MADR(N: dword): LongWord; inline;
function DMA_BCR(N: dword): LongWord;  inline;
function DMA_CHCR(N: dword): LongWord; inline;
procedure DMA_MADR_Set(N: dword; value: LongWord); inline;
procedure DMA_BCR_Set(N: dword; value: LongWord);  inline;
procedure DMA_CHCR_Set(N: dword; value: LongWord); inline;

var
  DMA_DPCR: LongWord absolute (IO_BASE or $0F0);
  DMA_DICR: LongWord absolute (IO_BASE or $0F4);

{ --- Timers --------------------------------------------------------------- }

const
  TIMER_CTRL_ENABLE_SYNC     = 1 shl  0;
  TIMER_CTRL_SYNC_BITMASK    = 3 shl  1;
  TIMER_CTRL_SYNC_PAUSE      = 0 shl  1;
  TIMER_CTRL_SYNC_RESET1     = 1 shl  1;
  TIMER_CTRL_SYNC_RESET2     = 2 shl  1;
  TIMER_CTRL_SYNC_PAUSE_ONCE = 3 shl  1;
  TIMER_CTRL_RELOAD          = 1 shl  3;
  TIMER_CTRL_IRQ_ON_RELOAD   = 1 shl  4;
  TIMER_CTRL_IRQ_ON_OVERFLOW = 1 shl  5;
  TIMER_CTRL_IRQ_REPEAT      = 1 shl  6;
  TIMER_CTRL_IRQ_LATCH       = 1 shl  7;
  TIMER_CTRL_EXT_CLOCK       = 1 shl  8;
  TIMER_CTRL_PRESCALE        = 1 shl  9;
  TIMER_CTRL_IRQ             = 1 shl 10;
  TIMER_CTRL_RELOADED        = 1 shl 11;
  TIMER_CTRL_OVERFLOWED      = 1 shl 12;

function TIMER_VALUE(N: dword): Word;  inline;
function TIMER_CTRL_REG(N: dword): Word; inline; // renamed to avoid clash with constant block
function TIMER_RELOAD(N: dword): Word; inline;
procedure TIMER_VALUE_Set(N: dword; value: Word);  inline;
procedure TIMER_CTRL_REG_Set(N: dword; value: Word); inline;
procedure TIMER_RELOAD_Set(N: dword; value: Word); inline;


{ --- CD-ROM drive --------------------------------------------------------- }

const
  CDROM_HSTS_RA_BITMASK = 3 shl 0;
  CDROM_HSTS_ADPBUSY    = 1 shl 2;
  CDROM_HSTS_PRMEMPT    = 1 shl 3;
  CDROM_HSTS_PRMWRDY    = 1 shl 4;
  CDROM_HSTS_RSLRRDY    = 1 shl 5;
  CDROM_HSTS_DRQSTS     = 1 shl 6;
  CDROM_HSTS_BUSYSTS    = 1 shl 7;

  CDROM_HINT_INT_BITMASK = 7 shl 0;
  CDROM_HINT_INT0        = 1 shl 0;
  CDROM_HINT_INT1        = 1 shl 1;
  CDROM_HINT_INT2        = 1 shl 2;
  CDROM_HINT_BFEMPT      = 1 shl 3;
  CDROM_HINT_BFWRDY      = 1 shl 4;

  CDROM_HCHPCTL_SMEN = 1 shl 5;
  CDROM_HCHPCTL_BFWR = 1 shl 6;
  CDROM_HCHPCTL_BFRD = 1 shl 7;

  CDROM_HCLRCTL_CLRINT_BITMASK = 7 shl 0;
  CDROM_HCLRCTL_CLRINT0        = 1 shl 0;
  CDROM_HCLRCTL_CLRINT1        = 1 shl 1;
  CDROM_HCLRCTL_CLRINT2        = 1 shl 2;
  CDROM_HCLRCTL_CLRBFEMPT      = 1 shl 3;
  CDROM_HCLRCTL_CLRBFWRDY      = 1 shl 4;
  CDROM_HCLRCTL_SMADPCLR       = 1 shl 5;
  CDROM_HCLRCTL_CLRPRM         = 1 shl 6;
  CDROM_HCLRCTL_CHPRST         = 1 shl 7;

  CDROM_CI_SM       = 1 shl 0;
  CDROM_CI_FS       = 1 shl 2;
  CDROM_CI_BITLNGTH = 1 shl 4;
  CDROM_CI_EMPHASIS = 1 shl 6;

  CDROM_ADPCTL_ADPMUTE = 1 shl 0;
  CDROM_ADPCTL_CHNGATV = 1 shl 5;

var
  CDROM_HSTS: Byte       absolute (IO_BASE or $800);
  CDROM_RESULT: Byte     absolute (IO_BASE or $801);
  CDROM_RDDATA: Byte     absolute (IO_BASE or $802);
  CDROM_HINTMSK_R: Byte  absolute (IO_BASE or $803);
  CDROM_HINTSTS: Byte    absolute (IO_BASE or $803);

  CDROM_ADDRESS: Byte     absolute (IO_BASE or $800);
  CDROM_COMMAND: Byte     absolute (IO_BASE or $801);
  CDROM_PARAMETER: Byte   absolute (IO_BASE or $802);
  CDROM_HCHPCTL: Byte     absolute (IO_BASE or $803);
  CDROM_WRDATA: Byte      absolute (IO_BASE or $801);
  CDROM_HINTMSK_W: Byte   absolute (IO_BASE or $802);
  CDROM_HCLRCTL: Byte     absolute (IO_BASE or $803);
  CDROM_CI: Byte          absolute (IO_BASE or $801);
  CDROM_ATV0: Byte        absolute (IO_BASE or $802);
  CDROM_ATV1: Byte        absolute (IO_BASE or $803);
  CDROM_ATV2: Byte        absolute (IO_BASE or $801);
  CDROM_ATV3: Byte        absolute (IO_BASE or $802);
  CDROM_ADPCTL: Byte      absolute (IO_BASE or $803);


{ --- GPU ------------------------------------------------------------------ }

const
  GP1_STAT_PAGE_X_BITMASK      = $F  shl  0; // GP0_CMD_TEXPAGE
  GP1_STAT_PAGE_Y0             =  1  shl  4; // GP0_CMD_TEXPAGE
  GP1_STAT_BLEND_BITMASK       =  3  shl  5; // GP0_CMD_TEXPAGE
  GP1_STAT_BLEND_SEMITRANS     =  0  shl  5; // GP0_CMD_TEXPAGE
  GP1_STAT_BLEND_ADD           =  1  shl  5; // GP0_CMD_TEXPAGE
  GP1_STAT_BLEND_SUBTRACT      =  2  shl  5; // GP0_CMD_TEXPAGE
  GP1_STAT_BLEND_DIV4_ADD      =  3  shl  5; // GP0_CMD_TEXPAGE
  GP1_STAT_COLOR_BITMASK       =  3  shl  7; // GP0_CMD_TEXPAGE
  GP1_STAT_COLOR_4BPP          =  0  shl  7; // GP0_CMD_TEXPAGE
  GP1_STAT_COLOR_8BPP          =  1  shl  7; // GP0_CMD_TEXPAGE
  GP1_STAT_COLOR_16BPP         =  2  shl  7; // GP0_CMD_TEXPAGE
  GP1_STAT_DITHER              =  1  shl  9; // GP0_CMD_TEXPAGE
  GP1_STAT_UNLOCK_FB           =  1  shl 10; // GP0_CMD_TEXPAGE
  GP1_STAT_SET_MASK            =  1  shl 11; // GP0_CMD_FB_MASK
  GP1_STAT_USE_MASK            =  1  shl 12; // GP0_CMD_FB_MASK
  GP1_STAT_DISP_FIELD_BITMASK  =  1  shl 13;
  GP1_STAT_DISP_FIELD_EVEN     =  0  shl 13;
  GP1_STAT_DISP_FIELD_ODD      =  1  shl 13;
  GP1_STAT_PAGE_Y1             =  1  shl 15; // GP0_CMD_TEXPAGE
  GP1_STAT_FB_HRES_BITMASK     =  7  shl 16; // GP1_CMD_FB_MODE
  GP1_STAT_FB_VRES_BITMASK     =  1  shl 19; // GP1_CMD_FB_MODE
  GP1_STAT_FB_VRES_256         =  0  shl 19; // GP1_CMD_FB_MODE
  GP1_STAT_FB_VRES_512         =  1  shl 19; // GP1_CMD_FB_MODE
  GP1_STAT_FB_MODE_BITMASK     =  1  shl 20; // GP1_CMD_FB_MODE
  GP1_STAT_FB_MODE_NTSC        =  0  shl 20; // GP1_CMD_FB_MODE
  GP1_STAT_FB_MODE_PAL         =  1  shl 20; // GP1_CMD_FB_MODE
  GP1_STAT_FB_COLOR_BITMASK    =  1  shl 21; // GP1_CMD_FB_MODE
  GP1_STAT_FB_COLOR_16BPP      =  0  shl 21; // GP1_CMD_FB_MODE
  GP1_STAT_FB_COLOR_24BPP      =  1  shl 21; // GP1_CMD_FB_MODE
  GP1_STAT_FB_INTERLACE        =  1  shl 22; // GP1_CMD_FB_MODE
  GP1_STAT_DISP_BLANK          =  1  shl 23; // GP1_CMD_DISP_BLANK
  GP1_STAT_IRQ                 =  1  shl 24;
  GP1_STAT_DREQ                =  1  shl 25;
  GP1_STAT_CMD_READY           =  1  shl 26;
  GP1_STAT_READ_READY          =  1  shl 27;
  GP1_STAT_WRITE_READY         =  1  shl 28;
  GP1_STAT_DREQ_MODE_BITMASK   =  3  shl 29; // GP1_CMD_DREQ_MODE
  GP1_STAT_DREQ_MODE_NONE      =  0  shl 29; // GP1_CMD_DREQ_MODE
  GP1_STAT_DREQ_MODE_FIFO      =  1  shl 29; // GP1_CMD_DREQ_MODE
  GP1_STAT_DREQ_MODE_GP0_WRITE =  2  shl 29; // GP1_CMD_DREQ_MODE
  GP1_STAT_DREQ_MODE_GP0_READ  =  3  shl 29; // GP1_CMD_DREQ_MODE
  GP1_STAT_DRAW_FIELD_BITMASK  =  1  shl 31;
  GP1_STAT_DRAW_FIELD_EVEN     =  0  shl 31;
  GP1_STAT_DRAW_FIELD_ODD      =  1  shl 31;
var
  GPU_GP0: LongWord absolute $1F801810;
  GPU_GP1: LongWord absolute $1F801814;

{ --- MDEC ----------------------------------------------------------------- }

const
  MDEC_CMD_LENGTH_BITMASK     =  $FFFF shl  0; // MDEC_CMD_OP_DECODE
  MDEC_CMD_USE_CHROMA         =      1 shl  0; // MDEC_CMD_OP_SET_QUANT_TABLE
  MDEC_CMD_SIGNED             =      1 shl 25; // MDEC_CMD_OP_DECODE
  MDEC_CMD_16BPP_MASK         =      1 shl 26; // MDEC_CMD_OP_DECODE
  MDEC_CMD_FORMAT_BITMASK     =      3 shl 27; // MDEC_CMD_OP_DECODE
  MDEC_CMD_FORMAT_4BPP        =      0 shl 27; // MDEC_CMD_OP_DECODE
  MDEC_CMD_FORMAT_8BPP        =      1 shl 27; // MDEC_CMD_OP_DECODE
  MDEC_CMD_FORMAT_24BPP       =      2 shl 27; // MDEC_CMD_OP_DECODE
  MDEC_CMD_FORMAT_16BPP       =      3 shl 27; // MDEC_CMD_OP_DECODE
  MDEC_CMD_OP_BITMASK         =      7 shl 29;
  MDEC_CMD_OP_NOP             =      0 shl 29;
  MDEC_CMD_OP_DECODE          =      1 shl 29;
  MDEC_CMD_OP_SET_QUANT_TABLE =      2 shl 29;
  MDEC_CMD_OP_SET_IDCT_TABLE  =      3 shl 29;

  MDEC_STAT_LENGTH_BITMASK =  $FFFF shl  0;
  MDEC_STAT_BLOCK_BITMASK  =      7 shl 16;
  MDEC_STAT_BLOCK_Y0       =      0 shl 16;
  MDEC_STAT_BLOCK_Y1       =      1 shl 16;
  MDEC_STAT_BLOCK_Y2       =      2 shl 16;
  MDEC_STAT_BLOCK_Y3       =      3 shl 16;
  MDEC_STAT_BLOCK_CR       =      4 shl 16;
  MDEC_STAT_BLOCK_CB       =      5 shl 16;
  MDEC_STAT_16BPP_MASK     =      1 shl 23;
  MDEC_STAT_SIGNED         =      1 shl 24;
  MDEC_STAT_FORMAT_BITMASK =      3 shl 25;
  MDEC_STAT_FORMAT_4BPP    =      0 shl 25;
  MDEC_STAT_FORMAT_8BPP    =      1 shl 25;
  MDEC_STAT_FORMAT_24BPP   =      2 shl 25;
  MDEC_STAT_FORMAT_16BPP   =      3 shl 25;
  MDEC_STAT_DREQ_OUT       =      1 shl 27;
  MDEC_STAT_DREQ_IN        =      1 shl 28;
  MDEC_STAT_BUSY           =      1 shl 29;
  MDEC_STAT_DATA_FULL      =      1 shl 30;
  MDEC_STAT_DATA_EMPTY     =      1 shl 31;

  MDEC_CTRL_DMA_OUT = 1 shl 29;
  MDEC_CTRL_DMA_IN  = 1 shl 30;
  MDEC_CTRL_RESET   = 1 shl 31;

var
  MDEC0: PLongWord absolute (IO_BASE or $820);
  MDEC1: PLongWord absolute (IO_BASE or $824);

{ --- SPU ------------------------------------------------------------------ }

const
  SPU_STAT_CDDA           = 1 shl  0;
  SPU_STAT_EXT            = 1 shl  1;
  SPU_STAT_CDDA_REVERB    = 1 shl  2;
  SPU_STAT_EXT_REVERB     = 1 shl  3;
  SPU_STAT_XFER_BITMASK   = 3 shl  4;
  SPU_STAT_XFER_NONE      = 0 shl  4;
  SPU_STAT_XFER_WRITE     = 1 shl  4;
  SPU_STAT_XFER_DMA_WRITE = 2 shl  4;
  SPU_STAT_XFER_DMA_READ  = 3 shl  4;
  SPU_STAT_IRQ            = 1 shl  6;
  SPU_STAT_DREQ           = 1 shl  7;
  SPU_STAT_WRITE_REQ      = 1 shl  8;
  SPU_STAT_READ_REQ       = 1 shl  9;
  SPU_STAT_BUSY           = 1 shl 10;
  SPU_STAT_CAPTURE_BUF    = 1 shl 11;

  SPU_CTRL_CDDA           = 1 shl  0;
  SPU_CTRL_EXT            = 1 shl  1;
  SPU_CTRL_CDDA_REVERB    = 1 shl  2;
  SPU_CTRL_EXT_REVERB     = 1 shl  3;
  SPU_CTRL_XFER_BITMASK   = 3 shl  4;
  SPU_CTRL_XFER_NONE      = 0 shl  4;
  SPU_CTRL_XFER_WRITE     = 1 shl  4;
  SPU_CTRL_XFER_DMA_WRITE = 2 shl  4;
  SPU_CTRL_XFER_DMA_READ  = 3 shl  4;
  SPU_CTRL_IRQ_ENABLE     = 1 shl  6;
  SPU_CTRL_REVERB         = 1 shl  7;
  SPU_CTRL_UNMUTE         = 1 shl 14;
  SPU_CTRL_ENABLE         = 1 shl 15;

function SPU_CH_VOL_L(N: dword): Word;     inline;
function SPU_CH_VOL_R(N: dword): Word;     inline;
function SPU_CH_FREQ(N: dword): Word;      inline;
function SPU_CH_ADDR(N: dword): Word;      inline;
function SPU_CH_ADSR1(N: dword): Word;     inline;
function SPU_CH_ADSR2(N: dword): Word;     inline;
function SPU_CH_ADSR_VOL(N: dword): Word;  inline;
function SPU_CH_LOOP_ADDR(N: dword): Word; inline;
procedure SPU_CH_VOL_L_Set(N: dword; value: Word);     inline;
procedure SPU_CH_VOL_R_Set(N: dword; value: Word);     inline;
procedure SPU_CH_FREQ_Set(N: dword; value: Word);      inline;
procedure SPU_CH_ADDR_Set(N: dword; value: Word);      inline;
procedure SPU_CH_ADSR1_Set(N: dword; value: Word);     inline;
procedure SPU_CH_ADSR2_Set(N: dword; value: Word);     inline;
procedure SPU_CH_LOOP_ADDR_Set(N: dword; value: Word); inline;

var
 SPU_MASTER_VOL_L: Word absolute (IO_BASE or $D80);
 SPU_MASTER_VOL_R: Word absolute (IO_BASE or $D82);
 SPU_REVERB_VOL_L: Word absolute (IO_BASE or $D84);
 SPU_REVERB_VOL_R: Word absolute (IO_BASE or $D86);
 SPU_FLAG_ON1: Word     absolute (IO_BASE or $D88);
 SPU_FLAG_ON2: Word     absolute (IO_BASE or $D8A);
 SPU_FLAG_OFF1: Word    absolute (IO_BASE or $D8C);
 SPU_FLAG_OFF2: Word    absolute (IO_BASE or $D8E);
 SPU_FLAG_FM1: Word     absolute (IO_BASE or $D90);
 SPU_FLAG_FM2: Word     absolute (IO_BASE or $D92);
 SPU_FLAG_NOISE1: Word  absolute (IO_BASE or $D94);
 SPU_FLAG_NOISE2: Word  absolute (IO_BASE or $D96);
 SPU_FLAG_REVERB1: Word absolute (IO_BASE or $D98);
 SPU_FLAG_REVERB2: Word absolute (IO_BASE or $D9A);
 SPU_FLAG_STATUS1: Word absolute (IO_BASE or $D9C);
 SPU_FLAG_STATUS2: Word absolute (IO_BASE or $D9E);

 SPU_REVERB_ADDR: Word absolute (IO_BASE or $DA2);
 SPU_IRQ_ADDR:    Word absolute (IO_BASE or $DA4);
 SPU_ADDR:        Word absolute (IO_BASE or $DA6);
 SPU_DATA:        Word absolute (IO_BASE or $DA8);
 SPU_CTRL_REG:    Word absolute (IO_BASE or $DAA);
 SPU_DMA_CTRL:    Word absolute (IO_BASE or $DAC);
 SPU_STAT:        Word absolute (IO_BASE or $DAE);

 SPU_CDDA_VOL_L: Word absolute (IO_BASE or $DB0);
 SPU_CDDA_VOL_R: Word absolute (IO_BASE or $DB2);
 SPU_EXT_VOL_L:  Word absolute (IO_BASE or $DB4);
 SPU_EXT_VOL_R:  Word absolute (IO_BASE or $DB6);
 SPU_VOL_STAT_L: Word absolute (IO_BASE or $DB8);
 SPU_VOL_STAT_R: Word absolute (IO_BASE or $DBA);

 SPU_REVERB_DAPF1:   Word absolute (IO_BASE or $DC0);
 SPU_REVERB_DAPF2:   Word absolute (IO_BASE or $DC2);
 SPU_REVERB_VIIR:    Word absolute (IO_BASE or $DC4);
 SPU_REVERB_VCOMB1:  Word absolute (IO_BASE or $DC6);
 SPU_REVERB_VCOMB2:  Word absolute (IO_BASE or $DC8);
 SPU_REVERB_VCOMB3:  Word absolute (IO_BASE or $DCA);
 SPU_REVERB_VCOMB4:  Word absolute (IO_BASE or $DCC);
 SPU_REVERB_VWALL:   Word absolute (IO_BASE or $DCE);
 SPU_REVERB_VAPF1:   Word absolute (IO_BASE or $DD0);
 SPU_REVERB_VAPF2:   Word absolute (IO_BASE or $DD2);
 SPU_REVERB_MLSAME:  Word absolute (IO_BASE or $DD4);
 SPU_REVERB_MRSAME:  Word absolute (IO_BASE or $DD6);
 SPU_REVERB_MLCOMB1: Word absolute (IO_BASE or $DD8);
 SPU_REVERB_MRCOMB1: Word absolute (IO_BASE or $DDA);
 SPU_REVERB_MLCOMB2: Word absolute (IO_BASE or $DDC);
 SPU_REVERB_MRCOMB2: Word absolute (IO_BASE or $DDE);
 SPU_REVERB_DLSAME:  Word absolute (IO_BASE or $DE0);
 SPU_REVERB_DRSAME:  Word absolute (IO_BASE or $DE2);
 SPU_REVERB_MLDIFF:  Word absolute (IO_BASE or $DE4);
 SPU_REVERB_MRDIFF:  Word absolute (IO_BASE or $DE6);
 SPU_REVERB_MLCOMB3: Word absolute (IO_BASE or $DE8);
 SPU_REVERB_MRCOMB3: Word absolute (IO_BASE or $DEA);
 SPU_REVERB_MLCOMB4: Word absolute (IO_BASE or $DEC);
 SPU_REVERB_MRCOMB4: Word absolute (IO_BASE or $DEE);
 SPU_REVERB_DLDIFF:  Word absolute (IO_BASE or $DF0);
 SPU_REVERB_DRDIFF:  Word absolute (IO_BASE or $DF2);
 SPU_REVERB_MLAPF1:  Word absolute (IO_BASE or $DF4);
 SPU_REVERB_MRAPF1:  Word absolute (IO_BASE or $DF6);
 SPU_REVERB_MLAPF2:  Word absolute (IO_BASE or $DF8);
 SPU_REVERB_MRAPF2:  Word absolute (IO_BASE or $DFA);
 SPU_REVERB_VLIN:    Word absolute (IO_BASE or $DFC);
 SPU_REVERB_VRIN:    Word absolute (IO_BASE or $DFE);


{ --- CPU configuration (CW33300) ----------------------------------------- }

const
  CPU_BCC_LOCK           = 1 shl  0;
  CPU_BCC_INV            = 1 shl  1;
  CPU_BCC_TAG            = 1 shl  2;
  CPU_BCC_RAM            = 1 shl  3;
  CPU_BCC_DBLKSZ_BITMASK = 3 shl  4;
  CPU_BCC_DBLKSZ_2       = 0 shl  4;
  CPU_BCC_DBLKSZ_4       = 1 shl  4;
  CPU_BCC_DS             = 1 shl  7;
  CPU_BCC_IBLKSZ_BITMASK = 3 shl  8;
  CPU_BCC_IBLKSZ_2       = 0 shl  8;
  CPU_BCC_IBLKSZ_4       = 1 shl  8;
  CPU_BCC_IS0            = 1 shl 10;
  CPU_BCC_IS1            = 1 shl 11;
  CPU_BCC_INTP           = 1 shl 12;
  CPU_BCC_RDPRI          = 1 shl 13;
  CPU_BCC_NOPAD          = 1 shl 14;
  CPU_BCC_BGNT           = 1 shl 15;
  CPU_BCC_LDSCH          = 1 shl 16;
  CPU_BCC_NOSTR          = 1 shl 17;

var
  CPU_BCC: LongWord absolute (CPU_BASE or $130);



{ --- JOYSTICK CONTROLLER ------------------------------------------------------------------ }
const
  ADDR_CONTROLLER_VALUE  = $01;
  ADDR_MEMORY_CARD_VALUE = $81;

  CMD_INIT_PRESSURE_VALUE   = Ord('@');
  CMD_POLL_VALUE            = Ord('B');
  CMD_CONFIG_MODE_VALUE     = Ord('C');
  CMD_SET_ANALOG_VALUE      = Ord('D');
  CMD_GET_ANALOG_VALUE      = Ord('E');
  CMD_GET_MOTOR_INFO_VALUE  = Ord('F');
  CMD_GET_MOTOR_LIST_VALUE  = Ord('G');
  CMD_GET_MOTOR_STATE_VALUE = Ord('H');
  CMD_GET_MODE_VALUE        = Ord('L');
  CMD_REQUEST_CONFIG_VALUE  = Ord('M');
  CMD_RESPONSE_CONFIG_VALUE = Ord('O');
  CMD_CARD_READ_VALUE       = Ord('R');
  CMD_CARD_GET_SIZE_VALUE   = Ord('S');
  CMD_CARD_WRITE_VALUE      = Ord('W');

  { Controller type strings by 4-bit ID (0..15) }
  ControllerTypes: array[0..15] of PChar = (
    'Unknown',             { ID $0 }
    'Mouse',               { ID $1 }
    'neGcon',              { ID $2 }
    'Konami Justifier',    { ID $3 }
    'Digital controller',  { ID $4 }
    'Analog stick',        { ID $5 }
    'Guncon',              { ID $6 }
    'Analog controller',   { ID $7 }
    'Multitap',            { ID $8 }
    'Keyboard',            { ID $9 }
    'Unknown',             { ID $A }
    'Unknown',             { ID $B }
    'Unknown',             { ID $C }
    'Unknown',             { ID $D }
    'Jogcon',              { ID $E }
    'Configuration mode'   { ID $F }
  );

  { Button names by bit index (0..15) }
  ButtonNames: array[0..15] of PChar = (
    'Select',    { Bit  0 }
    'L3',        { Bit  1 }
    'R3',        { Bit  2 }
    'Start',     { Bit  3 }
    'Up',        { Bit  4 }
    'Right',     { Bit  5 }
    'Down',      { Bit  6 }
    'Left',      { Bit  7 }
    'L2',        { Bit  8 }
    'R2',        { Bit  9 }
    'L1',        { Bit 10 }
    'R1',        { Bit 11 }
    'Triangle',  { Bit 12 }
    'Circle',    { Bit 13 }
    'X',         { Bit 14 }
    'Square'     { Bit 15 }
  );

const
  DTR_DELAY   = 60;
  DSR_TIMEOUT = 120;
  
procedure InitJOYSTICKControllers;
procedure SelectJoystickPort(port: Integer);
function ExchangeSIO0Byte(value: Byte): Byte;
function WaitForSIO0Acknowledge(timeout: Integer): Boolean;
function ExchangeSIO0Packet(address: Byte; request: PByte; response: PByte; reqLength: Integer; maxRespLength: Integer): Integer;


type

  TThread = record
    pc, at, v0, v1, a0, a1, a2, a3 : LongWord;
    t0, t1, t2, t3, t4, t5, t6, t7 : LongWord;
    s0, s1, s2, s3, s4, s5, s6, s7 : LongWord;
    t8, t9, gp, sp, fp, ra, hi, lo : LongWord;
  end;
  PThread = ^TThread;

  TArgFunction = procedure(arg: Pointer); cdecl;
  TVoidFunction = procedure; cdecl;

  IRQChannel = Byte;
  DMAChannel = Byte;

var
  interruptHandler: TArgFunction = nil; export name 'interruptHandler';
  interruptHandlerArg: Pointer = nil; export name 'interruptHandlerArg';

  _mainThread: TThread;

  currentThread : PThread; export name 'currentThread';
  nextThread    : PThread; export name 'nextThread';

procedure _unhandledException(cause: longint; badv: dword); cdecl; export;


{
 * Enables all interrupts at the COP0 side (without altering the IRQ_MASK
 * register). If any IRQs occurred and were not acknowledged while interrupts
 * were disabled, any callback set using setInterruptHandler() will be invoked
 * immediately.
}
procedure enableInterrupts;

{
 * Disables all interrupts at the COP0 side (without altering the
 * IRQ_MASK register). This function is not atomic, but can be used safely as
 * long as no other code is manipulating the COP0 SR register while interrupts
 * are enabled.

   return True if interrupts were previously enabled, false otherwise
}
function disableInterrupts: boolean;


{
 * Forces all pending memory writes to complete and stalls until the
 * write queue is empty. Calling this function is not necessary when accessing
 * memory or hardware registers through KSEG1 as the write queue is only enabled
 * when using KUSEG or KSEG0.
}
procedure flushWriteQueue;


{
 * Initializes a thread structure with the provided entry point
 * (function) and stacktop. The function *must not* return. The stack should be
 * aligned to 8 bytes as required by the MIPS ABI.
 *
 * param arg Optional argument to entry point
 * param stack Pointer to last 8 bytes in the stack
}
procedure initThread(thread: PThread; func: TArgFunction; arg: Pointer; stack: Pointer);


{
 * Sets up the exception handler, disables the one provided by the BIOS
 * kernel and flushes the instruction cache. Must be called only once, before
 * *any* other function in this header is used.
}
procedure installExceptionHandler;


{
 * Restores the BIOS kernel's exception handler. Must be called before
 * returning to the kernel or launching another executable.
}
procedure uninstallExceptionHandler;


{
 * brief Disables interrupts and sets the function that will be called whenever
 * a future interrupt or syscall occurs. Must be called after
 * installExceptionHandler() and before interrupts are enabled. As the callback
 * will run from within the exception handler, it is subject to several
 * limitations:
 *
 * - it cannot call functions that rely on syscalls such as enableInterrupts(),
 *   switchThreadImmediate() or setInterruptHandler();
 * - it cannot wait for other interrupts to occur;
 * - it must return quickly, as IRQs fired while the exception handler is
 *   running may otherwise be missed.
 *
 * Interrupts must be re-enabled manually using enableInterrupts() after setting
 * a new handler.
}
procedure setInterruptHandler(func: TArgFunction; arg: Pointer);


{
 * Temporarily disables interrupts, then calls the BIOS function to clear
 * the instruction cache.
}
procedure flushCache;


{
 * Jumps to the entry point in the BIOS. This function does not return.
}
procedure softReset;


{
	This tiny stub is going to be relocated to the address the CPU jumps to
	when an exception occurs (0x80000080) at runtime, overriding the default
	one installed by the BIOS. We're going to fetch a pointer to the current
	thread, grab the EPC (i.e. the address of the instruction that was being
	executed before the exception occurred) and jump to the exception handler.
	NOTE: we can't use any registers other than $k0 and $k1 here, as doing so
	would destroy their contents and corrupt the current thread's state.
}
procedure _exceptionVector; cdecl; external;


{
 * Blocks for (roughly) the specified number of microseconds. This
 * function will reset hardware timer 2 and use it for timing. Disabling
 * interrupts prior to calling delayMicroseconds() is highly recommended to
 * prevent jitter, but not strictly necessary unless the interrupt handler
 * accesses timer 2.
}
procedure delayMicroseconds(time: dword); cdecl; external;


{
 * Blocks for (roughly) the specified number of microseconds. This
 * function does not rely on a hardware timer, so interrupts may throw off
 * timings if not explicitly disabled prior to calling delayMicrosecondsBusy().
}
procedure delayMicrosecondsBusy(time: dword); cdecl; external;

{
 * Checks if the specified interrupt was fired but not yet acknowledged;
 * if so, acknowledges it and returns true. This function can be used in a
 * callback set using setInterruptHandler() to check for individual IRQs that
 * need to be processed, but will also work with interrupts that are not
 * explicitly enabled in the IRQ_MASK register.
 *
 * Note that most interrupts must additionally be acknowledged at the device
 * side (through DMA/SIO/SPU/CD-ROM registers or by issuing the GP1 IRQ
 * acknowledge command) once this function returns true. Lightgun, vblank and
 * timer interrupts do not require device-side acknowledgement.
 *
 * param irq
 * return True if the IRQ was pending and got acknowledged, false otherwise
}
function acknowledgeInterrupt(irq: IRQChannel): boolean;


{
 * Waits for the specified interrupt to be fired for up to the specified
 * number of microseconds (with 10 us granularity). This function will work with
 * interrupts that are not explicitly enabled in the IRQ_MASK register, but will
 * *not* work with interrupts that have been enabled if any callback set using
 * setInterruptHandler() acknowledges them.
 *
 * return False in case of a timeout, true otherwise
}
function waitForInterrupt(irq: IRQChannel; timeout: longint): boolean;


{
 * Waits for the specified DMA channel to finish any ongoing transfer for
 * up to the specified number of microseconds (with 10 us granularity).
 *
 * return False in case of a timeout, true otherwise
}
function waitForDMATransfer(dma: DMAChannel; timeout: longint): boolean;


{
 * Pauses the thread calling this function immediately and starts/resumes
 * executing the specified one. Once the other thread switches back, execution
 * will resume from after the call to switchThreadImmediate(). This function
 * must *not* be used in IRQ handlers; use switchThread() (which will behave
 * identically as thread switches are processed right after IRQ handling)
 * instead.
 *
 * param thread Pointer to new thread or NULL for main thread
}
procedure switchThreadImmediate(thread: PThread);


var
  HandleCDROMIRQ : procedure = nil;
  HandleVSyncIRQ : procedure = nil;

  vblankCount : dword = 0;

procedure InterruptHandlerFunction(arg: Pointer); cdecl;
procedure InitIRQ;


function PrintHexValue(value: LongWord): string;


implementation

const
  CauseNames: array[0..8] of PChar = (
    'Load address error',
    'Store address error',
    'Instruction bus error',
    'Data bus error',
    'Syscall',
    'Break instruction',
    'Reserved instruction',
    'Coprocessor unusable',
    'Arithmetic overflow'
  );

  RegisterNames: PChar =
    'pcatv0v1a0a1a2a3' +
    't0t1t2t3t4t5t6t7' +
    's0s1s2s3s4s5s6s7' +
    't8t9gpspfprahi lo';


{ Serial I/O }

function SIO_DATA(N: dword): Byte; inline; begin Result:= Pbyte((IO_BASE or $040) + (16*N))^; end;
function SIO_STAT(N: dword): Word; inline; begin Result:= Pword((IO_BASE or $044) + (16*N))^; end;
function SIO_MODE(N: dword): Word; inline; begin Result:= Pword((IO_BASE or $048) + (16*N))^; end;
function SIO_CTRL(N: dword): Word; inline; begin Result:= Pword((IO_BASE or $04A) + (16*N))^; end;
function SIO_BAUD(N: dword): Word; inline; begin Result:= Pword((IO_BASE or $04E) + (16*N))^; end;

procedure SIO_DATA_Set(N: dword; value: byte); inline; 
begin 
  Pbyte((IO_BASE or $040) + (16*N))^:= value; 
end;

procedure SIO_STAT_Set(N: dword; value: word); inline; 
begin
  asm
    li $t0, IO_BASE | 0x044
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;

procedure SIO_MODE_Set(N: dword; value: word); inline; 
begin
  asm
    li $t0, IO_BASE | 0x048
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;

procedure SIO_CTRL_Set(N: dword; value: word); inline; 
begin
  asm
    li $t0, IO_BASE | 0x04A
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;

procedure SIO_BAUD_Set(N: dword; value: word); inline; 
begin
  asm
    li $t0, IO_BASE | 0x04E
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;


{ DMA }

function DMA_DPCR_CH_PRIORITY_BITMASK(N: dword): dword; inline; begin result:= (7) shl (4 * N); end;
function DMA_DPCR_CH_PRIORITY(N, priority: dword): dword; inline; begin result:= (priority and 7) shl (4 * N); end;
function DMA_DPCR_CH_ENABLE(N: dword): dword; inline; begin result:= (1 shl 3) shl (4 * N); end;

function DMA_DICR_CH_MODE(N: dword): dword; inline; begin result:= 1 shl (N +  0); end;
function DMA_DICR_CH_ENABLE(N: dword): dword; inline; begin result:= 1 shl (N + 16); end;
function DMA_DICR_CH_STAT(N: dword): dword; inline; begin result:= 1 shl (N + 24); end;

function DMA_MADR(N: dword): LongWord; inline; begin result:= pdword((IO_BASE or $080) + (16*N))^; end;
function DMA_BCR(N: dword): LongWord;  inline; begin result:= pdword((IO_BASE or $084) + (16*N))^; end;
function DMA_CHCR(N: dword): LongWord; inline; begin result:= pdword((IO_BASE or $088) + (16*N))^; end;

procedure DMA_MADR_Set(N: dword; value: LongWord); inline; 
begin
  asm
    li $t0, IO_BASE | 0x080
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lw $t1, 0($v0)
    nop

    sw $t1, 0($t0)
  end;
end;


procedure DMA_BCR_Set(N: dword; value: LongWord); inline; 
begin
  asm
    li $t0, IO_BASE | 0x084
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lw $t1, 0($v0)
    nop

    sw $t1, 0($t0)
  end;
end;

procedure DMA_CHCR_Set(N: dword; value: LongWord); inline; 
begin
  asm
    li $t0, IO_BASE | 0x088
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lw $t1, 0($v0)
    nop

    sw $t1, 0($t0)
  end;
end;


{ Timers }

function TIMER_VALUE(N: dword): Word; inline; begin result:= Pword((IO_BASE or $100) + (16*N))^; end;
function TIMER_CTRL_REG(N: dword): Word; inline; begin result:= Pword((IO_BASE or $104) + (16*N))^; end;
function TIMER_RELOAD(N: dword): Word; inline; begin result:= Pword((IO_BASE or $108) + (16*N))^; end;

procedure TIMER_VALUE_Set(N: dword; value: Word); inline; 
begin
  asm
    li $t0, IO_BASE | 0x100
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;


procedure TIMER_CTRL_REG_Set(N: dword; value: Word); inline; 
begin
  asm
    li $t0, IO_BASE | 0x104
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;


procedure TIMER_RELOAD_Set(N: dword; value: Word); inline; 
begin
  asm
    li $t0, IO_BASE | 0x108
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;


{ SPU channels }

function SPU_CH_VOL_L(N: dword): Word; inline; begin result:= Pword((IO_BASE or $C00) + (16*N))^; end;
function SPU_CH_VOL_R(N: dword): Word; inline; begin result:= Pword((IO_BASE or $C02) + (16*N))^; end;
function SPU_CH_FREQ(N: dword): Word; inline; begin result:= Pword((IO_BASE or $C04) + (16*N))^; end;
function SPU_CH_ADDR(N: dword): Word; inline; begin result:= Pword((IO_BASE or $C06) + (16*N))^; end;
function SPU_CH_ADSR1(N: dword): Word; inline; begin result:= Pword((IO_BASE or $C08) + (16*N))^; end;
function SPU_CH_ADSR2(N: dword): Word; inline; begin result:= Pword((IO_BASE or $C0A) + (16*N))^; end;
function SPU_CH_ADSR_VOL(N: dword): Word;  inline; begin result:= Pword((IO_BASE or $C0C) + (16*N))^; end;
function SPU_CH_LOOP_ADDR(N: dword): Word; inline; begin result:= Pword((IO_BASE or $C0E) + (16*N))^; end;

procedure SPU_CH_VOL_L_Set(N: dword; value: Word); inline; 
begin
  asm
    li $t0, IO_BASE | 0xC00
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;

procedure SPU_CH_VOL_R_Set(N: dword; value: Word); inline; 
begin
  asm
    li $t0, IO_BASE | 0xC02
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;


procedure SPU_CH_FREQ_Set(N: dword; value: Word); inline; 
begin
  asm
    li $t0, IO_BASE | 0xC04
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;

  
procedure SPU_CH_ADDR_Set(N: dword; value: Word); inline; 
begin
  asm
    li $t0, IO_BASE | 0xC06
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;


procedure SPU_CH_ADSR1_Set(N: dword; value: Word); inline; 
begin
  asm
    li $t0, IO_BASE | 0xC08
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;




procedure SPU_CH_ADSR2_Set(N: dword; value: Word); inline; 
begin
  asm
    li $t0, IO_BASE | 0xC0A
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;


procedure SPU_CH_LOOP_ADDR_Set(N: dword; value: Word); inline; 
begin
  asm
    li $t0, IO_BASE | 0xC0E
    la $v1, N
    lw $t2, 0($v1)
    nop
    sll  $t2, $t2, 4
    addu $t0, $t0, $t2

    la $v0, value    
    lhu $t1, 0($v0)
    nop

    sh $t1, 0($t0)
  end;
end;


const

  HexDigits: PChar = '0123456789abcdef';

function PrintHexValue(value: LongWord): string;
var
  i: Integer;
begin
  result:= '0x';
  for i := 8 downto 1 do
  begin
    result:= result + HexDigits[(value shr 28) and $F];
    value := value shl 4;
  end;
end;



procedure _unhandledException(cause: longint; badv: dword); cdecl;
var
  i: Integer;
  reg: PLongWord;
  name: PChar;
  addr, endAddr: PLongWord;
begin
  // Cause description
  if (cause >= 4) and (cause <= 12) then
    Writeln(CauseNames[cause - 4]);

  if cause <= 5 then
  begin
    Write('@ ');
    PrintHexValue(badv);
    Writeln;
  end;

  // Register dump
  Writeln('Register dump:');
  name := RegisterNames;
  reg := @currentThread^.pc;

  for i := 31 downto 0 do
  begin
    Write('  ');
    Write(name^); Inc(name);
    Write(name^); Inc(name);
    Write('=');
    PrintHexValue(reg^);
    Inc(reg);

    if (i mod 4) = 0 then
      Writeln;
  end;

  // Stack dump
  Writeln('Stack dump:');
  addr := PLongWord(UIntPtr(currentThread^.sp) - 7 * SizeOf(LongWord));
  endAddr := PLongWord(UIntPtr(currentThread^.sp) + 7 * SizeOf(LongWord));

  while addr <= endAddr do
  begin
    if UIntPtr(addr) = currentThread^.sp then
      Write('> ')
    else
      Write('  ');

    PrintHexValue(UInt32(UIntPtr(addr)));
    Write(': ');
    PrintHexValue(addr^);
    Writeln;
    Inc(addr);
  end;

end;



type
  PBIOS_API_TABLE = ^TBIOS_API_TABLE;
  TBIOS_API_TABLE = array[0..255] of TVoidFunction;

  PBIOSVector = ^TBIOSVector;
  TBIOSVector = array[0..3] of UInt32;

const
  BIOS_ENTRY_POINT: Pointer = Pointer($BFC00000);
  BIOS_API_TABLE: PBIOS_API_TABLE = PBIOS_API_TABLE($80000200);
  BIOS_BP_VECTOR: PBIOSVector = PBIOSVector($80000040);
  BIOS_EXC_VECTOR: PBIOSVector = PBIOSVector($80000080);



procedure enableInterrupts;
begin

	cop0_setSTATUS(cop0_getSTATUS or COP0_STATUS_IEc);

end;


function disableInterrupts: boolean;
var
	sr : dword;

begin

	sr:= cop0_getSTATUS;
	cop0_setSTATUS(sr and not COP0_STATUS_IEc);

	result:= (sr and COP0_STATUS_IEc) <> 0;

end;


procedure flushWriteQueue; inline;
var
  dummy: Byte;

begin

  // Read from PS1 memory-mapped address to flush write queue
  dummy := PByte($BFC00000)^;

end;


procedure initThread(thread: PThread; func: TArgFunction; arg: Pointer; stack: Pointer);
var
  gpValue: LongWord;

begin

  // Read the current MIPS gp register
  asm
    sw $gp, gpValue
  end;

  thread^.pc := LongWord(@func);   // Entry point address
  thread^.a0 := LongWord(arg);     // Argument pointer
  thread^.gp := gpValue;           // Current GP register
  thread^.sp := LongWord(stack);   // Stack top
  thread^.fp := LongWord(stack);   // Frame pointer
  thread^.ra := 0;                 // Clear return address

end;



var
  _flushCache: TVoidFunction = nil;
  _savedBreakpointVector: array[0..15] of Byte;
  _savedExceptionVector: array[0..15] of Byte;


procedure installExceptionHandler;
begin

	// Clear all pending IRQ flags and prevent the interrupt controller from
	// generating further IRQs.
	IRQ_MASK:= 0;
	IRQ_STAT:= 0;
	DMA_DPCR:= 0;
	DMA_DICR:= DMA_DICR_CH_STAT_BITMASK;

	// Disable interrupts and the GTE at the COP0 side.
	cop0_setSTATUS(COP0_STATUS_CU0);

	// Grab a direct pointer to the BIOS function to flush the instruction
	// cache. This is the only function that must always run from the BIOS ROM
	// as it temporarily disables main RAM.
	
  // 3. Grab BIOS flush cache function
  _flushCache := BIOS_API_TABLE^[$44];

  // 4. Overwrite default BIOS exception and breakpoint vectors
  Move(BIOS_BP_VECTOR^, _savedBreakpointVector, 16);
  Move(BIOS_EXC_VECTOR^, _savedExceptionVector, 16);
  // Copy the first 16 bytes of _exceptionVector into the PS1 vectors
  Move(Pointer(@_exceptionVector)^, BIOS_BP_VECTOR^, 16);
  Move(Pointer(@_exceptionVector)^, BIOS_EXC_VECTOR^, 16);


  // 5. Flush the instruction cache
  _flushCache;

  // 6. Re-enable DMA channels
  DMA_DPCR:= $0BBBBBBB;
  DMA_DICR:= DMA_DICR_IRQ_ENABLE;


	// Ensure interrupts and the GTE are enabled at the COP0 side.
	cop0_setSTATUS(COP0_STATUS_IEc or COP0_STATUS_Im2 or COP0_STATUS_CU0 or COP0_STATUS_CU2);

end;


procedure uninstallExceptionHandler;
begin

  // 1. Clear pending IRQ flags and mask further IRQs
  IRQ_MASK := 0;
  IRQ_STAT := 0;
  DMA_DPCR := 0;
  DMA_DICR := DMA_DICR_CH_STAT_BITMASK;

  // 2. Disable interrupts and the GTE at COP0
  cop0_setSTATUS(COP0_STATUS_CU0);

  // 3. Restore original BIOS vectors
  Move(_savedBreakpointVector, Pointer(BIOS_BP_VECTOR)^, 16);
  Move(_savedExceptionVector, Pointer(BIOS_EXC_VECTOR)^, 16);

  // 4. Flush the instruction cache
  _flushCache();

end;


procedure setInterruptHandler(func: TArgFunction; arg: Pointer);
begin

  // Disable interrupts
  disableInterrupts();

  // Set the interrupt handler and its argument
  interruptHandler    := func;
  interruptHandlerArg := arg;

end;


procedure flushCache;
var
	enable : boolean;

begin
	
	if not assigned(_flushCache) then _flushCache:= BIOS_API_TABLE^[$44];

	enable:= disableInterrupts;

	_flushCache();

	if enable then enableInterrupts;

end;


procedure softReset;
var
  ResetFunc : TVoidFunction;

begin

  disableInterrupts;           // Call the procedure
  ResetFunc:= TVoidFunction(BIOS_ENTRY_POINT);  // Cast pointer to callable procedure
  ResetFunc;                  // Call the BIOS entry point

end;


function acknowledgeInterrupt(irq: IRQChannel): boolean;
begin

	result:= false;

	if ((IRQ_STAT and (1 shl irq))) <> 0 then begin
		
		IRQ_STAT:= word(not (1 shl irq));

		result:= true;

	end;
	
end;


function waitForInterrupt(irq: IRQChannel; timeout: longint): boolean;
begin

	result:= true;
	repeat
		
		if acknowledgeInterrupt(irq) then exit;

		delayMicroseconds(10);

		timeout:= timeout - 10;
	until timeout <= 0;

	result:= false;

end;


function waitForDMATransfer(dma: DMAChannel; timeout: longint): boolean;
begin

	result:= true;

	repeat
		
		if (DMA_CHCR(dma) and DMA_CHCR_ENABLE) = 0 then exit;
			

		delayMicroseconds(10);

		timeout:= timeout - 10;
	until timeout <= 0;

	result:= false;
end;


procedure switchThread(thread: PThread);
begin
  if thread = nil then
    thread := @_mainThread;

  nextThread:= thread;

end;


procedure switchThreadImmediate(thread: PThread);
begin

	switchThread(thread);

	// Execute a syscall to force the switch to happen.
	asm
		syscall 0
	end;

end;



// Acknowledge and dispatch the hardware interrupt.
procedure InterruptHandlerFunction(arg: Pointer); cdecl;
begin

  if acknowledgeInterrupt(IRQ_CDROM) then if assigned(HandleCDROMIRQ) then HandleCDROMIRQ;


  if acknowledgeInterrupt(IRQ_VSYNC) then begin
  
    inc(vblankCount);
  
    if assigned(HandleVSyncIRQ) then HandleVSyncIRQ;

  end;

end;


procedure InitIRQ;
begin

  installExceptionHandler;

  setInterruptHandler(@InterruptHandlerFunction, nil);

  IRQ_MASK := (1 shl IRQ_CDROM) or (1 shl IRQ_VSYNC);

  enableInterrupts;

end;


procedure InitJOYSTICKControllers;
begin
  { Reset the serial interface }
  SIO_CTRL_Set(0, SIO_CTRL_RESET);

  { Configure mode: baud div 1, 8 data bits }
  SIO_MODE_Set(0, SIO_MODE_BAUD_DIV1 or SIO_MODE_DATA_8);

  { Set baud rate to 250000 bps }
  SIO_BAUD_Set(0, F_CPU div 250000);

  { Enable TX, RX and DSR interrupt }
  SIO_CTRL_Set(0, SIO_CTRL_TX_ENABLE or SIO_CTRL_RX_ENABLE or SIO_CTRL_DSR_IRQ_ENABLE);
end;


procedure SelectJoystickPort(port: Integer);
var
  v: LongWord;

begin

  { Set/clear bit that selects port set for DTR (chip select) }
  v := SIO_CTRL(0);

  if port <> 0 then
    v := v or SIO_CTRL_CS_PORT_2
  else
    v := v and (not SIO_CTRL_CS_PORT_2);

  SIO_CTRL_Set(0, v);

end;


function ExchangeSIO0Byte(value: Byte): Byte;
begin
  
  { Wait until TX FIFO can accept a byte }
  while (SIO_STAT(0) and SIO_STAT_TX_NOT_FULL) = 0 do
    asm
      nop
    end;
  
  { Send byte }
  SIO_DATA_set(0, value);

  { Wait until RX FIFO has received a byte }
  while (SIO_STAT(0) and SIO_STAT_RX_NOT_EMPTY) = 0 do
    asm
      nop
    end;

  { Read and return received byte }
  Result := Byte(SIO_DATA(0));

end;


function WaitForSIO0Acknowledge(timeout: Integer): Boolean;
begin

  Result := False;

  while timeout > 0 do begin

    if (IRQ_STAT and (1 shl IRQ_SIO0)) <> 0 then begin
      { Clear IRQ_SIO0 flag }
      IRQ_STAT := word(not (1 shl IRQ_SIO0));

      { Acknowledge on SIO side }
      SIO_CTRL_Set(0, SIO_CTRL(0) or SIO_CTRL_ACKNOWLEDGE);

      Result := True;
      Exit;
    end;

    delayMicroseconds(10);
    Dec(timeout, 10);

  end;

end;


function ExchangeSIO0Packet(address: Byte; request: PByte; response: PByte; reqLength: Integer; maxRespLength: Integer): Integer;
var
  respLength: Integer;

begin

  { Reset IRQ flag and assert DTR + ACK to start a packet }
  IRQ_STAT:= word(not (1 shl IRQ_SIO0));
  SIO_CTRL_Set(0, SIO_CTRL(0) or SIO_CTRL_DTR or SIO_CTRL_ACKNOWLEDGE);

  delayMicroseconds(DTR_DELAY);

  respLength := 0;

  { Send address byte }
  SIO_DATA_Set(0, address);

  { Wait for DSR acknowledge; if present, flush RX buffer }
  if WaitForSIO0Acknowledge(DSR_TIMEOUT) then begin

    while (SIO_STAT(0) and SIO_STAT_RX_NOT_EMPTY) <> 0 do
      SIO_DATA(0); { read/discard }

    { Full-duplex transfer, pad TX with 0 if request is shorter than response }
    while respLength < maxRespLength do begin

      if reqLength > 0 then begin
        response^ := ExchangeSIO0Byte(request^);
        Inc(request);
        Dec(reqLength);
      end else begin
        response^ := ExchangeSIO0Byte(0);
      end;

      Inc(response);
      Inc(respLength);

      { Stop when device stops pulsing DSR }
      if not WaitForSIO0Acknowledge(DSR_TIMEOUT) then Break;

    end;

  end;

  { Release DTR (let device go idle) }
  delayMicroseconds(DTR_DELAY);
  SIO_CTRL_Set(0, SIO_CTRL(0) and (not SIO_CTRL_DTR));

  Result := respLength;

end;




initialization

  currentThread:= @_mainThread;
  nextThread:= @_mainThread;

end.
