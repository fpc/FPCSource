{$ifdef NDS_INTERFACE}
type 
  f32 = integer;

const
  DIV_CR = REG_DIVCNT;

  DIV_NUMERATOR64 = REG_DIV_NUMER;
  DIV_NUMERATOR32 = REG_DIV_NUMER_L;

  DIV_DENOMINATOR64 = REG_DIV_DENOM;
  DIV_DENOMINATOR32 = REG_DIV_DENOM_L;

  DIV_RESULT64 = REG_DIV_RESULT;
  DIV_RESULT32 = REG_DIV_RESULT_L;

  DIV_REMAINDER64 = REG_DIVREM_RESULT;
  DIV_REMAINDER32 = REG_DIVREM_RESULT_L;

  SQRT_CR = REG_SQRTCNT;

  SQRT_PARAM64 = REG_SQRT_PARAM;
  SQRT_PARAM32 = REG_SQRT_PARAM_L;

  SQRT_RESULT32 = REG_SQRT_RESULT;


  DISPLAY_CR = REG_DISPCNT;

{$ifdef ARM9}
  {$define   WAIT_CR := REG_EXMEMCNT}
{$else}
  {$define   WAIT_CR := REG_EXMEMSTAT}
{$endif}

{$define   DISP_SR := REG_DISPSTAT}
{$define   DISP_Y := REG_VCOUNT}
  
  BG0_CR = REG_BG0CNT;
  BG1_CR = REG_BG1CNT;
  BG2_CR = REG_BG2CNT;
  BG3_CR = REG_BG3CNT;
  
  BG0_X0 = REG_BG0HOFS; 
  BG0_Y0 = REG_BG0VOFS;
  BG1_X0 = REG_BG1HOFS;
  BG1_Y0 = REG_BG1VOFS;
  BG2_X0 = REG_BG2HOFS;
  BG2_Y0 = REG_BG2VOFS;
  BG3_X0 = REG_BG3HOFS;
  BG3_Y0 = REG_BG3VOFS;
  
  BG2_XDX = REG_BG2PA;
  BG2_XDY = REG_BG2PB;
  BG2_YDX = REG_BG2PC;
  BG2_YDY = REG_BG2PD;
  BG2_CX = REG_BG2X;
  BG2_CY = REG_BG2Y;
  
  BG3_XDX = REG_BG3PA;
  BG3_XDY = REG_BG3PB;
  BG3_YDX = REG_BG3PC;
  BG3_XDY = REG_BG3PD;
  BG3_CX = REG_BG3X;
  BG3_CY = REG_BG3Y;
  
  REG_WIN0H		: pcuint16 = pointer($4000040);
  REG_WIN1H		: pcuint16 = pointer($4000042);
  REG_WIN0V		: pcuint16 = pointer($4000044);
  REG_WIN1V		: pcuint16 = pointer($4000046);
  REG_WININ		: pcuint16 = pointer($4000048);
  REG_WINOUT		: pcuint16 = pointer($400004A);
  
  MOSAIC_CR = REG_MOSAIC;

  BLEND_CR = REG_BLDCNT;
  BLEND_AB = REG_BLDALPHA;
  BLEND_Y = REG_BLDY;

  SUB_BLEND_CR = REG_BLDCNT_SUB;
  SUB_BLEND_AB = REG_BLDALPHA_SUB;
  SUB_BLEND_Y = REG_BLDY_SUB;

{$ifdef ARM7}
  {$define SERIAL_CR := REG_SPICNT}
  {$define SERIAL_DATA := REG_SPIDATA}
  {$define SIO_CR := REG_SIOCNT}
  {$define R_CR := REG_RCNT}  
{$endif ARM7}

  DISP_CAPTURE = REG_DISPCAPCNT;

  BRIGHTNESS = REG_MASTER_BRIGHT;
  SUB_BRIGHTNESS = REG_MASTER_BRIGHT_SUB;

  (*	secondary screen *)
  SUB_DISPLAY_CR = REG_DISPCNT_SUB;

  SUB_BG0_CR = REG_BG0CNT_SUB;
  SUB_BG1_CR = REG_BG1CNT_SUB;
  SUB_BG2_CR = REG_BG2CNT_SUB;
  SUB_BG3_CR = REG_BG3CNT_SUB;
  
  SUB_BG0_X0 = REG_BG0HOFS_SUB; 
  SUB_BG0_Y0 = REG_BG0VOFS_SUB;
  SUB_BG1_X0 = REG_BG1HOFS_SUB;
  SUB_BG1_Y0 = REG_BG1VOFS_SUB;
  SUB_BG2_X0 = REG_BG2HOFS_SUB;
  SUB_BG2_Y0 = REG_BG2VOFS_SUB;
  SUB_BG3_X0 = REG_BG3HOFS_SUB;
  SUB_BG3_Y0 = REG_BG3VOFS_SUB;
  
  SUB_BG2_XDX = REG_BG2PA_SUB;
  SUB_BG2_XDY = REG_BG2PB_SUB;
  SUB_BG2_YDX = REG_BG2PC_SUB;
  SUB_BG2_YDY = REG_BG2PD_SUB;
  SUB_BG2_CX = REG_BG2X_SUB;
  SUB_BG2_CY = REG_BG2Y_SUB;
  
  SUB_BG3_XDX = REG_BG3PA_SUB;
  SUB_BG3_XDY = REG_BG3PB_SUB;
  SUB_BG3_YDX = REG_BG3PC_SUB;
  SUB_BG3_XDY = REG_BG3PD_SUB;
  SUB_BG3_CX = REG_BG3X_SUB;
  SUB_BG3_CY = REG_BG3Y_SUB;
  
  REG_WIN0H_SUB		: pcuint16 = pointer($4001040);
  REG_WIN1H_SUB		: pcuint16 = pointer($4001042);
  REG_WIN0V_SUB		: pcuint16 = pointer($4001044);
  REG_WIN1V_SUB		: pcuint16 = pointer($4001046);
  REG_WININ_SUB		: pcuint16 = pointer($4001048);
  REG_WINOUT_SUB	: pcuint16 = pointer($400104A);
  
  SUB_MOSAIC_CR = REG_MOSAIC_SUB;
  
  REG_BLDMOD_SUB: pcuint16 = pointer($4001050);
  REG_COLV_SUB	: pcuint16 = pointer($4001052);
  REG_COLY_SUB	: pcuint16 = pointer($4001054);
  
  (*common*)
 
  REG_DMA0SAD		: pcuint32 = pointer($40000B0);
  REG_DMA0SAD_L	: pcuint16 = pointer($40000B0);
  REG_DMA0SAD_H	: pcuint16 = pointer($40000B2);
  REG_DMA0DAD		: pcuint32 = pointer($40000B4);
  REG_DMA0DAD_L	: pcuint16 = pointer($40000B4);
  REG_DMA0DAD_H	: pcuint16 = pointer($40000B6);
  REG_DMA0CNT		: pcuint32 = pointer($40000B8);
  REG_DMA0CNT_L	: pcuint16 = pointer($40000B8);
  REG_DMA0CNT_H	: pcuint16 = pointer($40000BA);
  
  REG_DMA1SAD		: pcuint32 = pointer($40000BC);
  REG_DMA1SAD_L	: pcuint16 = pointer($40000BC);
  REG_DMA1SAD_H	: pcuint16 = pointer($40000BE);
  REG_DMA1DAD		: pcuint32 = pointer($40000C0);
  REG_DMA1DAD_L	: pcuint16 = pointer($40000C0);
  REG_DMA1DAD_H	: pcuint16 = pointer($40000C2);
  REG_DMA1CNT		: pcuint32 = pointer($40000C4);
  REG_DMA1CNT_L	: pcuint16 = pointer($40000C4);
  REG_DMA1CNT_H	: pcuint16 = pointer($40000C6);
  
  REG_DMA2SAD		: pcuint32 = pointer($40000C8);
  REG_DMA2SAD_L	: pcuint16 = pointer($40000C8);
  REG_DMA2SAD_H	: pcuint16 = pointer($40000CA);
  REG_DMA2DAD		: pcuint32 = pointer($40000CC);
  REG_DMA2DAD_L	: pcuint16 = pointer($40000CC);
  REG_DMA2DAD_H	: pcuint16 = pointer($40000CE);
  REG_DMA2CNT		: pcuint32 = pointer($40000D0);
  REG_DMA2CNT_L	: pcuint16 = pointer($40000D0);
  REG_DMA2CNT_H	: pcuint16 = pointer($40000D2);
  
  REG_DMA3SAD		: pcuint32 = pointer($40000D4);
  REG_DMA3SAD_L	: pcuint16 = pointer($40000D4);
  REG_DMA3SAD_H	: pcuint16 = pointer($40000D6);
  REG_DMA3DAD		: pcuint32 = pointer($40000D8);
  REG_DMA3DAD_L	: pcuint16 = pointer($40000D8);
  REG_DMA3DAD_H	: pcuint16 = pointer($40000DA);
  REG_DMA3CNT		: pcuint32 = pointer($40000DC);
  REG_DMA3CNT_L	: pcuint16 = pointer($40000DC);
  REG_DMA3CNT_H	: pcuint16 = pointer($40000DE);
  
  REG_TIME		: pcuint16 = pointer($4000100);
  REG_TM0D		: pcuint16 = pointer($4000100);
  REG_TM0CNT	: pcuint16 = pointer($4000102);
  REG_TM1D		: pcuint16 = pointer($4000106);
  REG_TM2D		: pcuint16 = pointer($4000108);
  REG_TM2CNT	: pcuint16 = pointer($400010A);
  REG_TM3D		: pcuint16 = pointer($400010C);
  REG_TM3CNT	: pcuint16 = pointer($400010E);
  
  
//  REG_SIOCNT		: pcuint16 = pointer($4000128);
  REG_SIOMLT_SEND	: pcuint16 = pointer($400012A);
  
{$define KEYS		:= REG_KEYINPUT}
{$define KEYS_CR := REG_KEYCNT}
  //???
//  REG_RCNT		: pcuint16 = pointer($4000134);
  REG_HS_CTRL		: pcuint16 = pointer($4000140);
  
  (* Interupt	enable registers *)
{$define IE  := REG_IE}
{$define _IF :=	REG_IF}
{$define IME	:= REG_IME}
  
  (*controls power  0x30f	is all on *)
{$define POWER_CR := REG_POWERCNT}
  
  (* ram	controllers	0x8	is enabled,	other bits have	to do with mapping *)
  REG_VRAM_A_CR	: pcuint8 = pointer($4000240);
  REG_VRAM_B_CR	: pcuint8 = pointer($4000241);
  REG_VRAM_C_CR	: pcuint8 = pointer($4000242);
  REG_VRAM_D_CR	: pcuint8 = pointer($4000243);
  REG_VRAM_E_CR	: pcuint8 = pointer($4000244);
  REG_VRAM_F_CR	: pcuint8 = pointer($4000245);
  REG_VRAM_G_CR	: pcuint8 = pointer($4000246);
  REG_VRAM_H_CR	: pcuint8 = pointer($4000248);
  REG_VRAM_I_CR	: pcuint8 = pointer($4000249);
  REG_WRAM_CNT	: pcuint8 = pointer($4000247);
  
  
  
  
  (*3D graphics suff*)
  REG_GFX_FIFO		  : pcuint32 = pointer($4000400);
  REG_GFX_STATUS		: pcuint32 = pointer($4000600);
  REG_GFX_CONTROL		: pcuint16 = pointer($4000060);
  REG_COLOR			    : pcuint32 = pointer($4000480);
  REG_VERTEX16		  : pcuint32 = pointer($400048C);
  REG_TEXT_COORD		: pcuint32 = pointer($4000488);
  REG_TEXT_FORMAT		: pcuint32 = pointer($40004A8);
  
  
  REG_CLEAR_COLOR		: pcuint32 = pointer($4000350);
  REG_CLEAR_DEPTH		: pcuint16 = pointer($4000354);
  
  REG_LIGHT_VECTOR	: pcuint32 = pointer($40004C8);
  REG_LIGHT_COLOR		: pcuint32 = pointer($40004CC);
  REG_NORMAL        : pcuint32 = pointer($4000484);
  
  REG_DIFFUSE_AMBIENT		: pcuint32 = pointer($40004C0);
  REG_SPECULAR_EMISSION	: pcuint32 = pointer($40004C4);
  REG_SHININESS			    : pcuint32 = pointer($40004D0);
  
  REG_POLY_FORMAT		: pcuint32 = pointer($40004A4);
  
  REG_GFX_BEGIN     : pcuint32 = pointer($4000500);
  REG_GFX_END	      : pcuint32 = pointer($4000504);
  REG_GFX_FLUSH     : pcuint32 = pointer($4000540);
  REG_GFX_VIEWPORT	: pcuint32 = pointer($4000580);
  
  REG_MTX_CONTROL		: pcuint32 = pointer($4000440);
  REG_MTX_PUSH		  : pcuint32 = pointer($4000444);
  REG_MTX_POP			  : pcuint32 = pointer($4000448);
  REG_MTX_SCALE     : pcint32 = pointer($400046C);
  REG_MTX_TRANSLATE	: pcint32 = pointer($4000470);
  REG_MTX_RESTORE		: pcuint32 = pointer($4000450);
  REG_MTX_STORE     : pcuint32 = pointer($400044C);
  REG_MTX_IDENTITY	: pcuint32 = pointer($4000454);
  REG_MTX_LOAD4x4		: ^f32 = pointer($4000458);
  REG_MTX_LOAD4x3		: ^f32 = pointer($400045C);
  REG_MTX_MULT4x4		: ^f32 = pointer($4000460);
  REG_MTX_MULT4x3		: ^f32 = pointer($4000464);
  REG_MTX_MULT3x3		: ^f32 = pointer($4000468);
  
  // Card	bus
  
  CARD_CR1     = REG_AUXSPICNT;
  CARD_CR1H    = REG_AUXSPICNTH;
  CARD_CR2     = REG_ROMCTRL;
  CARD_EEPDATA = REG_AUXSPIDATA;
  
  REG_CARD_DATA		: pcuint32 = pointer($04100000);
  REG_CARD_DATA_RD	: pcuint32 = pointer($04100010);
  
  REG_CARD_1B0		: pcuint32 = pointer($040001B0);
  REG_CARD_1B4		: pcuint32 = pointer($040001B4);
  REG_CARD_1B8		: pcuint16 = pointer($040001B8);
  REG_CARD_1BA		: pcuint16 = pointer($040001BA);
  
  SOUND_CR          = REG_SOUNDCNT;
  SOUND_MASTER_VOL  = REG_MASTER_VOLUME;
  SOUND_BIAS        = REG_SOUNDBIAS;
{$endif NDS_INTERFACE}
