{$mode objfpc}
unit ps1GTE;
interface
uses ps1COP0;

type
	// sign.3.12 fixed point
	GTEVector16 = packed record
					x, y : smallint;
					z, _padding : smallint;
	end;
	pGTEVector16 = ^GTEVector16;
	// sign.3.12 fixed point
	GTEMatrix = packed record
					m : array[0..2, 0..2] of smallint;
					_padding : smallint;
	end;
	pGTEMatrix = ^GTEMatrix;


const
	GTE_COP2_INSTR = $4A000000;					// this is the COP2 opcode


{ Command definitions and Flags}
const
	GTE_CMD_GET_REAL_BITMASK = $3F;		 		// If You need the real value do an AND with this
 	GTE_CMD_RTPS  	= ($01 shl 20) or $01;   	// Perspective transformation (1 vertex)
  	GTE_CMD_NCLIP 	= ($14 shl 20) or $06;   	// Normal clipping
	GTE_CMD_OP    	= ($17 shl 20) or $0C;   	// Outer product (Cross product of 2 vectors)
	GTE_CMD_DPCS  	= ($07 shl 20) or $10;   	// Depth cue (1 vertex)
	GTE_CMD_INTPL 	= ($09 shl 20) or $11;   	// Depth cue with vector
	GTE_CMD_MVMVA 	= ($04 shl 20) or $12;   	// Matrix-vector multiplication
	GTE_CMD_NCDS  	= ($0E shl 20) or $13;   	// Normal color depth (1 vertex)
	GTE_CMD_CDP   	= ($12 shl 20) or $14;   	// Color depth cue
	GTE_CMD_NCDT  	= ($0F shl 20) or $16;   	// Normal color depth (3 vertices)
	GTE_CMD_NCCS  	= ($10 shl 20) or $1B;   	// Normal color color (1 vertex)
	GTE_CMD_CC    	= ($13 shl 20) or $1C;   	// Color color
	GTE_CMD_NCS   	= ($0C shl 20) or $1E;   	// Normal color (1 vertex) (NORM)
	GTE_CMD_NCT   	= ($0D shl 20) or $20;   	// Normal color (3 vertices)
	GTE_CMD_SQR   	= ($0A shl 20) or $28;   	// Square of vector
	GTE_CMD_DCPL  	= ($06 shl 20) or $29;   	// Depth cue with light
	GTE_CMD_DPCT  	= ($0F shl 20) or $2A;   	// Depth cue (3 vertices)
	GTE_CMD_AVSZ3 	= ($15 shl 20) or $2D;   	// Average Z value (3 vertices)
	GTE_CMD_AVSZ4 	= ($16 shl 20) or $2E;   	// Average Z value (4 vertices)
	GTE_CMD_RTPT  	= ($02 shl 20) or $30;   	// Perspective transformation (3 vertices)
	GTE_CMD_GPF   	= ($19 shl 20) or $3D;   	// Linear interpolation
	GTE_CMD_GPL   	= ($1A shl 20) or $3E;   	// Linear interpolation with base
	GTE_CMD_NCCT  	= ($11 shl 20) or $3F;   	// Normal color color (3 vertices)

	GTE_LM          =  1 shl 10; 				// Saturate IR to 0x0000-0x7fff

	GTE_CV_BITMASK  =  3 shl 13;
	GTE_CV_TR       =  0 shl 13; 				// Use TR as translation vector for MVMVA
	GTE_CV_BK       =  1 shl 13; 				// Use BK as translation vector for MVMVA
	GTE_CV_FC       =  2 shl 13; 				// Use FC as translation vector for MVMVA
	GTE_CV_NONE     =  3 shl 13; 				// Skip translation for MVMVA

	GTE_V_BITMASK   =  3 shl 15;
	GTE_V_V0        =  0 shl 15; 				// Use V0 as operand for MVMVA
	GTE_V_V1        =  1 shl 15; 				// Use V1 as operand for MVMVA
	GTE_V_V2        =  2 shl 15; 				// Use V2 as operand for MVMVA
	GTE_V_IR        =  3 shl 15; 				// Use IR as operand for MVMVA

	GTE_MX_BITMASK  =  3 shl 17;
	GTE_MX_RT       =  0 shl 17; 				// Use rotation matrix as operand for MVMVA
	GTE_MX_LLM      =  1 shl 17; 				// Use light matrix as operand for MVMVA
	GTE_MX_LCM      =  2 shl 17; 				// Use light color matrix as operand for MVMVA

	GTE_SF          =  1 shl 19;  				// Shift results by 12 bits


{ Control register definitions }
const
	GTE_RT11RT12   =  0;  { Rotation matrix }
	GTE_RT13RT21   =  1;  { Rotation matrix }
	GTE_RT22RT23   =  2;  { Rotation matrix }
	GTE_RT31RT32   =  3;  { Rotation matrix }
	GTE_RT33       =  4;  { Rotation matrix }
	GTE_TRX        =  5;  { Translation vector }
	GTE_TRY        =  6;  { Translation vector }
	GTE_TRZ        =  7;  { Translation vector }
	GTE_L11L12     =  8;  { Light matrix }
	GTE_L13L21     =  9;  { Light matrix }
	GTE_L22L23     = 10;  { Light matrix }
	GTE_L31L32     = 11;  { Light matrix }
	GTE_L33        = 12;  { Light matrix }
	GTE_RBK        = 13;  { Background color }
	GTE_GBK        = 14;  { Background color }
	GTE_BBK        = 15;  { Background color }
	GTE_LC11LC12   = 16;  { Light color matrix }
	GTE_LC13LC21   = 17;  { Light color matrix }
	GTE_LC22LC23   = 18;  { Light color matrix }
	GTE_LC31LC32   = 19;  { Light color matrix }
	GTE_LC33       = 20;  { Light color matrix }
	GTE_RFC        = 21;  { Far color }
	GTE_GFC        = 22;  { Far color }
	GTE_BFC        = 23;  { Far color }
	GTE_OFX        = 24;  { Screen coordinate offset }
	GTE_OFY        = 25;  { Screen coordinate offset }
	GTE_H          = 26;  { Projection plane distance }
	GTE_DQA        = 27;  { Depth cue scale factor }
	GTE_DQB        = 28;  { Depth cue base }
	GTE_ZSF3       = 29;  { Average Z scale factor }
	GTE_ZSF4       = 30;  { Average Z scale factor }
	GTE_FLAG       = 31;  { Error/overflow flags }


{ Status flags }
const
	GTE_FLAG_IR0_SATURATED   = 1 shl 12;
	GTE_FLAG_SY2_SATURATED   = 1 shl 13;
	GTE_FLAG_SX2_SATURATED   = 1 shl 14;
	GTE_FLAG_MAC0_UNDERFLOW  = 1 shl 15;
	GTE_FLAG_MAC0_OVERFLOW   = 1 shl 16;
	GTE_FLAG_DIVIDE_OVERFLOW = 1 shl 17;
	GTE_FLAG_Z_SATURATED     = 1 shl 18;
	GTE_FLAG_B_SATURATED     = 1 shl 19;
	GTE_FLAG_G_SATURATED     = 1 shl 20;
	GTE_FLAG_R_SATURATED     = 1 shl 21;
	GTE_FLAG_IR3_SATURATED   = 1 shl 22;
	GTE_FLAG_IR2_SATURATED   = 1 shl 23;
	GTE_FLAG_IR1_SATURATED   = 1 shl 24;
	GTE_FLAG_MAC3_UNDERFLOW  = 1 shl 25;
	GTE_FLAG_MAC2_UNDERFLOW  = 1 shl 26;
	GTE_FLAG_MAC1_UNDERFLOW  = 1 shl 27;
	GTE_FLAG_MAC3_OVERFLOW   = 1 shl 28;
	GTE_FLAG_MAC2_OVERFLOW   = 1 shl 29;
	GTE_FLAG_MAC1_OVERFLOW   = 1 shl 30;
	GTE_FLAG_ERROR           = 1 shl 31;


procedure gte_setControlRegRT11RT12(value: dword);
procedure gte_setControlRegRT13RT21(value: dword);
procedure gte_setControlRegRT22RT23(value: dword);
procedure gte_setControlRegRT31RT32(value: dword);
procedure gte_setControlRegRT33(value: dword);
procedure gte_setControlRegTRX(value: longint);
procedure gte_setControlRegTRY(value: longint);
procedure gte_setControlRegTRZ(value: longint);
procedure gte_setControlRegL11L12(value: dword);
procedure gte_setControlRegL13L21(value: dword);
procedure gte_setControlRegL22L23(value: dword);
procedure gte_setControlRegL31L32(value: dword);
procedure gte_setControlRegL33(value: dword);
procedure gte_setControlRegRBK(value: dword);
procedure gte_setControlRegGBK(value: dword);
procedure gte_setControlRegBBK(value: dword);
procedure gte_setControlRegLC11LC12(value: dword);
procedure gte_setControlRegLC13LC21(value: dword);
procedure gte_setControlRegLC22LC23(value: dword);
procedure gte_setControlRegLC31LC32(value: dword);
procedure gte_setControlRegLC33(value: dword);
procedure gte_setControlRegRFC(value: dword);
procedure gte_setControlRegGFC(value: dword);
procedure gte_setControlRegBFC(value: dword);
procedure gte_setControlRegOFX(value: dword);
procedure gte_setControlRegOFY(value: dword);
procedure gte_setControlRegH(value: dword);
procedure gte_setControlRegDQA(value: dword);
procedure gte_setControlRegDQB(value: dword);
procedure gte_setControlRegZSF3(value: dword);	
procedure gte_setControlRegZSF4(value: dword);
procedure gte_setControlRegFLAG(value: dword);

function gte_getControlRegRT11RT12: dword;
function gte_getControlRegRT13RT21: dword;
function gte_getControlRegRT22RT23: dword;
function gte_getControlRegRT31RT32: dword;
function gte_getControlRegRT33: longint;
function gte_getControlRegTRX: longint;
function gte_getControlRegTRY: longint;
function gte_getControlRegTRZ: longint;
function gte_getControlRegL11L12: dword;
function gte_getControlRegL13L21: dword;
function gte_getControlRegL22L23: dword;
function gte_getControlRegL31L32: dword;
function gte_getControlRegL33: longint;
function gte_getControlRegRBK: dword;
function gte_getControlRegGBK: dword;
function gte_getControlRegBBK: dword;
function gte_getControlRegLC11LC12: dword;
function gte_getControlRegLC13LC21: dword;
function gte_getControlRegLC22LC23: dword;
function gte_getControlRegLC31LC32: dword;
function gte_getControlRegLC33: dword;
function gte_getControlRegRFC: longint;
function gte_getControlRegGFC: longint;
function gte_getControlRegBFC: longint;
function gte_getControlRegOFX: longint;
function gte_getControlRegOFY: longint;
function gte_getControlRegH: dword;
function gte_getControlRegDQA: longint;
function gte_getControlRegDQB: longint;
function gte_getControlRegZSF3: longint;
function gte_getControlRegZSF4: longint;
function gte_getControlRegFLAG: dword;

	
procedure gte_setRotationMatrix(v11, v12, v13,
								v21, v22, v23,
								v31, v32, v33: smallint);
procedure gte_setLightMatrix(v11, v12, v13,
							 v21, v22, v23,
							 v31, v32, v33: smallint);
procedure gte_setLightColorMatrix(v11, v12, v13,
							 	  v21, v22, v23,
							  	  v31, v32, v33: smallint);

procedure gte_loadRotationMatrix(values: pdword);				// GTEMatrix
procedure gte_loadLightMatrix(values: pdword);					// GTEMatrix
procedure gte_loadLightColorMatrix(values: pdword);				// GTEMatrix

procedure gte_storeRotationMatrix(output: pGTEMatrix);
procedure gte_storeLightMatrix(output: pGTEMatrix);
procedure gte_storeLightColorMatrix(output: pGTEMatrix);


{ Data register definitions }
const
	GTE_VXY0 =  0;  { Input vector 0 }
	GTE_VZ0  =  1;  { Input vector 0 }
	GTE_VXY1 =  2;  { Input vector 1 }
	GTE_VZ1  =  3;  { Input vector 1 }
	GTE_VXY2 =  4;  { Input vector 2 }
	GTE_VZ2  =  5;  { Input vector 2 }
	GTE_RGBC =  6;  { Input color and GPU command }
	GTE_OTZ  =  7;  { Average Z value output }
	GTE_IR0  =  8;  { Scalar accumulator }
	GTE_IR1  =  9;  { Vector accumulator }
	GTE_IR2  = 10;  { Vector accumulator }
	GTE_IR3  = 11;  { Vector accumulator }
	GTE_SXY0 = 12;  { X/Y coordinate output FIFO }
	GTE_SXY1 = 13;  { X/Y coordinate output FIFO }
	GTE_SXY2 = 14;  { X/Y coordinate output FIFO }
	GTE_SXYP = 15;  { X/Y coordinate output FIFO }
	GTE_SZ0  = 16;  { Z coordinate output FIFO }
	GTE_SZ1  = 17;  { Z coordinate output FIFO }
	GTE_SZ2  = 18;  { Z coordinate output FIFO }
	GTE_SZ3  = 19;  { Z coordinate output FIFO }
	GTE_RGB0 = 20;  { Color and GPU command output FIFO }
	GTE_RGB1 = 21;  { Color and GPU command output FIFO }
	GTE_RGB2 = 22;  { Color and GPU command output FIFO }
	GTE_MAC0 = 24;  { Extended scalar accumulator }
	GTE_MAC1 = 25;  { Extended vector accumulator }
	GTE_MAC2 = 26;  { Extended vector accumulator }
	GTE_MAC3 = 27;  { Extended vector accumulator }
	GTE_IRGB = 28;  { RGB conversion input }
	GTE_ORGB = 29;  { RGB conversion output }
	GTE_LZCS = 30;  { Leading zero count input }
	GTE_LZCR = 31;  { Leading zero count output }


procedure gte_setDataRegVXY0(value: dword);
procedure gte_setDataRegVZ0(value: dword);
procedure gte_setDataRegVXY1(value: dword);
procedure gte_setDataRegVZ1(value: dword);
procedure gte_setDataRegVXY2(value: dword);
procedure gte_setDataRegVZ2(value: dword);
procedure gte_setDataRegRGBC(value: dword);
procedure gte_setDataRegOTZ(value: dword);
procedure gte_setDataRegIR0(value: dword);
procedure gte_setDataRegIR1(value: dword);
procedure gte_setDataRegIR2(value: dword);
procedure gte_setDataRegIR3(value: dword);
procedure gte_setDataRegSXY0(value: dword);
procedure gte_setDataRegSXY1(value: dword);
procedure gte_setDataRegSXY2(value: dword);
procedure gte_setDataRegSXYP(value: dword);
procedure gte_setDataRegSZ0(value: dword);
procedure gte_setDataRegSZ1(value: dword);
procedure gte_setDataRegSZ2(value: dword);
procedure gte_setDataRegSZ3(value: dword);
procedure gte_setDataRegRGB0(value: dword);
procedure gte_setDataRegRGB1(value: dword);
procedure gte_setDataRegRGB2(value: dword);
procedure gte_setDataRegMAC0(value: dword);
procedure gte_setDataRegMAC1(value: dword);
procedure gte_setDataRegMAC2(value: dword);
procedure gte_setDataRegMAC3(value: dword);
procedure gte_setDataRegIRGB(value: dword);
procedure gte_setDataRegORGB(value: dword);
procedure gte_setDataRegLZCS(value: dword);
procedure gte_setDataRegLZCR(value: dword);

function gte_getDataRegVXY0: dword;
function gte_getDataRegVZ0: longint;
function gte_getDataRegVXY1: dword;
function gte_getDataRegVZ1: longint;
function gte_getDataRegVXY2: dword;
function gte_getDataRegVZ2: longint;
function gte_getDataRegRGBC: dword;
function gte_getDataRegOTZ: dword;
function gte_getDataRegIR0: longint;
function gte_getDataRegIR1: longint;
function gte_getDataRegIR2: longint;
function gte_getDataRegIR3: longint;
function gte_getDataRegSXY0: dword;
function gte_getDataRegSXY1: dword;
function gte_getDataRegSXY2: dword;
function gte_getDataRegSXYP: dword;
function gte_getDataRegSZ0: dword;
function gte_getDataRegSZ1: dword;
function gte_getDataRegSZ2: dword;
function gte_getDataRegSZ3: dword;
function gte_getDataRegRGB0: dword;
function gte_getDataRegRGB1: dword;
function gte_getDataRegRGB2: dword;
function gte_getDataRegMAC0: longint;
function gte_getDataRegMAC1: longint;
function gte_getDataRegMAC2: longint;
function gte_getDataRegMAC3: longint;
function gte_getDataRegIRGB: dword;
function gte_getDataRegORGB: dword;
function gte_getDataRegLZCS: dword;
function gte_getDataRegLZCR: dword;

procedure gte_loadDataRegVXY0(offset: dword; ptr: Pointer);
procedure gte_loadDataRegVZ0(offset: dword; ptr: Pointer);
procedure gte_loadDataRegVXY1(offset: dword; ptr: Pointer);
procedure gte_loadDataRegVZ1(offset: dword; ptr: Pointer);
procedure gte_loadDataRegVXY2(offset: dword; ptr: Pointer);
procedure gte_loadDataRegVZ2(offset: dword; ptr: Pointer);
procedure gte_loadDataRegRGBC(offset: dword; ptr: Pointer);
procedure gte_loadDataRegOTZ(offset: dword; ptr: Pointer);
procedure gte_loadDataRegIR0(offset: dword; ptr: Pointer);
procedure gte_loadDataRegIR1(offset: dword; ptr: Pointer);
procedure gte_loadDataRegIR2(offset: dword; ptr: Pointer);
procedure gte_loadDataRegIR3(offset: dword; ptr: Pointer);
procedure gte_loadDataRegSXY0(offset: dword; ptr: Pointer);
procedure gte_loadDataRegSXY1(offset: dword; ptr: Pointer);
procedure gte_loadDataRegSXY2(offset: dword; ptr: Pointer);
procedure gte_loadDataRegSXYP(offset: dword; ptr: Pointer);
procedure gte_loadDataRegSZ0(offset: dword; ptr: Pointer);
procedure gte_loadDataRegSZ1(offset: dword; ptr: Pointer);
procedure gte_loadDataRegSZ2(offset: dword; ptr: Pointer);
procedure gte_loadDataRegSZ3(offset: dword; ptr: Pointer);
procedure gte_loadDataRegRGB0(offset: dword; ptr: Pointer);
procedure gte_loadDataRegRGB1(offset: dword; ptr: Pointer);
procedure gte_loadDataRegRGB2(offset: dword; ptr: Pointer);
procedure gte_loadDataRegMAC0(offset: dword; ptr: Pointer);
procedure gte_loadDataRegMAC1(offset: dword; ptr: Pointer);
procedure gte_loadDataRegMAC2(offset: dword; ptr: Pointer);
procedure gte_loadDataRegMAC3(offset: dword; ptr: Pointer);
procedure gte_loadDataRegIRGB(offset: dword; ptr: Pointer);
procedure gte_loadDataRegORGB(offset: dword; ptr: Pointer);
procedure gte_loadDataRegLZCS(offset: dword; ptr: Pointer);
procedure gte_loadDataRegLZCR(offset: dword; ptr: Pointer);

procedure gte_storeDataRegVXY0(offset: dword; ptr: Pointer);
procedure gte_storeDataRegVZ0(offset: dword; ptr: Pointer);
procedure gte_storeDataRegVXY1(offset: dword; ptr: Pointer);
procedure gte_storeDataRegVZ1(offset: dword; ptr: Pointer);
procedure gte_storeDataRegVXY2(offset: dword; ptr: Pointer);
procedure gte_storeDataRegVZ2(offset: dword; ptr: Pointer);
procedure gte_storeDataRegRGBC(offset: dword; ptr: Pointer);
procedure gte_storeDataRegOTZ(offset: dword; ptr: Pointer);
procedure gte_storeDataRegIR0(offset: dword; ptr: Pointer);
procedure gte_storeDataRegIR1(offset: dword; ptr: Pointer);
procedure gte_storeDataRegIR2(offset: dword; ptr: Pointer);
procedure gte_storeDataRegIR3(offset: dword; ptr: Pointer);
procedure gte_storeDataRegSXY0(offset: dword; ptr: Pointer);
procedure gte_storeDataRegSXY1(offset: dword; ptr: Pointer);
procedure gte_storeDataRegSXY2(offset: dword; ptr: Pointer);
procedure gte_storeDataRegSXYP(offset: dword; ptr: Pointer);
procedure gte_storeDataRegSZ0(offset: dword; ptr: Pointer);
procedure gte_storeDataRegSZ1(offset: dword; ptr: Pointer);	
procedure gte_storeDataRegSZ2(offset: dword; ptr: Pointer);
procedure gte_storeDataRegSZ3(offset: dword; ptr: Pointer);
procedure gte_storeDataRegRGB0(offset: dword; ptr: Pointer);
procedure gte_storeDataRegRGB1(offset: dword; ptr: Pointer);
procedure gte_storeDataRegRGB2(offset: dword; ptr: Pointer);
procedure gte_storeDataRegMAC0(offset: dword; ptr: Pointer);
procedure gte_storeDataRegMAC1(offset: dword; ptr: Pointer);
procedure gte_storeDataRegMAC2(offset: dword; ptr: Pointer);
procedure gte_storeDataRegMAC3(offset: dword; ptr: Pointer);
procedure gte_storeDataRegIRGB(offset: dword; ptr: Pointer);
procedure gte_storeDataRegORGB(offset: dword; ptr: Pointer);
procedure gte_storeDataRegLZCS(offset: dword; ptr: Pointer);
procedure gte_storeDataRegLZCR(offset: dword; ptr: Pointer);
	

procedure gte_setV0(x, y, z: smallint);
procedure gte_loadV0(input: pGTEVector16);
procedure gte_storeV0(output: pGTEVector16);
procedure gte_setV1(x, y, z: smallint);
procedure gte_loadV1(input: pGTEVector16);
procedure gte_storeV1(output: pGTEVector16);
procedure gte_setV2(x, y, z: smallint);
procedure gte_loadV2(input: pGTEVector16);
procedure gte_storeV2(output: pGTEVector16);

procedure gte_setRowVectors(v11, v12, v13,
							v21, v22, v23,
							v31, v32, v33: smallint);
procedure gte_setColumnVectors(v11, v12, v13,
							   v21, v22, v23,
							   v31, v32, v33: smallint);


procedure setupGTE(width, height: Integer);
{
const
	sintable : array [0..359] of longint = (
	0,71,143,214,286,357,428,499,570,641,711,782,852,921,991,1060,1129,1198,1266,1334,1401,1468,1534,1600,1666,
	1731,1796,1860,1923,1986,2048,2110,2171,2231,2290,2349,2408,2465,2522,2578,2633,2687,2741,2793,2845,2896,2946,2996,3044,
	3091,3138,3183,3228,3271,3314,3355,3396,3435,3474,3511,3547,3582,3617,3650,3681,3712,3742,3770,3798,3824,3849,3873,3896,
	3917,3937,3956,3974,3991,4006,4021,4034,4046,4056,4065,4074,4080,4086,4090,4094,4095,4096,4095,4094,4090,4086,4080,4074,
	4065,4056,4046,4034,4021,4006,3991,3974,3956,3937,3917,3896,3873,3849,3824,3798,3770,3742,3712,3681,3650,3617,3582,3547,
	3511,3474,3435,3396,3355,3314,3271,3228,3183,3138,3091,3044,2996,2946,2896,2845,2793,2741,2687,2633,2578,2522,2465,2408,
	2349,2290,2231,2171,2110,2048,1986,1923,1860,1796,1731,1666,1600,1534,1468,1401,1334,1266,1198,1129,1060,991,921,852,
	782,711,641,570,499,428,357,286,214,143,71,0,-71,-143,-214,-286,-357,-428,-499,-570,-641,-711,-782,-852,
	-921,-991,-1060,-1129,-1198,-1266,-1334,-1401,-1468,-1534,-1600,-1666,-1731,-1796,-1860,-1923,-1986,-2048,-2110,-2171,-2231,-2290,-2349,-2408,
	-2465,-2522,-2578,-2633,-2687,-2741,-2793,-2845,-2896,-2946,-2996,-3044,-3091,-3138,-3183,-3228,-3271,-3314,-3355,-3396,-3435,-3474,-3511,-3547,
	-3582,-3617,-3650,-3681,-3712,-3742,-3770,-3798,-3824,-3849,-3873,-3896,-3917,-3937,-3956,-3974,-3991,-4006,-4021,-4034,-4046,-4056,-4065,-4074,
	-4080,-4086,-4090,-4094,-4095,-4096,-4095,-4094,-4090,-4086,-4080,-4074,-4065,-4056,-4046,-4034,-4021,-4006,-3991,-3974,-3956,-3937,-3917,-3896,
	-3873,-3849,-3824,-3798,-3770,-3742,-3712,-3681,-3650,-3617,-3582,-3547,-3511,-3474,-3435,-3396,-3355,-3314,-3271,-3228,-3183,-3138,-3091,-3044,
	-2996,-2946,-2896,-2845,-2793,-2741,-2687,-2633,-2578,-2522,-2465,-2408,-2349,-2290,-2231,-2171,-2110,-2048,-1986,-1923,-1860,-1796,-1731,-1666,
	-1600,-1534,-1468,-1401,-1334,-1266,-1198,-1129,-1060,-991,-921,-852,-782,-711,-641,-570,-499,-428,-357,-286,-214,-143,-71);

function isin(x: Int32): Int32;
function icos(x: Int32): Int32;
}
implementation
uses ps1GPU;

procedure gte_setControlRegRT11RT12(value: dword); assembler;
asm
	ctc2 $a0, $RT11RT12
end;


procedure gte_setControlRegRT13RT21(value: dword); assembler;
asm
	ctc2 $a0, $RT13RT21
end;


procedure gte_setControlRegRT22RT23(value: dword); assembler;
asm
	ctc2 $a0, $RT22RT23
end;


procedure gte_setControlRegRT31RT32(value: dword); assembler;
asm
	ctc2 $a0, $RT31RT32
end;


procedure gte_setControlRegRT33(value: dword); assembler;
asm
	ctc2 $a0, $RT33
end;


procedure gte_setControlRegTRX(value: longint); assembler;
asm
	ctc2 $a0, $TRX
end;


procedure gte_setControlRegTRY(value: longint); assembler;
asm
	ctc2 $a0, $TRY
end;


procedure gte_setControlRegTRZ(value: longint); assembler;
asm
	ctc2 $a0, $TRZ
end;


procedure gte_setControlRegL11L12(value: dword); assembler;
asm
	ctc2 $a0, $L11L12
end;


procedure gte_setControlRegL13L21(value: dword); assembler;
asm
	ctc2 $a0, $L13L21
end;


procedure gte_setControlRegL22L23(value: dword); assembler;
asm
	ctc2 $a0, $L22L23
end;


procedure gte_setControlRegL31L32(value: dword); assembler;
asm
	ctc2 $a0, $L31L32
end;


procedure gte_setControlRegL33(value: dword); assembler;
asm
	ctc2 $a0, $L33
end;


procedure gte_setControlRegRBK(value: dword); assembler;
asm
	ctc2 $a0, $RBK
end;


procedure gte_setControlRegGBK(value: dword); assembler;
asm
	ctc2 $a0, $GBK
end;


procedure gte_setControlRegBBK(value: dword); assembler;
asm
	ctc2 $a0, $BBK
end;


procedure gte_setControlRegLC11LC12(value: dword); assembler;
asm
	ctc2 $a0, $LC11LC12
end;


procedure gte_setControlRegLC13LC21(value: dword); assembler;
asm
	ctc2 $a0, $LC13LC21
end;


procedure gte_setControlRegLC22LC23(value: dword); assembler;
asm
	ctc2 $a0, $LC22LC23
end;


procedure gte_setControlRegLC31LC32(value: dword); assembler;
asm
	ctc2 $a0, $LC31LC32
end;


procedure gte_setControlRegLC33(value: dword); assembler;
asm
	ctc2 $a0, $LC33
end;


procedure gte_setControlRegRFC(value: dword); assembler;
asm
	ctc2 $a0, $RFC
end;


procedure gte_setControlRegGFC(value: dword); assembler;
asm
	ctc2 $a0, $GFC
end;


procedure gte_setControlRegBFC(value: dword); assembler;
asm
	ctc2 $a0, $BFC
end;


procedure gte_setControlRegOFX(value: dword); assembler;
asm
	ctc2 $a0, $OFX
end;


procedure gte_setControlRegOFY(value: dword); assembler;
asm
	ctc2 $a0, $OFY
end;


procedure gte_setControlRegH(value: dword); assembler;
asm
	ctc2 $a0, $H
end;


procedure gte_setControlRegDQA(value: dword); assembler;
asm
	ctc2 $a0, $DQA
end;


procedure gte_setControlRegDQB(value: dword); assembler;
asm
	ctc2 $a0, $DQB
end;


procedure gte_setControlRegZSF3(value: dword); assembler;
asm
	ctc2 $a0, $ZSF3
end;


procedure gte_setControlRegZSF4(value: dword); assembler;
asm
	ctc2 $a0, $ZSF4
end;


procedure gte_setControlRegFLAG(value: dword); assembler;
asm
	ctc2 $a0, $FLAG
end;


function gte_getControlRegRT11RT12: dword; assembler;
asm
	cfc2 $v0, $RT11RT12
end;


function gte_getControlRegRT13RT21: dword; assembler;
asm
	cfc2 $v0, $RT13RT21
end;


function gte_getControlRegRT22RT23: dword; assembler;
asm
	cfc2 $v0, $RT22RT23
end;


function gte_getControlRegRT31RT32: dword; assembler;
asm
	cfc2 $v0, $RT31RT32
end;


function gte_getControlRegRT33: longint; assembler;
asm
	cfc2 $v0, $RT33
end;


function gte_getControlRegTRX: longint; assembler;
asm
	cfc2 $v0, $TRX
end;


function gte_getControlRegTRY: longint; assembler;
asm
	cfc2 $v0, $TRY
end;


function gte_getControlRegTRZ: longint; assembler;
asm
	cfc2 $v0, $TRZ
end;


function gte_getControlRegL11L12: dword; assembler;
asm
	cfc2 $v0, $L11L12
end;


function gte_getControlRegL13L21: dword; assembler;
asm
	cfc2 $v0, $L13L21
end;


function gte_getControlRegL22L23: dword; assembler;
asm
	cfc2 $v0, $L22L23
end;


function gte_getControlRegL31L32: dword; assembler;
asm
	cfc2 $v0, $L31L32
end;


function gte_getControlRegL33: longint; assembler;
asm
	cfc2 $v0, $L33
end;


function gte_getControlRegRBK: dword; assembler;
asm
	cfc2 $v0, $RBK
end;


function gte_getControlRegGBK: dword; assembler;
asm
	cfc2 $v0, $GBK
end;


function gte_getControlRegBBK: dword; assembler;
asm
	cfc2 $v0, $BBK
end;


function gte_getControlRegLC11LC12: dword; assembler;
asm
	cfc2 $v0, $LC11LC12
end;


function gte_getControlRegLC13LC21: dword; assembler;
asm
	cfc2 $v0, $LC13LC21
end;


function gte_getControlRegLC22LC23: dword; assembler;
asm
	cfc2 $v0, $LC22LC23
end;


function gte_getControlRegLC31LC32: dword; assembler;
asm
	cfc2 $v0, $LC31LC32
end;


function gte_getControlRegLC33: dword; assembler;
asm
	cfc2 $v0, $LC33
end;


function gte_getControlRegRFC: longint; assembler;
asm
	cfc2 $v0, $RFC
end;


function gte_getControlRegGFC: longint; assembler;
asm
	cfc2 $v0, $GFC
end;


function gte_getControlRegBFC: longint; assembler;
asm
	cfc2 $v0, $BFC
end;


function gte_getControlRegOFX: longint; assembler;
asm
	cfc2 $v0, $OFX
end;


function gte_getControlRegOFY: longint; assembler;
asm
	cfc2 $v0, $OFY
end;


function gte_getControlRegH: dword; assembler;
asm
	cfc2 $v0, $H
end;


function gte_getControlRegDQA: longint; assembler;
asm
	cfc2 $v0, $DQA
end;


function gte_getControlRegDQB: longint; assembler;
asm
	cfc2 $v0, $DQB
end;


function gte_getControlRegZSF3: longint; assembler;
asm
	cfc2 $v0, $ZSF3
end;


function gte_getControlRegZSF4: longint; assembler;
asm
	cfc2 $v0, $ZSF4
end;


function gte_getControlRegFLAG: dword; assembler;
asm
	cfc2 $v0, $FLAG
end;


procedure gte_setRotationMatrix(v11, v12, v13,
								v21, v22, v23,
								v31, v32, v33: smallint);
begin
		gte_setControlRegRT11RT12((v11 and $ffff) or (v12 shl 16));
		gte_setControlRegRT13RT21((v13 and $ffff) or (v21 shl 16));
		gte_setControlRegRT22RT23((v22 and $ffff) or (v23 shl 16));
		gte_setControlRegRT31RT32((v31 and $ffff) or (v32 shl 16));
		gte_setControlRegRT33(v33);
end;


procedure gte_setLightMatrix(v11, v12, v13,
							 v21, v22, v23,
							 v31, v32, v33: smallint);
begin
		gte_setControlRegL11L12((v11 and $ffff) or (v12 shl 16));
		gte_setControlRegL13L21((v13 and $ffff) or (v21 shl 16));
		gte_setControlRegL22L23((v22 and $ffff) or (v23 shl 16));
		gte_setControlRegL31L32((v31 and $ffff) or (v32 shl 16));
		gte_setControlRegL33(v33);
end;


procedure gte_setLightColorMatrix(v11, v12, v13,
							 	  v21, v22, v23,
							  	  v31, v32, v33: smallint);
begin
		gte_setControlRegLC11LC12((v11 and $ffff) or (v12 shl 16));
		gte_setControlRegLC13LC21((v13 and $ffff) or (v21 shl 16));
		gte_setControlRegLC22LC23((v22 and $ffff) or (v23 shl 16));
		gte_setControlRegLC31LC32((v31 and $ffff) or (v32 shl 16));
		gte_setControlRegLC33(v33);
end;


procedure gte_loadRotationMatrix(values: pdword);
begin

  gte_setControlRegRT11RT12(Values[0]);
  gte_setControlRegRT13RT21(Values[1]);
  gte_setControlRegRT22RT23(Values[2]);
  gte_setControlRegRT31RT32(Values[3]);
  gte_setControlRegRT33(Values[4]);

end;


procedure gte_loadLightMatrix(values: pdword);
begin

  gte_setControlRegL11L12(Values[0]);
  gte_setControlRegL13L21(Values[1]);
  gte_setControlRegL22L23(Values[2]);
  gte_setControlRegL31L32(Values[3]);
  gte_setControlRegL33(Values[4]);

end;


procedure gte_loadLightColorMatrix(values: pdword);
begin

  gte_setControlRegLC11LC12(Values[0]);
  gte_setControlRegLC13LC21(Values[1]);
  gte_setControlRegLC22LC23(Values[2]);
  gte_setControlRegLC31LC32(Values[3]);
  gte_setControlRegLC33(Values[4]);

end;


procedure gte_storeRotationMatrix(output: pGTEMatrix);
begin
  // Row 0
  output^.m[0,0]:= gte_getControlRegRT11RT12 and $FFFF;
  output^.m[0,1]:= gte_getControlRegRT11RT12 shr 16;
  output^.m[0,2]:= gte_getControlRegRT13RT21 and $FFFF;
  // Row 1
  output^.m[1,0]:= gte_getControlRegRT13RT21 shr 16;
  output^.m[1,1]:= gte_getControlRegRT22RT23 and $FFFF;
  output^.m[1,2]:= gte_getControlRegRT22RT23 shr 16;
  // Row 2
  output^.m[2,0]:= gte_getControlRegRT31RT32 and $FFFF;
  output^.m[2,1]:= gte_getControlRegRT31RT32 shr 16;
  output^.m[2,2]:= gte_getControlRegRT33 and $FFFF;
end;


procedure gte_storeLightMatrix(output: pGTEMatrix);
begin
  // Row 0
  output^.m[0,0]:= gte_getControlRegL11L12 and $FFFF;
  output^.m[0,1]:= gte_getControlRegL11L12 shr 16;
  output^.m[0,2]:= gte_getControlRegL13L21 and $FFFF;
  // Row 1
  output^.m[1,0]:= gte_getControlRegL13L21 shr 16;
  output^.m[1,1]:= gte_getControlRegL22L23 and $FFFF;
  output^.m[1,2]:= gte_getControlRegL22L23 shr 16;
  // Row 2
  output^.m[2,0]:= gte_getControlRegL31L32 and $FFFF;
  output^.m[2,1]:= gte_getControlRegL31L32 shr 16;
  output^.m[2,2]:= gte_getControlRegL33 and $FFFF;
end;


procedure gte_storeLightColorMatrix(output: pGTEMatrix);
begin
  // Row 0
  output^.m[0,0]:= gte_getControlRegLC11LC12 and $FFFF;
  output^.m[0,1]:= gte_getControlRegLC11LC12 shr 16;
  output^.m[0,2]:= gte_getControlRegLC13LC21 and $FFFF;
  // Row 1
  output^.m[1,0]:= gte_getControlRegLC13LC21 shr 16;
  output^.m[1,1]:= gte_getControlRegLC22LC23 and $FFFF;
  output^.m[1,2]:= gte_getControlRegLC22LC23 shr 16;
  // Row 2
  output^.m[2,0]:= gte_getControlRegLC31LC32 and $FFFF;
  output^.m[2,1]:= gte_getControlRegLC31LC32 shr 16;
  output^.m[2,2]:= gte_getControlRegLC33 and $FFFF;
end;


procedure gte_setDataRegVXY0(value: dword); assembler;
asm
	mtc2 $a0, $VXY0
end;


procedure gte_setDataRegVZ0(value: dword); assembler;
asm
	mtc2 $a0, $VZ0
end;

procedure gte_setDataRegVXY1(value: dword); assembler;
asm
	mtc2 $a0, $VXY1
end;

procedure gte_setDataRegVZ1(value: dword); assembler;
asm
	mtc2 $a0, $VZ1
end;


procedure gte_setDataRegVXY2(value: dword); assembler;
asm
	mtc2 $a0, $VXY2
end;


procedure gte_setDataRegVZ2(value: dword); assembler;
asm
	mtc2 $a0, $VZ2
end;


procedure gte_setDataRegRGBC(value: dword); assembler;
asm
	mtc2 $a0, $RGBC
end;


procedure gte_setDataRegOTZ(value: dword); assembler;
asm
	mtc2 $a0, $OTZ
end;


procedure gte_setDataRegIR0(value: dword); assembler;
asm
	mtc2 $a0, $IR0
end;


procedure gte_setDataRegIR1(value: dword); assembler;
asm
	mtc2 $a0, $IR1
end;


procedure gte_setDataRegIR2(value: dword); assembler;
asm
	mtc2 $a0, $IR2
end;


procedure gte_setDataRegIR3(value: dword); assembler;
asm
	mtc2 $a0, $IR3
end;


procedure gte_setDataRegSXY0(value: dword); assembler;
asm
	mtc2 $a0, $SXY0
end;


procedure gte_setDataRegSXY1(value: dword); assembler;
asm
	mtc2 $a0, $SXY1
end;


procedure gte_setDataRegSXY2(value: dword); assembler;
asm
	mtc2 $a0, $SXY2
end;


procedure gte_setDataRegSXYP(value: dword); assembler;
asm
	mtc2 $a0, $SXYP
end;


procedure gte_setDataRegSZ0(value: dword); assembler;
asm
	mtc2 $a0, $SZ0
end;


procedure gte_setDataRegSZ1(value: dword); assembler;
asm
	mtc2 $a0, $SZ1
end;


procedure gte_setDataRegSZ2(value: dword); assembler;
asm
	mtc2 $a0, $SZ2
end;


procedure gte_setDataRegSZ3(value: dword); assembler;
asm
	mtc2 $a0, $SZ3
end;


procedure gte_setDataRegRGB0(value: dword); assembler;
asm
	mtc2 $a0, $RGB0
end;


procedure gte_setDataRegRGB1(value: dword); assembler;
asm
	mtc2 $a0, $RGB1
end;


procedure gte_setDataRegRGB2(value: dword); assembler;
asm
	mtc2 $a0, $RGB2
end;


procedure gte_setDataRegMAC0(value: dword); assembler;
asm
	mtc2 $a0, $MAC0
end;


procedure gte_setDataRegMAC1(value: dword); assembler;
asm
	mtc2 $a0, $MAC1
end;


procedure gte_setDataRegMAC2(value: dword); assembler;
asm
	mtc2 $a0, $MAC2
end;


procedure gte_setDataRegMAC3(value: dword); assembler;
asm
	mtc2 $a0, $MAC3
end;


procedure gte_setDataRegIRGB(value: dword); assembler;
asm
	mtc2 $a0, $IRGB
end;


procedure gte_setDataRegORGB(value: dword); assembler;
asm
	mtc2 $a0, $ORGB
end;


procedure gte_setDataRegLZCS(value: dword); assembler;
asm
	mtc2 $a0, $LZCS
end;


procedure gte_setDataRegLZCR(value: dword); assembler;
asm
	mtc2 $a0, $LZCR
end;


function gte_getDataRegVXY0: dword; assembler;
asm
	mfc2 $v0, $VXY0
end;


function gte_getDataRegVZ0: longint; assembler;
asm
	mfc2 $v0, $VZ0
end;


function gte_getDataRegVXY1: dword; assembler;
asm
	mfc2 $v0, $VXY1
end;


function gte_getDataRegVZ1: longint; assembler;
asm
	mfc2 $v0, $VZ1
end;


function gte_getDataRegVXY2: dword; assembler;
asm
	mfc2 $v0, $VXY2
end;


function gte_getDataRegVZ2: longint; assembler;
asm
	mfc2 $v0, $VZ2
end;


function gte_getDataRegRGBC: dword; assembler;
asm
	mfc2 $v0, $RGBC
end;


function gte_getDataRegOTZ: dword; assembler;
asm
	mfc2 $v0, $OTZ	
end;


function gte_getDataRegIR0: longint; assembler;
asm
	mfc2 $v0, $IR0
end;


function gte_getDataRegIR1: longint; assembler;
asm
	mfc2 $v0, $IR1
end;


function gte_getDataRegIR2: longint; assembler;
asm
	mfc2 $v0, $IR2
end;


function gte_getDataRegIR3: longint; assembler;
asm
	mfc2 $v0, $IR3
end;


function gte_getDataRegSXY0: dword; assembler;
asm
	mfc2 $v0, $SXY0
end;


function gte_getDataRegSXY1: dword; assembler;
asm
	mfc2 $v0, $SXY1
end;


function gte_getDataRegSXY2: dword; assembler;
asm
	mfc2 $v0, $SXY2
end;


function gte_getDataRegSXYP: dword; assembler;
asm
	mfc2 $v0, $SXYP
end;


function gte_getDataRegSZ0: dword; assembler;
asm
	mfc2 $v0, $SZ0
end;


function gte_getDataRegSZ1: dword; assembler;
asm
	mfc2 $v0, $SZ1
end;


function gte_getDataRegSZ2: dword; assembler;
asm
	mfc2 $v0, $SZ2
end;


function gte_getDataRegSZ3: dword; assembler;
asm
	mfc2 $v0, $SZ3
end;


function gte_getDataRegRGB0: dword; assembler;
asm
	mfc2 $v0, $RGB0
end;


function gte_getDataRegRGB1: dword; assembler;
asm
	mfc2 $v0, $RGB1
end;


function gte_getDataRegRGB2: dword; assembler;
asm
	mfc2 $v0, $RGB2
end;


function gte_getDataRegMAC0: longint; assembler;
asm
	mfc2 $v0, $MAC0
end;


function gte_getDataRegMAC1: longint; assembler;
asm
	mfc2 $v0, $MAC1
end;


function gte_getDataRegMAC2: longint; assembler;
asm
	mfc2 $v0, $MAC2
end;


function gte_getDataRegMAC3: longint; assembler;
asm
	mfc2 $v0, $MAC3
end;


function gte_getDataRegIRGB: dword; assembler;
asm
	mfc2 $v0, $IRGB
end;


function gte_getDataRegORGB: dword; assembler;
asm
	mfc2 $v0, $ORGB
end;


function gte_getDataRegLZCS: dword; assembler;
asm
	mfc2 $v0, $LZCS
end;


function gte_getDataRegLZCR: dword; assembler;
asm
	mfc2 $v0, $LZCR
end;


procedure gte_loadDataRegVXY0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $VXY0, 0($t0)
end;


procedure gte_loadDataRegVZ0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $VZ0, 0($t0)
end;


procedure gte_loadDataRegVXY1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $VXY1, 0($t0)
end;


procedure gte_loadDataRegVZ1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $VZ1, 0($t0)
end;


procedure gte_loadDataRegVXY2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $VXY2, 0($t0)
end;


procedure gte_loadDataRegVZ2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $VZ2, 0($t0)
end;


procedure gte_loadDataRegRGBC(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $RGBC, 0($t0)
end;


procedure gte_loadDataRegOTZ(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $OTZ, 0($t0)
end;


procedure gte_loadDataRegIR0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $IR0, 0($t0)
end;


procedure gte_loadDataRegIR1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $IR1, 0($t0)
end;


procedure gte_loadDataRegIR2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $IR2, 0($t0)
end;


procedure gte_loadDataRegIR3(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $IR3, 0($t0)
end;


procedure gte_loadDataRegSXY0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $SXY0, 0($t0)
end;


procedure gte_loadDataRegSXY1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $SXY1, 0($t0)
end;


procedure gte_loadDataRegSXY2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $SXY2, 0($t0)
end;


procedure gte_loadDataRegSXYP(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $SXYP, 0($t0)
end;


procedure gte_loadDataRegSZ0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $SZ0, 0($t0)
end;


procedure gte_loadDataRegSZ1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $SZ1, 0($t0)
end;


procedure gte_loadDataRegSZ2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $SZ2, 0($t0)
end;


procedure gte_loadDataRegSZ3(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $SZ3, 0($t0)
end;


procedure gte_loadDataRegRGB0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $RGB0, 0($t0)
end;


procedure gte_loadDataRegRGB1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $RGB1, 0($t0)
end;


procedure gte_loadDataRegRGB2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $RGB2, 0($t0)
end;


procedure gte_loadDataRegMAC0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $MAC0, 0($t0)
end;


procedure gte_loadDataRegMAC1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $MAC1, 0($t0)
end;


procedure gte_loadDataRegMAC2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $MAC2, 0($t0)
end;


procedure gte_loadDataRegMAC3(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $MAC3, 0($t0)
end;


procedure gte_loadDataRegIRGB(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $IRGB, 0($t0)
end;


procedure gte_loadDataRegORGB(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $ORGB, 0($t0)
end;


procedure gte_loadDataRegLZCS(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $LZCS, 0($t0)
end;


procedure gte_loadDataRegLZCR(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  lwc2 $LZCR, 0($t0)
end;


procedure gte_storeDataRegVXY0(offset: dword; ptr: Pointer); assembler;
asm
	addu $t0, $a1, $a0
  	swc2 $VXY0, 0($t0)
end;


procedure gte_storeDataRegVZ0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $VZ0, 0($t0)
end;


procedure gte_storeDataRegVXY1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $VXY1, 0($t0)
end;


procedure gte_storeDataRegVZ1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $VZ1, 0($t0)
end;


procedure gte_storeDataRegVXY2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $VXY2, 0($t0)
end;


procedure gte_storeDataRegVZ2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $VZ2, 0($t0)
end;


procedure gte_storeDataRegRGBC(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $RGBC, 0($t0)
end;


procedure gte_storeDataRegOTZ(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $OTZ, 0($t0)
end;


procedure gte_storeDataRegIR0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $IR0, 0($t0)
end;


procedure gte_storeDataRegIR1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $IR1, 0($t0)
end;


procedure gte_storeDataRegIR2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $IR2, 0($t0)
end;


procedure gte_storeDataRegIR3(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $IR3, 0($t0)
end;


procedure gte_storeDataRegSXY0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $SXY0, 0($t0)
end;


procedure gte_storeDataRegSXY1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $SXY1, 0($t0)
end;


procedure gte_storeDataRegSXY2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $SXY2, 0($t0)
end;


procedure gte_storeDataRegSXYP(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $SXYP, 0($t0)
end;


procedure gte_storeDataRegSZ0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $SZ0, 0($t0)
end;


procedure gte_storeDataRegSZ1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $SZ1, 0($t0)
end;


procedure gte_storeDataRegSZ2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $SZ2, 0($t0)
end;


procedure gte_storeDataRegSZ3(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $SZ3, 0($t0)
end;


procedure gte_storeDataRegRGB0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $RGB0, 0($t0)
end;



procedure gte_storeDataRegRGB1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $RGB1, 0($t0)
end;


procedure gte_storeDataRegRGB2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $RGB2, 0($t0)
end;


procedure gte_storeDataRegMAC0(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $MAC0, 0($t0)
end;


procedure gte_storeDataRegMAC1(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $MAC1, 0($t0)
end;


procedure gte_storeDataRegMAC2(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $MAC2, 0($t0)
end;


procedure gte_storeDataRegMAC3(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $MAC3, 0($t0)
end;


procedure gte_storeDataRegIRGB(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $IRGB, 0($t0)
end;


procedure gte_storeDataRegORGB(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $ORGB, 0($t0)
end;


procedure gte_storeDataRegLZCS(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $LZCS, 0($t0)
end;


procedure gte_storeDataRegLZCR(offset: dword; ptr: Pointer); assembler;
asm
  addu $t0, $a1, $a0
  swc2 $LZCR, 0($t0)
end;


procedure gte_setV0(x, y, z: smallint); inline;
begin
	gte_setDataRegVXY0(dword((x and $ffff) or (y shl 16)));
	gte_setDataRegVZ0(dword(z));
end;


procedure gte_loadV0(input: pGTEVector16); inline;
begin
	gte_loadDataRegVXY0(0, input);
	gte_loadDataRegVZ0(4, input);
end;


procedure gte_storeV0(output: pGTEVector16); inline;
begin
	gte_storeDataRegVXY0(0, output);
	gte_storeDataRegVZ0(4, output);
end;


procedure gte_setV1(x, y, z: smallint); inline;
begin
	gte_setDataRegVXY1((x and $ffff) or (y shl 16));
	gte_setDataRegVZ1(z);
end;


procedure gte_loadV1(input: pGTEVector16); inline;
begin
	gte_loadDataRegVXY1(0, input);
	gte_loadDataRegVZ1(4, input);
end;


procedure gte_storeV1(output: pGTEVector16); inline;
begin
	gte_storeDataRegVXY1(0, @output);
	gte_storeDataRegVZ1(4, @output);
end;


procedure gte_setV2(x, y, z: smallint); inline;
begin
	gte_setDataRegVXY2((x and $ffff) or (y shl 16));
	gte_setDataRegVZ2(z);
end;


procedure gte_loadV2(input: pGTEVector16); inline;
begin
	gte_loadDataRegVXY2(0, input);
	gte_loadDataRegVZ2(4, input);
end;


procedure gte_storeV2(output: pGTEVector16); inline;
begin
	gte_storeDataRegVXY2(0, output);
	gte_storeDataRegVZ2(4, output);
end;


procedure gte_setRowVectors(v11, v12, v13,
							v21, v22, v23,
							v31, v32, v33: smallint);
begin
	gte_setDataRegVXY0(dword((v11 and $ffff) or (v12 shl 16)));
	gte_setDataRegVZ0(dword(v13));
	gte_setDataRegVXY1(dword((v21 and $ffff) or (v22 shl 16)));
	gte_setDataRegVZ1(dword(v23));
	gte_setDataRegVXY2(dword((v31 and $ffff) or (v32 shl 16)));
	gte_setDataRegVZ2(dword(v33));
end;


procedure gte_setColumnVectors(v11, v12, v13,
							   v21, v22, v23,
							   v31, v32, v33: smallint);
begin
	gte_setDataRegVXY0(dword((v11 and $ffff) or (v21 shl 16)));
	gte_setDataRegVZ0(dword(v31));
	gte_setDataRegVXY1(dword((v12 and $ffff) or (v22 shl 16)));
	gte_setDataRegVZ1(dword(v32));
	gte_setDataRegVXY2(dword((v13 and $ffff) or (v23 shl 16)));
	gte_setDataRegVZ2(dword(v33));
end;


procedure setupGTE(width, height: Integer);
begin
    
  cop0_setSTATUS(cop0_getSTATUS or COP0_STATUS_CU2);

  gte_setControlRegOFX((width shl 16) div 2);
  gte_setControlRegOFY((height shl 16) div 2);
  gte_setControlRegH(240);
  gte_setControlRegZSF3(ORDERING_TABLE_SIZE div 3);
  gte_setControlRegZSF4(ORDERING_TABLE_SIZE div 4);

end;

{
function isin(x: Int32): Int32;
begin
  result:= sintable[x mod 359];
end;


function icos(x: Int32): Int32;
begin
  result:= SinTable[(90 + x) mod 359];
end;
}
end.