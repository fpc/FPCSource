unit libds; 
interface
uses libstd;

const
	// CD-ROM Mode (used int CdlSetmode)
	DslModeStream		= 	$100;// Normal Streaming 
	DslModeStream2		= 	$120;// SUB HEADER information includes
	DslModeSpeed		= 	$80;// 0: normal speed	1: double speed
	DslModeRT			= 	$40;// 0: ADPCM off		1: ADPCM on
	DslModeSize1		= 	$20;// 0: 2048 byte		1: 2340byte
	DslModeSize0		= 	$10;// 0: -			1: 2328byte
	DslModeSF			= 	$08;// 0: Channel off	1: Channel on
	slModeRept			= 	$04;// 0: Report off	1: Report on
	DslModeAP			= 	$02;// 0: AutoPause off	1: AutoPause on
	DslModeDA			= 	$01;// 0: CD-DA off		1: CD-DA on

	// Status contents
	DslStatPlay			= 	$80;// playing CD-DA
	DslStatSeek			= 	$40;// seeking
	DslStatRead			= 	$20;// reading data sectors
	DslStatShellOpen	=	$10;// once shell open
	DslStatSeekError	=	$04;// seek error detected
	DslStatStandby		=	$02;// spindle motor rotating
	DslStatError		=	$01;// command error detected

	// Macros for DsGetDiskType()
	DslStatNoDisk		=	$01;
	DslOtherFormat		=	$02;
	DslCdromFormat		=	$04;

	// CD-ROM Primitive Commands
	DslNop				=	$01;// no operation
	DslSetloc			=	$02;// set head position
	DslPlay				=	$03;// play CD-DA
	DslForward			=	$04;// forward DA play
	DslBackward			=	$05;// backward DA play
	DslReadN			=	$06;// read data with retry
	DslStandby			=	$07;// start spindle motor
	DslStop				=	$08;// stop spindle motor
	DslPause			=	$09;// pause
	DslMute				=	$0b;// mute on
	DslDemute			=	$0c;// mute off
	DslSetfilter		=	$0d;// set subheader filter
	DslSetmode			=	$0e;// set mode 
	DslGetparam			=	$0f;// get mode
	DslGetlocL			=	$10;// get head position (data sector)
	DslGetlocP			=	$11;// get head position (DA sector)
	DslGetTN			=	$13;// get number of TOC
	DslGetTD			=	$14;// get TOC data
	DslSeekL			=	$15;// logical seek
	DslSeekP			=	$16;// phisical seek
	DslReadS			=	$1B;// read data without retry

	// Interrupts
	DslNoIntr			=	$00;// No interrupt
	DslDataReady		=	$01;// Data Read
	DslComplete			=	$02;// Command Complete
	DslAcknowledge		=	$03;// Acknowledge (reserved)
	DslDataEnd			=	$04;// End of Data Detected
	DslDiskError		=	405;// Error Detected

	DslNoResult			=	$06;
	DslFinished			=	$07;

function btoi(b: longint): byte;	// BCD to u_char
function itob(i: byte): longint;	// u_char to BCD

// Position
const
  	DslMAXTOC			=	100;


// Callback
//typedef void ( *DslRCB )( u_char, u_char*, u_long* );
type
	DslCB = procedure (b: byte; p: pchar);
	DslRCB = procedure (b: byte; p: pchar; l: Plongint);


// Location
type 
	DslLOC = packed record
				minute : byte;		// minute (BCD)
				second : byte;		// second (BCD)
				sector : byte;		// sector (BCD)
				track : byte;		// track (void)
			 end;
	PDslLOC = ^DslLOC;

// ADPCM Filter
	DslFILTER = packed record
					_file : byte;		// file ID (always 1)
					chan : byte;		// channel ID
					pad : u_short;
				end;

// Attenuator
	DslATV = packed record
				val0 : byte;		// volume for CD(L) -> SPU (L)
				val1 : byte;		// volume for CD(L) -> SPU (R)
				val2 : byte;		// volume for CD(R) -> SPU (L)
				val3 : byte;		// volume for CD(R) -> SPU (R)
			 end;
	PDslATV = ^DslATV;

// Low Level File System for DsSearchFile() 
const
	DslMAXFILE		= 	64;		// max number of files in a directory
	DslMAXDIR		=	128;	// max number of total directories
	DslMAXLEVEL		=	8;		// max levels of directories

type
	DslFILE = packed record
				pos : DslLOC;					// file location
				size : dword;					// file size
				name : array [0..15] of char;	// file name (body)
			  end;
	PDslFILE = ^DslFILE;

// system status
const
	DslReady	=	1;
	DslBusy		=	2;
	DslNoCD		=	3;

// maximum number of commands that can be added to the queue
	DslMaxCOMMANDS	=	8;

// maximum number of command execution results
	DslMaxRESULTS	=	8;

function DsInit: integer; stdcall external;
function DsReset: integer; stdcall external;
procedure DsClose; stdcall external;
function DsCommand(com: byte; param: pchar; cbsync: DslCB; count: integer ): integer; stdcall external;
function DsPacket(mode: byte; pos:PDslLOC; com: byte; func: DslCB; count: integer): integer; stdcall external;
function DsSyncCallback(func: DslCB): DslCB; stdcall external;
function DsReadyCallback(func: DslCB): DslCB; stdcall external;
function DsSync(id: integer; res: pointer): integer; stdcall external;
function DsReady(res: pointer): integer; stdcall external;
procedure DsFlush; stdcall external;
function DsSystemStatus: integer; stdcall external;
function DsQueueLen: integer; stdcall external;
function DsStatus: byte; stdcall external;
function DsShellOpen: integer; stdcall external;

function DsMix(vol: PDslATV): integer; stdcall external;
function DsGetSector(madr: pointer; size: integer): integer; stdcall external;
function DsGetSector2(madr: pointer; size: integer): integer; stdcall external;
function DsGetToc(loc: PDslLOC): integer; stdcall external;
procedure DsDataCallback(func: pointer); stdcall external;
function DsDataSync(mode: integer): integer; stdcall external;
function DsIntToPos(i: integer; p: PDslLOC): PDslLOC; stdcall external;
function DsPosToInt(p: PDslLOC): integer; stdcall external;
function DsSetDebug(level: integer ): integer; stdcall external;
function DsLastPos(p: PDslLOC): PDslLOC; stdcall external;
function DsLastCom: byte; stdcall external;

function DsComstr(com: byte): pchar; stdcall external;
function DsIntstr(intr: byte): pchar; stdcall external;

function DsStartReadySystem(func: DslRCB; count: integer): integer; stdcall external;
procedure DsEndReadySystem; stdcall external;
function DsReadySystemMode(mode: integer): integer; stdcall external;

function DsControlF(com: byte; param: pchar): integer; stdcall external;
function DsControl(com: byte; param: pchar; res: pointer): integer; stdcall external;
function DsControlB(com: byte; param: pchar; res: pointer): integer; stdcall external;

function DsRead(pos: PDslLOC; sectors: integer; buf: Plongint; mode: integer): integer; stdcall external;
function DsReadSync(res: pointer): integer; stdcall external;
function DsReadCallback(func: DslCB): DslCB; stdcall external;
procedure DsReadBreak; stdcall external;
function DsRead2(pos: PDslLOC; mode: integer): integer; stdcall external;

function DsSearchFile(fp: PDslFILE; name: pchar ): PDslFILE; stdcall external;
function DsReadFile(_file: pchar; addr: Plongint; nbyte: integer): integer; stdcall external;
//struct EXEC* DsReadExec( char* file ); stdcall external;
function DsPlay(mode: integer; tracks: Pinteger; offset: integer): integer; stdcall external;

procedure DsGetDiskType; stdcall external;

implementation

function btoi(b: longint): byte;
begin
	btoi:= b div 16 * 10 + b mod 16;
end;

function itob(i: byte): longint;
begin
	itob:= i div 10 * 16 + i mod 10;
end;

begin
end.