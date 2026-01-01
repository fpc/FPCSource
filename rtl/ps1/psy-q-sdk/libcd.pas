{
 *  (C) Copyright 1993/1994 Sony Computer Entertainment ,Tokyo,Japan.
 *                      All Rights Reserved
 *
 *              libcd.h: CD-ROM sub system hendler
 *
 * CD-ROM Primitive Command list:
 *
 *	Symbol		type	Contents
 *	------------------------------------------------------
 *	CdlNop		B	NOP
 *	CdlSetloc	B	Set position
 *	CdlPlay		B	CD-DA Play
 *	CdlForward	B	Forward
 *	CdlBackward	B	Backward
 *	CdlReadN	B	Read with retry
 *	CdlStandby	N	Standby
 *	CdlStop		N	Stop
 *	CdlPause	N	Pause
 *	CdlMute		B	Mute on
 *	CdlDemute	B	Mute off
 *	CdlSetfilter	B	Set SubHeader filter
 *	CdlSetmode	B	Set mode
 *	CdlGetlocL	B	Get logical position
 *	CdlGetlocP	B	Get physical position
 *	CdlSeekL	N	Logical Seek
 *	CdlSeekP	N	Physical Seek
 *	CdlReadS	B	Read without retry
 *	------------------------------------------------------
 *			B: Blocking, N: Non-Blocking operation
 *
 *
 *	Symbol		arg		result
 *	--------------------------------------------------------------
 *	CdlNop		-		status
 *	CdlSetloc	min,sec,sector	status
 *	CdlPlay       	-		status
 *	CdlForward	-		status
 *	CdlBackward	-		status
 *	CdlReadN	-		status
 *	CdlStandby	-		status
 *	CdlStop		-		status
 *	CdlPause	-		status
 *	CdlMute		-		status
 *	CdlDemute	-		status
 *	CdlSetfilter	file,chan	status
 *	CdlSetmode	mode		status
 *	CdlGetlocL	-		min,sec,sector,mode,file, chan
 *	CdlGetlocP	-		track,index,min,sec,frame,
 *							amin,asec,aframe
 *	CdlSeekL	-		status
 *	CdlSeekP	-		status
 *	CdlReadS	-		status
 *	--------------------------------------------------------------
}

unit libcd;
interface
uses libstd;

const
// CD-ROM Mode (used int CdlSetmode)
	CdlModeStream  	= $100;    	// Normal Streaming
	CdlModeStream2 	= $120;    	// SUB HEADER information includes
	CdlModeSpeed	= $80;		// 0: normal speed	1: double speed
	CdlModeRT 		= $40;		// 0: ADPCM off		1: ADPCM on
	CdlModeSize1 	= $20;		// 0: 2048 byte		1: 2340byte
	CdlModeSize0 	= $10;		// 0: -				1: 2328byte
	CdlModeSF 		= $08;		// 0: Channel off	1: Channel on
	CdlModeRept 	= $04;		// 0: Report off	1: Report on
	CdlModeAP 		= $02;		// 0: AutoPause off	1: AutoPause on
	CdlModeDA 		= $01;		// 0: CD-DA off		1: CD-DA on


// Status Contents
	CdlStatPlay		= $80;	// playing CD-DA
	CdlStatSeek		= $40;	// seeking
	CdlStatRead		= $20;	// reading data sectors
	CdlStatShellOpen= $10;	// once shell open
	CdlStatSeekError= $04;	// seek error detected
	CdlStatStandby 	= $02;	// spindle motor rotating
	CdlStatError 	= $01;	// command error detected

// Macros for CdGetDiskType()
	CdlStatNoDisk	= 0;
	CdlOtherFormat	= 1;
	CdlCdromFormat	= 2;

// CD-ROM Primitive Commands
	CdlNop			= $01;
	CdlSetloc		= $02;
	CdlPlay			= $03;
	CdlForward		= $04;
	CdlBackward		= $05;
	CdlReadN		= $06;
	CdlStandby		= $07;
	CdlStop			= $08;
	CdlPause		= $09;
	CdlMute			= $0b;
	CdlDemute		= $0c;
	CdlSetfilter	= $0d;
	CdlSetmode		= $0e;
	CdlGetparam		= $0f;
	CdlGetlocL		= $10;
	CdlGetlocP		= $11;
	CdlGetTN		= $13;
	CdlGetTD		= $14;
	CdlSeekL		= $15;
	CdlSeekP		= $16;
	CdlReadS		= $1B;

// Interrupts
	CdlNoIntr		= $00;	// No interrupt
	CdlDataReady	= $01;	// Data Ready
	CdlComplete		= $02;	// Command Complete
	CdlAcknowledge	= $03;	// Acknowledge (reserved)
	CdlDataEnd		= $04;	// End of Data Detected
	CdlDiskError	= $05;	// Error Detected


// Library Macros
function btoi(b: longint): byte;	// BCD to u_char
function itob(i: byte): longint;	// u_char to BCD

procedure CdSeekL(p: dword);
procedure CdSeekP(p: dword);
procedure CdStandby;
procedure CdPause;
procedure CdStop;
procedure CdMute;
procedure CdDeMute;
procedure CdForward;
procedure CdBackward;


// Position
const
	CdlMAXTOC 	= 100;

// Callback
type
	CdlCB = procedure (b: byte; p: pchar);

// Location
type
	CdlLOC = packed record
		 		minute : byte;		// minute (BCD)
				second : byte;		// second (BCD)
				sector : byte;		// sector (BCD)
				track : byte;		// track (void)
			 end;
	PCdlLOC = ^CdlLOC;

// ADPCM Filter
	CdlFILTER = packed record
					_file : byte;		// file ID (always 1)
					chan : byte;		// channel ID
					pad : u_short;
				end;

// Attenuator
	CdlATV = packed record
				val0 : byte;		// volume for CD(L) -> SPU (L)
				val1 : byte;		// volume for CD(L) -> SPU (R)
				val2 : byte;		// volume for CD(R) -> SPU (L)
				val3 : byte;		// volume for CD(R) -> SPU (R)
			 end;
	PCdlATV = ^CdlATV;

// Low Level File System for CdSearchFile()
const
	CdlMAXFILE 	=	64;		// max number of files in a directory
	CdlMAXDIR 	= 	128;	// max number of total directories
	CdlMAXLEVEL = 	8;		// max levels of directories

type
	CdlFILE = packed record
				pos : CdlLOC;					// file location
				size : u_long;					// file size
				name : array [0..15] of char;	// file name (body) */
			  end;
	PCdlFILE = ^CdlFILE;

// Streaming Structures
type
	StHEADER = packed record	// CD-ROM STR structure
			    id : u_short;
			    _type : u_short;
			    secCount : u_short;
			    nSectors : u_short;
			    frameCount : u_long;
			    frameSize : u_long;

			    width : u_short;
			    height : u_short;
			    dummy1 : u_long;
			    dummy2 : u_long;
			    loc : CdlLOC;
			   end;
const
	StFREE 		=	$0000;
	StREWIND 	=   $0001;
	StCOMPLETE 	=   $0002;
	StBUSY 		=   $0003;
	StLOCK 		=   $0004;

	EDC 		= 	0;
	SECTOR_SIZE = 	512;	// Sector Size (word)
	HEADER_SIZE = 	8;		// Header Size (word)

	StSTATUS        = $00;
	StVER           = $00;
	StTYPE          = $01;
	StSECTOR_OFFSET = $02;
	StSECTOR_SIZE   = $03;
	StFRAME_NO      = $04;
	StFRAME_SIZE    = $06;

	StMOVIE_WIDTH   = $08;
	StMOVIE_HEIGHT  = $09;


// Prototypes for Streaming
procedure StSetRing(ring_addr: Pdword; ring_size:Pdword); stdcall external;
procedure StClearRing; stdcall external;
procedure StUnSetRing; stdcall external;
procedure StSetStream(mode: u_long; start_frame, end_frame: u_long; func1, func2: pointer); stdcall external;
procedure StSetEmulate(addr: Pdword; mode: u_long; start_frame, end_frame: u_long; func1, func2: pointer); stdcall external;
function StFreeRing(base: Pdword): u_long; stdcall external;
function StGetNext(addr: pointer; header: pointer): u_long; stdcall external;
function StGetNextS(addr: pointer; header: pointer): u_long; stdcall external;
function StNextStatus(addr: pointer; header: pointer): u_short; stdcall external;
procedure StRingStatus(free_sectors: Pshort; over_sectors: Pshort); stdcall external;
procedure StSetMask(mask: u_long; start, _end: u_long); stdcall external;
procedure StCdInterrupt; stdcall external;
function StGetBackloc(loc: PCdlLOC): longint; stdcall external;
function StSetChannel(channel: u_long): longint; stdcall external;

// Prototypes
procedure CdFlush; stdcall external;
function CdSearchFile(fp: PCdlFILE; name: pchar): PCdlFILE; stdcall external;
function CdIntToPos(i: longint; p: PCdlLOC): PCdlLOC; stdcall external;
function CdComstr(com: byte): pchar; stdcall external;
function CdIntstr(intr: byte): pchar; stdcall external;
function CdControl(com: byte; param: Pbyte; result: Pbyte): longint; stdcall external;
function CdControlB(com: byte; param: Pbyte; result: Pbyte): longint; stdcall external;
function CdControlF(com: byte; param: Pbyte): longint; stdcall external;
function CdGetSector(madr: pointer; size: longint): longint; stdcall external;
function CdGetSector2(madr: pointer; size: longint): longint; stdcall external;
function CdDataSync(mode: longint): longint; stdcall external;
function CdGetToc(loc: PCdlLOC): longint; stdcall external;
function CdPlay(mode: longint; track: Plongint; offset: longint): longint; stdcall external;
function CdMix(vol: PCdlATV): longint; stdcall external;
function CdPosToInt(p: PCdlLOC): longint; stdcall external;
function CdRead(sectors: longint; buf: pointer; mode: longint): longint; stdcall external;
function CdRead2(mode: longint): longint; stdcall external;
function CdReadFile(_file: pchar; addr: pointer; nbyte: longint): longint; stdcall external;
function CdReadSync(mode: longint; res: pointer): longint; stdcall external;
function CdReady(mode: longint; res: pointer): longint; stdcall external;
function CdSetDebug(level: longint): longint; stdcall external;
procedure CdSync(mode: longint; res: pointer); stdcall external;
function CdDataCallback(func: pointer): pointer; stdcall external;
function CdReadCallback(func: CdlCB): CdlCB; stdcall external;
function CdReadyCallback(func: CdlCB): CdlCB; stdcall external;
function CdSyncCallback(func: CdlCB): CdlCB; stdcall external;
function CdInit: longint; stdcall external;
function CdReset(mode: longint): longint; stdcall external;
function CdStatus: longint; stdcall external;
function CdLastCom: longint; stdcall external;
function CdLastPos: PCdlLOC; stdcall external;
function CdMode: longint; stdcall external;
function CdDiskReady(mode: longint): longint; stdcall external;
function CdGetDiskType: longint; stdcall external;
// CdReadExec(char *file): EXEC;
procedure CdReadBreak; stdcall external;

implementation


function btoi(b: longint): byte;
begin
	btoi:= b div 16 * 10 + b mod 16;
end;

function itob(i: byte): longint;
begin
	itob:= i div 10 * 16 + i mod 10;
end;

procedure CdSeekL(p: dword);
begin
	CdControl(CdlSeekL, @p, nil);
end;


procedure CdSeekP(p: dword);
begin
	CdControl(CdlSeekP, @p, nil);
end;

procedure CdStandby;
begin
	CdControl(CdlStandby, nil, nil);
end;

procedure CdPause;
begin
	CdControl(CdlPause, nil, nil);
end;

procedure CdStop;
begin
	CdControl(CdlStop, nil, nil);
end;

procedure CdMute;
begin
	CdControl(CdlMute, nil, nil);
end;

procedure CdDeMute;
begin
	CdControl(CdlDemute, nil, nil);
end;

procedure CdForward;
begin
	CdControl(CdlForward, nil, nil);
end;

procedure CdBackward;
begin
	CdControl(CdlBackward, nil, nil);
end;

begin
end.