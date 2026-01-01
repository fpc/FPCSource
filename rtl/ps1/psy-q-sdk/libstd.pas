{$MODE OBJFPC}
unit libstd;
interface

type
	Pdword = ^dword;
	Pshort = ^smallint;
	Pbyte = ^byte;
	Pinteger = ^integer;




// stddef.h
type
	size_t = dword;
	wchar_t = dword;
    u_char = byte;
	u_short = smallint;
	u_int = longint;
	u_long = longint;

const
	WEOF = $ffffffff;
	NULL = 0;




// errno.h
const
	EPERM 	= 1;		// Not owner
	ENOENT 	= 2;		// No such file or directory
	ESRCH 	= 3;		// No such process
	EINTR 	= 4;		// Interrupted system call
	EIO 	= 5;		// I/O error
	ENXIO 	= 6;		// No such device or address
	E2BIG 	= 7;		// Arg list too long
	ENOEXEC = 8;		// Exec format error
	EBADF 	= 9;		// Bad file number
	ECHILD 	= 10;		// No children
	EAGAIN 	= 11;		// No more processes
	ENOMEM 	= 12;		// Not enough core
	EACCES 	= 13;		// Permission denied
	EFAULT 	= 14;		// Bad address
	ENOTBLK = 15;		// Block device required
	EBUSY 	= 16;		// Mount device busy
	EEXIST 	= 17;		// File exists
	EXDEV 	= 18;		// Cross-device link
	ENODEV 	= 19;		// No such device
	ENOTDIR = 20;		// Not a directory
	EISDIR 	= 21;		// Is a directory
	EINVAL 	= 22;		// Invalid argument
	ENFILE	= 23;		// File table overflow
	EMFILE 	= 24;		// Too many open files
	ENOTTY 	= 25;		// Not a typewriter
	ETXTBSY = 26;		// Text file busy
	EFBIG 	= 27;		// File too large
	ENOSPC 	= 28;		// No space left on device
	ESPIPE 	= 29;		// Illegal seek
	EROFS 	= 30;		// Read-only file system
	EFORMAT = 31;		// Bad file format
	EPIPE 	= 32;		// Broken pipe

	// math software
	EDOM 	= 33;		// Argument too large
	ERANGE 	= 34;		// Result too large

	// non-blocking and interrupt i/o
	EWOULDBLOCK = 35;	// Operation would block
	EINPROGRESS	= 36;	// Operation now in progress
	EALREADY 	= 37;	// Operation already in progress

var
	errno : longint; external;


// asm.h
const
	R0 = 0;
	R1 = 1;
	R2 = 2;
	R3 = 3;
	R4 = 4;
	R5 = 5;
	R6 = 6;
	R7 = 7;
	R8 = 8;
	R9 = 9;
	R10 = 10;
	R11 = 11;
	R12 = 12;
	R13 = 13;
	R14 = 14;
	R15 = 15;
	R16 = 16;
	R17 = 17;
	R18 = 18;
	R19 = 19;
	R20 = 20;
	R21 = 21;
	R22 = 22;
	R23 = 23;
	R24 = 24;
	R25 = 25;
	R26 = 26;
	R27 = 27;
	R28 = 28;
	R29 = 29;
	R30 = 30;
	R31 = 31;

	zero = 0;
	AT = 1;		// assembler temp
	v0 = 2;		// return value
	v1 = 3;
	a0 = 4;		// argument registers
	a1 = 5;
	a2 = 6;
	a3 = 7;
	t0 = 8;		// caller saved
	t1 = 9;
	t2 = 10;
	t3 = 11;
	t4 = 12;
	t5 = 13;
	t6 = 14;
	t7 = 15;
	s0 = 16;	// callee saved
	s1 = 17;
	s2 = 18;
	s3 = 19;
	s4 = 20;
	s5 = 21;
	s6 = 22;
	s7 = 23;
	t8 = 24;	// code generator
	t9 = 25;
	k0 = 26;	// kernel temporary
	k1 = 27;
	gp = 28;	// global pointer
	sp = 29;	// stack pointer
	fp = 30; 	// frame pointer
	ra = 31;	// return address

	// register offset
	R_R0 = 0;
	R_R1 = 1;
	R_R2 = 2;
	R_R3 = 3;
	R_R4 = 4;
	R_R5 = 5;
	R_R6 = 6;
	R_R7 = 7;
	R_R8 = 8;
	R_R9 = 9;
	R_R10 =	10;
	R_R11 =	11;
	R_R12 = 12;
	R_R13 =	13;
	R_R14 = 14;
	R_R15 = 15;
	R_R16 = 16;
	R_R17 = 17;
	R_R18 = 18;
	R_R19 = 19;
	R_R20 = 20;
	R_R21 = 21;
	R_R22 = 22;
	R_R23 = 23;
	R_R24 = 24;
	R_R25 =	25;
	R_R26 =	26;
	R_R27 =	27;
	R_R28 = 28;
	R_R29 = 29;
	R_R30 =	30;
	R_R31 =	31;
	R_EPC =	32;
	R_MDHI=	33;
	R_MDLO=	34;
	R_SR  =	35;
	R_CAUSE= 36;
	NREGS =	40;


	// compiler defined bindings
	R_ZERO = R_R0;
	R_AT = R_R1;
	R_V0 = R_R2;
	R_V1 = R_R3;
	R_A0 = R_R4;
	R_A1 = R_R5;
	R_A2 = R_R6;
	R_A3 = R_R7;
	R_T0 = R_R8;
	R_T1 = R_R9;
	R_T2 = R_R10;
	R_T3 = R_R11;
	R_T4 = R_R12;
	R_T5 = R_R13;
	R_T6 = R_R14;
	R_T7 = R_R15;
	R_S0 = R_R16;
	R_S1 = R_R17;
	R_S2 = R_R18;
	R_S3 = R_R19;
	R_S4 = R_R20;
	R_S5 = R_R21;
	R_S6 = R_R22;
	R_S7 = R_R23;
	R_T8 = R_R24;
	R_T9 = R_R25;
	R_K0 = R_R26;
	R_K1 = R_R27;
	R_GP = R_R28;
	R_SP = R_R29;
	R_FP = R_R30;
	R_RA = R_R31;


// types.h
type
	ushort = smallint;	// sys III compat */
	uint = dword;		// sys V compat */
	ulong = dword;		// sys V compat */

	_quad = packed record
		 		val : array [0..1] of longint;
		 	end;
	quad = _quad;

	daddr_t = longint;
	caddr_t = pchar;
	qaddr_t = ^longint;	// should be typedef quad * qaddr_t; */
	ino_t = dword;
	swblk_t = longint;

	time_t = longint;
	dev_t = smallint;
	off_t = longint;
	uid_t = u_short;
	gid_t = u_short;

const
	NBBY = 8;		// number of bits in a byte */

type
	_physadr = 	packed record
				 	r : array [0..0] of longint;
				end;
	physadr = ^_physadr;

	label_t	= 	packed record
					val : array [0..11] of longint;
				end;


// kernel.h 	Rev. 3
const
	DescMask 	= $ff000000;
	DescTH 		= DescMask;
	DescHW   	= $f0000000;
	DescEV   	= $f1000000;
	DescRC   	= $f2000000;
	DescUEV   	= $f3000000;		// User event
	DescSW   	= $f4000000;		// BIOS

	HwVBLANK	= DescHW or $01;	// VBLANK
	HwGPU		= DescHW or $02;	// GPU
	HwCdRom		= DescHW or $03;	// CDROM Recorder
	HwDMAC		= DescHW or $04;	// DMA controller
	HwRTC0		= DescHW or $05;	// RTC0
	HwRTC1		= DescHW or $06;	// RTC1
	HwRTC2		= DescHW or $07;	// RTC2
	HwCNTL		= DescHW or $08;	// Controller
	HwSPU		= DescHW or $09;	// SPU
	HwPIO		= DescHW or $0a;	// PIO
	HwSIO		= DescHW or $0b;	// SIO

	HwCPU		= DescHW or $10;	// Exception
	HwCARD		= DescHW or $11;	// memory card
	HwCARD_0	= DescHW or $12;	// memory card
	HwCARD_1	= DescHW or $13;	// memory card
	SwCARD		= DescSW or $01;	// memory card
	SwMATH      = DescSW or $02;	// libmath

	RCntCNT0    = DescRC or $00;  	// display pixel
	RCntCNT1  	= DescRC or $01;  	// horizontal sync
	RCntCNT2  	= DescRC or $02;  	// one-eighth of system clock
	RCntCNT3  	= DescRC or $03;  	// vertical sync target value fixed to 1

	RCntMdINTR	= $1000;
	RCntMdNOINTR= $2000;
	RCntMdSC	= $0001;
	RCntMdSP	= $0000;
	RCntMdFR	= $0000;
	RCntMdGATE	= $0010;

	EvSpCZ		= $0001;			// counter becomes zero
	EvSpINT		= $0002;			// interrupted
	EvSpIOE		= $0004;			// end of i/o
	EvSpCLOSE	= $0008;			// file was closed
	EvSpACK		= $0010;			// command acknowledged
	EvSpCOMP	= $0020;			// command completed
	EvSpDR		= $0040;			// data ready
	EvSpDE		= $0080;			// data end
	EvSpTIMOUT  = $0100;      	    // time out
	EvSpUNKNOWN = $0200;       		// unknown command
	EvSpIOER	= $0400;			// end of read buffer
	EvSpIOEW	= $0800;			// end of write buffer
	EvSpTRAP    = $1000;    	    // general interrupt
	EvSpNEW		= $2000;			// new device
	EvSpSYSCALL	= $4000;			// system call instruction
	EvSpERROR	= $8000;			// error happened
	EvSpPERROR	= $8001;			// previous write error happened
	EvSpEDOM    = $0301;			// domain error in libmath
	EvSpERANGE  = $0302;			// range error in libmath

	EvMdINTR	= $1000;
	EvMdNOINTR	= $2000;

	EvStUNUSED  = $0000;
	EvStWAIT    = $1000;
	EvStACTIVE  = $2000;
	EvStALREADY = $4000;

	TcbMdRT		= $1000;			// reserved by system
	TcbMdPRI	= $2000;			// reserved by system

	TcbStUNUSED	= $1000;
	TcbStACTIVE	= $4000;


type

	EXEC = packed record
			 pc0 : dword; 									// Execution start address
			 gp0 : dword; 									// gp register initial value
			 t_addr : dword; 								// Starting address of initialized text section
			 t_size : dword; 								// Size of text section
			 d_addr : dword; 								// Starting address of initialized data section
			 d_size : dword; 								// Size of initialized data section
			 b_addr : dword; 								// Uninitialized data section start address
			 b_size : dword; 								// Uninitialized data section size
			 s_addr : dword; 								// Stack start address (specified by the user)
			 s_size : dword; 								// Stack size (specified by the user)
			 sp : dword; 									// Register shunt variable
			 fp : dword; 									// Register shunt variable
			 gp : dword; 									// Register shunt variable
			 ret : dword; 									// Register shunt variable
			 base : dword; 									// Register shunt variable
	end;
	PEXEC = ^EXEC;


	ToT = packed record
				head : pdword;
				size : longint;
	end;

	TCB = packed record
				status : longint;
				mode : longint;
				reg : array [0..NREGS - 1] of dword;		// never change the offset of this
				system : array [0..5] of longint;			// reserved by system
	end;
	PTCB = ^TCB;

	TCBH = packed record
				entry : PTCB;								// NULL
				flag : longint;
	end;

	EvCB = packed record
				desc : dword;
				status : longint;
				spec : longint;
				mode : longint;
				FHandler : pointer;
				system : array [0..1] of longint;			// reserved by system
	end;

	XF_HDR = packed record
				key : array [0..7] of char;
				text : dword;
				data : dword;
				_exec : EXEC;
				title : array [0..59] of char;				// "PlayStation(tm) Executable A1"
	end;


var
	SysToT : array [0..31] of ToT; external;

	SysClearRCnt : array [0..0] of longint; external;


type
	PDIRENTRY = ^DIRENTRY;
	DIRENTRY = packed record
					name : array [0..29] of char; 			// Filename
					attr : longint; 						// Attributes (dependent on file system)
					size : longint; 						// File size (in bytes)
					next : PDIRENTRY; 						// Pointer to next file entry (for user)
					system : array [0..7] of char; 			// Reserved by system
	end;



// abs.h
function abs(num: longint): longint; stdcall; external;



// Basic system types and major/minor device constructing/busting macros.
function major(x: longint): dword; // major part of a device
function minor(x: longint): longint; // minor part of a device
function makedev(x, y: longint): longint; // make a device number



// memory.h
function memcpy(dest: pointer; src: pointer; count: dword): pointer; stdcall external;
function memmove(dest: pointer; const src: pointer; count: dword): pointer; stdcall external;
function memcmp(const lhs: pointer; const rhs: pointer; count: dword): longint; stdcall external;
function memchr(const ptr: pointer; val: char; num: dword): pointer; stdcall external;
function memset(const dest: pointer; ch: byte; count: dword): pointer; stdcall external;
function bcopy(const src: pointer; dest: pointer; len: dword): pointer; stdcall external;
function bzero(s: pointer; n: dword): pointer; stdcall external;
function bcmp(const s1: pointer; const s2: pointer; n: dword): longint; stdcall external;



// malloc.h
procedure InitHeap(p: pdword; l: dword); stdcall external;				// this doesn't work - use InitHeap2
procedure free(p: pointer); stdcall external;							// this doesn't work - use free2
function malloc(l: size_t): pointer; stdcall external;					// this doesn't work - use malloc2
function calloc(nitems: size_t; l: size_t): pointer; stdcall external;	// this doesn't work - use calloc2
function realloc(p: pointer; l: size_t): pointer; stdcall external;		// this doesn't work - use realloc2
procedure InitHeap2(p: pdword; l: dword); stdcall external;
procedure free2(p: pointer); stdcall external;
function malloc2(l: size_t): pointer; stdcall external;
function calloc2(l1: size_t; l2: size_t): pointer; stdcall external;
function realloc2(p: pointer; l: size_t): pointer; stdcall external;
procedure InitHeap3(p: pdword; l: dword); stdcall external;
procedure free3(p: pointer); stdcall external;
function malloc3(l: size_t): pointer; stdcall external;
function calloc3(l1: size_t; l2: size_t): pointer; stdcall external;
function realloc3(p: pointer; l: size_t): pointer; stdcall external;


// strings.h
const
	LMAX = 256;

function strcat(dest: pchar; const src: pchar): pchar; stdcall external;
function strncat(dest: pchar; const src: pchar; num: size_t): pchar; stdcall; external;
function strcmp(str1: pchar; str2: pchar): longint; stdcall; external;
function strncmp(const str1: pchar; const str2: pchar; num: size_t): longint; stdcall; external;
function strcpy(dest: pchar; src: pchar): pchar; stdcall; external;
function strncpy(dest: pchar; const src: pchar; num: size_t): pchar; stdcall; external;
function strlen(s: pchar): longint; stdcall; external;
function index(const s: pchar; pos: byte): pchar; stdcall; external;
function rindex(const s: pchar; pos: byte): pchar; stdcall; external;

function strchr(const src: pchar; ch: char): pchar; stdcall; external;
function strrchr(const src: pchar; ch: char): pchar; stdcall; external;
function strpbrk(const str1: pchar; const str2: pchar): pchar; stdcall; external;
function strspn(const str1: pchar; const str2: pchar): longint; stdcall; external;
function strcspn(const str1: pchar; const str2: pchar): longint; stdcall; external;
function strtok(str1: pchar; const str2: pchar): pchar; stdcall; external;
function strstr(const str1: pchar; const str2: pchar): pchar; stdcall; external;

function strdup(p: pchar): pchar;


// rand.h
const
	RAND_MAX = 32767;

function rand: longint; stdcall; external;
procedure srand(x: longint); stdcall; external;



// stdlib.h
type
	Tcmp = function (const a: pointer; const b: pointer): longint;
	Pcmp = ^Tcmp;

function bsearch(const key: pointer; const ptr: pointer; count: size_t; size: size_t; comp: Pcmp ) : pointer; stdcall; external;
procedure exit; stdcall; external;



// stdio.h
const
	BUFSIZ = 1024;
	EOF = -1;
	SEEK_SET = 0;
	SEEK_CUR = 1;
	SEEK_END = 2;

function printf(const fmt: pchar): longint; varargs; stdcall; external;
function sprintf(buffer: pchar; const fmt: pchar): longint; varargs; stdcall; external;

function getc(s: longint): char; stdcall; external;
function getchar: char; stdcall; external;
function gets(s: pchar): pchar; stdcall; external;
procedure putc(ch: char; s: longint); stdcall; external;
procedure putchar(ch: char); stdcall; external;
procedure puts(const s: pchar); stdcall; external;



// convert.h
function atoi(const str: pchar): longint; stdcall; external;
function atol(const str: pchar): longint; stdcall; external;
function strtol(const str: pchar; endptr: pointer; base: longint): longint; stdcall; external;
function strtoul(const str: pchar; endptr: pointer; base: longint): dword; stdcall; external;
function labs(x: longint): longint; stdcall; external;



// limits.h
const
	CHAR_BIT = 8;
	SCHAR_MIN = -128;
	SCHAR_MAX = 127;
	UCHAR_MAX =	255;
	CHAR_MIN = SCHAR_MIN;
	CHAR_MAX = SCHAR_MAX;
	SHRT_MIN = -32768;
	SHRT_MAX = 32767;
	USHRT_MAX = 65535;
	INT_MIN = -2147483648;
	INT_MAX = 2147483647;
	UINT_MAX = 4294967295;
	LONG_MIN = -2147483648;
	LONG_MAX = 2147483647;
	ULONG_MAX =	4294967295;

	USI_MAX	= 4294967295;	// max decimal value of an "unsigned"
	WORD_BIT = 32;		// # of bits in a "word" or "int"

	MB_LEN_MAX = 4;



// qsort.h
procedure qsort(base: pointer; num: size_t; size: size_t; compar: pointer); stdcall; external;



// ctype.h
const
	_U = $01;	// upper case letter
	_L = $02;	// lower case letter
	_N = $04;	// digit
	_S = $08;	// space, tab, newline, vertical tab, formfeed, or carriage return
	_P = $10;	// punctuation character
	_C = $20;	// control character or delete
	_X = $40;   // hexadecimal digit [0-9a-fA-F]
	_B = $80;	// blank (space)

//extern	char	_ctype_[];

function toupper(ch: char): char; stdcall; external;
function tolower(ch: char): char; stdcall; external;

function isalpha(ch: char): boolean;
function isupper(ch: char): boolean;
function islower(ch: char): boolean;
function isdigit(ch: char): boolean;
function isxdigit(ch: char): boolean;
function isspace(ch: char): boolean;
function ispunct(ch: char): boolean;
function isalnum(ch: char): boolean;
function isprint(ch: char): boolean;
function isgraph(ch: char): boolean;
function iscntrl(ch: char): boolean;
function isascii(ch: char): boolean;
function toascii(ch: char): char;
//function _toupper(ch: char)	((unsigned char)(c)-'a'+'A')
//function _tolower(ch: char)	((unsigned char)(c)-'A'+'a')



// ioctl.h
function FIOCNBLOCK: longint;
function FIOCSCAN: longint;    // scan for input
// tty and sio
function TIOCRAW: longint; 	   // disable xon/xoff control
function TIOCFLUSH: longint;   // flush input buffer
function TIOCREOPEN: longint;  // reopen
function TIOCBAUD: longint;    // set baud rate
function TIOCEXIT: longint;    // console interrupt
function TIOCDTR: longint;     // control DTR line
function TIOCRTS: longint;     // control RTS line
function TIOCLEN: longint;     // stop<<16 | char
					           // stop 0:none 1:1 2:1.5 3:2bit
					           // char 0:5 1:6 2:7 3:8bit
function TIOCPARITY: longint;  // parity 0:none 1:e 3:o
function TIOSTATUS: longint;   // return status
function TIOERRRST: longint;   // error reset
function TIOEXIST: longint;    // exist test with DTR/CTS
function TIORLEN: longint;     // receive buffer length
// disk
function DIOFORMAT: longint;   // format




// fcntl.h
const
	FREAD	=	$0001;		// readable
	FWRITE	=	$0002;		// writable
	FNBLOCK	=	$0004;		// non-blocking reads
	FRLOCK	=	$0010;		// read locked (non-shared)
	FWLOCK	=	$0020;		// write locked (non-shared)
	FAPPEND	=	$0100;		// append on each write
	FCREAT	=	$0200;		// create if nonexistant
	FTRUNC	=	$0400;		// truncate to zero length
	FSCAN	=	$1000;		// scan type
	FRCOM	=	$2000;		// remote command entry
	FNBUF	=	$4000;		// no ring buf. and console interrupt
	FASYNC	=	$8000;		// asyncronous i/o



// file.h
const
	// Flag for open()
	O_RDONLY  = FREAD;
	O_WRONLY  = FWRITE;
	O_RDWR    = FREAD or FWRITE;
	O_CREAT   = FCREAT;  // open with file create
	O_NOBUF   = FNBUF;	// no device buffer and console interrupt
	O_NBLOCK  = FNBLOCK;	// non blocking mode
	O_NOWAIT  = FASYNC;	// asyncronous i/o



// fs.h
// device table
type

	Tdevice_table = packed record
		dt_string : pchar; 					// device name
		dt_type : longint;					// device "type"
		dt_bsize : longint;					// file system type
		dt_desc : pchar;					// device description
		dt_init : function(): plongint;		// device init routine
		dt_open : function(): plongint;		// device open routine
		dt_strategy : function(): plongint;	// device strategy routine, returns cnt
		dt_close : function(): plongint;	// device close routine
		dt_ioctl : function(): plongint;	// device ioctl routine
		dt_read : function(): plongint;		// fs read routine, returns count
		dt_write : function(): plongint;	// fs write routine, return count
		dt_delete : function(): plongint;	// file delete routine
		dt_undelete : function(): plongint;	// file delete routine
		dt_firstfile : function(): plongint;// directory search routine
		dt_nextfile : function(): plongint;	// directory search routine
		dt_format : function(): plongint;
		dt_cd : function(): plongint;
		dt_rename : function(): plongint;
		dt_remove : function(): plongint;
		dt_else : function(): plongint;
	end;
	Pdevice_table = ^Tdevice_table;

// device types
const
	DTTYPE_CHAR	 = $1;	// character device
	DTTYPE_CONS	 = $2;	// can be console
	DTTYPE_BLOCK = $4;	// block device
	DTTYPE_RAW	 = $8;	// raw device that uses fs switch
	DTTYPE_FS	 = $10;


// character device flags
	DB_RAW = $1;			// don't interpret special chars
	DB_STOPPED	= $2;		// stop output
	DB_BREAK = $4;			// cntl-c raise console interrupt

// character device buffer
	CBUFSIZE = 256;

type
	device_buf = packed record
		db_flags : longint;							// character device flags
		db_in : pchar;								// pts at next free char
		db_out : pchar;								// pts at next filled char
		db_buf : array [0..CBUFSIZE - 1] of char;	// circular buffer for input
	end;

// circular buffer functions */
{
	CIRC_EMPTY(x)	((x)->db_in == (x)->db_out)
	CIRC_FLUSH(x)	((x)->db_in = (x)->db_out = (x)->db_buf)
	CIRC_STOPPED(x)	((x)->db_flags & DB_STOPPED)
}

// io block
	iob = packed record
		i_flgs : longint;
		i_unit : longint;		// pseudo device unit
		i_ma : pchar;			// memory address of i/o buffer
		i_cc : dword;			// character count of transfer
		i_offset : dword;		// seek offset in file
		i_fstype : longint;		// file system type
		i_errno : longint;		// error # return
		i_dp : Pdevice_table;	// pointer into device_table
	    i_size : dword;
	    i_head : longint;
	    i_fd : longint;			// file descriptor
    end;

// Request codes
const
	READ	 = 1;
	WRITE	 = 2;

	NIOB	 = 16;	// max number of open files



// r3000.h
// Segment base addresses and sizes
	K0BASE = $80000000;
	K0SIZE = $20000000;
	K1BASE = $A0000000;
	K1SIZE = $20000000;
	K2BASE = $C0000000;
	K2SIZE = $20000000;

// Exception vectors
	UT_VEC = K0BASE;							// utlbmiss vector
	E_VEC  = K0BASE + $80;						// exception vector
	R_VEC  = K1BASE + $1fc00000;				// reset vector

// Address conversion macros
function K0_TO_K1(x: dword): dword;				// kseg0 to kseg1
function K1_TO_K0(x: dword): dword;				// kseg1 to kseg0
function K0_TO_PHYS(x: dword): dword;			// kseg0 to physical
function K1_TO_PHYS(x: dword): dword;			// kseg1 to physical
function PHYS_TO_K0(x: dword): dword;			// physical to kseg0
function PHYS_TO_K1(x: dword): dword;			// physical to kseg1

// Address predicates
function IS_KSEG0(x: dword): boolean;
function IS_KSEG1(x: dword): boolean;
//function IS_KSEG2(x: dword): boolean;
//function IS_KPTESEG(x: dword): boolean;
function IS_KUSEG(x: dword): boolean;


const
// Cache size constants
	MINCACHE =	4 * 1024;
	MAXCACHE =	64 * 1024;


// Status register
	SR_CUMASK =	$f0000000;					// coproc usable bits

	SR_CU3    =	$80000000;					// Coprocessor 3 usable
	SR_CU2    =	$40000000;					// Coprocessor 2 usable
	SR_CU1	  =	$20000000;					// Coprocessor 1 usable
	SR_CU0	  =	$10000000;					// Coprocessor 0 usable

	SR_BEV	  =	$00400000;					// use boot exception vectors

// Cache control bits
	SR_TS	  =	$00200000;					// TLB shutdown
	SR_PE	  =	$00100000;					// cache parity error
	SR_CM	  =	$00080000;					// cache miss
	SR_PZ	  =	$00040000;					// cache parity zero
	SR_SWC	  =	$00020000;					// swap cache
	SR_ISC	  =	$00010000;					// Isolate data cache

	SR_MM_MODE = $00010000;					// lwl/swl/etc become scache/etc


// Interrupt enable bits
// (NOTE: bits set to 1 enable the corresponding level interrupt)
	SR_IMASK	= $0000ff00;			// Interrupt mask
	SR_IMASK8	= $00000000;			// mask level 8
	SR_IMASK7	= $00008000;			// mask level 7
	SR_IMASK6	= $0000c000;			// mask level 6
	SR_IMASK5	= $0000e000;			// mask level 5
	SR_IMASK4	= $0000f000;			// mask level 4
	SR_IMASK3	= $0000f800;			// mask level 3
	SR_IMASK2	= $0000fc00;			// mask level 2
	SR_IMASK1	= $0000fe00;			// mask level 1
	SR_IMASK0	= $0000ff00;			// mask level 0

	SR_IBIT8	= $00008000;			// bit level 8
	SR_IBIT7	= $00004000;			// bit level 7
	SR_IBIT6	= $00002000;			// bit level 6
	SR_IBIT5	= $00001000;			// bit level 5
	SR_IBIT4	= $00000800;			// bit level 4
	SR_IBIT3	= $00000400;			// bit level 3
	SR_IBIT2	= $00000200;			// bit level 2
	SR_IBIT1	= $00000100;			// bit level 1

	SR_KUO		= $00000020;			// old kernel/user, 0 => k, 1 => u
	SR_IEO		= $00000010;			// old interrupt enable, 1 => enable
	SR_KUP		= $00000008;			// prev kernel/user, 0 => k, 1 => u
	SR_IEP		= $00000004;			// prev interrupt enable, 1 => enable
	SR_KUC		= $00000002;			// cur kernel/user, 0 => k, 1 => u
	SR_IEC		= $00000001;			// cur interrupt enable, 1 => enable

	SR_IMASKSHIFT	= 8;

	SR_FMT		= '\20\40BD\26TS\25PE\24CM\23PZ\22SwC\21IsC\20IM7\17IM6\16IM5\15IM4\14IM3\13IM2\12IM1\11IM0\6KUo\5IEo\4KUp\3IEp\2KUc\1IEc';

// Cause Register

	CAUSE_BD		= $80000000;		// Branch delay slot */
	CAUSE_CEMASK	= $30000000;		// coprocessor error */
	CAUSE_CESHIFT	= 28;

/// Interrupt pending bits
	CAUSE_IP8	= $00008000;			// External level 8 pending
	CAUSE_IP7	= $00004000;			// External level 7 pending
	CAUSE_IP6	= $00002000;			// External level 6 pending
	CAUSE_IP5	= $00001000;			// External level 5 pending
	CAUSE_IP4	= $00000800;			// External level 4 pending
	CAUSE_IP3	= $00000400;			// External level 3 pending
	CAUSE_SW2	= $00000200;			// Software level 2 pending
	CAUSE_SW1	= $00000100;			// Software level 1 pending

	CAUSE_IPMASK	= $0000FF00;		// Pending interrupt mask
	CAUSE_IPSHIFT	= 8;

	CAUSE_EXCMASK	= $0000003C;		// Cause code bits
	CAUSE_EXCSHIFT	= 2;

	CAUSE_FMT		= '\20\40BD\36CE1\35CE0\20IP8\17IP7\16IP6\15IP5\14IP4\13IP3\12SW2\11SW1\1INT';


// Cause register exception codes
function EXC_CODE(x: dword): dword;

const
// Hardware exception codes
	EXC_INT		= 0 shl 2;			// interrupt
	EXC_MOD		= 1 shl 2;			// TLB mod
	EXC_RMISS	= 2 shl 2;			// Read TLB Miss
	EXC_WMISS	= 3 shl 2;			// Write TLB Miss
	EXC_RADE	= 4 shl 2;			// Read Address Error
	EXC_WADE	= 5 shl 2;			// Write Address Error
	EXC_IBE		= 6 shl 2;			// Instruction Bus Error
	EXC_DBE		= 7 shl 2;			// Data Bus Error
	EXC_SYSCALL	= 8 shl 2;			// SYSCALL
	EXC_BREAK	= 9 shl 2;			// BREAKpoint
	EXC_II		= 10 shl 2;			// Illegal Instruction
	EXC_CPU		= 11 shl 2;			// CoProcessor Unusable
	EXC_OV		= 12 shl 2;			// OVerflow

// software exception codes
	SEXC_SEGV	= 16 shl 2;			// Software detected seg viol
	SEXC_RESCHED= 17 shl 2;			// resched request
	SEXC_PAGEIN	= 18 shl 2;			// page-in request
	SEXC_CPU	= 19 shl 2;			// coprocessor unusable


// Coprocessor 0 registers
	C0_INX		= 0; 				// tlb index
	C0_RAND 	= 1;				// tlb random
	C0_TLBLO 	= 2;				// tlb entry low
	C0_CTXT 	= 4;				// tlb context

	C0_PIDMASK	= 6;				// Mips2

	C0_BADVADDR = 8;			// bad virtual address

	C0_TLBHI 	= 10;				// tlb entry hi
	C0_PID		= 10;				// Mips2

	C0_SR 		= 12;					// status register
	C0_CAUSE 	= 13;				// exception cause
	C0_EPC 		= 14;				// exception pc
	C0_PRID 	= 15;				// revision identifier
	C0_ERREG 	= 16;				// Mips2

// Coprocessor 0 operations
	C0_READI  	= $1;		// read ITLB entry addressed by C0_INDEX
	C0_WRITEI 	= $2;		// write ITLB entry addressed by C0_INDEX
	C0_WRITER 	= $6;		// write ITLB entry addressed by C0_RAND
	C0_PROBE  	= $8;		// probe for ITLB entry addressed by TLBHI
	C0_RFE	  	= $10;		// restore for exception

// Flags for the nofault handler. 0 means no fault is expected.
	NF_BADADDR	= 1;			// badaddr, wbadaddr
	NF_COPYIO	= 2;			// copyin, copyout
	NF_ADDUPC	= 3;			// addupc
	NF_FSUMEM	= 4;			// fubyte, subyte, fuword, suword
	NF_USERACC	= 5;			// useracc
	NF_SOFTFP	= 6;			// softfp
	NF_REVID	= 7;			// revision ids
	NF_NENTRIES	= 8;

// TLB size constants
	TLBWIREDBASE    = 0;               					// WAG for now
	NWIREDENTRIES   = 8;               					// WAG for now
	TLBRANDOMBASE   = NWIREDENTRIES;
	NTLBENTRIES     = 64;
	NRANDOMENTRIES  = (NTLBENTRIES - NWIREDENTRIES);


	TLBRAND_RANDMASK   = $00003f00;
	TLBRAND_RANDSHIFT  = 8;


// Chip interrupt vector
	NC0VECS	= 8;



// extern int (*c0vec_tbl[])();
var
	c0vec_tbl : array of function: longint; external;


const
	BRK_KERNEL 		 = $f1;
	EXCEPT_NORM      = 1;
	EXCEPT_UTLB      = 2;
	EXCEPT_BRKPT   	 = 3;
	EXCEPT_DB    	 = 4;
	EXCEPT_GDB    	 = 4;
	EXCEPT_INT    	 = 9;
	EXCEPT_ELSE    	 = $ff;



// setjmp.h
// simple non-local-jump for single task environment
// jmp_buf indices
const
	JB_PC = 0;
	JB_SP = 1;
	JB_FP = 2;
	JB_S0 = 3;
	JB_S1 = 4;
	JB_S2 = 5;
	JB_S3 = 6;
	JB_S4 = 7;
	JB_S5 = 8;
	JB_S6 =	9;
	JB_S7 =	10;
	JB_GP =	11;
	JB_SIZE = 12;

type
	jmp_buf = array [0..JB_SIZE - 1] of longint;

function setjmp(buf: jmp_buf): longint; external;
procedure longjmp(buf: jmp_buf; i: longint); external;



// libsio.h
// status bits
const
	SR_IRQ		= $200;
	SR_CTS		= $100;
	SR_DSR		= $80;
	SR_FE		= $20;
	SR_OE		= $10;
	SR_PERROR	= $8;
	SR_TXU		= $4;
	SR_RXRDY	= $2;
	SR_TXRDY	= $1;

	SIO_CTS		= $100;
	SIO_DSR		= $80;
	SIO_FE		= $20;
	SIO_OE		= $10;
	SIO_PERROR	= $8;
	SIO_TXU		= $4;
	SIO_RXRDY	= $2;
	SIO_TXRDY	= $1;


	// control bits
	CR_DSRIEN	= $1000;
	CR_RXIEN	= $800;
	CR_TXIEN	= $400;
	CR_BUFSZ_1	= $0;
	CR_BUFSZ_2	= $100;
	CR_BUFSZ_4	= $200;
	CR_BUFSZ_8	= $300;
	CR_INTRST	= $40;
	CR_RTS		= $20;
	CR_ERRRST	= $10;
	CR_BRK		= $8;
	CR_RXEN		= $4;
	CR_DTR		= $2;
	CR_TXEN		= $1;

	SIO_BIT_DTR	= CR_DTR;
	SIO_BIT_RTS	= CR_RTS;

	// mode bits
	MR_SB_00	= $0;
	MR_SB_01	= $40;
	MR_SB_10	= $80;
	MR_SB_11	= $C0;
	MR_P_EVEN	= $20;
	MR_PEN		= $10;
	MR_CHLEN_5	= $0;
	MR_CHLEN_6	= $4;
	MR_CHLEN_7	= $8;
	MR_CHLEN_8	= $C;
	MR_BR_1		= $1;
	MR_BR_16	= $2;
	MR_BR_64	= $3;

type
		Sio1CallbackFunction = function: pointer;

function AddSIO(baud: longint): longint; external;
function DelSIO: longint; external;
function _sio_control(cmd, arg, param: dword): longint; external;
function Sio1Callback(func: Sio1CallbackFunction): longint; external;


generic function __va_rounded_size<T>: SizeInt;
generic procedure va_start<T>(out AP: Pointer; var LastArg: T);
procedure va_end(out AP: Pointer);
generic function va_arg<T>(var AP: Pointer):T;


implementation


generic function __va_rounded_size<T>: SizeInt;inline;
begin
  Result := ((sizeof(T) + sizeof (longint) - 1) div sizeof (longint)) * sizeof (longint);
end;


generic procedure va_start<T>(out AP: Pointer; var LastArg: T);inline;
begin
  AP := Pointer(@LastArg) + specialize __va_rounded_size<T>;
end;


procedure va_end(out AP: Pointer);inline;
begin
  AP := nil;
end;


generic function va_arg<T>(var AP: Pointer):T;inline;
type
  PT=^T;
begin
  Result := PT(AP)^;
  Inc(AP, specialize __va_rounded_size<T>);
end;


function strdup(p: pchar): pchar;
begin
 	strdup:= malloc2(strlen(p) + 1);
 	strcpy(strdup, p);
end;


function isalpha(ch: char): boolean;
begin
	isalpha:= boolean(byte(ch) and (_U or _L));
end;


function isupper(ch: char): boolean;
begin
	isupper:= boolean(byte(ch) and _U);
end;


function islower(ch: char): boolean;
begin
	islower:= boolean(byte(ch) and _L);
end;


function isdigit(ch: char): boolean;
begin
	isdigit:= boolean(byte(ch) and _N);
end;


function isxdigit(ch: char): boolean;
begin
	isxdigit:= boolean(byte(ch) and (_X or _N));
end;


function isspace(ch: char): boolean;
begin
	isspace:= boolean(byte(ch) and _S);
end;


function ispunct(ch: char): boolean;
begin
	ispunct:= boolean(byte(ch) and _P);
end;


function isalnum(ch: char): boolean;
begin
	isalnum:= boolean(byte(ch) and (_U or _L or _N));
end;


function isprint(ch: char): boolean;
begin
	isprint:= boolean(byte(ch) and (_P or _U or _L or _N or _B));
end;


function isgraph(ch: char): boolean;
begin
	isgraph:= boolean(byte(ch) and (_P or _U or _L or _N));
end;


function iscntrl(ch: char): boolean;
begin
	iscntrl:= boolean(byte(ch) and _C);
end;


function isascii(ch: char): boolean;
begin
	isascii:= boolean(byte(ch) <= $7f);
end;


function toascii(ch: char): char;
begin
	toascii:= char(byte(ch) and $7f);
end;


function major(x: longint): dword;
begin
	major:= dword((x shr 8) and 0377);
end;


function minor(x: longint): longint;
begin
	minor:=	x and 0377;
end;


function makedev(x, y: longint): longint;
begin
	makedev:= ((x shl 8) or y);
end;


function FIOCNBLOCK: longint;
begin
	FIOCNBLOCK:= (dword('f') shl 8) or 1;
end;


function FIOCSCAN: longint;
begin
	FIOCSCAN:= (dword('f') shl 8) or 2;
end;


function TIOCRAW: longint;
begin
	TIOCRAW:= (dword('t') shl 8) or 1;
end;


function TIOCFLUSH: longint;
begin
	TIOCFLUSH:= (dword('t') shl 8) or 2;
end;


function TIOCREOPEN: longint;
begin
	TIOCREOPEN:= (dword('t') shl 8) or 3;
end;


function TIOCBAUD: longint;
begin
	TIOCBAUD:= (dword('t') shl 8) or 4;
end;


function TIOCEXIT: longint;
begin
	TIOCEXIT:= (dword('t') shl 8) or 5;
end;


function TIOCDTR: longint;
begin
	TIOCDTR:= (dword('t') shl 8) or 6;
end;


function TIOCRTS: longint;
begin
	TIOCRTS:= (dword('t') shl 8) or 7;
end;


function TIOCLEN: longint;
begin
	TIOCLEN:= (dword('t') shl 8) or 8;
end;


function TIOCPARITY: longint;
begin
	TIOCPARITY:= (dword('t') shl 8) or 9;
end;


function TIOSTATUS: longint;
begin
	TIOSTATUS:= (dword('t') shl 8) or 10;
end;


function TIOERRRST: longint;
begin
	TIOERRRST:= (dword('t') shl 8) or 11;
end;


function TIOEXIST: longint;
begin
	TIOEXIST:= (dword('t') shl 8) or 12;
end;


function TIORLEN: longint;
begin
	TIORLEN:= (dword('t') shl 8) or 13;
end;


function DIOFORMAT: longint;
begin
	DIOFORMAT:= (dword('d') shl 8) or 1;
end;


// Address conversion macros
function K0_TO_K1(x: dword): dword;
begin
	result:= x or $A0000000;
end;

function K1_TO_K0(x: dword): dword;
begin
	result:= x and $9FFFFFFF;
end;

function K0_TO_PHYS(x: dword): dword;
begin
	result:= x and $1FFFFFFF;
end;

function K1_TO_PHYS(x: dword): dword;
begin
	result:= x and $1FFFFFFF;
end;

function PHYS_TO_K0(x: dword): dword;
begin
	result:= x or $80000000;
end;

function PHYS_TO_K1(x: dword): dword;
begin
	result:= x or $A0000000;
end;

// Address predicates
function IS_KSEG0(x: dword): boolean;
begin
	result:= ((x >= K0BASE) and (x < K1BASE));
end;

function IS_KSEG1(x: dword): boolean;
begin
	result:= ((x >= K1BASE) and (x < K2BASE));
end;

{
function IS_KSEG2(x: dword): boolean;
begin
	result:= ((x >= K2BASE) and (x < KPTEBASE));
end;

function IS_KPTESEG(x: dword): boolean;
begin
	result:= (x >= KPTEBASE);
end;
}

function IS_KUSEG(x: dword): boolean;
begin
	result:= (x < K0BASE);
end;

function EXC_CODE(x: dword): dword;
begin
	result:= x shl 2;
end;

begin
end.