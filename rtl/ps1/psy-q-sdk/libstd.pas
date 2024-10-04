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
	HwCdRom		= DescHW or $03;	// CDROM Decorder
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
	EvSpERROR	= $8000;			// error happned
	EvSpPERROR	= $8001;			// previous write error happned
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

	NREGS		= 40;

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
	
	




//
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
procedure InitHeap(p: pdword; l: dword); stdcall external;
procedure free(p: pointer); stdcall external;
function malloc(l: size_t): pointer; stdcall external;
function calloc(nitems: size_t; l: size_t): pointer; stdcall external;
function realloc(p: pointer; l: size_t): pointer; stdcall external;
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




// abs.h
function abs(x: longint): longint; stdcall; external;




// convert.h
function atoi(const str: pchar): longint; stdcall; external;
function atol(const str: pchar): longint; stdcall; external;
function strtol(const str: pchar; endptr: pointer; base: longint): longint; stdcall; external;
function strtoul(const str: pchar; endptr: pointer; base: longint): dword; stdcall; external;
function labs(x: longint): longint; stdcall; external;




//limits.h
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
function TIOCEXIT: longint;    // console interrup
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






implementation


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



begin
end.