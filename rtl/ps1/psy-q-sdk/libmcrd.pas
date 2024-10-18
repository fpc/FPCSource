// libmcrd.h	Rev. 4.2
unit libmcrd;
interface
uses libstd;
type
	MemCB = function(cmds, rslt: longint): pointer;
const
 		McFuncExist			= 1;
 		McFuncAccept		= 2;
 		McFuncReadFile		= 3;
 		McFuncWriteFile		= 4;
 		McFuncReadData		= 5;
 		McFuncWriteData		= 6;

		McErrNone			= 0;
		McErrCardNotExist	= 1;
		McErrCardInvalid	= 2;
		McErrNewCard		= 3;
		McErrNotFormat		= 4;
		McErrFileNotExist	= 5;
		McErrAlreadyExist	= 6;
		McErrBlockFull		= 7;
		McErrExtend			= $8000;


procedure MemCardInit(val: longint); external;
procedure MemCardEnd; external;
procedure MemCardStart; external;
procedure MemCardStop; external;
function MemCardExist(chan: longint): longint; external;
function MemCardAccept(chan: longint): longint; external;
function MemCardOpen(chan: longint; filename: pchar; flag: longint): longint; external;
procedure MemCardClose; external;
function MemCardReadData(adrs: pdword; ofs: longint; bytes: longint): longint; external;
function MemCardReadFile(chan: longint; filename: pchar; adrs: pdword; ofs: longint; bytes: longint): longint; external;
function MemCardWriteData(adrs: pdword; ofs: longint; bytes: longint): longint; external;
function MemCardWriteFile(chan: longint; filename: pchar; adrs: pdword; ofs: longint; bytes: longint): longint; external;
function MemCardCreateFile(chan: longint; filename: pchar; blocks: longint): longint; external;
function MemCardDeleteFile(chan: longint; filename: pchar): longint; external;
function MemCardFormat(chan: longint): longint; external;
function MemCardUnformat(chan: longint): longint; external;
function MemCardSync(mode: longint; cmds: plongint; rslt: plongint): longint; external;
function MemCardCallback(func: MemCB): MemCB; external;
function MemCardGetDirentry(chan: longint; name: pchar; dir: PDIRENTRY; files: plongint; ofs: longint; max: longint): longint; external;

implementation
begin
end.