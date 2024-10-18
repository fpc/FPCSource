//	libpad.h
unit libpad;
interface
const
	 PadStateDiscon		= 0;
	 PadStateFindPad	= 1;
	 PadStateFindCTP1	= 2;
	 PadStateFindCTP2	= 3;
	 PadStateReqInfo	= 4;
	 PadStateExecCmd	= 5;
	 PadStateStable		= 6;

	 InfoModeCurID		= 1;
	 InfoModeCurExID	= 2;
	 InfoModeCurExOffs	= 3;
	 InfoModeIdTable	= 4;

	 InfoActFunc		= 1;
	 InfoActSub			= 2;
	 InfoActSize		= 3;
	 InfoActCurr		= 4;
	 InfoActSign		= 5;

	 PadMaxCurr			= 60;	// PS maximum current supply
	 PadCurrCTP1		= 10;	// SCPH-1150 biblator current


procedure PadInitDirect(pad1, pad2: pbyte); external;
procedure PadInitMtap(pad1, pad2: pbyte); external;
procedure PadInitGun(buf: pbyte; len: longint); external;
function PadChkVsync: longint; external;
procedure PadStartCom; external;
procedure PadStopCom; external;
function PadEnableCom(mode: dword): dword; external;
procedure PadEnableGun(mask: byte); external;
procedure PadRemoveGun; external;
function PadGetState(port: longint): longint; external;
function PadInfoMode(port, term, ofs: longint): longint; external;
function PadInfoAct(port, actno, term: longint): longint; external;
function PadInfoComb(port, listno, term: longint): longint; external;
function PadSetActAlign(port: longint; data: pointer): longint; external;
function PadSetMainMode(socket, offs, lock: longint): longint; external;
procedure PadSetAct(port: longint; data: pointer; len: longint); external;

implementation
begin
end.