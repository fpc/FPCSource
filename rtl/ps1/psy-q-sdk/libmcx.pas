// libmcx.h
unit libmcx;
interface

const
	 McxFuncGetApl		= 1;
	 McxFuncExecApl		= 2;
	 McxFuncGetTime		= 3;
	 McxFuncGetMem		= 4;
	 McxFuncSetMem		= 5;
	 McxFuncShowTrans	= 6;
	 McxFuncHideTrans	= 7;
	 McxFuncCurrCtrl	= 8;
	 McxFuncSetLED		= 9;
	 McxFuncGetSerial	= 10;
	 McxFuncExecFlag	= 11;
	 McxFuncAllInfo		= 12;
	 McxFuncFlashAcs	= 13;
	 McxFuncReadDev		= 14;
	 McxFuncWriteDev	= 15;
	 McxFuncGetUIFS		= 16;
	 McxFuncSetUIFS		= 17;
	 McxFuncSetTime		= 18;
	 McxFuncCardType	= 19;


	 McxSyncRun			= 0;
	 McxSyncNone		= -1;
	 McxSyncFin			= 1;

	 McxErrSuccess		= 0;
	 McxErrNoCard		= 1;
	 McxErrInvalid		= 2;
	 McxErrNewCard		= 3;



procedure McxStartCom; external;
procedure McxStopCom; external;
function McxSync(mode: longint; cmd: plongint; res: plongint): longint; external;

function McxGetApl(port: longint; aplno: plongint): longint; external;
function McxExecApl(port: longint; aplno: longint; arg: longint): longint; external;
function McxGetTime(port: longint; time: pointer): longint; external;
function McxGetMem(port: longint; data: pointer; start, len: dword): longint; external;
function McxSetMem(port: longint; data: pointer; start, len: dword): longint; external;
function McxShowTrans(port, dir, timeout: longint): longint; external;
function McxHideTrans(port: longint): longint; external;
function McxCurrCtrl(port, sound, infred, led: longint): longint; external;
function McxFlashAcs(port, mode: longint): longint; external;
function McxGetSerial(port: longint; serial: pdword): longint; external;
function McxSetLED(port, mode: longint): longint; external;
function McxAllInfo(port: longint; state: pointer): longint; external;
function McxExecFlag(port, block, exec: longint): longint; external;
function McxReadDev(port, dev: longint; param, data: pointer): longint; external;
function McxWriteDev(port, dev: longint; param, data: pointer): longint; external;
function McxSetTime(port: longint; time: pointer): longint; external;
function McxGetUIFS(port: longint; data: pointer): longint; external;
function McxSetUIFS(port: longint; data: pointer): longint; external;
function McxCardType(port: longint): longint; external;


implementation
begin
end.