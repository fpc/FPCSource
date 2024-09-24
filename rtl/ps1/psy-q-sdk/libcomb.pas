unit libcomb; 
interface
const
// Status bits
	COMB_CTS		= $100;
	COMB_DSR		= $80;
	COMB_FE			= $20;
	COMB_OE			= $10;
	COMB_PERROR		= $8;
	COMB_TXU		= $4;
	COMB_RXRDY		= $2;
	COMB_TXRDY		= $1;

// Control bits
	COMB_BIT_DTR	= $1;
	COMB_BIT_RTS	= $2;

// Macros
function CombSioStatus: longint;					// Return serial controller status
function CombControlStatus: longint;				// Return control line status
function CombGetMode: longint;						// Return communication mode
function CombGetBPS: longint;						// Return transfer rate
function CombGetPacketSize: longint;				// Return current packet size
function CombBytesToWrite: longint;					// Return # bytes remaining in write buffer
function CombBytesToRead: longint;					// Return # bytes remaining to be read
function CombBytesRemaining(a: longint): longint;	// Return # bytes remaining to read or write
function CombAsyncRequest(a: longint): longint;		// Return async read/write request

function CombSetControl(a: longint): longint;		// Set the control line status
function CombSetMode(a: longint): longint;			// Sets communications mode
function CombSetBPS(a: longint): longint;			// Sets the transfer rate
function CombSetPacketSize(a: longint): longint;	// Sets the packet size

function CombReset: longint;						// Reset serial controller
function CombResetError: longint;					// Reset error bits
function CombCancelWrite: longint;					// Cancel async write request
function CombCancelRead: longint;					// Cancel async read request

function CombSetRTS(a: longint): longint;						// Set RTS to 'a'
function CombCTS: longint;							// Return status of CTS

function CombWaitCallback(a: longint): longint;		// Install wait callback function

function CombResetVBLANK: longint;					// Restart VBLANK signal


procedure AddCOMB; stdcall external;
procedure DelCOMB; stdcall external;
procedure ChangeClearSIO(x: longint); stdcall external;
function _comb_control(a, b, c: dword): longint; stdcall external;


implementation

function CombSioStatus: longint;
begin
	CombSioStatus:= _comb_control(0,0,0);
end;

function CombControlStatus: longint;
begin
	CombControlStatus:= _comb_control(0,1,0);
end;

function CombGetMode: longint;
begin
	CombGetMode:= _comb_control(0,2,0);
end;

function CombGetBPS: longint;
begin
	CombGetBPS:= _comb_control(0,3,0);
end;

function CombGetPacketSize: longint;
begin
	CombGetPacketSize:= _comb_control(0,4,0);
end;

function CombBytesToWrite: longint;
begin
	CombBytesToWrite:= _comb_control(0,5,0);
end;

function CombBytesToRead: longint;
begin
	CombBytesToRead:= _comb_control(0,5,1);
end;

function CombBytesRemaining(a: longint): longint;
begin
	CombBytesRemaining:= _comb_control(0,5,a);
end;

function CombAsyncRequest(a: longint): longint;
begin
	CombAsyncRequest:= _comb_control(0,6,a);
end;

function CombSetControl(a: longint): longint;
begin
	CombSetControl:= _comb_control(1,1,a);
end;

function CombSetMode(a: longint): longint;
begin
	CombSetMode:= _comb_control(1,2,a);
end;

function CombSetBPS(a: longint): longint;
begin
	CombSetBPS:= _comb_control(1,3,a);
end;

function CombSetPacketSize(a: longint): longint;
begin
	CombSetPacketSize:= _comb_control(1,4,a);
end;

function CombReset: longint;
begin
	CombReset:= _comb_control(2,0,0);
end;

function CombResetError: longint;
begin
	CombResetError:= _comb_control(2,1,0);
end;

function CombCancelWrite: longint;
begin
	CombCancelWrite:= _comb_control(2,2,0);
end;

function CombCancelRead: longint;
begin
	CombCancelRead:= _comb_control(2,3,0);
end;

function CombSetRTS(a: longint): longint;
begin
	CombSetRTS:= _comb_control(3,0,a);
end;

function CombCTS: longint;
begin
	CombCTS:= _comb_control(3,1,0);
end;

function CombWaitCallback(a: longint): longint;
begin
	CombWaitCallback:= _comb_control(4,0,a);
end;

function CombResetVBLANK: longint;
begin
	CombResetVBLANK:= _comb_control(5,0,0);
end;

begin
end.