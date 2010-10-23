
var PowerSDLNetBase : pLibrary;

const
    POWERSDL_NETNAME : PChar = 'powersdl_net.library';


function SDLNet_Init : LongInt; syscall r12base PowerSDLNetBase 028;
procedure SDLNet_Quit; syscall r12base PowerSDLNetBase 034;
function SDLNet_ResolveHost(address : pIPaddress; const host : pChar; port : Word) : LongInt; syscall r12base PowerSDLNetBase 040;
function SDLNet_ResolveIP(ip : pIPaddress) : pChar; syscall r12base PowerSDLNetBase 046;
function SDLNet_TCP_Open(ip : pIPaddress) : Pointer; syscall r12base PowerSDLNetBase 052;
function SDLNet_TCP_Accept(server : Pointer) : Pointer; syscall r12base PowerSDLNetBase 058;
function SDLNet_TCP_GetPeerAddress(sock : Pointer) : pIPaddress; syscall r12base PowerSDLNetBase 064;
function SDLNet_TCP_Send(sock : Pointer; const data : Pointer; len : LongInt) : LongInt; syscall r12base PowerSDLNetBase 070;
function SDLNet_TCP_Recv(sock : Pointer; data : Pointer; maxlen : LongInt) : LongInt; syscall r12base PowerSDLNetBase 076;
procedure SDLNet_TCP_Close(sock : Pointer); syscall r12base PowerSDLNetBase 082;
function SDLNet_AllocPacket(size : LongInt) : pUDPpacket; syscall r12base PowerSDLNetBase 088;
function SDLNet_ResizePacket(packet : pUDPpacket; newsize : LongInt) : LongInt; syscall r12base PowerSDLNetBase 094;
procedure SDLNet_FreePacket(packet : pUDPpacket); syscall r12base PowerSDLNetBase 100;
function SDLNet_AllocPacketV(howmany : LongInt; size : LongInt) : ppUDPpacket; syscall r12base PowerSDLNetBase 106;
procedure SDLNet_FreePacketV(var packetV : pUDPpacket); syscall r12base PowerSDLNetBase 112;
function SDLNet_UDP_Open(port : Word) : Pointer; syscall r12base PowerSDLNetBase 118;
function SDLNet_UDP_Bind(sock : Pointer; channel : LongInt; address : pIPaddress) : LongInt; syscall r12base PowerSDLNetBase 124;
procedure SDLNet_UDP_Unbind(sock : Pointer; channel : LongInt); syscall r12base PowerSDLNetBase 130;
function SDLNet_UDP_GetPeerAddress(sock : Pointer; channel : LongInt) : pIPaddress; syscall r12base PowerSDLNetBase 136;
function SDLNet_UDP_SendV(sock : Pointer; var packets : pUDPpacket; npackets : LongInt) : LongInt; syscall r12base PowerSDLNetBase 142;
function SDLNet_UDP_Send(sock : Pointer; channel : LongInt; packet : pUDPpacket) : LongInt; syscall r12base PowerSDLNetBase 148;
function SDLNet_UDP_RecvV(sock : Pointer; var packets : pUDPpacket) : LongInt; syscall r12base PowerSDLNetBase 154;
function SDLNet_UDP_Recv(sock : Pointer; packet : pUDPpacket) : LongInt; syscall r12base PowerSDLNetBase 160;
procedure SDLNet_UDP_Close(sock : Pointer); syscall r12base PowerSDLNetBase 166;
function SDLNet_AllocSocketSet(maxsockets : LongInt) : Pointer; syscall r12base PowerSDLNetBase 172;
function SDLNet_AddSocket(set_ : Pointer; sock : Pointer) : LongInt; syscall r12base PowerSDLNetBase 178;
function SDLNet_DelSocket(set_ : Pointer; sock : Pointer) : LongInt; syscall r12base PowerSDLNetBase 184;
function SDLNet_CheckSockets(set_ : Pointer; timeout : DWord) : LongInt; syscall r12base PowerSDLNetBase 190;
procedure SDLNet_FreeSocketSet(set_ : Pointer); syscall r12base PowerSDLNetBase 196;
function SDLNet_Linked_Version : pSDL_version; syscall r12base PowerSDLNetBase 202;
