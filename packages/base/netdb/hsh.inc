
Type
  THostAddr = array[1..4] of byte;
  PHostAddr = ^THostAddr;
  TNetAddr = THostAddr;
  PNetAddr = ^TNetAddr;

Const
  NoAddress : THostAddr = (0,0,0,0);
  NoNet : TNetAddr = (0,0,0,0);

function HostAddrToStr (Entry : THostAddr) : String;
function StrToHostAddr(IP : String) : THostAddr ;
function NetAddrToStr (Entry : TNetAddr) : String;
function StrToNetAddr(IP : String) : TNetAddr;
Function HostToNet (Host : ThostAddr) : THostAddr;
Function NetToHost (Net : TNetAddr) : TNetAddr;
Function HostToNet (Host : Longint) : Longint;
Function NetToHost (Net : Longint) : Longint;
Function ShortHostToNet (Host : Word) : Word;
Function ShortNetToHost (Net : Word) : Word;
