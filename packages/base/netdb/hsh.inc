
Type
  THostAddr = array[1..4] of byte;
  PHostAddr = ^THostAddr;
  TNetAddr = THostAddr;
  PNetAddr = ^TNetAddr;

  THostAddr6 = array[0..7] of word;
  PHostAddr6 = ^THostAddr6;
  TNetAddr6 = THostAddr6;
  PNetAddr6 = ^TNetAddr6;

Const
  NoAddress : THostAddr = (0,0,0,0);
  NoNet : TNetAddr = (0,0,0,0);

  NoAddress6 : THostAddr6 = (0,0,0,0,0,0,0,0);
  NoNet6: THostAddr6 = (0,0,0,0,0,0,0,0);

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


function HostAddrToStr6 (Entry : THostAddr6) : String;
function StrToHostAddr6(IP : String) : THostAddr6;
function NetAddrToStr6 (Entry : TNetAddr6) : String;
function StrToNetAddr6(IP : String) : TNetAddr6;
