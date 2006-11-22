{ 
  Should be aliases but I copy pasted them from winsock2 for now...
}

const
{ Address families. }
  AF_UNSPEC       = winsock2.AF_UNSPEC;               { unspecified }
  AF_UNIX         = winsock2.AF_UNIX;               { local to host (pipes, portals) }
  AF_INET         = winsock2.AF_INET;               { internetwork: UDP, TCP, etc. }
  AF_IMPLINK      = winsock2.AF_IMPLINK;               { arpanet imp addresses }
  AF_PUP          = winsock2.AF_PUP;               { pup protocols: e.g. BSP }
  AF_CHAOS        = winsock2.AF_CHAOS;               { mit CHAOS protocols }
  AF_IPX          = winsock2.AF_IPX;               { IPX and SPX }
  AF_NS           = winsock2.AF_NS;               { XEROX NS protocols }
  AF_ISO          = winsock2.AF_ISO;               { ISO protocols }
  AF_OSI          = winsock2.AF_OSI;          { OSI is ISO }
  AF_ECMA         = winsock2.AF_ECMA;               { european computer manufacturers }
  AF_DATAKIT      = winsock2.AF_DATAKIT;               { datakit protocols }
  AF_CCITT        = winsock2.AF_CCITT;              { CCITT protocols, X.25 etc }
  AF_SNA          = winsock2.AF_SNA;              { IBM SNA }
  AF_DECnet       = winsock2.AF_DECnet;              { DECnet }
  AF_DLI          = winsock2.AF_DLI;              { Direct data link interface }
  AF_LAT          = winsock2.AF_LAT;              { LAT }
  AF_HYLINK       = winsock2.AF_HYLINK;              { NSC Hyperchannel }
  AF_APPLETALK    = winsock2.AF_APPLETALK;              { AppleTalk }
  AF_NETBIOS      = winsock2.AF_NETBIOS;              { NetBios-style addresses }
  AF_VOICEVIEW    = winsock2.AF_VOICEVIEW;              { VoiceView }
  AF_FIREFOX      = winsock2.AF_FIREFOX;              { FireFox }
  AF_UNKNOWN1     = winsock2.AF_UNKNOWN1;              { Somebody is using this! }
  AF_BAN          = winsock2.AF_BAN;              { Banyan }
  AF_ATM          = winsock2.AF_ATM; // Native ATM Services
  AF_INET6        = winsock2.AF_INET6; // Internetwork Version 6
  AF_MAX          = winsock2.AF_MAX;

  PF_UNSPEC       = AF_UNSPEC;
  PF_UNIX         = AF_UNIX;
  PF_INET         = AF_INET;
  PF_IMPLINK      = AF_IMPLINK;
  PF_PUP          = AF_PUP;
  PF_CHAOS        = AF_CHAOS;
  PF_NS           = AF_NS;
  PF_IPX          = AF_IPX;
  PF_ISO          = AF_ISO;
  PF_OSI          = AF_OSI;
  PF_ECMA         = AF_ECMA;
  PF_DATAKIT      = AF_DATAKIT;
  PF_CCITT        = AF_CCITT;
  PF_SNA          = AF_SNA;
  PF_DECnet       = AF_DECnet;
  PF_DLI          = AF_DLI;
  PF_LAT          = AF_LAT;
  PF_HYLINK       = AF_HYLINK;
  PF_APPLETALK    = AF_APPLETALK;
  PF_VOICEVIEW    = AF_VOICEVIEW;
  PF_FIREFOX      = AF_FIREFOX;
  PF_UNKNOWN1     = AF_UNKNOWN1;
  PF_BAN          = AF_BAN;
  PF_ATM          = AF_ATM;
  PF_INET6        = AF_INET6;

  PF_MAX          = AF_MAX;
