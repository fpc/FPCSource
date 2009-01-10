{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

********************************************************************* }

unit tsp;

interface

{$mode objfpc}
{$calling cdecl}

uses
  Windows;

//***************************************************************
//
// consts
//
//***************************************************************

const
  CELLTSP_PROVIDERINFO_STRING                   = 'Cellular TAPI Service Provider';
  CELLTSP_LINENAME_STRING                       = 'Cellular Line';
  CELLTSP_PHONENAME_STRING                      = 'Cellular Phone';
  //Maximum length constants
  CELLDEVCONFIG_MAXLENGTH_GPRSACCESSPOINTNAME   = 64;
  CELLDEVCONFIG_MAXLENGTH_GPRSADDRESS           = 64;
  CELLDEVCONFIG_MAXLENGTH_GPRSPARAMETERS        = 32;
  //LINEBEARERMODE_ constant to identify GPRS calls
  LINEBEARERMODE_GPRS                           = (8 shl 16);
  //When using RAS, the following string MUST be used as the destination address for all GPRS calls
  GPRS_DEST_ADDRESS                             = '~GPRS!';
  //LINEDISCONNECTMODE_ constants to provide more detailed error reporting
  LINEDISCONNECTMODE_PHONECONNECTIONFAILURE     = (1 or ($d0 shl 16));
  LINEDISCONNECTMODE_INVALIDSIMCARD             = (1 or ($d1 shl 16));
  LINEDISCONNECTMODE_SIMCARDBUSY                = (1 or ($d2 shl 16));
  LINEDISCONNECTMODE_NETWORKSERVICENOTAVAILABLE = (1 or ($d3 shl 16));
  LINEDISCONNECTMODE_EMERGENCYONLY              = (1 or ($d4 shl 16));
  //Bearer Service CE constants define bearer service connection elements for CELLBEARERINFO structure
  CELLDEVCONFIG_CONNELEM_UNKNOWN                = $00000000; //Bearer service unknown
  CELLDEVCONFIG_CONNELEM_TRANSPARENT            = $00000001; //Link layer correction enabled
  CELLDEVCONFIG_CONNELEM_NONTRANSPARENT         = $00000002; //No link layer correction present
  CELLDEVCONFIG_CONNELEM_BOTH_TRANSPARENT       = $00000003; //Both available, transparent preferred
  CELLDEVCONFIG_CONNELEM_BOTH_NONTRANSPARENT    = $00000004; //Both available, non-transparent preferred
  //Telephony service type constants for CELLBEARERINFO structure
  //These flags only apply for data access. When a voice call is being
  //placed, this flag is ignored
  CELLDEVCONFIG_SERVICE_UNKNOWN                 = $00000000; //Unknown service
  CELLDEVCONFIG_SERVICE_MODEM_ASYNC             = $00000001; //Asynchronous modem
  CELLDEVCONFIG_SERVICE_MODEM_SYNC              = $00000002; //Synchronous modem
  CELLDEVCONFIG_SERVICE_PADACCESS_ASYNC         = $00000003; //PAD Access (asynchronous)
  CELLDEVCONFIG_SERVICE_PACKETACCESS_SYNC       = $00000004; //Packet Access (synchronous)
  //Data rate constants define protocol dependent data rates for CELLBEARERINFO structure
  CELLDEVCONFIG_SPEED_UNKNOWN                   = $00000000; //Unknown speed
  CELLDEVCONFIG_SPEED_AUTO                      = $00000001; //Automatic selection of speed
  CELLDEVCONFIG_SPEED_300_V21                   = $00000002; //300 bps (V.21)
  CELLDEVCONFIG_SPEED_300_V110                  = $00000003; //300 bps (V.100)
  CELLDEVCONFIG_SPEED_1200_V22                  = $00000004; //1200 bps (V.22)
  CELLDEVCONFIG_SPEED_1200_75_V23               = $00000005; //1200/75 bps (V.23)
  CELLDEVCONFIG_SPEED_1200_V110                 = $00000006; //1200 bps (V.100)
  CELLDEVCONFIG_SPEED_1200_V120                 = $00000007; //1200 bps (V.120)
  CELLDEVCONFIG_SPEED_2400_V22BIS               = $00000008; //2400 bps (V.22bis)
  CELLDEVCONFIG_SPEED_2400_V26TER               = $00000009; //2400 bps (V.26ter)
  CELLDEVCONFIG_SPEED_2400_V110                 = $0000000a; //2400 bps (V.110 or X.31 flag stuffing)
  CELLDEVCONFIG_SPEED_2400_V120                 = $0000000b; //2400 bps (V.120)
  CELLDEVCONFIG_SPEED_4800_V32                  = $0000000c; //4800 bps (V.32)
  CELLDEVCONFIG_SPEED_4800_V110                 = $0000000d; //4800 bps (V.110 or X.31 flag stuffing)
  CELLDEVCONFIG_SPEED_4800_V120                 = $0000000e; //4800 bps (V.120)
  CELLDEVCONFIG_SPEED_9600_V32                  = $0000000f; //9600 bps (V.32)
  CELLDEVCONFIG_SPEED_9600_V34                  = $00000010; //9600 bps (V.34)
  CELLDEVCONFIG_SPEED_9600_V110                 = $00000011; //9600 bps (V.110 or X.31 flag stuffing)
  CELLDEVCONFIG_SPEED_9600_V120                 = $00000012; //9600 bps (V.120)
  CELLDEVCONFIG_SPEED_14400_V34                 = $00000013; //14400 bps (V.34)
  CELLDEVCONFIG_SPEED_14400_V110                = $00000014; //14400 bps (V.100 or X.31 flag stuffing
  CELLDEVCONFIG_SPEED_14400_V120                = $00000015; //14400 bps (V.120
  CELLDEVCONFIG_SPEED_19200_V34                 = $00000016; //9200 bps (V.34)
  CELLDEVCONFIG_SPEED_19200_V110                = $00000017; //19200 bps (V.110 or X.31 flag stuffing)
  CELLDEVCONFIG_SPEED_19200_V120                = $00000018; //19200 bps (V.120)
  CELLDEVCONFIG_SPEED_28800_V34                 = $00000019; //28800 bps (V.34)
  CELLDEVCONFIG_SPEED_28800_V110                = $0000001a; //28800 bps (V.110 or X.31 flag stuffing)
  CELLDEVCONFIG_SPEED_28800_V120                = $0000001b; //28800 bps (V.120)
  CELLDEVCONFIG_SPEED_38400_V110                = $0000001c; //38400 bps (V.110 or X.31 flag stuffing)
  CELLDEVCONFIG_SPEED_38400_V120                = $0000001d; //38400 bps (V.120)
  CELLDEVCONFIG_SPEED_48000_V110                = $0000001e; //48000 bps (V.110 or X.31 flag stuffing)
  CELLDEVCONFIG_SPEED_48000_V120                = $0000001f; //48000 bps (V.120)
  CELLDEVCONFIG_SPEED_56000_V110                = $00000020; //56000 bps (V.110 or X.31 flag stuffing)
  CELLDEVCONFIG_SPEED_56000_V120                = $00000021; //56000 bps (V.120)
  CELLDEVCONFIG_SPEED_56000_TRANSP              = $00000022; //56000 bps (bit transparent)
  CELLDEVCONFIG_SPEED_64000_TRANSP              = $00000023; //64000 bps (bit transparent)
  CELLDEVCONFIG_SPEED_32000_PIAFS32K            = $00000024; //32000 bps (PIAFS32k)
  CELLDEVCONFIG_SPEED_64000_PIAFS64K            = $00000025; //64000 bps (PIAFS64k)
  CELLDEVCONFIG_SPEED_28800_MULTIMEDIA          = $00000026; //28800 bps (MultiMedia)
  CELLDEVCONFIG_SPEED_32000_MULTIMEDIA          = $00000027; //32000 bps (MultiMedia)
  CELLDEVCONFIG_SPEED_33600_MULTIMEDIA          = $00000028; //33600 bps (MultiMedia)
  CELLDEVCONFIG_SPEED_56000_MULTIMEDIA          = $00000029; //56000 bps (MultiMedia)
  CELLDEVCONFIG_SPEED_64000_MULTIMEDIA          = $0000002a; //64000 bps (MultiMedia)
  //Data compression directions constants for CELLDATACOMPINFO structure
  CELLDEVCONFIG_DATACOMPDIR_UNKNOWN             = $00000000; //Unknown direction
  CELLDEVCONFIG_DATACOMPDIR_NONE                = $00000001; //No compression
  CELLDEVCONFIG_DATACOMPDIR_TRANSMIT            = $00000002; //Transmit only
  CELLDEVCONFIG_DATACOMPDIR_RECEIVE             = $00000003; //Receive only
  CELLDEVCONFIG_DATACOMPDIR_BOTH                = $00000004; //Both directions, accept any direction
  //GPRS data compression settings constants for CELLGPRSCONNECTIONINFO structure
  CELLDEVCONFIG_GPRSCOMPRESSION_UNKNOWN         = $00000000; //Compression unknown
  CELLDEVCONFIG_GPRSCOMPRESSION_OFF             = $00000001; //Compression off
  CELLDEVCONFIG_GPRSCOMPRESSION_ON              = $00000002; //Compression on
  //GPRS protocol identifier constants for CELLGPRSCONNECTIONINFO structure
  CELLDEVCONFIG_GPRSPROTOCOL_UNKNOWN            = $00000000; //Unknown
  CELLDEVCONFIG_GPRSPROTOCOL_X25                = $00000001; //Unsupported
  CELLDEVCONFIG_GPRSPROTOCOL_IP                 = $00000002; //IP (Radio acts as PPP server)
  CELLDEVCONFIG_GPRSPROTOCOL_IHOSP              = $00000003; //Unsupported
  CELLDEVCONFIG_GPRSPROTOCOL_PPP                = $00000004; //Point to Point protocol (PPP frames forwarded to end server)
  //GPRS L2 protocol identifier constants for CELLGPRSCONNECTIONINFO structure
  CELLDEVCONFIG_GPRSL2PROTOCOL_UNKNOWN          = $00000000; //Unknown
  CELLDEVCONFIG_GPRSL2PROTOCOL_NULL             = $00000001; //Unsupported
  CELLDEVCONFIG_GPRSL2PROTOCOL_PPP              = $00000002; //WinCE uses PPP
  CELLDEVCONFIG_GPRSL2PROTOCOL_PAD              = $00000003; //Unsupported
  CELLDEVCONFIG_GPRSL2PROTOCOL_X25              = $00000004; //Unsupported
  //GPRS precedence classes for CELLGPRSQOSSETTINGS structure
  CELLDEVCONFIG_GPRSPRECEDENCECLASS_UNKNOWN     = $00000000; //Unknown
  CELLDEVCONFIG_GPRSPRECEDENCECLASS_SUBSCRIBED  = $00000001; //Subscribed value stored in network
  CELLDEVCONFIG_GPRSPRECEDENCECLASS_HIGH        = $00000002; //High priority
  CELLDEVCONFIG_GPRSPRECEDENCECLASS_NORMAL      = $00000003; //Normal priority
  CELLDEVCONFIG_GPRSPRECEDENCECLASS_LOW         = $00000004; //Low priority
  //GPRS delay classes for CELLGPRSQOSSETTINGS structure
  CELLDEVCONFIG_GPRSDELAYCLASS_UNKNOWN          = $00000000; //Unknown
  CELLDEVCONFIG_GPRSDELAYCLASS_SUBSCRIBED       = $00000001;
  CELLDEVCONFIG_GPRSDELAYCLASS_PREDICTIVE1      = $00000002;
  CELLDEVCONFIG_GPRSDELAYCLASS_PREDICTIVE2      = $00000003;
  CELLDEVCONFIG_GPRSDELAYCLASS_PREDICTIVE3      = $00000004;
  CELLDEVCONFIG_GPRSDELAYCLASS_BESTEFFORT       = $00000005;
  //GPRS reliability classes for CELLGPRSQOSSETTINGS structure
  CELLDEVCONFIG_GPRSRELIABILITYCLASS_UNKNOWN    = $00000000; //Unknown
  CELLDEVCONFIG_GPRSRELIABILITYCLASS_SUBSCRIBED = $00000001;
  CELLDEVCONFIG_GPRSRELIABILITYCLASS_1          = $00000002;
  CELLDEVCONFIG_GPRSRELIABILITYCLASS_2          = $00000003;
  CELLDEVCONFIG_GPRSRELIABILITYCLASS_3          = $00000004;
  CELLDEVCONFIG_GPRSRELIABILITYCLASS_4          = $00000005;
  CELLDEVCONFIG_GPRSRELIABILITYCLASS_5          = $00000006;
  //GPRS peak throughput classes for CELLGPRSQOSSETTINGS structure
  CELLDEVCONFIG_PEAKTHRUCLASS_UNKNOWN           = $00000000; //Unknown
  CELLDEVCONFIG_PEAKTHRUCLASS_SUBSCRIBED        = $00000001;
  CELLDEVCONFIG_PEAKTHRUCLASS_8000              = $00000002; //kbit/second
  CELLDEVCONFIG_PEAKTHRUCLASS_16000             = $00000003;
  CELLDEVCONFIG_PEAKTHRUCLASS_32000             = $00000004;
  CELLDEVCONFIG_PEAKTHRUCLASS_64000             = $00000005;
  CELLDEVCONFIG_PEAKTHRUCLASS_128000            = $00000006;
  CELLDEVCONFIG_PEAKTHRUCLASS_256000            = $00000007;
  CELLDEVCONFIG_PEAKTHRUCLASS_512000            = $00000008;
  CELLDEVCONFIG_PEAKTHRUCLASS_1024000           = $00000009;
  CELLDEVCONFIG_PEAKTHRUCLASS_2048000           = $0000000a;
  //GPRS mean throughput classes for CELLGPRSQOSSETTINGS structure
  CELLDEVCONFIG_MEANTHRUCLASS_UNKNOWN           = $00000000; //Unknown
  CELLDEVCONFIG_MEANTHRUCLASS_SUBSCRIBED        = $00000001;
  CELLDEVCONFIG_MEANTHRUCLASS_100               = $00000002; //octets/hour
  CELLDEVCONFIG_MEANTHRUCLASS_500               = $00000004;
  CELLDEVCONFIG_MEANTHRUCLASS_1000              = $00000005;
  CELLDEVCONFIG_MEANTHRUCLASS_2000              = $00000006;
  CELLDEVCONFIG_MEANTHRUCLASS_5000              = $00000007;
  CELLDEVCONFIG_MEANTHRUCLASS_10000             = $00000008;
  CELLDEVCONFIG_MEANTHRUCLASS_20000             = $00000009;
  CELLDEVCONFIG_MEANTHRUCLASS_50000             = $0000000a;
  CELLDEVCONFIG_MEANTHRUCLASS_100000            = $0000000b;
  CELLDEVCONFIG_MEANTHRUCLASS_200000            = $0000000c;
  CELLDEVCONFIG_MEANTHRUCLASS_500000            = $0000000d;
  CELLDEVCONFIG_MEANTHRUCLASS_1000000           = $0000000e;
  CELLDEVCONFIG_MEANTHRUCLASS_2000000           = $0000000f;
  CELLDEVCONFIG_MEANTHRUCLASS_5000000           = $00000010;
  CELLDEVCONFIG_MEANTHRUCLASS_10000000          = $00000011;
  CELLDEVCONFIG_MEANTHRUCLASS_20000000          = $00000012;
  CELLDEVCONFIG_MEANTHRUCLASS_50000000          = $00000013;
  CELLDEVCONFIG_MEANTHRUCLASS_DONTCARE          = $00000014; //Best effort
  //Flags for the CELLDEVCONFIG structure
  CELLDEVCONFIG_FLAG_TERMINALAFTERDIALING       = $00000001; //(Only valid when used with lineSetDevConfig)

//***********************************************************
//
// types
//
//***********************************************************

type
  CALLER_ID_OPTIONS = (CALLER_ID_DEFAULT); //Accept the default behavior

  //Structure that can be passed as data for LINECALLPARAMS.dwDevSpecific[Size, Offset]
  linecallparamsdevspecific_tag = record
    cidoOptions: CALLER_ID_OPTIONS;
  end; //linecallparamsdevspecific_tag
  LINECALLPARAMSDEVSPECIFIC  = linecallparamsdevspecific_tag;
  PLINECALLPARAMSDEVSPECIFIC = ^linecallparamsdevspecific_tag;

  //Structure containing bearer information
  //Used to set different bearers, specifically to enable quick connect.
  CELLBEARERINFO_tag = record
    dwSpeed            : LongInt; //One of the CELLDEVCONFIG_SPEED_* constants
    dwService          : LongInt; //One of the CELLDEVCONFIG_SERVICE_* constants
    dwConnectionElement: LongInt; //One of the CELLDEVCONFIG_CONNELEM_* constants
  end; //CELLBEARERINFO_tag
  CELLBEARERINFO  = CELLBEARERINFO_tag;
  PCELLBEARERINFO = ^CELLBEARERINFO_tag;

  //Structure containing data compression information
  //Used for v.42bis compression settings.
  CELLDATACOMPINFO_tag = record
    dwDirection      : LongInt; //One of the CELLDEVCONFIG_DATACOMPDIR_* constants
    dwRequired       : Boolean; //Data compression required
    dwMaxDictEntries : LongInt; //Maximum number of dictionary entries
    dwMaxStringLength: LongInt; //Maximum string length
  end; //CELLDATACOMPINFO_tag
  CELLDATACOMPINFO  = CELLDATACOMPINFO_tag;
  PCELLDATACOMPINFO = ^CELLDATACOMPINFO_tag;

  //Structure containing radio link protocol settings
  //Radio link protocol is a link layer correction protocol that increases
  //the perceived reliability of the air link.
  CELLRADIOLINKINFO_tag = record
    dwVersion           : LongInt; //Version number
    dwIws               : LongInt; //IWF to MS window size
    dwMws               : LongInt; //MS to IWF window size
    dwAckTimer          : LongInt; //Acknowledgement timer [T1] (milliseconds)
    dwRetransmitAttempts: LongInt; //Retransmit attempts [N2]
    dwResequenceTimer   : LongInt; //Resequence timer [T4] (milliseconds)
  end; //CELLRADIOLINKINFO_tag
  CELLRADIOLINKINFO  = CELLRADIOLINKINFO_tag;
  PCELLRADIOLINKINFO = ^CELLRADIOLINKINFO_tag;

  //Structure containing GPRS Quality Of Service (QOS) settings
  //Used to specify minimum and requested QOS settings
  CELLGPRSQOSSETTINGS_tag = record
    dwPrecedenceClass : LongInt; //One of the CELLDEVCONFIG_GPRSPRECEDENCECLASS_* constants }
    dwDelayClass      : LongInt; //One of the CELLDEVCONFIG_GPRSDELAYCLASS_* constants }
    dwReliabilityClass: LongInt; //One of the CELLDEVCONFIG_GPRSRELIABILITYCLASS_* constants }
    dwPeakThruClass   : LongInt; //One of the CELLDEVCONFIG_GPRSPEAKTHRUCLASS_* constants }
    dwMeanThruClass   : LongInt; //One of the CELLDEVCONFIG_GPRSMEANTHRUCLASS_* constants }
  end; //CELLGPRSQOSSETTINGS_tag
  CELLGPRSQOSSETTINGS  = CELLGPRSQOSSETTINGS_tag;
  PCELLGPRSQOSSETTINGS = ^CELLGPRSQOSSETTINGS_tag;

  //Structure containing GPRS connection information }
  //Used in the initiation of GPRS calls. }
  CELLGPRSCONNECTIONINFO_tag = record
    dwProtocolType            : LongInt; //One of the CELLDEVCONFIG_GPRSPROTOCOL_* constants
    dwL2ProtocolType          : LongInt; //One of the CELLDEVCONFIG_GPRSL2PROTOCOL_* constants
    wszAccessPointName        : array[0..CELLDEVCONFIG_MAXLENGTH_GPRSACCESSPOINTNAME-1] of WCHAR;//Logical name to select the GPRS gateway
    wszAddress                : array[0..CELLDEVCONFIG_MAXLENGTH_GPRSADDRESS-1] of WCHAR;//The packet address to use (if empty, then a dynamic address will be requested)
    dwDataCompression         : LongInt; //One of the CELLDEVCONFIG_GPRSCOMPRESSION_* constants
    dwHeaderCompression       : LongInt; //One of the CELLDEVCONFIG_GPRSCOMPRESSION_* constants
    szParameters              : array[0..CELLDEVCONFIG_MAXLENGTH_GPRSPARAMETERS-1] of Char; //Prococol-specific parameters (NULL terminated)
    bRequestedQOSSettingsValid: Boolean; //TRUE iff sgqsRequestedQOSSettings is valid
    cgqsRequestedQOSSettings  : CELLGPRSQOSSETTINGS;//GPRS QOS settings structure
    bMinimumQOSSettingsValid  : Boolean; //TRUE iff sgqsMinimumQOSSettings is valid
    cgqsMinimumQOSSettings    : CELLGPRSQOSSETTINGS; //GPRS QOS settings structure
  end; //CELLGPRSCONNECTIONINFO_tag
  CELLGPRSCONNECTIONINFO  = CELLGPRSCONNECTIONINFO_tag;
  PCELLGPRSCONNECTIONINFO = ^CELLGPRSCONNECTIONINFO_tag;

  //A service-provider-specific configuration structure }
  //This structure is specific to this version of this TSP. Future versions }
  //may append additional data elements to the end of this structure. }
  CELLDEVCONFIG_tag = record
    dwTotalSize             : LongInt; //Standard TAPI structure variable }
    dwNeededSize            : LongInt; //Standard TAPI structure variable }
    dwUsedSize              : LongInt; //Standard TAPI structure variable }
    bBearerInfoValid        : Boolean; //TRUE iff sbiBearerInfo is valid }
    cbiBearerInfo           : CELLBEARERINFO; //Bearer structure }
    bDataCompInfoValid      : Boolean; //TRUE iff sdciDataCompInfo is valid }
    cdciDataCompInfo        : CELLDATACOMPINFO; //Data compression structure }
    bRadioLinkInfoValid     : Boolean; //TRUE iff srliRadioLinkInfo is valid }
    crliRadioLinkInfo       : CELLRADIOLINKINFO; //RLP structure }
    bGPRSConnectionInfoValid: Boolean; //TRUE iff sgciGPRSConnectionInfo is valid }
    cgciGPRSConnectionInfo  : CELLGPRSCONNECTIONINFO; //GPRS connection structure }
    dwFlags                 : LongInt; //Bit-mask of CELLDEVCONFIG_FLAG_* constants }
  end; //CELLDEVCONFIG_tag
  CELLDEVCONFIG  = CELLDEVCONFIG_tag;
  PCELLDEVCONFIG = ^CELLDEVCONFIG_tag;


implementation

end.
