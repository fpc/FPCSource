{
    This file is part of the Free Pascal run time library
    for Netware.
    Copyright (c) 1999-2002 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    First Version of AIO, currently UNTESTED, i have to write some samples
    to test it.

 **********************************************************************}
unit aio;
interface

const
  aionlm='aio.nlm';


{$PACKRECORDS C}


{----------------------------------------------------------------------------
      Predefined hardware types for use with the AIOAcquirePort function.
 ---------------------------------------------------------------------------- }

const
   AIO_COMX_TYPE = 1;
   AIO_ARTIC_TYPE = 2;
   AIO_WNIM_TYPE = 3;

{----------------------------------------------------------------------------
      Function completion code status values.

      Note that all error statuses are negative values.
 ---------------------------------------------------------------------------- }
   AIO_SUCCESS = 0;
   AIO_BAD_HANDLE = -(1);
   AIO_FAILURE = -(2);
   AIO_FUNC_NOT_SUPPORTED = -(3);
   AIO_INVALID_PARAMETER = -(5);
   AIO_PORT_NOT_AVAILABLE = -(6);
   AIO_QUALIFIED_SUCCESS = -(7);
   AIO_NO_MORE_PORTS = -(8);
   AIO_TYPE_NUMBER_INVALID = -(10);
   AIO_BOARD_NUMBER_INVALID = -(11);
   AIO_PORT_NUMBER_INVALID = -(12);
   AIO_RESOURCE_TAG_INVALID = -(13);
   AIO_DATA_PRESENT = -(14);
   AIO_BAD_REQUEST_TYPE = -(15);
   AIO_PORT_GONE = -(20);
   AIO_RTAG_INVALID = -(21);
{ This is only for non-CLIB application  }
{ 'NYSA'  }
   ASYNCIOSignature = $4E595341;

{----------------------------------------------------------------------------
      Definitions for use with the AIOSetExternalControl function.
 ---------------------------------------------------------------------------- }
   AIO_EXTERNAL_CONTROL          = 1;
   AIO_EXTCTRL_DTR               = 1;
   AIO_EXTCTRL_RTS               = 2;
   AIO_BREAK_CONTROL             = 2;
   AIO_SET_BREAK_OFF             = 0;
   AIO_SET_BREAK_ON              = 1;
   AIO_FLOW_CONTROL              = 3;
   AIO_SOFTWARE_FLOW_CONTROL_OFF = 0;
   AIO_SOFTWARE_FLOW_CONTROL_ON  = 1;
   AIO_HARDWARE_FLOW_CONTROL_OFF = 0;
   AIO_HARDWARE_FLOW_CONTROL_ON  = 2;
   AIO_FLOW_CONTROL_CHARACTERS   = 4;
   AIO_SET_DEADMAN_TIMER         = 5;

{----------------------------------------------------------------------------
      Definitions for use with the AIOGetExternalStatus function.
 ---------------------------------------------------------------------------- }
   AIO_EXTSTA_RI    = $00000001;
   AIO_EXTSTA_DCD   = $00000008;
   AIO_EXTSTA_DSR   = $00000010;
   AIO_EXTSTA_CTS   = $00000020;
   AIO_EXTSTA_BREAK = $00000080;

{----------------------------------------------------------------------------
      Definitions for use with the AIOFlushBuffers function.
 ---------------------------------------------------------------------------- }
   AIO_FLUSH_WRITE_BUFFER = 1;
   AIO_FLUSH_READ_BUFFER  = 2;

{----------------------------------------------------------------------------
      Definitions for use with the AIOReadStatus function.
 ---------------------------------------------------------------------------- }
   AIO_RECEIVE_ACTIVE = 0;
   AIO_RECEIVE_FULL   = 1;

{----------------------------------------------------------------------------
      Definitions for use with the AIOWriteStatus function.
 ---------------------------------------------------------------------------- }
   AIO_TRANSMIT_IDLE   = 0;
   AIO_TRANSMIT_ACTIVE = 1;
   AIO_TRANSMIT_XOFFED = 2;

{----------------------------------------------------------------------------
      Definitions for use with the AIOAcquirePort function.
 ---------------------------------------------------------------------------- }
   AIO_HARDWARE_TYPE_WILDCARD = -(1);
   AIO_BOARD_NUMBER_WILDCARD  = -(1);
   AIO_PORT_NUMBER_WILDCARD   = -(1);


{----------------------------------------------------------------------------
      Definitions for use with the AIOConfigurePort function.
 ---------------------------------------------------------------------------- }
{ BitRate  }
   AIO_BAUD_50       = 0;
   AIO_BAUD_75       = 1;
   AIO_BAUD_110      = 2;
   AIO_BAUD_134p5    = 3;
   AIO_BAUD_150      = 4;
   AIO_BAUD_300      = 5;
   AIO_BAUD_600      = 6;
   AIO_BAUD_1200     = 7;
   AIO_BAUD_1800     = 8;
   AIO_BAUD_2000     = 9;
   AIO_BAUD_2400     = 10;
   AIO_BAUD_3600     = 11;
   AIO_BAUD_4800     = 12;
   AIO_BAUD_7200     = 13;
   AIO_BAUD_9600     = 14;
   AIO_BAUD_19200    = 15;
   AIO_BAUD_38400    = 16;
   AIO_BAUD_57600    = 17;
   AIO_BAUD_115200   = 18;
{ DataBits  }
   AIO_DATA_BITS_5   = 0;
   AIO_DATA_BITS_6   = 1;
   AIO_DATA_BITS_7   = 2;
   AIO_DATA_BITS_8   = 3;
{ StopBits  }
   AIO_STOP_BITS_1   = 0;
   AIO_STOP_BITS_1p5 = 1;
   AIO_STOP_BITS_2   = 2;
{ Parity  }
   AIO_PARITY_NONE   = 0;
   AIO_PARITY_ODD    = 1;
   AIO_PARITY_EVEN   = 2;
   AIO_PARITY_MARK   = 3;
   AIO_PARITY_SPACE  = 4;
{ FlowControl  }
   AIO_SOFTWARE_FLOWCONTROL_OFF = 0;
   AIO_SOFTWARE_FLOWCONTROL_ON  = 1;
   AIO_HARDWARE_FLOWCONTROL_OFF = 0;
   AIO_HARDWARE_FLOWCONTROL_ON  = 2;
   AIO_DROPOUT_VALUE            = $FF;

{----------------------------------------------------------------------------
      Definitions for use with AIOPORTCAPABILITIES structure.
 ---------------------------------------------------------------------------- }

type

   PAIOPORTCAPABILITIES = ^TAIOPORTCAPABILITIES;
   TAIOPORTCAPABILITIES = record
        returnLength       : WORD;       { byte length of capabilities data }
        majorVersion       : BYTE;
        minorVersion       : BYTE;
        notSupportedMask   : LONGINT;
        minBitRate         : BYTE;
        maxBitRate         : BYTE;       { minimum bit rate index supported }
        minDataBits        : BYTE;       { minimum data bits per char index supported }
        maxDataBits        : BYTE;       { maximum data bits per char index supported }
        minStopBits        : BYTE;       { minimum stop bits per char index supported }
        maxStopBits        : BYTE;       { maximum stop bits per char index supported }
        minParityMode      : BYTE;       { minimum parity mode index supported }
        maxParityMode      : BYTE;       { maximum parity mode index supported }
        minFlowCtrlMode    : BYTE;       { minimum flow control mode index supported }
        maxFlowCtrlMode    : BYTE;       { maximum flow control mode index supported }
        miscCapabilities   : LONGINT;    { miscellaneous capability flags }
        minReadBufferSize  : LONGINT;    { minimum length of receive buffer }
        maxReadBufferSize  : LONGINT;    { maximum length of receive buffer }
        minWriteBufferSize : LONGINT;    { minimum length of transmit buffer }
        maxWriteBufferSize : LONGINT;    { maximum length of transmit buffer }
        minDeadmanTime     : WORD;       { minimum deadman time (seconds) }
        maxDeadmanTime     : WORD;       { maximum deadman time (seconds) }
     end;

const
   AIO_PORT_NS_MINBITRATE         = $80000000;
   AIO_PORT_NS_MAXBITRATE         = $40000000;
   AIO_PORT_NS_MINDATABITS        = $20000000;
   AIO_PORT_NS_MAXDATABITS        = $10000000;
   AIO_PORT_NS_MINSTOPBITS        = $08000000;
   AIO_PORT_NS_MAXSTOPBITS        = $04000000;
   AIO_PORT_NS_MINPARITYMODE      = $02000000;
   AIO_PORT_NS_MAXPARITYMODE      = $01000000;
   AIO_PORT_NS_MINFLOWCTRLMODE    = $00800000;
   AIO_PORT_NS_MAXFLOWCTRLMODE    = $00400000;
   AIO_PORT_NS_MISCCAPABILITIES   = $00200000;
   AIO_PORT_NS_MINREADBUFFERSIZE  = $00100000;
   AIO_PORT_NS_MAXREADBUFFERSIZE  = $00080000;
   AIO_PORT_NS_MINWRITEBUFFERSIZE = $00040000;
   AIO_PORT_NS_MAXWRITEBUFFERSIZE = $00020000;
   AIO_PORT_NS_MINDEADMANTIME     = $00010000;
   AIO_PORT_NS_MAXDEADMANTIME     = $00008000;
   AIO_PORT_CAPS_NOT_SUPPORTED    = $00007FFF;
   AIO_PORT_CAPS_MAJOR_VERSION    = 1;
   AIO_PORT_CAPS_MINOR_VERSION    = 0;
   AIO_CAP_OUTPUT_BREAK           = $00000002;
   AIO_CAP_FLOWCTRLCHARS          = $00000004;
   AIO_CAP_PROGRAMMABLE           = $00000008;
   AIO_CAP_INPUT                  = $00000010;
   AIO_CAP_OUTPUT                 = $00000020;

{  byte length of driver capabilities structure }
type

   PAIODVRCAPABILITIES = ^TAIODVRCAPABILITIES;
   TAIODVRCAPABILITIES = record
        returnLength : WORD;
        byteData     : array[0..1] of BYTE;
     end;

const
   AIO_NO_STRUCT_DATA_RETURNED = 2;


{----------------------------------------------------------------------------
      Definitions for use with the AIOGetPortsRollCall function.
 ---------------------------------------------------------------------------- }

type

   PAIOPORTINFO = ^TAIOPORTINFO;
   TAIOPORTINFO = record
        returnLength       : WORD;      {  byte length of port info data }
        majorVersion       : BYTE;
        minorVersion       : BYTE;
        notSupportedMask   : longint;
        hardwareType       : longint;   { value used with AIOAcquirePort }
        boardNumber        : longint;   { " }
        portNumber         : longint;   { " }
        availability       : WORD;      {  availability of port for acquire }
        externalStatus     : longint;   {  current external status value for port }
        chgdExternalStatus : longint;   {  changed external status value for port  }
     end;

const
   AIO_INFO_NS_HARDWARETYPE       = $80000000;
   AIO_INFO_NS_BOARDNUMBER        = $40000000;
   AIO_INFO_NS_PORTNUMBER         = $20000000;
   AIO_INFO_NS_AVAILABILITY       = $10000000;
   AIO_INFO_NS_EXTERNALSTATUS     = $08000000;
   AIO_INFO_NS_CHGDEXTERNALSTATUS = $04000000;
   AIO_PORT_INFO_NOT_SUPPORTED    = $03FFFFFF;
   AIO_PORT_INFO_MAJOR_VERSION    = 1;
   AIO_PORT_INFO_MINOR_VERSION    = 0;
   AIO_AVAILABLE_FOR_ACQUIRE      = 0;
   AIO_ALREADY_ACQUIRED           = 1;
   AIO_UNAVAILABLE                = $FF;
   AIO_INITIAL                    = 0;
   AIO_SUCCESSOR                  = 1;

{----------------------------------------------------------------------------
      Definitions for use with the AIOGetPortConfiguration function.
 ---------------------------------------------------------------------------- }

type

   PAIOPORTCONFIG = ^TAIOPORTCONFIG;
   TAIOPORTCONFIG = record
        returnLength     : WORD;           { byte length of port configuration data }
        majorVersion     : BYTE;
        minorVersion     : BYTE;
        notSupportedMask : LONGINT;
        hardwareType     : longint;        { value used with AIOAcquirePort     }
        boardNumber      : longint;        { "     " }
        portNumber       : longint;        { "     " }
        bitRate          : BYTE;           { Bits per second index }
        dataBits         : BYTE;           { Bits per character index }
        stopBits         : BYTE;           { Stop bits per char index }
        parityMode       : BYTE;           { Generated parity index }
        flowCtrlMode     : BYTE;           { Flow control mode }
        breakMode        : BYTE;           { Break control mode }
        readSize         : LONGINT;        { Receive buffer size }
        writeSize        : LONGINT;        { Transmit buffer size }
        transmitXon      : BYTE;
        transmitXoff     : BYTE;
        receiveXon       : BYTE;
        receiveXoff      : BYTE;
        externalControl  : WORD;           { set with AIO_EXTERNAL_CONTROL }
     end;

const
   AIO_CONFIG_NS_HARDWARETYPE    = $80000000;
   AIO_CONFIG_NS_BOARDNUMBER     = $40000000;
   AIO_CONFIG_NS_PORTNUMBER      = $20000000;
   AIO_CONFIG_NS_BITRATE         = $10000000;
   AIO_CONFIG_NS_DATABITS        = $08000000;
   AIO_CONFIG_NS_STOPBITS        = $04000000;
   AIO_CONFIG_NS_PARITYMODE      = $02000000;
   AIO_CONFIG_NS_FLOWCTRLMODE    = $01000000;
   AIO_CONFIG_NS_BREAKMODE       = $00800000;
   AIO_CONFIG_NS_READSIZE        = $00400000;
   AIO_CONFIG_NS_WRITESIZE       = $00200000;
   AIO_CONFIG_NS_TRANSMITXON     = $00100000;
   AIO_CONFIG_NS_TRANSMITXOFF    = $00080000;
   AIO_CONFIG_NS_RECEIVEXON      = $00040000;
   AIO_CONFIG_NS_RECEIVEXOFF     = $00020000;
   AIO_CONFIG_NS_EXTERNALCONTROL = $00010000;
   AIO_PORT_CONFIG_NOT_SUPPORTED = $0007FFFF;
   AIO_PORT_CONFIG_MAJOR_VERSION = 1;
   AIO_PORT_CONFIG_MINOR_VERSION = 0;
   AIO_EXTCTRL_DTR_ENABLE        = 1;
   AIO_EXTCTRL_DTR_DISABLE       = 0;
   AIO_EXTCTRL_RTS_ENABLE        = 2;
   AIO_EXTCTRL_RTS_DISABLE       = 0;
   AIO_BREAK_MODE_OFF            = 0;
   AIO_BREAK_MODE_ON             = 1;

type

   PAIODVRCONFIG = ^TAIODVRCONFIG;
   TAIODVRCONFIG = record
        returnLength : WORD;             {  byte length of driver config structure }
        byteData     : array[0..1] of BYTE;
     end;

{----------------------------------------------------------------------------
    Definitions for use with the AIOGetStatistics function.
  ---------------------------------------------------------------------------- }


   PAIOPORTSTATISTICS = ^TAIOPORTSTATISTICS;
   TAIOPORTSTATISTICS = record
        returnLength     : WORD;         { byte length of port statistics structure }
        majorVersion     : BYTE;
        minorVersion     : BYTE;
        notSupportedMask : LONGINT;
        receiveBytes     : LONGINT;      { total number of bytes received on port }
        transmitBytes    : LONGINT;      { total number of bytes transmitted from port }
        parityErrors     : LONGINT;      { number of receive parity errors  }
        framingErrors    : LONGINT;      { number of receive framing errors }
        overrunSoftware  : LONGINT;      { number of software overruns (occurrences) }
        overrunHardware  : LONGINT;      { number of hardware overruns (occurrences) }
     end;

const
   AIO_STATS_NS_RECEIVEBYTES    = $80000000;
   AIO_STATS_NS_TRANSMITBYTES   = $40000000;
   AIO_STATS_NS_PARITYERRORS    = $20000000;
   AIO_STATS_NS_FRAMINGERRORS   = $10000000;
   AIO_STATS_NS_OVERRUNSOFTWARE = $08000000;
   AIO_STATS_NS_OVERRUNHARDWARE = $04000000;
   AIO_PORT_STATS_NOT_SUPPORTED = $03FFFFFF;
   AIO_PORT_STATS_MAJOR_VERSION = 1;
   AIO_PORT_STATS_MINOR_VERSION = 0;

type

   PAIODVRSTATISTICS = ^TAIODVRSTATISTICS;
   TAIODVRSTATISTICS = record
        returnLength : WORD;                { byte length of driver statistics structure }
        byteData     : array[0..1] of BYTE;
     end;


{----------------------------------------------------------------------------
      Definitions for use with AIOGetDriverList function.
 ---------------------------------------------------------------------------- }

   PAIODRIVERLISTENTRY = ^TAIODRIVERLISTENTRY;
   TAIODRIVERLISTENTRY = record
        hardwareType : longint;
        ports : longint;
        name  : array[0..127] of char;
     end;

   PAIODRIVERLIST = ^TAIODRIVERLIST;
   TAIODRIVERLIST = record
        returnLength : WORD;
        driver       : array[0..0] of TAIODRIVERLISTENTRY;
     end;

const
   AIO_DRIVER_LIST_GET_FIRST = -(1);

{----------------------------------------------------------------------------
      Definitions for use with AIOGetBoardList function.
 ---------------------------------------------------------------------------- }
type

   PAIOBOARDLISTENTRY = ^TAIOBOARDLISTENTRY;
   TAIOBOARDLISTENTRY = record
        boardNumber : longint;
        ports       : longint;
        name        : array[0..127] of char;
     end;

   PAIOBOARDLIST = ^TAIOBOARDLIST;
   TAIOBOARDLIST = record
        returnLength : WORD;
        board        : array[0..0] of TAIOBOARDLISTENTRY;
     end;

const
   AIO_BOARD_LIST_GET_FIRST = -(1);

{----------------------------------------------------------------------------
      Definitions for use with AIOSetControlData function.
 ---------------------------------------------------------------------------- }
{  byte length of control data structure }
type

   PAIOCONTROLDATA = ^TAIOCONTROLDATA;
   TAIOCONTROLDATA = record
        returnLength : WORD;
        byteData     : array[0..1] of BYTE;
     end;


{----------------------------------------------------------------------------
      Definitions for use with AIOGetFirstPortInfo and AIOGetNextPortInfo
 ---------------------------------------------------------------------------- }

   PAIOPORTSEARCH = ^TAIOPORTSEARCH;
   TAIOPORTSEARCH = record
        typeMask  : longint;
        boardMask : longint;
        portMask  : longint;
        reserved  : array[0..5] of longint;
     end;

{----------------------------------------------------------------------------
      Definition of AIO functions.
 ---------------------------------------------------------------------------- }


function AIOAcquirePort(hardwareType:Plongint; boardNumber:Plongint; portNumber:Plongint; portHandle:Plongint):longint;cdecl;external aionlm name 'AIOAcquirePort';
function AIOAcquirePort(var hardwareType,boardNumber,portNumber,portHandle:longint):longint;cdecl;external aionlm name 'AIOAcquirePort';

function AIOAcquirePortWithRTag(hardwareType:Plongint; boardNumber:Plongint; portNumber:Plongint; portHandle:Plongint; RTag:longint):longint;cdecl;external aionlm name 'AIOAcquirePortWithRTag';
function AIOAcquirePortWithRTag(var hardwareType,boardNumber,portNumber,portHandle:longint; RTag:longint):longint;cdecl;external aionlm name 'AIOAcquirePortWithRTag';

function AIOConfigurePort(portHandle:longint; bitRate:byte; dataBits:byte; stopBits:byte; parityMode:byte;
           flowCtrlMode:byte):longint;cdecl;external aionlm name 'AIOConfigurePort';
function AIOFlushBuffers(portHandle:longint; flushFlag:WORD):longint;cdecl;external aionlm name 'AIOFlushBuffers';
function AIOGetBoardList(hardwareType:longint; boardIndex:longint; pBoardList:PAIOBOARDLIST):longint;cdecl;external aionlm name 'AIOGetBoardList';
function AIOGetDriverList(lastHardwareType:longint; pDriverList:PAIODRIVERLIST):longint;cdecl;external aionlm name 'AIOGetDriverList';
function AIOGetExternalStatus(portHandle:longint; extStatus:PLongint; chgdExtStatus:PLongint):longint;cdecl;external aionlm name 'AIOGetExternalStatus';
function AIOGetExternalStatus(portHandle:longint; var extStatus,chgdExtStatus:Longint):longint;cdecl;external aionlm name 'AIOGetExternalStatus';

function AIOGetFirstPortInfo(hardwareType:longint; boardNumber:longint; portNumber:longint; portSearchP:PAIOPORTSEARCH; portInfoP:PAIOPORTINFO;
           capabilitiesP:PAIOPORTCAPABILITIES; dvrCapabilitiesP:PAIODVRCAPABILITIES; NLMModuleNameP:Pchar):longint;cdecl;external aionlm name 'AIOGetFirstPortInfo';
function AIOGetNextPortInfo(portSearchP:PAIOPORTSEARCH; portInfoP:PAIOPORTINFO; capabilitiesP:PAIOPORTCAPABILITIES; dvrCapabilitiesP:PAIODVRCAPABILITIES; NLMModuleNameP:Pchar):longint;cdecl;external aionlm name 'AIOGetNextPortInfo';
function AIOGetName_FirstPortInfo(hardwareType:longint; boardNumber:longint; portNumber:longint; portSearchP:PAIOPORTSEARCH; portInfoP:PAIOPORTINFO;
           capabilitiesP:PAIOPORTCAPABILITIES; dvrCapabilitiesP:PAIODVRCAPABILITIES; NLMModuleNameP:Pchar):longint;cdecl;external aionlm name 'AIOGetName_FirstPortInfo';
function AIOGetName_NextPortInfo(portSearchP:PAIOPORTSEARCH; portInfoP:PAIOPORTINFO; capabilitiesP:PAIOPORTCAPABILITIES; dvrCapabilitiesP:PAIODVRCAPABILITIES; NLMModuleNameP:Pchar):longint;cdecl;external aionlm name 'AIOGetName_NextPortInfo';
function AIOGetPortCapability(portHandle:longint; pCapabilities:PAIOPORTCAPABILITIES; pDvrCapabilities:PAIODVRCAPABILITIES):longint;cdecl;external aionlm name 'AIOGetPortCapability';
function AIOGetPortConfiguration(portHandle:longint; pPortConfig:PAIOPORTCONFIG; pDvrConfig:PAIODVRCONFIG):longint;cdecl;external aionlm name 'AIOGetPortConfiguration';
function AIOGetPortStatus(portHandle:longint; writeCount:PLongint; writeState:PWORD; readCount:PLongint; readState:PWORD;
           extStatus:PLongint; chgdExtStatus:PLongint):longint;cdecl;external aionlm name 'AIOGetPortStatus';
function AIOGetPortStatus(portHandle:longint; var writeCount:Longint; var writeState:WORD; var readCount:Longint; var readState:WORD;
           var extStatus,chgdExtStatus:Longint):longint;cdecl;external aionlm name 'AIOGetPortStatus';

function AIOGetReadBufferSize(portHandle:longint; readSize:PLongint):longint;cdecl;external aionlm name 'AIOGetReadBufferSize';
function AIOGetReadBufferSize(portHandle:longint; var readSize:Longint):longint;cdecl;external aionlm name 'AIOGetReadBufferSize';

function AIOGetPortStatistics(portHandle:longint; pPortStatistics:PAIOPORTSTATISTICS; pDvrStatistics:PAIODVRSTATISTICS):longint;cdecl;external aionlm name 'AIOGetPortStatistics';
function AIOGetPortStatistics(portHandle:longint; var pPortStatistics:TAIOPORTSTATISTICS; var pDvrStatistics:TAIODVRSTATISTICS):longint;cdecl;external aionlm name 'AIOGetPortStatistics';

function AIOGetWriteBufferSize(portHandle:longint; writeSize:PLongint):longint;cdecl;external aionlm name 'AIOGetWriteBufferSize';
function AIOGetWriteBufferSize(portHandle:longint; var writeSize:Longint):longint;cdecl;external aionlm name 'AIOGetWriteBufferSize';

function AIOReadData(portHandle:longint; buffer:Pchar; length:longint; numberBytesRead:PLongint):longint;cdecl;external aionlm name 'AIOReadData';
function AIOReadData(portHandle:longint; var buffer; length:longint; var numberBytesRead:Longint):longint;cdecl;external aionlm name 'AIOReadData';

function AIOReadStatus(portHandle:longint; count:PLongint; state:PWORD):longint;cdecl;external aionlm name 'AIOReadStatus';
function AIOReadStatus(portHandle:longint; var count:Longint; var state:WORD):longint;cdecl;external aionlm name 'AIOReadStatus';

function AIOReleasePort(portHandle:longint):longint;cdecl;external aionlm name 'AIOReleasePort';
function AIOSetControlData(portHandle:longint; requestType:longint; requestStructValue:PAIOCONTROLDATA):longint;cdecl;external aionlm name 'AIOSetControlData';
function AIOSetControlData(portHandle:longint; requestType:longint; var requestStructValue:TAIOCONTROLDATA):longint;cdecl;external aionlm name 'AIOSetControlData';

function AIOSetExternalControl(portHandle:longint; requestType:longint; requestValue:longint):longint;cdecl;external aionlm name 'AIOSetExternalControl';
function AIOSetFlowControl(portHandle:longint; flowCtrlMode:longint):longint;cdecl;external aionlm name 'AIOSetFlowControl';
function AIOSetFlowControlCharacters(portHandle:longint; transmitXon:byte; transmitXoff:byte; receiveXon:byte; receiveXoff:byte):longint;cdecl;external aionlm name 'AIOSetFlowControlCharacters';
function AIOSetReadBufferSize(portHandle:longint; bufferSize:longint):longint;cdecl;external aionlm name 'AIOSetReadBufferSize';
function AIOSetWriteBufferSize(portHandle:longint; bufferSize:longint):longint;cdecl;external aionlm name 'AIOSetWriteBufferSize';

function AIOWriteData(portHandle:longint; buffer:Pchar; length:longint; numberBytesWritten:PLongint):longint;cdecl;external aionlm name 'AIOWriteData';
function AIOWriteData(portHandle:longint; var buffer; length:longint; var numberBytesWritten:Longint):longint;cdecl;external aionlm name 'AIOWriteData';

function AIOWriteStatus(portHandle:longint; count:PLongint; state:PWORD):longint;cdecl;external aionlm name 'AIOWriteStatus';
function AIOWriteStatus(portHandle:longint; var count:Longint; var state:WORD):longint;cdecl;external aionlm name 'AIOWriteStatus';


implementation


end.
