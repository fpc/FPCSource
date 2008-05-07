{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{exported functions list = to do,
 * please remove functions done *
       ordinal    name

                  AddIPAddress
                  AllocateAndGetIfTableFromStack
                  AllocateAndGetIpAddrTableFromStack
                  CreateIpForwardEntry
                  CreateIpNetEntry
                  CreateProxyArpEntry
                  DeleteIPAddress
                  DeleteIpForwardEntry
                  DeleteIpNetEntry
                  DeleteProxyArpEntry
                  DllEntry
                  EnableRouter
                  FlushIpNetTable
                  GetAdapterIndex
                  GetAdapterOrderMap
                  GetAdaptersAddresses
                  GetBestInterface
                  GetBestInterfaceEx
                  GetBestRoute
                  GetFriendlyIfIndex
                  GetIcmpStatistics
                  GetIcmpStatisticsEx
                  GetIfEntry
                  GetIfTable
                  GetInterfaceInfo
                  GetIpAddrTable
                  GetIpForwardTable
                  GetIpNetTable
                  GetIpStatistics
                  GetIpStatisticsEx
                  GetNetworkParams
                  GetPerAdapterInfo
                  GetRTTAndHopCount
                  GetTcpStatistics
                  GetTcpStatisticsEx
                  GetTcpTable
                  GetUdpStatistics
                  GetUdpStatisticsEx
                  GetUdpTable
                  GetUniDirectionalAdapterInfo
                  Icmp6CreateFile
                  Icmp6ParseReplies
                  Icmp6SendEcho2
                  IcmpCloseHandle
                  IcmpCreateFile
                  IcmpParseReplies
                  IcmpSendEcho
                  IcmpSendEcho2
                  IpReleaseAddress
                  IpRenewAddress
                  IsLocalAddress
                  NotifyAddrChange
                  NotifyRouteChange
                  SendARP
                  SetIfEntry
                  SetIpForwardEntry
                  SetIpNetEntry
                  SetIpStatistics
                  SetIpTTL
                  SetTcpEntry
                  UnenableRouter
}

{$ifdef read_interface}

//*****************************************************************************
// consts
//*****************************************************************************
const
    IpHlpApiDLL       = 'iphlpapi';

    //dwForwardProto types
    PROTO_IP_OTHER    =  1;
    PROTO_IP_LOCAL    =  2;
    PROTO_IP_NETMGMT  =  3;
    PROTO_IP_ICMP     =  4;

    // Definitions and structures used by getnetworkparams and getadaptersinfo apis
    MAX_ADAPTER_DESCRIPTION_LENGTH  = 128;
    MAX_ADAPTER_NAME_LENGTH         = 256;
    MAX_ADAPTER_ADDRESS_LENGTH      = 8  ;
    DEFAULT_MINIMUM_ENTITIES        = 32 ;
    MAX_HOSTNAME_LEN                = 128;
    MAX_DOMAIN_NAME_LEN             = 128;
    MAX_SCOPE_ID_LEN                = 256;

    // Node Type

    BROADCAST_NODETYPE              =1;
    PEER_TO_PEER_NODETYPE           =2;
    MIXED_NODETYPE                  =4;
    HYBRID_NODETYPE                 =8;

//*****************************************************************************
// types
//*****************************************************************************

type
    // IP_ADDRESS_STRING
    IP_ADDRESS_STRING  = Array[0..3,0..3] of Char;
    TIP_ADDRESS_STRING = IP_ADDRESS_STRING;
    IP_MASK_STRING     = IP_ADDRESS_STRING;
    TIP_MASK_STRING    = IP_ADDRESS_STRING;
    PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
    PIP_MASK_STRING    = PIP_ADDRESS_STRING;

    // IP_ADDR_STRING
    PIP_ADDR_STRING = ^_IP_ADDR_STRING;
    _IP_ADDR_STRING = Record
      Next      : PIP_ADDR_STRING;
      IpAddress : IP_ADDRESS_STRING;
      IpMask    : IP_MASK_STRING;
      Context   : DWORD;
    end;
    IP_ADDR_STRING  = _IP_ADDR_STRING;
    TIP_ADDR_STRING = IP_ADDR_STRING;

    // ADAPTER_INFO
    PIP_ADAPTER_INFO = ^_IP_ADAPTER_INFO;
    _IP_ADAPTER_INFO = Record
      Next                : PIP_ADAPTER_INFO;
      ComboIndex          : DWORD;
      AdapterName         : Array[0..MAX_ADAPTER_NAME_LENGTH + 3] of Char;
      Description         : Array[0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of Char;
      AddressLength       : UINT;
      Address             : Array [0..MAX_ADAPTER_ADDRESS_LENGTH-1] of Byte;
      Index               : DWORD;
      aType               : UINT;
      DhcpEnabled         : UINT;
      CurrentIpAddress    : PIP_ADDR_STRING;
      IpAddressList       : IP_ADDR_STRING;
      GatewayList         : IP_ADDR_STRING;
      DhcpServer          : IP_ADDR_STRING;
      HaveWins            : BOOL;
      PrimaryWinsServer   : IP_ADDR_STRING;
      SecondaryWinsServer : IP_ADDR_STRING;
      LeaseObtained       : time_t;
      LeaseExpires        : time_t;
    end;
    IP_ADAPTER_INFO  = _IP_ADAPTER_INFO;
    TIP_ADAPTER_INFO = _IP_ADAPTER_INFO;

//*****************************************************************************
// functions
//*****************************************************************************

function GetNumberOfInterfaces(var dwNumIf : DWORD): DWORD; external IpHlpApiDLL name 'GetNumberOfInterfaces';
function GetAdaptersInfo( pAdapterInfo : PIP_ADAPTER_INFO; var OutBufLen : ULONG): DWORD; external IpHlpApiDLL name 'GetAdaptersInfo';

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}


