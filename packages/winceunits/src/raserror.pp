{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }
//
// Module Name:
//
//   raserror.h
//
// Abstract:
//
//   Remote Access external API
//   RAS specific error codes
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit RASError;

interface

const
      RASBASE = 600;
      SUCCESS = 0;


      PENDING                              = RASBASE + 0;
      ERROR_INVALID_PORT_HANDLE            = RASBASE + 1;
      ERROR_PORT_ALREADY_OPEN              = RASBASE + 2;
      ERROR_BUFFER_TOO_SMALL               = RASBASE + 3;
      ERROR_WRONG_INFO_SPECIFIED           = RASBASE + 4;
      ERROR_CANNOT_SET_PORT_INFO           = RASBASE + 5;
      ERROR_PORT_NOT_CONNECTED             = RASBASE + 6;
      ERROR_EVENT_INVALID                  = RASBASE + 7;
      ERROR_DEVICE_DOES_NOT_EXIST          = RASBASE + 8;
      ERROR_DEVICETYPE_DOES_NOT_EXIST      = RASBASE + 9;
      ERROR_BUFFER_INVALID                 = RASBASE + 10;
      ERROR_ROUTE_NOT_AVAILABLE            = RASBASE + 11;
      ERROR_ROUTE_NOT_ALLOCATED            = RASBASE + 12;
      ERROR_INVALID_COMPRESSION_SPECIFIED  = RASBASE + 13;
      ERROR_OUT_OF_BUFFERS                 = RASBASE + 14;
      ERROR_PORT_NOT_FOUND                 = RASBASE + 15;
      ERROR_ASYNC_REQUEST_PENDING          = RASBASE + 16;
      ERROR_ALREADY_DISCONNECTING          = RASBASE + 17;
      ERROR_PORT_NOT_OPEN                  = RASBASE + 18;
      ERROR_PORT_DISCONNECTED              = RASBASE + 19;
      ERROR_NO_ENDPOINTS                   = RASBASE + 20;
      ERROR_CANNOT_OPEN_PHONEBOOK          = RASBASE + 21;
      ERROR_CANNOT_LOAD_PHONEBOOK          = RASBASE + 22;
      ERROR_CANNOT_FIND_PHONEBOOK_ENTRY    = RASBASE + 23;
      ERROR_CANNOT_WRITE_PHONEBOOK         = RASBASE + 24;
      ERROR_CORRUPT_PHONEBOOK              = RASBASE + 25;
      ERROR_CANNOT_LOAD_STRING             = RASBASE + 26;
      ERROR_KEY_NOT_FOUND                  = RASBASE + 27;
      ERROR_DISCONNECTION                  = RASBASE + 28;
      ERROR_REMOTE_DISCONNECTION           = RASBASE + 29;
      ERROR_HARDWARE_FAILURE               = RASBASE + 30;
      ERROR_USER_DISCONNECTION             = RASBASE + 31;
      ERROR_INVALID_SIZE                   = RASBASE + 32;
      ERROR_PORT_NOT_AVAILABLE             = RASBASE + 33;
      ERROR_CANNOT_PROJECT_CLIENT          = RASBASE + 34;
      ERROR_UNKNOWN                        = RASBASE + 35;
      ERROR_WRONG_DEVICE_ATTACHED          = RASBASE + 36;
      ERROR_BAD_STRING                     = RASBASE + 37;
      ERROR_REQUEST_TIMEOUT                = RASBASE + 38;
      ERROR_CANNOT_GET_LANA                = RASBASE + 39;
      ERROR_NETBIOS_ERROR                  = RASBASE + 40;
      ERROR_SERVER_OUT_OF_RESOURCES        = RASBASE + 41;
      ERROR_NAME_EXISTS_ON_NET             = RASBASE + 42;
      ERROR_SERVER_GENERAL_NET_FAILURE     = RASBASE + 43;
      WARNING_MSG_ALIAS_NOT_ADDED          = RASBASE + 44;
      ERROR_AUTH_INTERNAL                  = RASBASE + 45;
      ERROR_RESTRICTED_LOGON_HOURS         = RASBASE + 46;
      ERROR_ACCT_DISABLED                  = RASBASE + 47;
      ERROR_PASSWD_EXPIRED                 = RASBASE + 48;
      ERROR_NO_DIALIN_PERMISSION           = RASBASE + 49;
      ERROR_SERVER_NOT_RESPONDING          = RASBASE + 50;
      ERROR_FROM_DEVICE                    = RASBASE + 51;
      ERROR_UNRECOGNIZED_RESPONSE          = RASBASE + 52;
      ERROR_MACRO_NOT_FOUND                = RASBASE + 53;
      ERROR_MACRO_NOT_DEFINED              = RASBASE + 54;
      ERROR_MESSAGE_MACRO_NOT_FOUND        = RASBASE + 55;
      ERROR_DEFAULTOFF_MACRO_NOT_FOUND     = RASBASE + 56;
      ERROR_FILE_COULD_NOT_BE_OPENED       = RASBASE + 57;
      ERROR_DEVICENAME_TOO_LONG            = RASBASE + 58;
      ERROR_DEVICENAME_NOT_FOUND           = RASBASE + 59;
      ERROR_NO_RESPONSES                   = RASBASE + 60;
      ERROR_NO_COMMAND_FOUND               = RASBASE + 61;
      ERROR_WRONG_KEY_SPECIFIED            = RASBASE + 62;
      ERROR_UNKNOWN_DEVICE_TYPE            = RASBASE + 63;
      ERROR_ALLOCATING_MEMORY              = RASBASE + 64;
      ERROR_PORT_NOT_CONFIGURED            = RASBASE + 65;
      ERROR_DEVICE_NOT_READY               = RASBASE + 66;
      ERROR_READING_INI_FILE               = RASBASE + 67;
      ERROR_NO_CONNECTION                  = RASBASE + 68;
      ERROR_BAD_USAGE_IN_INI_FILE          = RASBASE + 69;
      ERROR_READING_SECTIONNAME            = RASBASE + 70;
      ERROR_READING_DEVICETYPE             = RASBASE + 71;
      ERROR_READING_DEVICENAME             = RASBASE + 72;
      ERROR_READING_USAGE                  = RASBASE + 73;
      ERROR_READING_MAXCONNECTBPS          = RASBASE + 74;
      ERROR_READING_MAXCARRIERBPS          = RASBASE + 75;
      ERROR_LINE_BUSY                      = RASBASE + 76;
      ERROR_VOICE_ANSWER                   = RASBASE + 77;
      ERROR_NO_ANSWER                      = RASBASE + 78;
      ERROR_NO_CARRIER                     = RASBASE + 79;
      ERROR_NO_DIALTONE                    = RASBASE + 80;
      ERROR_IN_COMMAND                     = RASBASE + 81;
      ERROR_WRITING_SECTIONNAME            = RASBASE + 82;
      ERROR_WRITING_DEVICETYPE             = RASBASE + 83;
      ERROR_WRITING_DEVICENAME             = RASBASE + 84;
      ERROR_WRITING_MAXCONNECTBPS          = RASBASE + 85;
      ERROR_WRITING_MAXCARRIERBPS          = RASBASE + 86;
      ERROR_WRITING_USAGE                  = RASBASE + 87;
      ERROR_WRITING_DEFAULTOFF             = RASBASE + 88;
      ERROR_READING_DEFAULTOFF             = RASBASE + 89;
      ERROR_EMPTY_INI_FILE                 = RASBASE + 90;
      ERROR_AUTHENTICATION_FAILURE         = RASBASE + 91;
      ERROR_PORT_OR_DEVICE                 = RASBASE + 92;
      ERROR_NOT_BINARY_MACRO               = RASBASE + 93;
      ERROR_DCB_NOT_FOUND                  = RASBASE + 94;
      ERROR_STATE_MACHINES_NOT_STARTED     = RASBASE + 95;
      ERROR_STATE_MACHINES_ALREADY_STARTED = RASBASE + 96;
      ERROR_PARTIAL_RESPONSE_LOOPING       = RASBASE + 97;
      ERROR_UNKNOWN_RESPONSE_KEY           = RASBASE + 98;
      ERROR_RECV_BUF_FULL                  = RASBASE + 99;
      ERROR_CMD_TOO_LONG                   = RASBASE + 100;
      ERROR_UNSUPPORTED_BPS                = RASBASE + 101;
      ERROR_UNEXPECTED_RESPONSE            = RASBASE + 102;
      ERROR_INTERACTIVE_MODE               = RASBASE + 103;
      ERROR_BAD_CALLBACK_NUMBER            = RASBASE + 104;
      ERROR_INVALID_AUTH_STATE             = RASBASE + 105;
      ERROR_WRITING_INITBPS                = RASBASE + 106;
      ERROR_X25_DIAGNOSTIC                 = RASBASE + 107;
      ERROR_ACCT_EXPIRED                   = RASBASE + 108;
      ERROR_CHANGING_PASSWORD              = RASBASE + 109;
      ERROR_OVERRUN                        = RASBASE + 110;
      ERROR_RASMAN_CANNOT_INITIALIZE	      = RASBASE + 111;
      ERROR_BIPLEX_PORT_NOT_AVAILABLE      = RASBASE + 112;
      ERROR_NO_ACTIVE_ISDN_LINES           = RASBASE + 113;
      ERROR_NO_ISDN_CHANNELS_AVAILABLE     = RASBASE + 114;
      ERROR_TOO_MANY_LINE_ERRORS           = RASBASE + 115;
      ERROR_IP_CONFIGURATION               = RASBASE + 116;
      ERROR_NO_IP_ADDRESSES                = RASBASE + 117;
      ERROR_PPP_TIMEOUT                    = RASBASE + 118;
      ERROR_PPP_REMOTE_TERMINATED          = RASBASE + 119;
      ERROR_PPP_NO_PROTOCOLS_CONFIGURED    = RASBASE + 120;
      ERROR_PPP_NO_RESPONSE                = RASBASE + 121;
      ERROR_PPP_INVALID_PACKET             = RASBASE + 122;
      ERROR_PHONE_NUMBER_TOO_LONG          = RASBASE + 123;
      ERROR_IPXCP_NO_DIALOUT_CONFIGURED    = RASBASE + 124;
      ERROR_IPXCP_NO_DIALIN_CONFIGURED     = RASBASE + 125;
      ERROR_IPXCP_DIALOUT_ALREADY_ACTIVE   = RASBASE + 126;
      ERROR_ACCESSING_TCPCFGDLL            = RASBASE + 127;
      ERROR_NO_IP_RAS_ADAPTER              = RASBASE + 128;
      ERROR_SLIP_REQUIRES_IP               = RASBASE + 129;
      ERROR_PROJECTION_NOT_COMPLETE        = RASBASE + 130;
      ERROR_PROTOCOL_NOT_CONFIGURED        = RASBASE + 131;
      ERROR_PPP_NOT_CONVERGING             = RASBASE + 132;
      ERROR_PPP_CP_REJECTED                = RASBASE + 133;
      ERROR_PPP_LCP_TERMINATED             = RASBASE + 134;
      ERROR_PPP_REQUIRED_ADDRESS_REJECTED  = RASBASE + 135;
      ERROR_PPP_NCP_TERMINATED             = RASBASE + 136;
      ERROR_PPP_LOOPBACK_DETECTED          = RASBASE + 137;
      ERROR_PPP_NO_ADDRESS_ASSIGNED        = RASBASE + 138;
      ERROR_CANNOT_USE_LOGON_CREDENTIALS   = RASBASE + 139;
      ERROR_TAPI_CONFIGURATION             = RASBASE + 140;
      ERROR_NO_LOCAL_ENCRYPTION            = RASBASE + 141;
      ERROR_NO_REMOTE_ENCRYPTION           = RASBASE + 142;
      ERROR_REMOTE_REQUIRES_ENCRYPTION     = RASBASE + 143;
      ERROR_IPXCP_NET_NUMBER_CONFLICT      = RASBASE + 144;
      ERROR_INVALID_SMM                    = RASBASE + 145;
      ERROR_SMM_UNINITIALIZED              = RASBASE + 146;
      ERROR_NO_MAC_FOR_PORT                = RASBASE + 147;
      ERROR_SMM_TIMEOUT                    = RASBASE + 148;
      ERROR_BAD_PHONE_NUMBER               = RASBASE + 149;
      ERROR_WRONG_MODULE                   = RASBASE + 150;
      ERROR_PPP_MAC						                  = RASBASE + 151;
      ERROR_PPP_LCP						                  = RASBASE + 152;
      ERROR_PPP_AUTH						                 = RASBASE + 153;
      ERROR_PPP_NCP						                  = RASBASE + 154;
      ERROR_POWER_OFF						                = RASBASE + 155;
      ERROR_POWER_OFF_CD					              = RASBASE + 156;


      ERROR_DIAL_ALREADY_IN_PROGRESS       = RASBASE + 157;
      ERROR_RASAUTO_CANNOT_INITIALIZE      = RASBASE + 158;
      ERROR_UNABLE_TO_AUTHENTICATE_SERVER  = RASBASE + 178;


      RASBASEEND                           = RASBASE + 158;

      ROUTEBASE                            = 900;

      ERROR_IDLE_DISCONNECTED              = ROUTEBASE + 26;
// The port has been disconnected due to inactivity.%0

implementation

end.