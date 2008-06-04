{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }
//------------------------------------------------------------------------------
//
//      Bluetooth Test Client
//
// Module Name:
//
//      bt_sdp.h
//
// Abstract:
//
//      This file contains constants and structures for Simple Discovery Protocol.
//
//------------------------------------------------------------------------------

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit bt_sdp;

interface

uses Windows;

const
      SDP_ERROR_INVALID_SDP_VERSION           = $0001;
      SDP_ERROR_INVALID_RECORD_HANDLE         = $0002;
      SDP_ERROR_INVALID_REQUEST_SYNTAX        = $0003;
      SDP_ERROR_INVALID_PDU_SIZE              = $0004;
      SDP_ERROR_INVALID_CONTINUATION_STATE    = $0005;
      SDP_ERROR_INSUFFICIENT_RESOURCES        = $0006;

const      
      SDP_ATTRIB_RECORD_HANDLE            = $0000;
      SDP_ATTRIB_CLASS_ID_LIST            = $0001;
      SDP_ATTRIB_RECORD_STATE             = $0002;
      SDP_ATTRIB_SERVICE_ID               = $0003;
      SDP_ATTRIB_PROTOCOL_DESCRIPTOR_LIST = $0004;
      SDP_ATTRIB_BROWSE_GROUP_LIST        = $0005;
      SDP_ATTRIB_LANG_BASE_ATTRIB_ID_LIST = $0006;
      SDP_ATTRIB_INFO_TIME_TO_LIVE        = $0007;
      SDP_ATTRIB_AVAILABILITY             = $0008;
      SDP_ATTRIB_PROFILE_DESCRIPTOR_LIST  = $0009;
      SDP_ATTRIB_DOCUMENTATION_URL        = $000A;
      SDP_ATTRIB_CLIENT_EXECUTABLE_URL    = $000B;
      SDP_ATTRIB_ICON_URL                 = $000C;

//
// Attribute IDs in the range of = $000D - = $01FF are reserved for future use
//
      SDP_ATTRIB_PROFILE_SPECIFIC                     = $0200;

      STRING_NAME_OFFSET                              = $0000;
      STRING_DESCRIPTION_OFFSET                       = $0001;
      STRING_PROVIDER_NAME_OFFSET                     = $0002;

      SDP_ATTRIB_SDP_VERSION_NUMBER_LIST              = $0200;
      SDP_ATTRIB_SDP_DATABASE_STATE                   = $0201;

      SDP_ATTRIB_BROWSE_GROUP_ID                      = $0200;

      SDP_ATTRIB_CORDLESS_EXTERNAL_NETWORK            = $0301;

      SDP_ATTRIB_FAX_CLASS_1_SUPPORT                  = $0302;
      SDP_ATTRIB_FAX_CLASS_2_0_SUPPORT                = $0303;
      SDP_ATTRIB_FAX_CLASS_2_SUPPORT                  = $0304;
      SDP_ATTRIB_FAX_AUDIO_FEEDBACK_SUPPORT           = $0305;

      SDP_ATTRIB_HEADSET_REMOTE_AUDIO_VOLUME_CONTROL  = $0302;

      SDP_ATTRIB_LAN_LPSUBNET                         = $0200;

      SDP_ATTRIB_OBJECT_PUSH_SUPPORTED_FORMATS_LIST   = $0303;

      SDP_ATTRIB_SYNCH_SUPPORTED_DATA_STORES_LIST     = $0301;

//  this is in the assigned numbers doc, but it does not show up in any profile
      SDP_ATTRIB_SERVICE_VERSION                      = $0300;

      SDP_ATTRIB_PAN_SECURITY_DESCRIPTION             = $030A;
      SDP_ATTRIB_PAN_NET_ACCESS_TYPE                  = $030B;
      SDP_ATTRIB_PAN_MAX_NET_ACCESS_RATE              = $030C;
      SDP_ATTRIB_PAN_IPV4_SUBNET                      = $030D;
      SDP_ATTRIB_PAN_IPV6_SUBNET                      = $030E;


// Bluetooth base UUID for service discovery
const
//      Bluetooth_Base_UUID:GUID = '{00000000-0000-1000-7007-00805F9B34FB}';
      Bluetooth_Base_UUID :GUID = '{00000000-0000-1000-8000-00805F9B34FB}';
      
      SDP_PROTOCOL_UUID   :GUID = '{00000001-0000-1000-8000-00805F9B34FB}';
      UDP_PROTOCOL_UUID   :GUID = '{00000002-0000-1000-8000-00805F9B34FB}';
      RFCOMM_PROTOCOL_UUID:GUID = '{00000003-0000-1000-8000-00805F9B34FB}';
      TCP_PROTOCOL_UUID   :GUID = '{00000004-0000-1000-8000-00805F9B34FB}';
      TCSBIN_PROTOCOL_UUID:GUID = '{00000005-0000-1000-8000-00805F9B34FB}';
      TCSAT_PROTOCOL_UUID :GUID = '{00000006-0000-1000-8000-00805F9B34FB}';
      OBEX_PROTOCOL_UUID  :GUID = '{00000008-0000-1000-8000-00805F9B34FB}';
      IP_PROTOCOL_UUID    :GUID = '{00000009-0000-1000-8000-00805F9B34FB}';
      FTP_PROTOCOL_UUID   :GUID = '{0000000A-0000-1000-8000-00805F9B34FB}';
      HTTP_PROTOCOL_UUID  :GUID = '{0000000C-0000-1000-8000-00805F9B34FB}';
      WSP_PROTOCOL_UUID   :GUID = '{0000000E-0000-1000-8000-00805F9B34FB}';
      BNEP_PROTOCOL_UUID  :GUID = '{0000000F-0000-1000-8000-00805F9B34FB}';
      L2CAP_PROTOCOL_UUID :GUID = '{00000100-0000-1000-8000-00805F9B34FB}';

      ServiceDiscoveryServerServiceClassID_UUID :GUID = '{00001000-0000-1000-8008-00805F9B34FB}';
      BrowseGroupDescriptorServiceClassID_UUID  :GUID = '{00001001-0000-1000-8000-00805F9B34FB}';
      PublicBrowseGroupServiceClass_UUID        :GUID = '{00001002-0000-1000-8000-00805F9B34FB}';
      SerialPortServiceClass_UUID               :GUID = '{00001101-0000-1000-8000-00805F9B34FB}';
      LANAccessUsingPPPServiceClass_UUID        :GUID = '{00001102-0000-1000-8000-00805F9B34FB}';
      DialupNetworkingServiceClass_UUID         :GUID = '{00001103-0000-1000-8000-00805F9B34FB}';
      IrMCSyncServiceClass_UUID                 :GUID = '{00001104-0000-1000-8000-00805F9B34FB}';
      OBEXObjectPushServiceClass_UUID           :GUID = '{00001105-0000-1000-8000-00805F9B34FB}';
      OBEXFileTransferServiceClass_UUID         :GUID = '{00001106-0000-1000-8000-00805F9B34FB}';
      IrMCSyncCommandServiceClass_UUID          :GUID = '{00001107-0000-1000-8000-00805F9B34FB}';
      HeadsetServiceClass_UUID                  :GUID = '{00001108-0000-1000-8000-00805F9B34FB}';
      CordlessTelephonyServiceClass_UUID        :GUID = '{00001109-0000-1000-8000-00805F9B34FB}';
      AudioSourceServiceClass_UUID              :GUID = '{0000110A-0000-1000-8000-00805F9B34FB}';
      AudioSinkServiceClass_UUID                :GUID = '{0000110B-0000-1000-8000-00805F9B34FB}';
      AV_RemoteControlTargetServiceClass_UUID   :GUID = '{0000110C-0000-1000-8000-00805F9B34FB}';
      AdvancedAudioDistributionServiceClass_UUID:GUID = '{0000110D-0000-1000-8000-00805F9B34FB}';
      AV_RemoteControlServiceClass_UUID         :GUID = '{0000110E-0000-1000-8000-00805F9B34FB}';
      VideoConferencingServiceClass_UUID        :GUID = '{0000110F-0000-1000-8000-00805F9B34FB}';
      IntercomServiceClass_UUID                 :GUID = '{00001110-0000-1000-8000-00805F9B34FB}';
      FaxServiceClass_UUID                      :GUID = '{00001111-0000-1000-8000-00805F9B34FB}';
      HeadsetAudioGatewayServiceClass_UUID      :GUID = '{00001112-0000-1000-8000-00805F9B34FB}';
      PANUServiceClass_UUID                     :GUID = '{00001115-0000-1000-8000-00805F9B34FB}';
      NAPServiceClass_UUID                      :GUID = '{00001116-0000-1000-8000-00805F9B34FB}';
      GNServiceClass_UUID                       :GUID = '{00001117-0000-1000-8000-00805F9B34FB}';
      HandsfreeServiceClass_UUID                :GUID = '{0000111E-0000-1000-8000-00805F9B34FB}';
      HandsfreeAudioGatewayServiceClass_UUID    :GUID = '{0000111F-0000-1000-8000-00805F9B34FB}';
      PnPInformationServiceClass_UUID           :GUID = '{00001200-0000-1000-8000-00805F9B34FB}';
      GenericNetworkingServiceClass_UUID        :GUID = '{00001201-0000-1000-8000-00805F9B34FB}';
      GenericFileTransferServiceClass_UUID      :GUID = '{00001202-0000-1000-8000-00805F9B34FB}';
      GenericAudioServiceClass_UUID             :GUID = '{00001203-0000-1000-8000-00805F9B34FB}';
      GenericTelephonyServiceClass_UUID         :GUID = '{00001204-0000-1000-8000-00805F9B34FB}';


const
      SDP_PROTOCOL_UUID16     = $0001;
      UDP_PROTOCOL_UUID16     = $0002;
      RFCOMM_PROTOCOL_UUID16  = $0003;
      TCP_PROTOCOL_UUID16     = $0004;
      TCSBIN_PROTOCOL_UUID16  = $0005;
      TCSAT_PROTOCOL_UUID16   = $0006;
      OBEX_PROTOCOL_UUID16    = $0008;
      IP_PROTOCOL_UUID16      = $0009;
      FTP_PROTOCOL_UUID16     = $000A;
      HTTP_PROTOCOL_UUID16    = $000C;
      WSP_PROTOCOL_UUID16     = $000E;
      BNEP_PROTOCOL_UUID16    = $000F;
      HID_PROTOCOL_UUID16     = $0011;
      AVCTP_PROTOCOL_UUID16   = $0017;
      AVDTP_PROTOCOL_UUID16   = $0019;
      L2CAP_PROTOCOL_UUID16   = $0100;

      ServiceDiscoveryServerServiceClassID_UUID16     = $1000;
      BrowseGroupDescriptorServiceClassID_UUID16      = $1001;
      PublicBrowseGroupServiceClassID_UUID16          = $1002;
      SerialPortServiceClassID_UUID16                 = $1101;
      LANAccessUsingPPPServiceClassID_UUID16          = $1102;
      DialupNetworkingServiceClassID_UUID16           = $1103;
      IrMCSyncServiceClassID_UUID16                   = $1104;
      OBEXObjectPushServiceClassID_UUID16             = $1105;
      OBEXFileTransferServiceClassID_UUID16           = $1106;
      IrMcSyncCommandServiceClassID_UUID16            = $1107;
      HeadsetServiceClassID_UUID16                    = $1108;
      CordlessServiceClassID_UUID16                   = $1109;
      AudioSourceServiceClassID_UUID16                = $110A;
      AudioSinkServiceClassID_UUID16                  = $110B;
      AV_RemoteControlTargetServiceClassID_UUID16     = $110C;
      AdvancedAudioDistributionServiceClassID_UUID16  = $110D;
      AV_RemoteControlServiceClassID_UUID16           = $110E;
      VideoConferencingServiceClassID_UUID16          = $110F;
      IntercomServiceClassID_UUID16                   = $1110;
      FaxServiceClassID_UUID16                        = $1111;
      HeadsetAudioGatewayServiceClassID_UUID16        = $1112;
      PANUServiceClassID_UUID16                       = $1115;
      NAPServiceClassID_UUID16                        = $1116;
      GNServiceClassID_UUID16                         = $1117;
      HandsfreeServiceClassID_UUID16                  = $111E;
      HandsfreeAudioGatewayServiceClassID_UUID16      = $111F;
      PnPInformationServiceClassID_UUID16             = $1200;
      GenericNetworkingServiceClassID_UUID16          = $1201;
      GenericFileTransferServiceClassID_UUID16        = $1202;
      GenericAudioServiceClassID_UUID16               = $1203;
      GenericTelephonyServiceClassID_UUID16           = $1204;

      PSM_SDP                 = $0001;
      PSM_RFCOMM              = $0003;
      PSM_TCS_BIN             = $0005;
      PSM_TCS_BIN_CORDLESS    = $0007;


// HID specific SDP attrib ids.
const
      SDP_ATTRIB_HID_DEVICE_RELEASE_NUMBER    = $0200;
      SDP_ATTRIB_HID_PARSER_VERSION           = $0201;
      SDP_ATTRIB_HID_DEVICE_SUBCLASS          = $0202;
      SDP_ATTRIB_HID_COUNTRY_CODE             = $0203;
      SDP_ATTRIB_HID_VIRTUAL_CABLE            = $0204;
      SDP_ATTRIB_HID_RECONNECT_INITIATE       = $0205;
      SDP_ATTRIB_HID_DESCRIPTOR_LIST          = $0206;
      SDP_ATTRIB_HID_LANGID_BASE_LIST         = $0207;
      SDP_ATTRIB_HID_SDP_DISABLE              = $0208;
      SDP_ATTRIB_HID_BATTERY_POWER            = $0209;
      SDP_ATTRIB_HID_REMOTE_WAKE              = $020A;
      SDP_ATTRIB_HID_PROFILE_VERSION          = $020B;
      SDP_ATTRIB_HID_SUPERVISION_TIMEOUT      = $020C;
      SDP_ATTRIB_HID_NORMALLY_CONNECTABLE     = $020D;
      SDP_ATTRIB_HID_BOOT_DEVICE              = $020E;


implementation

end.