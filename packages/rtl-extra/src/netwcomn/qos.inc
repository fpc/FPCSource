{
    This file is part of the Free Pascal run time library.
    This unit contains the declarations for the WinSock2
    Socket Library for Netware and Win32

    Copyright (c) 1999-2002 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    { This module defines the Quality of Service structures and types used
      by Winsock applications. }

  {
      Definitions for valued-based Service Type for each direction of data flow.
    }

  type

     SERVICETYPE = u_long;
  { No data in this direction  }

  const
     SERVICETYPE_NOTRAFFIC = $00000000;
  { Best Effort  }
     SERVICETYPE_BESTEFFORT = $00000001;
  { Controlled Load  }
     SERVICETYPE_CONTROLLEDLOAD = $00000002;
  { Guaranteed  }
     SERVICETYPE_GUARANTEED = $00000003;
  { Used to notify
                                                             change to user  }
     SERVICETYPE_NETWORK_UNAVAILABLE = $00000004;
  { corresponds to
                                                             "General Parameters"
                                                             defined by IntServ  }
     SERVICETYPE_GENERAL_INFORMATION = $00000005;
  { used to indicate
                                                             that the flow spec
                                                             contains no change
                                                             from any previous
                                                             one  }
     SERVICETYPE_NOCHANGE = $00000006;
  { Non-Conforming Traffic  }
     SERVICETYPE_NONCONFORMING = $00000009;
  { Custom ServiceType 1  }
     SERVICETYPE_CUSTOM1 = $0000000A;
  { Custom ServiceType 2  }
     SERVICETYPE_CUSTOM2 = $0000000B;
  { Custom ServiceType 3  }
     SERVICETYPE_CUSTOM3 = $0000000C;
  { Custom ServiceType 4  }
     SERVICETYPE_CUSTOM4 = $0000000D;
  {
      Definitions for bitmap-based Service Type for each direction of data flow.
    }
     SERVICE_BESTEFFORT = $80020000;
     SERVICE_CONTROLLEDLOAD = $80040000;
     SERVICE_GUARANTEED = $80080000;
     SERVICE_CUSTOM1 = $80100000;
     SERVICE_CUSTOM2 = $80200000;
     SERVICE_CUSTOM3 = $80400000;
     SERVICE_CUSTOM4 = $80800000;
  {
      Number of available Service Types.
    }
     NUM_SERVICETYPES = 8;
  {
     to turn on immediate traffic control, OR ( | ) this flag with the
     ServiceType field in the FLOWSPEC
    }
  { #define SERVICE_IMMEDIATE_TRAFFIC_CONTROL   0x80000000   // obsolete }
     SERVICE_NO_TRAFFIC_CONTROL = $81000000;
  {
     this flag can be used with the immediate traffic control flag above to
     prevent any rsvp signaling messages from being sent. Local traffic
     control will be invoked, but no RSVP Path messages will be sent.This flag
     can also be used in conjunction with a receiving flowspec to suppress
     the automatic generation of a Reserve message.  The application would
     receive notification that a Path  message had arrived and would then need
     to alter the QOS by issuing WSAIoctl( SIO_SET_QOS ), to unset this flag
     and thereby cause Reserve messages to go out.
    }
     SERVICE_NO_QOS_SIGNALING = $40000000;
  { rsvp status code  }
     STATUS_QOS_RELEASED = $10101010;
  {
      Flow Specifications for each direction of data flow.
    }
  { In Bytes/sec  }
  { In Bytes  }
  { In Bytes/sec  }
  { In microseconds  }
  { In microseconds  }
  { In Bytes  }
  { In Bytes  }

  type

     Tflowspec = record
          TokenRate : u_long;
          TokenBucketSize : u_long;
          PeakBandwidth : u_long;
          Latency : u_long;
          DelayVariation : u_long;
          ServiceType : SERVICETYPE;
          MaxSduSize : u_long;
          MinimumPolicedSize : u_long;
       end;
     PFLOWSPEC = ^Tflowspec;
     LPFLOWSPEC = ^Tflowspec;
  {
     this value can be used in the FLOWSPEC structure to instruct the Rsvp Service
     provider to derive the appropriate default value for the parameter.  Note
     that not all values in the FLOWSPEC structure can be defaults. In the
     ReceivingFlowspec, all parameters can be defaulted except the ServiceType.
     In the SendingFlowspec, the MaxSduSize and MinimumPolicedSize can be
     defaulted. Other defaults may be possible. Refer to the appropriate
     documentation.
    }

  const
     QOS_NOT_SPECIFIED = $FFFFFFFF;
     NULL_QOS_TYPE = $FFFFFFFD;
  {
     define a value that can be used for the PeakBandwidth, which will map into
     positive infinity when the FLOWSPEC is converted into IntServ floating point
     format.  We can't use (-1) because that value was previously defined to mean
     "select the default".
    }
     POSITIVE_INFINITY_RATE = $FFFFFFFE;
  {
     the provider specific structure can have a number of objects in it.
     Each next structure in the
     ProviderSpecific will be the QOS_OBJECT_HDR struct that prefaces the actual
     data with a type and length for that object.  This QOS_OBJECT struct can
     repeat several times if there are several objects.  This list of objects
     terminates either when the buffer length has been reached ( WSABUF ) or
     an object of type QOS_END_OF_LIST is encountered.
    }
  { the length of object buffer INCLUDING
                                this header  }

  type

     TQOS_OBJECT_HDR = record
          ObjectType : u_long;
          ObjectLength : u_long;
       end;
     LPQOS_OBJECT_HDR = ^TQOS_OBJECT_HDR;
     PQOS_OBJECT_HDR = ^TQOS_OBJECT_HDR;
  {
     general QOS objects start at this offset from the base and have a range
     of 1000
    }

  const
     QOS_GENERAL_ID_BASE = 2000;
     QOS_OBJECT_PRIORITY = $00000000 + QOS_GENERAL_ID_BASE;
  { QOS_PRIORITY structure passed  }
     QOS_OBJECT_END_OF_LIST = $00000001 + QOS_GENERAL_ID_BASE;
  { QOS_End_of_list structure passed  }
     QOS_OBJECT_SD_MODE = $00000002 + QOS_GENERAL_ID_BASE;
  { QOS_ShapeDiscard structure passed  }
     QOS_OBJECT_TRAFFIC_CLASS = $00000003 + QOS_GENERAL_ID_BASE;
  { QOS_Traffic class structure passed  }
     QOS_OBJECT_DESTADDR = $00000004 + QOS_GENERAL_ID_BASE;
  { QOS_DestAddr structure  }
     QOS_OBJECT_SHAPER_QUEUE_DROP_MODE = $00000005 + QOS_GENERAL_ID_BASE;
  { QOS_ShaperQueueDropMode structure  }
     QOS_OBJECT_SHAPER_QUEUE_LIMIT = $00000006 + QOS_GENERAL_ID_BASE;
  { QOS_ShaperQueueLimit structure  }
  {
     This structure defines the absolute priorty of the flow.  Priorities in the
     range of 0-7 are currently defined. Receive Priority is not currently used,
     but may at some point in the future.
    }
  { this gets mapped to layer 2 priority. }
  { there are none currently defined. }
  { this could be used to decide who
                                           gets forwarded up the stack first
                                           - not used now  }

  type

     TQOS_PRIORITY = record
          ObjectHdr : TQOS_OBJECT_HDR;
          SendPriority : u_char;
          SendFlags : u_char;
          ReceivePriority : u_char;
          Unused : u_char;
       end;
     LPQOS_PRIORITY = ^TQOS_PRIORITY;
     PQOS_PRIORITY  = ^TQOS_PRIORITY;
  {
     This structure is used to define the behaviour that the traffic
     control packet shaper will apply to the flow.

     PS_NONCONF_BORROW - the flow will receive resources remaining
      after all higher priority flows have been serviced. If a
      TokenRate is specified, packets may be non-conforming and
      will be demoted to less than best-effort priority.

     PS_NONCONF_SHAPE - TokenRate must be specified. Non-conforming
      packets will be retianed in the packet shaper until they become
      conforming.

     PS_NONCONF_DISCARD - TokenRate must be specified. Non-conforming
      packets will be discarded.

    }

     TQOS_SD_MODE = record
          ObjectHdr        : TQOS_OBJECT_HDR;
          ShapeDiscardMode : u_long;
       end;
     LPQOS_SD_MODE = ^TQOS_SD_MODE;
     PQOS_SD_MODE  = ^TQOS_SD_MODE;

  const
     TC_NONCONF_BORROW = 0;
     TC_NONCONF_SHAPE = 1;
     TC_NONCONF_DISCARD = 2;
     TC_NONCONF_BORROW_PLUS = 3;
  {
     This structure may carry an 802.1 TrafficClass parameter which
     has been provided to the host by a layer 2 network, for example,
     in an 802.1 extended RSVP RESV message. If this object is obtained
     from the network, hosts will stamp the MAC headers of corresponding
     transmitted packets, with the value in the object. Otherwise, hosts
     may select a value based on the standard Intserv mapping of
     ServiceType to 802.1 TrafficClass.

    }

  type

     TQOS_TRAFFIC_CLASS = record
          ObjectHdr    : TQOS_OBJECT_HDR;
          TrafficClass : u_long;
       end;
     LPQOS_TRAFFIC_CLASS = ^TQOS_TRAFFIC_CLASS;
     PQOS_TRAFFIC_CLASS  = ^TQOS_TRAFFIC_CLASS;
  {
     This structure allows overriding of the default schema used to drop
     packets when a flow's shaper queue limit is reached.

     DropMethod -
        QOS_SHAPER_DROP_FROM_HEAD - Drop packets from
                the head of the queue until the new packet can be
                accepted into the shaper under the current limit.  This
                behavior is the default.
        QOS_SHAPER_DROP_INCOMING - Drop the incoming,
                limit-offending packet.

    }

     TQOS_SHAPER_QUEUE_LIMIT_DROP_MODE = record
          ObjectHdr : TQOS_OBJECT_HDR;
          DropMode  : u_long;
       end;
     LPQOS_SHAPER_QUEUE_LIMIT_DROP_MODE = ^TQOS_SHAPER_QUEUE_LIMIT_DROP_MODE;
     PQOS_SHAPER_QUEUE_LIMIT_DROP_MODE  = ^TQOS_SHAPER_QUEUE_LIMIT_DROP_MODE;

  const
     QOS_SHAPER_DROP_INCOMING = 0;
     QOS_SHAPER_DROP_FROM_HEAD = 1;

  {  This structure allows the default per-flow limit on the shaper queue
     size to be overridden.

     QueueSizeLimit - Limit, in bytes, of the size of the shaper queue }

  type

     TQOS_SHAPER_QUEUE_LIMIT = record
          ObjectHdr      : TQOS_OBJECT_HDR;
          QueueSizeLimit : u_long;
       end;
     LPQOS_SHAPER_QUEUE_LIMIT = ^TQOS_SHAPER_QUEUE_LIMIT;
     PQOS_SHAPER_QUEUE_LIMIT  = ^TQOS_SHAPER_QUEUE_LIMIT;

