(* $OpenLDAP: pkg/ldap/include/ldap.h,v 1.168.2.16 2003/03/05 23:48:31 kurt Exp $ *)
(*
 * Copyright 1998-2003 The OpenLDAP Foundation, Redwood City, California, USA
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted only as authorized by the OpenLDAP
 * Public License.  A copy of this license is available at
 * http://www.OpenLDAP.org/license.html or in file LICENSE in the
 * top-level directory of the distribution.
 *)
(* Portions
 * Copyright (c) 1990 Regents of the University of Michigan.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of Michigan at Ann Arbor. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *)

const
  LDAP_VERSION1                     = 1;
  LDAP_VERSION2                     = 2;
  LDAP_VERSION3                     = 3;

  LDAP_VERSION_MIN                  = LDAP_VERSION2;
  LDAP_VERSION                      = LDAP_VERSION2;
  LDAP_VERSION_MAX                  = LDAP_VERSION3;

(*
 * We'll use 2000+draft revision for our API version number
 * As such, the number will be above the old RFC but below
 * whatever number does finally get assigned
 *)
const
  LDAP_API_VERSION                  = 3001;
  LDAP_VENDOR_NAME                  = 'OpenLDAP';

(* OpenLDAP API Features *)
  LDAP_API_FEATURE_X_OPENLDAP       = LDAP_VENDOR_VERSION;

{$IF defined(LDAP_API_FEATURE_X_OPENLDAP_REENTRANT) or (defined(LDAP_THREAD_SAFE) and defined(LDAP_API_FEATURE_X_OPENLDAP_THREAD_SAFE))}
    (* -lldap may or may not be thread safe *)
    (* -lldap_r, if available, is always thread safe *)
  {$DEFINE LDAP_API_FEATURE_THREAD_SAFE}
{$IFEND}
{$IF defined(LDAP_THREAD_SAFE) and defined(LDAP_API_FEATURE_X_OPENLDAP_THREAD_SAFE)}
  //{$DEFINE LDAP_API_FEATURE_SESSION_SAFE}
  //{$DEFINE LDAP_API_OPERATION_SESSION_SAFE}
{$IFEND}

  LDAP_PORT                         = 389;     (* ldap:///     default LDAP port *)
  LDAPS_PORT                        = 636;     (* ldaps:///    default LDAP over TLS port *)

  LDAP_ROOT_DSE                     = '';
  LDAP_NO_ATTRS                     = '1.1';
  LDAP_ALL_USER_ATTRIBUTES          = '*';
  LDAP_ALL_OPERATIONAL_ATTRIBUTES   = '+'; (* OpenLDAP extension *)

  LDAP_MAXINT                       = 2147483647;

(*
 * LDAP_OPTions defined by draft-ldapext-ldap-c-api-02
 * 0x0000 - 0x0fff reserved for api options
 * 0x1000 - 0x3fff reserved for api extended options
 * 0x4000 - 0x7fff reserved for private and experimental options
 *)
  LDAP_OPT_API_INFO                 = $0000;
  LDAP_OPT_DESC                     = $0001; (* deprecated *)
  LDAP_OPT_DEREF                    = $0002;
  LDAP_OPT_SIZELIMIT                = $0003;
  LDAP_OPT_TIMELIMIT                = $0004;
(* $05 - $07 not defined by current draft *)
  LDAP_OPT_REFERRALS                = $0008;
  LDAP_OPT_RESTART                  = $0009;
(* $0a - $10 not defined by current draft *)
  LDAP_OPT_PROTOCOL_VERSION         = $0011;
  LDAP_OPT_SERVER_CONTROLS          = $0012;
  LDAP_OPT_CLIENT_CONTROLS          = $0013;
(* $14 not defined by current draft *)
  LDAP_OPT_API_FEATURE_INFO         = $0015;

(* $16 - $2f not defined by current draft *)
  LDAP_OPT_HOST_NAME                = $0030;
  LDAP_OPT_RESULT_CODE              = $0031;
  LDAP_OPT_ERROR_NUMBER             = LDAP_OPT_RESULT_CODE;
  LDAP_OPT_ERROR_STRING             = $0032;
  LDAP_OPT_MATCHED_DN               = $0033;

(* $34 - $0fff not defined by current draft *)

(* private and experimental options *)
(* OpenLDAP specific options *)
  LDAP_OPT_DEBUG_LEVEL              = $5001;  (* debug level *)
  LDAP_OPT_TIMEOUT                  = $5002;  (* default timeout *)
  LDAP_OPT_REFHOPLIMIT              = $5003;  (* ref hop limit *)
  LDAP_OPT_NETWORK_TIMEOUT          = $5005;  (* socket level timeout *)
  LDAP_OPT_URI                      = $5006;
  LDAP_OPT_REFERRAL_URLS            = $5007;  (* Referral URLs *)

(* OpenLDAP TLS options *)
  LDAP_OPT_X_TLS                    = $6000;
  LDAP_OPT_X_TLS_CTX                = $6001;  (* SSL CTX *)
  LDAP_OPT_X_TLS_CACERTFILE         = $6002;
  LDAP_OPT_X_TLS_CACERTDIR          = $6003;
  LDAP_OPT_X_TLS_CERTFILE           = $6004;
  LDAP_OPT_X_TLS_KEYFILE            = $6005;
  LDAP_OPT_X_TLS_REQUIRE_CERT       = $6006;
(*   LDAP_OPT_X_TLS_PROTOCOL        = $6007; *)
  LDAP_OPT_X_TLS_CIPHER_SUITE       = $6008;
  LDAP_OPT_X_TLS_RANDOM_FILE        = $6009;
  LDAP_OPT_X_TLS_SSL_CTX            = $600a;
  LDAP_OPT_X_TLS_CRLCHECK           = $600b;
  LDAP_OPT_X_TLS_CONNECT_CB         = $6600c;
  LDAP_OPT_X_TLS_CONNECT_ARG        = $6600d;
  LDAP_OPT_X_TLS_DHFILE             = $6600e;

  LDAP_OPT_X_TLS_NEVER              = 0;
  LDAP_OPT_X_TLS_HARD               = 1;
  LDAP_OPT_X_TLS_DEMAND             = 2;
  LDAP_OPT_X_TLS_ALLOW              = 3;
  LDAP_OPT_X_TLS_TRY                = 4;

(* OpenLDAP SASL options *)
  LDAP_OPT_X_SASL_MECH              = $6100;
  LDAP_OPT_X_SASL_REALM             = $6101;
  LDAP_OPT_X_SASL_AUTHCID           = $6102;
  LDAP_OPT_X_SASL_AUTHZID           = $6103;
  LDAP_OPT_X_SASL_SSF               = $6104; (* read-only *)
  LDAP_OPT_X_SASL_SSF_EXTERNAL      = $6105; (* write-only *)
  LDAP_OPT_X_SASL_SECPROPS          = $6106; (* write-only *)
  LDAP_OPT_X_SASL_SSF_MIN           = $6107;
  LDAP_OPT_X_SASL_SSF_MAX           = $6108;
  LDAP_OPT_X_SASL_MAXBUFSIZE        = $6109;

(* Private API Extensions -- reserved for application use *)
  LDAP_OPT_PRIVATE_EXTENSION_BASE   = $7000;  (* Private API inclusive *)

(*
 * ldap_get_option() and ldap_set_option() return values.
 * As later versions may return other values indicating
 * failure, current applications should only compare returned
 * value against LDAP_OPT_SUCCESS.
 *)
  LDAP_OPT_SUCCESS                  =  0;
  LDAP_OPT_ERROR                    = -1;

(* on/off values *)
  LDAP_OPT_OFF                      = Pointer(0);

function LDAP_OPT_ON: Pointer;

const
  LDAP_API_INFO_VERSION             = 1;

type
  PLDAPAPIInfo                      = ^LDAPAPIInfo;
  LDAPAPIInfo                       = record
    ldapai_info_version     : cint;      (* version of LDAPAPIInfo (1) *)
    ldapai_api_version      : cint;      (* revision of API supported *)
    ldapai_protocol_version : cint;      (* highest LDAP version supported *)
    ldapai_extensions       : ppcchar;   (* names of API extensions *)
    ldapai_vendor_name      : pcchar;    (* name of supplier *)
    ldapai_vendor_version   : cint;      (* supplier-specific version * 100 *)
  end;

const
  LDAP_FEATURE_INFO_VERSION         = 1; (* version of api feature structure *)

type
  pldap_apifeature_info             = ^ldap_apifeature_info;
  ldap_apifeature_info              = record
    ldapaif_info_version    : cint;      (* version of this struct (1) *)
    ldapaif_name            : pcchar;    (* matches LDAP_API_FEATURE_... less the prefix *)
    ldapaif_version         : cint;      (* matches the value LDAP_API_FEATURE_... *)
  end;

  PLDAPAPIFeatureInfo               = ^LDAPAPIFeatureInfo;
  LDAPAPIFeatureInfo                = ldap_apifeature_info;

  PPPLDAPControl                    = ^PPLDAPControl;
  PPLDAPControl                     = ^PLDAPControl;
  PLDAPControl                      = ^LDAPControl;
  LDAPControl                       = record
    ldctl_oid               : pcchar;
    ldctl_value             : berval;
    ldctl_iscritical        : cchar;
  end;

const
(* LDAP Controls *)
(*  standard track controls *)
  LDAP_CONTROL_MANAGEDSAIT          = '2.16.840.1.113730.3.4.2'; (* RFC 3296 *)
  LDAP_CONTROL_SUBENTRIES           = '1.3.6.1.4.1.4203.1.10.1'; (* RFC 3672 *)
  LDAP_CONTROL_PAGEDRESULTS         = '1.2.840.113556.1.4.319';  (* RFC 2696 *)

  LDAP_CONTROL_VALUESRETURNFILTER   = '1.2.826.0.1.3344810.2.3'; (* RFC 3876 *)
  LDAP_CONTROL_X_VALUESRETURNFILTER = '1.2.826.0.1.334810.2.3';  (* bad OID *)

  LDAP_CONTROL_ASSERT               = '1.3.6.1.1.12';
  LDAP_CONTROL_PRE_READ             = '1.3.6.1.1.13.1';
  LDAP_CONTROL_POST_READ            = '1.3.6.1.1.13.2';

(*  standard track - not implemented in slapd(8) *)
  LDAP_CONTROL_SORTREQUEST          = '1.2.840.113556.1.4.473'; (* RFC 2891 *)
  LDAP_CONTROL_SORTRESPONSE         = '1.2.840.113556.1.4.474'; (* RFC 2891 *)

(* but not yet formalized controls *)
  LDAP_CONTROL_PROXY_AUTHZ          = '2.16.840.1.113730.3.4.18';

(* various works in progress *)
  LDAP_CONTROL_NOOP                 = '1.3.6.1.4.1.4203.666.5.2';

(* LDAP VLV *)(* not implemented in slapd(8) *)
  LDAP_CONTROL_VLVREQUEST           = '2.16.840.1.113730.3.4.9';
  LDAP_CONTROL_VLVRESPONSE          = '2.16.840.1.113730.3.4.10';

(* LDAP Sync -- draft-zeilenga-ldup-sync *)(* submitted for publication *)
  LDAP_SYNC_OID                     = '1.3.6.1.4.1.4203.1.9.1';
  LDAP_CONTROL_SYNC                 = LDAP_SYNC_OID + '.1';
  LDAP_CONTROL_SYNC_STATE           = LDAP_SYNC_OID + '.2';
  LDAP_CONTROL_SYNC_DONE            = LDAP_SYNC_OID + '.3';
  LDAP_SYNC_INFO                    = LDAP_SYNC_OID + '.4';

  LDAP_SYNC_NONE                    = $00;
  LDAP_SYNC_REFRESH_ONLY            = $01;
  LDAP_SYNC_RESERVED                = $02;
  LDAP_SYNC_REFRESH_AND_PERSIST     = $03;

  LDAP_SYNC_REFRESH_PRESENTS        = 0;
  LDAP_SYNC_REFRESH_DELETES         = 1;

  LDAP_TAG_SYNC_NEW_COOKIE          = ber_tag_t($80);
  LDAP_TAG_SYNC_REFRESH_DELETE      = ber_tag_t($a1);
  LDAP_TAG_SYNC_REFRESH_PRESENT     = ber_tag_t($a2);
  LDAP_TAG_SYNC_ID_SET              = ber_tag_t($a3);

  LDAP_TAG_SYNC_COOKIE              = ber_tag_t($04);
  LDAP_TAG_REFRESHDELETES           = ber_tag_t($01);
  LDAP_TAG_REFRESHDONE              = ber_tag_t($01);
  LDAP_TAG_RELOAD_HINT              = ber_tag_t($01);

  LDAP_SYNC_PRESENT                 = 0;
  LDAP_SYNC_ADD                     = 1;
  LDAP_SYNC_MODIFY                  = 2;
  LDAP_SYNC_DELETE                  = 3;

(* MS ActiveDirectory controls (for compatibility) *)
  LDAP_CONTROL_X_DOMAIN_SCOPE       = '1.2.840.113556.1.4.1339';
  LDAP_CONTROL_X_PERMISSIVE_MODIFY  = '1.2.840.113556.1.4.1413';

(* LDAP Unsolicited Notifications *)
  LDAP_NOTICE_OF_DISCONNECTION      = '1.3.6.1.4.1.1466.20036'; (* RFC 2251 *)
  LDAP_NOTICE_DISCONNECT            = LDAP_NOTICE_OF_DISCONNECTION;

(* LDAP Extended Operations *)
  LDAP_EXOP_START_TLS               = '1.3.6.1.4.1.1466.20037';    (* RFC 2830 *)

  LDAP_EXOP_MODIFY_PASSWD           = '1.3.6.1.4.1.4203.1.11.1';   (* RFC 3062 *)
  LDAP_TAG_EXOP_MODIFY_PASSWD_ID    = ber_tag_t($80);
  LDAP_TAG_EXOP_MODIFY_PASSWD_OLD   = ber_tag_t($81);
  LDAP_TAG_EXOP_MODIFY_PASSWD_NEW   = ber_tag_t($82);
  LDAP_TAG_EXOP_MODIFY_PASSWD_GEN   = ber_tag_t($80);

  LDAP_EXOP_X_WHO_AM_I              = '1.3.6.1.4.1.4203.1.11.3';
  LDAP_EXOP_X_CANCEL                = '1.3.6.1.4.1.4203.666.6.3';

(* LDAP Features *)
  LDAP_FEATURE_ALL_OP_ATTRS         = '1.3.6.1.4.1.4203.1.5.1';    (* RFC 3673 *)
  LDAP_FEATURE_OBJECTCLASS_ATTRS    = '1.3.6.1.4.1.4203.1.5.2'; (*  @objectClass - new number to be assigned *)
  LDAP_FEATURE_ABSOLUTE_FILTERS     = '1.3.6.1.4.1.4203.1.5.3';  (* (&) (|) *)
  LDAP_FEATURE_LANGUAGE_TAG_OPTIONS = '1.3.6.1.4.1.4203.1.5.4';
  LDAP_FEATURE_LANGUAGE_RANGE_OPTIONS = '1.3.6.1.4.1.4203.1.5.5';
  LDAP_FEATURE_MODIFY_INCREMENT     = '1.3.6.1.1.14';

(*
 * specific LDAP instantiations of BER types we know about
 *)

(* Overview of LBER tag construction
 *
 *  Bits
 *  ______
 *  8 7 | CLASS
 *  0 0 = UNIVERSAL
 *  0 1 = APPLICATION
 *  1 0 = CONTEXT-SPECIFIC
 *  1 1 = PRIVATE
 *      _____
 *      | 6 | DATA-TYPE
 *        0 = PRIMITIVE
 *        1 = CONSTRUCTED
 *          ___________
 *          | 5 ... 1 | TAG-NUMBER
 *)

(* general stuff *)
  LDAP_TAG_MESSAGE                  = ber_tag_t($30); (* constructed + 16 *)
  LDAP_TAG_MSGID                    = ber_tag_t($02); (* integer *)

  LDAP_TAG_LDAPDN                   = ber_tag_t($04); (* octet string *)
  LDAP_TAG_LDAPCRED                 = ber_tag_t($04); (* octet string *)

  LDAP_TAG_CONTROLS                 = ber_tag_t($a0); (* context specific + constructed + 0 *)
  LDAP_TAG_REFERRAL                 = ber_tag_t($a3); (* context specific + constructed + 3 *)

  LDAP_TAG_NEWSUPERIOR              = ber_tag_t($80); (* context-specific + primitive + 0 *)

  LDAP_TAG_EXOP_REQ_OID             = ber_tag_t($80); (* context specific + primitive *)
  LDAP_TAG_EXOP_REQ_VALUE           = ber_tag_t($81); (* context specific + primitive *)
  LDAP_TAG_EXOP_RES_OID             = ber_tag_t($8a); (* context specific + primitive *)
  LDAP_TAG_EXOP_RES_VALUE           = ber_tag_t($8b); (* context specific + primitive *)

  LDAP_TAG_IM_RES_OID               = ber_tag_t($80); (* context specific + primitive *)
  LDAP_TAG_IM_RES_VALUE             = ber_tag_t($81); (* context specific + primitive *)

  LDAP_TAG_SASL_RES_CREDS           = ber_tag_t($87); (* context specific + primitive *)


(* possible operations a client can invoke *)
  LDAP_REQ_BIND                     = ber_tag_t($60); (* application + constructed *)
  LDAP_REQ_UNBIND                   = ber_tag_t($42); (* application + primitive   *)
  LDAP_REQ_SEARCH                   = ber_tag_t($63); (* application + constructed *)
  LDAP_REQ_MODIFY                   = ber_tag_t($66); (* application + constructed *)
  LDAP_REQ_ADD                      = ber_tag_t($68); (* application + constructed *)
  LDAP_REQ_DELETE                   = ber_tag_t($4a); (* application + primitive   *)
  LDAP_REQ_MODDN                    = ber_tag_t($6c); (* application + constructed *)
  LDAP_REQ_MODRDN                   = LDAP_REQ_MODDN;
  LDAP_REQ_RENAME                   = LDAP_REQ_MODDN;
  LDAP_REQ_COMPARE                  = ber_tag_t($6e); (* application + constructed *)
  LDAP_REQ_ABANDON                  = ber_tag_t($50); (* application + primitive   *)
  LDAP_REQ_EXTENDED                 = ber_tag_t($77); (* application + constructed *)

(* possible result types a server can return *)
  LDAP_RES_BIND                     = ber_tag_t($61); (* application + constructed *)
  LDAP_RES_SEARCH_ENTRY             = ber_tag_t($64); (* application + constructed *)
  LDAP_RES_SEARCH_REFERENCE         = ber_tag_t($73); (* V3: application + constructed *)
  LDAP_RES_SEARCH_RESULT            = ber_tag_t($65); (* application + constructed *)
  LDAP_RES_MODIFY                   = ber_tag_t($67); (* application + constructed *)
  LDAP_RES_ADD                      = ber_tag_t($69); (* application + constructed *)
  LDAP_RES_DELETE                   = ber_tag_t($6b); (* application + constructed *)
  LDAP_RES_MODDN                    = ber_tag_t($6d); (* application + constructed *)
  LDAP_RES_MODRDN                   = LDAP_RES_MODDN; (* application + constructed *)
  LDAP_RES_RENAME                   = LDAP_RES_MODDN; (* application + constructed *)
  LDAP_RES_COMPARE                  = ber_tag_t($6f); (* application + constructed *)
  LDAP_RES_EXTENDED                 = ber_tag_t($78); (* V3: application + constructed *)
  LDAP_RES_INTERMEDIATE             = ber_tag_t($79); (* V3+: application + constructed *)

  LDAP_RES_ANY                      = -1;
  LDAP_RES_UNSOLICITED              =  0;


(* sasl methods *)
  LDAP_SASL_SIMPLE                  = pcchar(0);
  LDAP_SASL_NULL                    = '';


(* authentication methods available *)
  LDAP_AUTH_NONE                    = ber_tag_t($00); (* no authentication          *)
  LDAP_AUTH_SIMPLE                  = ber_tag_t($80); (* context specific + primitive *)
  LDAP_AUTH_SASL                    = ber_tag_t($a3); (* context specific + constructed *)
  LDAP_AUTH_KRBV4                   = ber_tag_t($ff); (* means do both of the following *)
  LDAP_AUTH_KRBV41                  = ber_tag_t($81); (* context specific + primitive *)
  LDAP_AUTH_KRBV42                  = ber_tag_t($82); (* context specific + primitive *)


(* filter types *)
  LDAP_FILTER_AND                   = ber_tag_t($a0); (* context specific + constructed *)
  LDAP_FILTER_OR                    = ber_tag_t($a1); (* context specific + constructed *)
  LDAP_FILTER_NOT                   = ber_tag_t($a2); (* context specific + constructed *)
  LDAP_FILTER_EQUALITY              = ber_tag_t($a3); (* context specific + constructed *)
  LDAP_FILTER_SUBSTRINGS            = ber_tag_t($a4); (* context specific + constructed *)
  LDAP_FILTER_GE                    = ber_tag_t($a5); (* context specific + constructed *)
  LDAP_FILTER_LE                    = ber_tag_t($a6); (* context specific + constructed *)
  LDAP_FILTER_PRESENT               = ber_tag_t($87); (* context specific + primitive   *)
  LDAP_FILTER_APPROX                = ber_tag_t($a8); (* context specific + constructed *)
  LDAP_FILTER_EXT                   = ber_tag_t($a9); (* context specific + constructed *)

(* extended filter component types *)
  LDAP_FILTER_EXT_OID               = ber_tag_t($81); (* context specific *)
  LDAP_FILTER_EXT_TYPE              = ber_tag_t($82); (* context specific *)
  LDAP_FILTER_EXT_VALUE             = ber_tag_t($83); (* context specific *)
  LDAP_FILTER_EXT_DNATTRS           = ber_tag_t($84); (* context specific *)

(* substring filter component types *)
  LDAP_SUBSTRING_INITIAL            = ber_tag_t($80); (* context specific *)
  LDAP_SUBSTRING_ANY                = ber_tag_t($81); (* context specific *)
  LDAP_SUBSTRING_FINAL              = ber_tag_t($82); (* context specific *)

(* search scopes *)
  LDAP_SCOPE_DEFAULT                = ber_int_t(-1);     (* OpenLDAP extension *)
  LDAP_SCOPE_BASE                   = ber_int_t($0000);
  LDAP_SCOPE_BASEOBJECT             = LDAP_SCOPE_BASE;
  LDAP_SCOPE_ONELEVEL               = ber_int_t($0001);
  LDAP_SCOPE_ONE                    = LDAP_SCOPE_ONELEVEL;
  LDAP_SCOPE_SUBTREE                = ber_int_t($0002);
  LDAP_SCOPE_SUB                    = LDAP_SCOPE_SUBTREE;


(*
 * possible error codes we can return
 *)

function LDAP_RANGE(n, x, y: ber_int_t): Boolean;

const
  LDAP_SUCCESS                      = $00;
  LDAP_OPERATIONS_ERROR             = $01;
  LDAP_PROTOCOL_ERROR               = $02;
  LDAP_TIMELIMIT_EXCEEDED           = $03;
  LDAP_SIZELIMIT_EXCEEDED           = $04;
  LDAP_COMPARE_FALSE                = $05;
  LDAP_COMPARE_TRUE                 = $06;
  LDAP_AUTH_METHOD_NOT_SUPPORTED    = $07;
  LDAP_STRONG_AUTH_NOT_SUPPORTED    = LDAP_AUTH_METHOD_NOT_SUPPORTED;
  LDAP_STRONG_AUTH_REQUIRED         = $08;
  LDAP_PARTIAL_RESULTS              = $09;   (* LDAPv2+ (not LDAPv3) *)

  LDAP_REFERRAL                     = $0a; (* LDAPv3 *)
  LDAP_ADMINLIMIT_EXCEEDED          = $0b; (* LDAPv3 *)
  LDAP_UNAVAILABLE_CRITICAL_EXTENSION = $0c; (* LDAPv3 *)
  LDAP_CONFIDENTIALITY_REQUIRED     = $0d; (* LDAPv3 *)
  LDAP_SASL_BIND_IN_PROGRESS        = $0e; (* LDAPv3 *)

function LDAP_ATTR_ERROR(n: ber_int_t): Boolean;

const
  LDAP_NO_SUCH_ATTRIBUTE            = $10;
  LDAP_UNDEFINED_TYPE               = $11;
  LDAP_INAPPROPRIATE_MATCHING       = $12;
  LDAP_CONSTRAINT_VIOLATION         = $13;
  LDAP_TYPE_OR_VALUE_EXISTS         = $14;
  LDAP_INVALID_SYNTAX               = $15;

function LDAP_NAME_ERROR(n: ber_int_t): Boolean;

const
  LDAP_NO_SUCH_OBJECT               = $20;
  LDAP_ALIAS_PROBLEM                = $21;
  LDAP_INVALID_DN_SYNTAX            = $22;
  LDAP_IS_LEAF                      = $23; (* not LDAPv3 *)
  LDAP_ALIAS_DEREF_PROBLEM          = $24;

function LDAP_SECURITY_ERROR(n: ber_int_t): Boolean;

const
  LDAP_PROXY_AUTHZ_FAILURE          = $2F; (* LDAPv3 proxy authorization *)
  LDAP_INAPPROPRIATE_AUTH           = $30;
  LDAP_INVALID_CREDENTIALS          = $31;
  LDAP_INSUFFICIENT_ACCESS          = $32;

function LDAP_SERVICE_ERROR(n: ber_int_t): Boolean;

const
  LDAP_BUSY                         = $33;
  LDAP_UNAVAILABLE                  = $34;
  LDAP_UNWILLING_TO_PERFORM         = $35;
  LDAP_LOOP_DETECT                  = $36;

function LDAP_UPDATE_ERROR(n: ber_int_t): Boolean;

const
  LDAP_NAMING_VIOLATION             = $40;
  LDAP_OBJECT_CLASS_VIOLATION       = $41;
  LDAP_NOT_ALLOWED_ON_NONLEAF       = $42;
  LDAP_NOT_ALLOWED_ON_RDN           = $43;
  LDAP_ALREADY_EXISTS               = $44;
  LDAP_NO_OBJECT_CLASS_MODS         = $45;
  LDAP_RESULTS_TOO_LARGE            = $46; (* CLDAP *)
  LDAP_AFFECTS_MULTIPLE_DSAS        = $47; (* LDAPv3 *)

  LDAP_OTHER                        = $50;

(* for the LDAP Sync operation *)
  LDAP_SYNC_REFRESH_REQUIRED        = $4100;

(* for the LDAP No-Op control *)
  LDAP_NO_OPERATION                 = $410e;

(* for the Assertion control *)
  LDAP_ASSERTION_FAILED             = $410f;

(* for the Cancel operation *)
  LDAP_CANCELLED                    = $4110;
  LDAP_NO_SUCH_OPERATION            = $4111;
  LDAP_TOO_LATE                     = $4112;
  LDAP_CANNOT_CANCEL                = $4113;

(* API Error Codes
 *
 * Based on draft-ietf-ldap-c-api-xx
 * but with new negative code values
 *)

function LDAP_API_ERROR(n: ber_int_t): Boolean;
function LDAP_API_RESULT(n: ber_int_t): Boolean;

const
  LDAP_SERVER_DOWN                  = -1;
  LDAP_LOCAL_ERROR                  = -2;
  LDAP_ENCODING_ERROR               = -3;
  LDAP_DECODING_ERROR               = -4;
  LDAP_TIMEOUT                      = -5;
  LDAP_AUTH_UNKNOWN                 = -6;
  LDAP_FILTER_ERROR                 = -7;
  LDAP_USER_CANCELLED               = -8;
  LDAP_PARAM_ERROR                  = -9;
  LDAP_NO_MEMORY                    = -10;
  LDAP_CONNECT_ERROR                = -11;
  LDAP_NOT_SUPPORTED                = -12;
  LDAP_CONTROL_NOT_FOUND            = -13;
  LDAP_NO_RESULTS_RETURNED          = -14;
  LDAP_MORE_RESULTS_TO_RETURN       = -15;
  LDAP_CLIENT_LOOP                  = -16;
  LDAP_REFERRAL_LIMIT_EXCEEDED      = -17;


(*
 * This structure represents both ldap messages and ldap responses.
 * These are really the same, except in the case of search responses,
 * where a response has multiple messages.
 *)

type
//typedef struct ldapmsg LDAPMessage;
  pldapmsg                          = Pointer;

  PPLDAPMessage                     = ^PLDAPMessage;
  PLDAPMessage                      = pldapmsg;


(* for modifications *)
const
  LDAP_MOD_OP                       = $0007;
  LDAP_MOD_ADD                      = $0000;
  LDAP_MOD_DELETE                   = $0001;
  LDAP_MOD_REPLACE                  = $0002;
  LDAP_MOD_INCREMENT                = $0003; (* OpenLDAP extension *)
  LDAP_MOD_BVALUES                  = $0080;

type
  PPPLDAPMod                        = ^PPLDAPMod;
  PPLDAPMod                         = ^PLDAPMod;
  PLDAPMod                          = ^LDAPMod;
  LDAPMod                           = record
    mod_op      : cint;

(* IMPORTANT: do not use code 0x1000 (or above),
 * it is used internally by the backends!
 * (see ldap/servers/slapd/slap.h)
 *)

    mod_type    : pcchar;

    case cint of
      0: (mod_vals    : record
           case cint of
             0: (modv_strvals  : ppcchar);
             1: (modv_bvals    : PPBerval);
         end);
      1: (case cint of
           0: (mod_values      : ppcchar);
           1: (mod_bvalues     : PPBerval));
  end;


(*
 * structure representing an ldap session which can
 * encompass connections to multiple servers (in the
 * face of referrals).
 *)
  PLDAP                             = Pointer;

const
  LDAP_DEREF_NEVER                  = $00;
  LDAP_DEREF_SEARCHING              = $01;
  LDAP_DEREF_FINDING                = $02;
  LDAP_DEREF_ALWAYS                 = $03;

  LDAP_NO_LIMIT                     = 0;

(* how many messages to retrieve results for *)
  LDAP_MSG_ONE                      = $00;
  LDAP_MSG_ALL                      = $01;
  LDAP_MSG_RECEIVED                 = $02;

(*
 * types for ldap URL handling
 *)

type
  pldap_url_desc                    = ^ldap_url_desc;
  ldap_url_desc                     = record
    lud_next        : pldap_url_desc;
    lud_scheme      : pcchar;
    lud_host        : pcchar;
    lud_port        : cint;
    lud_dn          : pcchar;
    lud_attrs       : ppcchar;
    lud_scope       : cint;
    lud_filter      : pcchar;
    lud_exts        : pcchar;
    lud_crit_exts   : cint;
  end;

  PLDAPURLDesc                      = ^LDAPURLDesc;
  LDAPURLDesc                       = ldap_url_desc;

const
  LDAP_URL_SUCCESS                  = $00;    (* Success *)
  LDAP_URL_ERR_MEM                  = $01;    (* can't allocate memory space *)
  LDAP_URL_ERR_PARAM                = $02;    (* parameter is bad *)

  LDAP_URL_ERR_BADSCHEME            = $03;    (* URL doesn't begin with "ldap[si]://" *)
  LDAP_URL_ERR_BADENCLOSURE         = $04;    (* URL is missing trailing ">" *)
  LDAP_URL_ERR_BADURL               = $05;    (* URL is bad *)
  LDAP_URL_ERR_BADHOST              = $06;    (* host port is bad *)
  LDAP_URL_ERR_BADATTRS             = $07;    (* bad (or missing) attributes *)
  LDAP_URL_ERR_BADSCOPE             = $08;    (* scope string is invalid (or missing) *)
  LDAP_URL_ERR_BADFILTER            = $09;    (* bad or missing filter *)
  LDAP_URL_ERR_BADEXTS              = $0a;    (* bad or missing extensions *)

(*
 * The API draft spec says we should declare (or cause to be declared)
 * 'struct timeval'.   We don't.  See IETF LDAPext discussions.
 *)
type
  ptimeval                          = Pointer;

(*
 * in options.c:
 *)
function ldap_get_option(
    ld              : PLDAP;
    option          : cint;
    outvalue        : Pointer
  ): cint; cdecl; external;

function ldap_set_option(
    ld              : PLDAP;
    option          : cint;
    const invalue   : Pointer
  ): cint; cdecl; external;

(* V3 REBIND Function Callback Prototype *)
type
  LDAP_REBIND_PROC = function(ld: PLDAP; const url: pcchar; request: ber_tag_t; msgid: ber_int_t; params: Pointer): cint; cdecl;

function ldap_set_rebind_proc(
    ld              : PLDAP;
    rebind_proc     : LDAP_REBIND_PROC;
    params          : Pointer
  ): cint; cdecl; external;

(* V3 referral selection Function Callback Prototype *)
type
  LDAP_NEXTREF_PROC = function(ld: PLDAP; refsp: pppcchar; cntp: pcint; params: Pointer): cint; cdecl;


function ldap_set_nextref_proc(
    ld              : PLDAP;
    nextref_proc    : LDAP_NEXTREF_PROC;
    params          : Pointer
  ): cint; cdecl; external;


(*
 * in controls.c:
 *)

function ldap_create_control(
    const requestOID: pcchar;
    ber             : PBerElement;
    iscritical      : cint;
    var ctrlp       : PLDAPControl
  ): cint; cdecl; external;

function ldap_find_control(
    const oid       : pcchar;
    ctrls           : PPLDAPControl
  ): PLDAPControl; cdecl; external;

procedure ldap_control_free(
    ctrl            : PLDAPControl
  ); cdecl; external;

procedure ldap_controls_free(
    ctrls           : PPLDAPControl
  ); cdecl; external;


(*
 * in dnssrv.c:
 *)

function ldap_domain2dn(
    const domain    : pcchar;
    var dn          : pcchar
  ): cint; cdecl; external;

function ldap_dn2domain(
    const dn        : pcchar;
    var domain      : pcchar
  ): cint; cdecl; external;

function ldap_domain2hostlist(
    const domain    : pcchar;
    var hostlist    : pcchar
  ): cint; cdecl; external;


(*
 * in extended.c:
 *)

function ldap_extended_operation(
    ld              : PLDAP;
    const reqoid    : pcchar;
    reqdata         : PBerval;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_extended_operation_s(
    ld              : PLDAP;
    const reqoid    : pcchar;
    reqdata         : PBerval;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    var retoidp     : pcchar;
    var retdatap    : PBerval
  ): cint; cdecl; external;

function ldap_parse_extended_result(
    ld              : PLDAP;
    res             : PLDAPMessage;
    var retoidp     : pcchar;
    var retdatap    : PBerval;
    freeit          : cbool
  ): cint; cdecl; external;

function ldap_parse_intermediate(
    ld              : PLDAP;
    res             : PLDAPMessage;
    var retoidp     : pcchar;
    var retdatap    : PBerval;
    serverctrls     : PPPLDAPControl;
    freeit          : cbool
  ): cint; cdecl; external;


(*
 * in groupings.c:
 *)
(*#ifdef LDAP_EXOP_GROUPING_CREATE

LDAP_F( int )
ldap_grouping_create LDAP_P((
    LDAP            *ld,
    LDAP_CONST cchar *grpoid,
    struct berval   *grpdata,
    LDAPControl     **serverctrls,
    LDAPControl     **clientctrls,
    int             *msgidp ));

LDAP_F( int )
ldap_grouping_create_s LDAP_P((
    LDAP            *ld,
    LDAP_CONST cchar *grpoid,
    struct berval   *grpdata,
    LDAPControl     **serverctrls,
    LDAPControl     **clientctrls,
    struct berval   **retgrpcookiep,
    struct berval   **retgrpdatap ));

LDAP_F( int )
ldap_parse_grouping_create_result LDAP_P((
    LDAP            *ld,
    LDAPMessage     *res,
    struct berval   **retgrpcookiep,
    struct berval   **retgrpdatap,
    LDAPControl     ***serverctrls,
    int             freeit ));

LDAP_F( int )
ldap_grouping_end LDAP_P((
    LDAP            *ld,
    LDAP_CONST cchar *grpoid,
    struct berval   *grpdata,
    LDAPControl     **serverctrls,
    LDAPControl     **clientctrls,
    int             *msgidp ));

LDAP_F( int )
ldap_grouping_end_s LDAP_P((
    LDAP            *ld,
    LDAP_CONST cchar *grpoid,
    struct berval   *grpdata,
    LDAPControl     **serverctrls,
    LDAPControl     **clientctrls,
    struct berval   **retgrpdatap ));

LDAP_F( int )
ldap_parse_grouping_end_result LDAP_P((
    LDAP            *ld,
    LDAPMessage     *res,
    struct berval   **retgrpdatap,
    LDAPControl     ***serverctrls,
    int             freeit ));

LDAP_F( int )
ldap_grouping_action_operation LDAP_P((
    LDAP            *ld,
    LDAP_CONST cchar *grpoid,
    struct berval   *grpdata,
    LDAPControl     **serverctrls,
    LDAPControl     **clientctrls,
    int             *msgidp ));

LDAP_F( int )
ldap_grouping_action_operation_s LDAP_P((
    LDAP            *ld,
    LDAP_CONST cchar *grpoid,
    struct berval   *grpdata,
    LDAPControl     **serverctrls,
    LDAPControl     **clientctrls,
    struct berval   **retgrpcookiep,
    struct berval   **retgrpdatap ));

LDAP_F( int )
ldap_parse_grouping_action_result LDAP_P((
    LDAP            *ld,
    LDAPMessage     *res,
    struct berval   **retgrpcookiep,
    struct berval   **retgrpdatap,
    LDAPControl     ***serverctrls,
    int             freeit ));

LDAP_F( int )
ldap_parse_grouping_end_notice LDAP_P((
    LDAP            *ld,
    LDAPMessage     *res,
    struct berval   **retdatap,
    struct berval   **retgrpcookiep,
    struct berval   **retgrpdatap,
    int             freeit ));

LDAP_F( int )
ldap_parse_grouping_info_notice LDAP_P((
    LDAP            *ld,
    LDAPMessage     *res,
    struct berval   **retdatap,
    struct berval   **retgrpcookiep,
    struct berval   **retgrpdatap,
    int             freeit ));
#endif
*)


(*
 * in abandon.c:
 *)

function ldap_abandon_ext(
    ld              : PLDAP;
    msgid           : cint;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl
  ): cint; cdecl; external;


(*
 * in add.c:
 *)

function ldap_add_ext(
    ld              : PLDAP;
    const dn        : pcchar;
    attrs           : PPLDAPMod;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_add_ext_s(
    ld              : PLDAP;
    const dn        : pcchar;
    attrs           : PPLDAPMod;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl
  ): cint; cdecl; external;


(*
 * in sasl.c:
 *)

function ldap_sasl_bind(
    ld              : PLDAP;
    const dn        : pcchar;
    const mechanism : pcchar;
    cred            : PBerval;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_sasl_bind_s(
    ld              : PLDAP;
    const dn        : pcchar;
    const mechanism : pcchar;
    cred            : PBerval;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    servercredp     : PPBerval
  ): cint; cdecl; external;

(* Interaction flags (should be passed about in a control)
 *  Automatic (default): use defaults, prompt otherwise
 *  Interactive: prompt always
 *  Quiet: never prompt
 *)
const
  LDAP_SASL_AUTOMATIC               = 0;
  LDAP_SASL_INTERACTIVE             = 1;
  LDAP_SASL_QUIET                   = 2;

(*
 * V3 SASL Interaction Function Callback Prototype
 *  when using Cyrus SASL, interact is pointer to sasl_interact_t
 *  should likely passed in a control (and provided controls)
 *)
type
  LDAP_SASL_INTERACT_PROC = function(ld: PLDAP; flags: cuint; defaults, interact: Pointer): cint; cdecl;

function ldap_sasl_interactive_bind_s(
    ld              : PLDAP;
    const dn        : pcchar;
    const mechanism : pcchar;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    flags           : cuint;
    proc            : LDAP_SASL_INTERACT_PROC;
    defaults        : Pointer
  ): cint; cdecl; external;

function ldap_parse_sasl_bind_result(
    ld              : PLDAP;
    res             : PLDAPMessage;
    var servercredp : PBerval;
    freeit          : cbool
  ): cint; cdecl; external;


(*
 * in compare.c:
 *)

function ldap_compare_ext(
    ld              : PLDAP;
    const dn        : pcchar;
    const attr      : pcchar;
    bvalue          : PBerval;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_compare_ext_s(
    ld              : PLDAP;
    const dn        : pcchar;
    const attr      : pcchar;
    bvalue          : PBerval;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl
  ): cint; cdecl; external;


(*
 * in delete.c:
 *)

function ldap_delete_ext(
    ld              : PLDAP;
    const dn        : pcchar;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_delete_ext_s(
    ld              : PLDAP;
    const dn        : pcchar;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl
  ): cint; cdecl; external;


(*
 * in error.c:
 *)

function ldap_parse_result(
    ld              : PLDAP;
    res             : PLDAPMessage;
    errcodep        : pcint;
    matcheddnp      : ppcchar;
    errmsgp         : ppcchar;
    referralsp      : pppcchar;
    serverctrls     : PPPLDAPControl;
    freeit          : cbool
  ): cint; cdecl; external;

function ldap_err2string(
    err             : cint
  ): pcchar; cdecl; external;


(*
 * in modify.c:
 *)

function ldap_modify_ext(
    ld              : PLDAP;
    const dn        : pcchar;
    mods            : PPLDAPMod;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_modify_ext_s(
    ld              : PLDAP;
    const dn        : pcchar;
    mods            : PPLDAPMod;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl
  ): cint; cdecl; external;


(*
 * in modrdn.c:
 *)

function ldap_rename(
    ld              : PLDAP;
    const dn        : pcchar;
    const newrdn    : pcchar;
    const newSuperior: pcchar;
    deleteoldrdn    : cbool;
    sctrls          : PPLDAPControl;
    cctrls          : PPPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_rename_s(
    ld              : PLDAP;
    const dn        : pcchar;
    const newrdn    : pcchar;
    const newSuperior: pcchar;
    deleteoldrdn    : cbool;
    sctrls          : PPLDAPControl;
    cctrls          : PPLDAPControl
  ): cint; cdecl; external;


(*
 * in open.c:
 *)

function ldap_create(
    var ld          : PLDAP
  ): cint; cdecl; external;

function ldap_initialize(
    var ld          : PLDAP;
    const url       : pcchar
  ): cint; cdecl; external;


(*
 * in tls.c
 *)

function ldap_tls_inplace(
    ld              : PLDAP
  ): cint; cdecl; external;

function ldap_install_tls(
    ld              : PLDAP
  ): cint; cdecl; external;

function ldap_start_tls(
    ld              : PLDAP;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_start_tls_s(
    ld              : PLDAP;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl
  ): cint; cdecl; external;


(*
 * in messages.c:
 *)

function ldap_first_message(
    ld              : PLDAP;
    chain           : PLDAPMessage
  ): PLDAPMessage; cdecl; external;

function ldap_next_message(
    ld              : PLDAP;
    msg             : PLDAPMessage
  ): PLDAPMessage; cdecl; external;

function ldap_count_messages(
    ld              : PLDAP;
    chain           : PLDAPMessage
  ): cint; cdecl; external;


(*
 * in references.c:
 *)

function ldap_first_reference(
    ld              : PLDAP;
    chain           : PLDAPMessage
  ): PLDAPMessage; cdecl; external;

function ldap_next_reference(
    ld              : PLDAP;
    ref             : PLDAPMessage
  ): PLDAPMessage; cdecl; external;

function ldap_count_references(
    ld              : PLDAP;
    chain           : PLDAPMessage
  ): cint; cdecl; external;

function ldap_parse_reference(
    ld              : PLDAP;
    ref             : PLDAPMessage;
    var referralsp  : ppcchar;
    serverctrls     : PPPLDAPControl;
    freeit          : cbool
  ): cint; cdecl; external;


(*
 * in getentry.c:
 *)

function ldap_first_entry(
    ld              : PLDAP;
    chain           : PLDAPMessage
  ): PLDAPMessage; cdecl; external;

function ldap_next_entry(
    ld              : PLDAP;
    entry           : PLDAPMessage
  ): PLDAPMessage; cdecl; external;

function ldap_count_entries(
    ld              : PLDAP;
    chain           : PLDAPMessage
  ): cint; cdecl; external;

function ldap_get_entry_controls(
    ld              : PLDAP;
    entry           : PLDAPMessage;
    var serverctrls : PPLDAPControl
  ): cint; cdecl; external;


(*
 * in addentry.c
 *)

function ldap_delete_result_entry(
    list            : PPLDAPMessage;
    e               : PLDAPMessage
  ): PLDAPMessage; cdecl; external;

procedure ldap_add_result_entry(
    list            : PPLDAPMessage;
    e               : PLDAPMessage
  ); cdecl; external;


(*
 * in getdn.c
 *)

function ldap_get_dn(
    ld              : PLDAP;
    entry           : PLDAPMessage
  ): pcchar; cdecl; external;

const
  LDAP_AVA_NULL                     = $0000;
  LDAP_AVA_STRING                   = $0001;
  LDAP_AVA_BINARY                   = $0002;
  LDAP_AVA_NONPRINTABLE             = $0004;
  LDAP_AVA_FREE_ATTR                = $0010;
  LDAP_AVA_FREE_VALUE               = $0020;

type
  pldap_ava                         = ^ldap_ava;
  ldap_ava                          = record
    la_attr     : berval;
    la_value    : berval;
    la_flags    : cuint;
    la_private  : Pointer;
  end;

  PLDAPAVA                          = ^LDAPAVA;
  LDAPAVA                           = ldap_ava;

  PLDAPRDN                          = ^LDAPRDN;
  LDAPRDN                           = ^PLDAPAVA;

  PLDAPDN                           = ^LDAPDN;
  LDAPDN                            = ^LDAPRDN;

const
(* DN formats *)
  LDAP_DN_FORMAT_LDAP               = $0000;
  LDAP_DN_FORMAT_LDAPV3             = $0010;
  LDAP_DN_FORMAT_LDAPV2             = $0020;
  LDAP_DN_FORMAT_DCE                = $0030;
  LDAP_DN_FORMAT_UFN                = $0040; (* dn2str only *)
  LDAP_DN_FORMAT_AD_CANONICAL       = $0050; (* dn2str only *)
  LDAP_DN_FORMAT_LBER               = $00F0; (* for testing only *)
  LDAP_DN_FORMAT_MASK               = $00F0;

(* DN flags *)
  LDAP_DN_PRETTY                    = $0100;
  LDAP_DN_SKIP                      = $0200;
  LDAP_DN_P_NOLEADTRAILSPACES       = $1000;
  LDAP_DN_P_NOSPACEAFTERRDN         = $2000;
  LDAP_DN_PEDANTIC                  = $F000;

procedure ldap_rdnfree(rdn: LDAPRDN); cdecl; external;
procedure ldap_dnfree(rdn: LDAPDN); cdecl; external;

function ldap_bv2dn(
    bv              : PBerval;
    var dn          : LDAPDN;
    flags           : cuint
  ): cint; cdecl; external;

function ldap_str2dn(
    const str       : pcchar;
    var dn          : LDAPDN;
    flags           : cuint
  ): cint; cdecl; external;

function ldap_dn2bv(
    dn              : LDAPDN;
    bv              : PBerval;
    flags           : cuint
  ): cint; cdecl; external;

function ldap_dn2str(
    dn              : LDAPDN;
    var str         : pcchar;
    flags           : cuint
  ): cint; cdecl; external;

function ldap_bv2rdn(
    bv              : PBerval;
    var rdn         : LDAPRDN;
    var next        : pcchar;
    flags           : cuint
  ): cint; cdecl; external;

function ldap_str2rdn(
    const str       : pcchar;
    var rdn         : LDAPRDN;
    var next        : pcchar;
    flags           : cuint
  ): cint; cdecl; external;

function ldap_rdn2bv(
    rdn             : LDAPRDN;
    bv              : PBerval;
    flags           : cuint
  ): cint; cdecl; external;

function ldap_rdn2str(
    rdn             : LDAPRDN;
    var str         : pcchar;
    flags           : cuint
  ): cint; cdecl; external;

function ldap_dn_normalize(
    const _in       : pcchar;
    iflags          : cuint;
    var _out        : pcchar;
    oflags          : cuint
  ): cint; cdecl; external;


type
  LDAPDN_rewrite_func = function(dn: LDAPDN; flags: cuint; ctx: Pointer): cint; cdecl;

function ldap_X509dn2bv(
    x509_name       : Pointer;
    dn              : PBerval;
    func            : LDAPDN_rewrite_func;
    flags           : cuint
  ): cint; cdecl; external;

function ldap_get_dn_ber(
    ld              : PLDAP;
    e               : PLDAPMessage;
    var berout      : PBerElement;
    dn              : PBerval
  ): cint; cdecl; external;

function ldap_get_attribute_ber(
    ld              : PLDAP;
    e               : PLDAPMessage;
    ber             : PBerElement;
    attr            : PBerval;
    var vals        : PBerval
  ): cint; cdecl; external;


(*
 * in getattr.c
 *)

function ldap_first_attribute(
    ld              : PLDAP;
    entry           : PLDAPMessage;
    var ber         : PBerElement
  ): pcchar; cdecl; external;

function ldap_next_attribute(
    ld              : PLDAP;
    entry           : PLDAPMessage;
    ber             : PBerElement
  ): pcchar; cdecl; external;


(*
 * in getvalues.c
 *)

function ldap_get_values_len(
    ld              : PLDAP;
    entry           : PLDAPMessage;
    const target    : pcchar
  ): PPBerval; cdecl; external;

function ldap_count_values_len(
    vals            : PPBerval
  ): cint; cdecl; external;

procedure ldap_value_free_len(
    vals            : PPBerval
  ); cdecl; external;


(*
 * in result.c:
 *)

function ldap_result(
    ld              : PLDAP;
    msgid           : cint;
    all             : cint;
    timeout         : ptimeval;
    var result      : PLDAPMessage
  ): cint; cdecl; external;

function ldap_msgtype(
    lm              : PLDAPMessage
  ): cint; cdecl; external;

function ldap_msgid(
    lm              : PLDAPMessage
  ): cint; cdecl; external;

function ldap_msgfree(
    lm              : PLDAPMessage
  ): cint; cdecl; external;

function ldap_msgdelete(
    ld              : PLDAP;
    msgid           : cint
  ): cint; cdecl; external;


(*
 * in search.c:
 *)

function ldap_search_ext(
    ld              : PLDAP;
    const base      : pcchar;
    scope           : cint;
    const filter    : pcchar;
    attrs           : ppcchar;
    attrsonly       : cbool;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    timout          : ptimeval;
    sizelimit       : cint;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_search_ext_s(
    ld              : PLDAP;
    const base      : pcchar;
    scope           : cint;
    const filter    : pcchar;
    attrs           : ppcchar;
    attrsonly       : cbool;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl;
    timout          : ptimeval;
    sizelimit       : cint;
    var res         : PLDAPMessage
  ): cint; cdecl; external;


(*
 * in unbind.c
 *)

function ldap_unbind_ext(
    ld              : PLDAP;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl
  ): cint; cdecl; external;

function ldap_unbind_ext_s(
    ld              : PLDAP;
    serverctrls     : PPLDAPControl;
    clientctrls     : PPLDAPControl
  ): cint; cdecl; external;


(*
 * in filter.c
 *)

function ldap_put_vrFilter(
    ber             : PBerElement;
    const vrf       : pcchar
  ): cint; cdecl; external;


(*
 * in free.c
 *)

function ldap_memalloc(
    s               : ber_len_t
  ): Pointer; cdecl; external;

function ldap_memrealloc(
    p               : Pointer;
    s               : ber_len_t
  ): Pointer; cdecl; external;

function ldap_memcalloc(
    n               : ber_len_t;
    s               : ber_len_t
  ): Pointer; cdecl; external;

procedure ldap_memfree(
    p               : Pointer
  ); cdecl; external;

procedure ldap_memvfree(
    v               : PPointer
  ); cdecl; external;

function ldap_strdup(
    const str       : pcchar
  ): pcchar; cdecl; external;

procedure ldap_mods_free(
    mods            : PPLDAPMod;
    freemods        : cbool
  ); cdecl; external;


(*
 * in url.c
 *)

function ldap_is_ldap_url(
    const url       : pcchar
  ): cint; cdecl; external;

function ldap_is_ldaps_url(
    const url       : pcchar
  ): cint; cdecl; external;

function ldap_is_ldapi_url(
    const url       : pcchar
  ): cint; cdecl; external;

function ldap_url_parse(
    const url       : pcchar;
    var ludpp       : PLDAPURLDesc
  ): cint; cdecl; external;

function ldap_url_desc2str(
    ludpp           : PLDAPURLDesc
  ): pcchar; cdecl; external;

procedure ldap_free_urldesc(
    ludpp           : PLDAPURLDesc
  ); cdecl; external;


(*
 * LDAP Cancel Extended Operation <draft-zeilenga-ldap-cancel-xx.txt>
 *  in cancel.c
 *)
const
  LDAP_API_FEATURE_CANCEL           = 1000;

function ldap_cancel(
    ld              : PLDAP;
    cancelid        : cint;
    sctrl           : PPLDAPControl;
    cctrl           : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;


function ldap_cancel_s(
    ld              : PLDAP;
    cancelid        : cint;
    sctrl           : PPLDAPControl;
    cctrl           : PPLDAPControl
  ): cint; cdecl; external;


(*
 * LDAP Server Side Sort
 *  in sortctrl.c
 *)

const
  LDAP_API_FEATURE_SERVER_SIDE_SORT = 1000;

(* structure for a sort-key *)
type
  PPLDAPSortKey                     = ^PLDAPSortKey;
  PLDAPSortKey                      = ^LDAPSortKey;
  LDAPSortKey                       = record
    attributeType   : pcchar;
    orderingRule    : pcchar;
    reverseOrder    : cbool;
  end;


function ldap_create_sort_keylist(
    var sortKeyList : PPLDAPSortKey;
    keyString       : pcchar
  ): cint; cdecl; external;

procedure ldap_free_sort_keylist(
    sortKeyList     : PPLDAPSortKey
  ); cdecl; external;

function ldap_create_sort_control(
    ld              : PLDAP;
    keyList         : PPLDAPSortKey;
    ctl_iscritical  : cbool;
    var ctrlp       : PLDAPControl
  ): cint; cdecl; external;

function ldap_parse_sort_control(
    ld              : PLDAP;
    ctrlp           : PPLDAPControl;
    var result      : culong;
    var attribute   : pcchar
  ): cint; cdecl; external;


(*
 * LDAP Virtual List View
 *  in vlvctrl.c
 *)
const
  LDAP_API_FEATURE_VIRTUAL_LIST_VIEW = 1000;

(* structure for virtual list *)
type
  PLDAPVLVInfo                      = ^LDAPVLVInfo;
  LDAPVLVInfo                       = record
    ldvlv_version       : cint;
    ldvlv_before_count  : culong;
    ldvlv_after_count   : culong;
    ldvlv_offset        : culong;
    ldvlv_count         : culong;
    ldvlv_attrvalue     : PBerval;
    ldvlv_context       : PBerval;
    ldvlv_extradata     : Pointer;
  end;

function ldap_create_vlv_control(
    ld              : PLDAP;
    ldvlistp        : PLDAPVLVInfo;
    var ctrlp       : PLDAPControl
  ): cint; cdecl; external;

function ldap_parse_vlv_control(
    ld              : PLDAP;
    ctrls           : PPLDAPControl;
    var target_posp : culong;
    var list_countp : culong;
    var contextp    : PBerval;
    var errcodep    : cint
  ): cint; cdecl; external;


(*
 * LDAP Transactions
 *  in txn.c
 *)

(*
#ifdef LDAP_GROUP_TRANSACTION
LDAP_F( int )
ldap_parse_txn_create LDAP_P((
    LDAP *ld,
    LDAPMessage *res,
    struct berval **cookie ));

LDAP_F( int )
ldap_txn_create LDAP_P((
    LDAP *ld,
    LDAPControl     **sctrls,
    LDAPControl     **cctrls,
    int             *msgidp ));

LDAP_F( int )
ldap_txn_create_s LDAP_P((
    LDAP *ld,
    struct berval **cookie,
    LDAPControl **sctrls,
    LDAPControl **cctrls ));

LDAP_F( int )
ldap_txn_end LDAP_P((
    LDAP *ld,
    struct berval *cookie,
    int commit,
    LDAPControl     **sctrls,
    LDAPControl     **cctrls,
    int             *msgidp ));

LDAP_F( int )
ldap_txn_end_s LDAP_P((
    LDAP *ld,
    struct berval *cookie,
    int commit,
    LDAPControl **sctrls,
    LDAPControl **cctrls ));
#endif*)


(*
 * LDAP Who Am I?
 *  in whoami.c
 *)
const
  LDAP_API_FEATURE_WHOAMI           = 1000;

function ldap_parse_whoami(
    ld              : PLDAP;
    res             : PLDAPMessage;
    var authzid     : PBerval
  ): cint; cdecl; external;

function ldap_whoami(
    ld              : PLDAP;
    sctrl           : PPLDAPControl;
    cctrl           : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_whoami_s(
    ld              : PLDAP;
    var authzid     : PBerval;
    sctrl           : PPLDAPControl;
    cctrl           : PPLDAPControl
  ): cint; cdecl; external;


(*
 * LDAP Password Modify
 *  in passwd.c
 *)
const
  LDAP_API_FEATURE_PASSWD_MODIFY    = 1000;

function ldap_parse_passwd(
    ld              : PLDAP;
    res             : PLDAPMessage;
    newpasswd       : PBerval
  ): cint; cdecl; external;

function ldap_passwd(
    ld              : PLDAP;
    user            : PBerval;
    oldpw           : PBerval;
    newpw           : PBerval;
    sctrl           : PPLDAPControl;
    cctrl           : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_passwd_s(
    ld              : PLDAP;
    user            : PBerval;
    oldpw           : PBerval;
    newpw           : PBerval;
    newpasswd       : PBerval;
    sctrl           : PPLDAPControl;
    cctrl           : PPLDAPControl
  ): cint; cdecl; external;


(*
 * LDAP Password Policy controls
 *  in ppolicy.c
 *)

(*
#ifdef LDAP_CONTROL_PASSWORDPOLICYREQUEST
#define LDAP_API_FEATURE_PASSWORD_POLICY 1000

typedef enum passpolicyerror_enum {
       PP_passwordExpired = 0,
       PP_accountLocked = 1,
       PP_changeAfterReset = 2,
       PP_passwordModNotAllowed = 3,
       PP_mustSupplyOldPassword = 4,
       PP_insufficientPasswordQuality = 5,
       PP_passwordTooShort = 6,
       PP_passwordTooYoung = 7,
       PP_passwordInHistory = 8,
       PP_noError = 65535
} LDAPPasswordPolicyError;

LDAP_F( int )
ldap_create_passwordpolicy_control LDAP_P((
        LDAP *ld,
        LDAPControl **ctrlp ));

LDAP_F( int )
ldap_parse_passwordpolicy_control LDAP_P((
        LDAP *ld,
        LDAPControl *ctrl,
        int *expirep,
        int *gracep,
        LDAPPasswordPolicyError *errorp ));

LDAP_F( const cchar * )
ldap_passwordpolicy_err2txt LDAP_P(( LDAPPasswordPolicyError ));
#endif*)


(*
 * hacks for NTLM
 *)
const
  LDAP_AUTH_NTLM_REQUEST            = ber_tag_t($8a);
  LDAP_AUTH_NTLM_RESPONSE           = ber_tag_t($8b);

function ldap_ntlm_bind(
    ld              : PLDAP;
    const dn        : pcchar;
    tag             : ber_tag_t;
    cred            : PBerval;
    sctrl           : PPLDAPControl;
    cctrl           : PPLDAPControl;
    var msgidp      : cint
  ): cint; cdecl; external;

function ldap_parse_ntlm_bind_result(
    ld              : PLDAP;
    res             : PLDAPMessage;
    challenge       : PBerval
  ): cint; cdecl; external;
