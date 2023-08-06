unit openssl3;

{$mode objfpc}{$H+}


{$LINKLIB libcrypto_shared.a}
{$LINKLIB libssl_shared.a}

interface
uses
  exec, utility, ctypes, Sysutils;

// OpenSSL3 Constants:
const
  OPENSSL3NAME = 'openssl3.library';

const
  // EVP.h Constants

  EVP_MAX_MD_SIZE       = 64; //* longest known is SHA512 */
  EVP_MAX_KEY_LENGTH    = 32;
  EVP_MAX_IV_LENGTH     = 16;
  EVP_MAX_BLOCK_LENGTH  = 32;
  SHA_DIGEST_LENGTH = 20;

// SSL types:
type
  PFunction = procedure;

  SSL_CTX = record end;
  PSSL_CTX = ^SSL_CTX;

  SslPtr = Pointer;
  PEVP_MD = SslPtr;
  PBIO_METHOD = SslPtr;
  PBIO = SslPtr;
  PSSL = SslPtr;
  PSSL_METHOD = SslPtr;
  PRSA = SslPtr;
  PASN1_cInt = SslPtr;
  PPasswdCb = SslPtr;
  PCallbackCb = SslPtr;

  PDH = pointer;
  PSTACK_OFX509 = pointer;

  PASN1_UTCTIME = SslPtr;
  PASN1_INTEGER = SSlPtr;
  POPENSSL_INIT_SETTINGS = SSLPtr;

  PX509_STORE_CTX = SslPtr;
  TSSLCTXVerifyCallback = function (ok : cInt; ctx : PX509_STORE_CTX) : Cint; cdecl;

  ASN1_STRING = record
  length: integer;
  asn1_type: integer;
  data: pointer;
  flags: longint;
  end;
  PASN1_STRING = ^ASN1_STRING;
  PASN1_TIME = PASN1_STRING;

  X509_NAME = record
    entries: pointer;
    modified: integer;
    bytes: pointer;
    hash: cardinal;
  end;
  PX509_NAME = ^X509_NAME;
  PDN = ^X509_NAME;

  X509_VAL = record
  notBefore: PASN1_TIME;
    notAfter: PASN1_TIME;
  end;
  PX509_VAL = ^X509_VAL;

  X509_CINF = record
    version: pointer;
    serialNumber: pointer;
    signature: pointer;
    issuer: pointer;
    validity: PX509_VAL;
    subject: pointer;
    key: pointer;
    issuerUID: pointer;
    subjectUID: pointer;
    extensions: pointer;
  end;
  PX509_CINF = ^X509_CINF;

  CRYPTO_EX_DATA = record
    sk: pointer;
    dummy: integer;
  end;

  X509 = record
    cert_info: PX509_CINF;
    sig_alg: pointer;  // ^X509_ALGOR
    signature: pointer;  // ^ASN1_BIT_STRING
    valid: integer;
    references: integer;
    name: PChar;
    ex_data: CRYPTO_EX_DATA;
    ex_pathlen: integer;
    ex_flags: integer;
    ex_kusage: integer;
    ex_xkusage: integer;
    ex_nscert: integer;
    skid: pointer;  // ^ASN1_OCTET_STRING
    akid: pointer;  // ?
    sha1_hash: array [0..SHA_DIGEST_LENGTH-1] of char;
    aux: pointer;  // ^X509_CERT_AUX
  end;
  pX509 = ^X509;
  PPX509 = ^PX509;

  DSA = record
    pad: integer;
    version: integer;
    write_params: integer;
    p: pointer;
    q: pointer;
    g: pointer;
    pub_key: pointer;
    priv_key: pointer;
    kinv: pointer;
    r: pointer;
    flags: integer;
    method_mont_p: PChar;
    references: integer;
    ex_data: record
        sk: pointer;
        dummy: integer;
      end;
    meth: pointer;
  end;
  pDSA = ^DSA;

  EVP_PKEY_PKEY = record
    case integer of
      0: (ptr: PChar);
      1: (rsa: pRSA);
      2: (dsa: pDSA);
      3: (dh: pDH);
   end;

  EVP_PKEY = record
    ktype: integer;
    save_type: integer;
    references: integer;
    pkey: EVP_PKEY_PKEY;
    save_parameters: integer;
    attributes: PSTACK_OFX509;
  end;
  PEVP_PKEY = ^EVP_PKEY;
  PPEVP_PKEY = ^PEVP_PKEY;


const
  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_WANT_X509_LOOKUP = 4;
  SSL_ERROR_SYSCALL = 5; //look at error stack/return value/errno
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_ERROR_WANT_CONNECT = 7;
  SSL_ERROR_WANT_ACCEPT = 8;
  SSL_ERROR_WANT_CHANNEL_ID_LOOKUP = 9;
  SSL_ERROR_PENDING_SESSION = 11;

  // BIO

  BIO_NOCLOSE         = $00;
  BIO_CLOSE           = $01;

  //* modifiers */
  BIO_FP_READ   = $02;
  BIO_FP_WRITE    = $04;
  BIO_FP_APPEND   = $08;
  BIO_FP_TEXT   = $10;

  BIO_C_SET_CONNECT                 = 100;
  BIO_C_DO_STATE_MACHINE            = 101;
  BIO_C_SET_NBIO              = 102;
  BIO_C_SET_PROXY_PARAM             = 103;
  BIO_C_SET_FD                      = 104;
  BIO_C_GET_FD                = 105;
  BIO_C_SET_FILE_PTR              = 106;
  BIO_C_GET_FILE_PTR              = 107;
  BIO_C_SET_FILENAME              = 108;
  BIO_C_SET_SSL               = 109;
  BIO_C_GET_SSL               = 110;
  BIO_C_SET_MD                = 111;
  BIO_C_GET_MD                      = 112;
  BIO_C_GET_CIPHER_STATUS           = 113;
  BIO_C_SET_BUF_MEM               = 114;
  BIO_C_GET_BUF_MEM_PTR       = 115;
  BIO_C_GET_BUFF_NUM_LINES          = 116;
  BIO_C_SET_BUFF_SIZE             = 117;
  BIO_C_SET_ACCEPT              = 118;
  BIO_C_SSL_MODE              = 119;
  BIO_C_GET_MD_CTX              = 120;
  BIO_C_GET_PROXY_PARAM             = 121;
  BIO_C_SET_BUFF_READ_DATA      = 122; // data to read first */
  BIO_C_GET_CONNECT       = 123;
  BIO_C_GET_ACCEPT        = 124;
  BIO_C_SET_SSL_RENEGOTIATE_BYTES   = 125;
  BIO_C_GET_SSL_NUM_RENEGOTIATES    = 126;
  BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT = 127;
  BIO_C_FILE_SEEK       = 128;
  BIO_C_GET_CIPHER_CTX        = 129;
  BIO_C_SET_BUF_MEM_EOF_RETURN  = 130;//*return end of input value*/
  BIO_C_SET_BIND_MODE   = 131;
  BIO_C_GET_BIND_MODE   = 132;
  BIO_C_FILE_TELL   = 133;
  BIO_C_GET_SOCKS   = 134;
  BIO_C_SET_SOCKS   = 135;

  BIO_C_SET_WRITE_BUF_SIZE  = 136;//* for BIO_s_bio */
  BIO_C_GET_WRITE_BUF_SIZE  = 137;
  BIO_C_MAKE_BIO_PAIR   = 138;
  BIO_C_DESTROY_BIO_PAIR  = 139;
  BIO_C_GET_WRITE_GUARANTEE = 140;
  BIO_C_GET_READ_REQUEST  = 141;
  BIO_C_SHUTDOWN_WR   = 142;
  BIO_C_NREAD0            = 143;
  BIO_C_NREAD     = 144;
  BIO_C_NWRITE0     = 145;
  BIO_C_NWRITE      = 146;
  BIO_C_RESET_READ_REQUEST  = 147;
  BIO_C_SET_MD_CTX    = 148;

  BIO_C_SET_PREFIX    = 149;
  BIO_C_GET_PREFIX    = 150;
  BIO_C_SET_SUFFIX    = 151;
  BIO_C_GET_SUFFIX    = 152;

  BIO_C_SET_EX_ARG    = 153;
  BIO_C_GET_EX_ARG    = 154;

  BIO_CTRL_RESET  =    1  ; { opt - rewind/zero etc }
  BIO_CTRL_EOF    =    2  ; { opt - are we at the eof }
  BIO_CTRL_INFO   =     3  ; { opt - extra tit-bits }
  BIO_CTRL_SET    =     4  ; { man - set the 'IO' type }
  BIO_CTRL_GET    =     5  ; { man - get the 'IO' type }
  BIO_CTRL_PUSH   =     6  ; { opt - internal, used to signify change }
  BIO_CTRL_POP    =     7  ; { opt - internal, used to signify change }
  BIO_CTRL_GET_CLOSE =  8  ; { man - set the 'close' on free }
  BIO_CTRL_SET_CLOSE =  9  ; { man - set the 'close' on free }
  ABIO_CTRL_PENDING   =  10  ; { opt - is their more data buffered }
  BIO_CTRL_FLUSH     =  11  ; { opt - 'flush' buffered output }
  BIO_CTRL_DUP       =  12  ; { man - extra stuff for 'duped' BIO }
  BIO_CTRL_WPENDING  =  13  ; { opt - number of bytes still to write }
  BIO_CTRL_SET_CALLBACK   = 14  ; { opt - set callback function }
  BIO_CTRL_GET_CALLBACK   = 15  ; { opt - set callback function }
  BIO_CTRL_SET_FILENAME   = 30  ; { BIO_s_file special }
  BIO_CTRL_DGRAM_CONNECT  = 31  ; { BIO dgram special }
  BIO_CTRL_DGRAM_SET_CONNECTED      = 32  ; { allow for an externally }
  BIO_CTRL_DGRAM_SET_RECV_TIMEOUT   = 33 ; { setsockopt, essentially }
  BIO_CTRL_DGRAM_GET_RECV_TIMEOUT   = 34 ; { getsockopt, essentially }
  BIO_CTRL_DGRAM_SET_SEND_TIMEOUT   = 35 ; { setsockopt, essentially }
  BIO_CTRL_DGRAM_GET_SEND_TIMEOUT   = 36 ; { getsockopt, essentially }
  BIO_CTRL_DGRAM_GET_RECV_TIMER_EXP = 37 ; { flag whether the last }
  BIO_CTRL_DGRAM_GET_SEND_TIMER_EXP = 38 ; { I/O operation tiemd out }
  BIO_CTRL_DGRAM_MTU_DISCOVER       = 39 ; { set DF bit on egress packets }
  BIO_CTRL_DGRAM_QUERY_MTU          = 40 ; { as kernel for current MTU }
  BIO_CTRL_DGRAM_GET_FALLBACK_MTU   = 47 ;
  BIO_CTRL_DGRAM_GET_MTU            = 41 ; { get cached value for MTU }
  BIO_CTRL_DGRAM_SET_MTU            = 42 ; { set cached value for }
  BIO_CTRL_DGRAM_MTU_EXCEEDED       = 43 ; { check whether the MTU }
  BIO_CTRL_DGRAM_GET_PEER           = 46 ;
  BIO_CTRL_DGRAM_SET_PEER           = 44 ; { Destination for the data }
  BIO_CTRL_DGRAM_SET_NEXT_TIMEOUT   = 45 ; { Next DTLS handshake timeout to }
  BIO_CTRL_DGRAM_SCTP_SET_IN_HANDSHAKE = 50;
  BIO_CTRL_DGRAM_SCTP_ADD_AUTH_KEY     = 51;
  BIO_CTRL_DGRAM_SCTP_NEXT_AUTH_KEY    = 52;
  BIO_CTRL_DGRAM_SCTP_AUTH_CCS_RCVD    = 53;
  BIO_CTRL_DGRAM_SCTP_GET_SNDINFO      = 60;
  BIO_CTRL_DGRAM_SCTP_SET_SNDINFO      = 61;
  BIO_CTRL_DGRAM_SCTP_GET_RCVINFO      = 62;
  BIO_CTRL_DGRAM_SCTP_SET_RCVINFO      = 63;
  BIO_CTRL_DGRAM_SCTP_GET_PRINFO       = 64;
  BIO_CTRL_DGRAM_SCTP_SET_PRINFO       = 65;
  BIO_CTRL_DGRAM_SCTP_SAVE_SHUTDOWN    = 70;

  ALLBACK = 15;
  SSL_CTRL_SET_MSG_CALLBACK_ARG = 16;
  SSL_CTRL_SET_MTU = 17;
  SSL_CTRL_SESS_NUMBER = 20;
  SSL_CTRL_SESS_CONNECT = 21;
  SSL_CTRL_SESS_CONNECT_GOOD = 22;
  SSL_CTRL_SESS_CONNECT_RENEGOTIATE = 23;
  SSL_CTRL_SESS_ACCEPT = 24;
  SSL_CTRL_SESS_ACCEPT_GOOD = 25;
  SSL_CTRL_SESS_ACCEPT_RENEGOTIATE = 26;
  SSL_CTRL_SESS_HIT = 27;
  SSL_CTRL_SESS_CB_HIT = 28;
  SSL_CTRL_SESS_MISSES = 29;
  SSL_CTRL_SESS_TIMEOUTS = 30;
  SSL_CTRL_SESS_CACHE_FULL = 31;
  SSL_CTRL_OPTIONS = 32;
  SSL_CTRL_MODE = 33;
  SSL_CTRL_GET_READ_AHEAD = 40;
  SSL_CTRL_SET_READ_AHEAD = 41;
  SSL_CTRL_SET_SESS_CACHE_SIZE = 42;
  SSL_CTRL_GET_SESS_CACHE_SIZE = 43;
  SSL_CTRL_SET_SESS_CACHE_MODE = 44;
  SSL_CTRL_GET_SESS_CACHE_MODE = 45;
  SSL_CTRL_GET_MAX_CERT_LIST = 50;
  SSL_CTRL_SET_MAX_CERT_LIST = 51;
  SSL_CTRL_SET_MAX_SEND_FRAGMENT              = 52;
  SSL_CTRL_SET_TLSEXT_SERVERNAME_CB           = 53;
  SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG          = 54;
  SSL_CTRL_SET_TLSEXT_HOSTNAME                = 55;
  SSL_CTRL_SET_TLSEXT_DEBUG_CB                = 56;
  SSL_CTRL_SET_TLSEXT_DEBUG_ARG               = 57;
  SSL_CTRL_GET_TLSEXT_TICKET_KEYS             = 58;
  SSL_CTRL_SET_TLSEXT_TICKET_KEYS             = 59;
  SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT        = 60;
  SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB     = 61;
  SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB_ARG = 62;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB           = 63;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG       = 64;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE         = 65;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS         = 66;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS         = 67;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS          = 68;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS          = 69;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP    = 70;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP    = 71;
  SSL_CTRL_SET_TLSEXT_TICKET_KEY_CB           = 72;
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB        = 75;
  SSL_CTRL_SET_SRP_VERIFY_PARAM_CB            = 76;
  SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB         = 77;
  SSL_CTRL_SET_SRP_ARG                        = 78;
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME           = 79;
  SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH           = 80;
  SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD           = 81;
  SSL_CTRL_GET_EXTRA_CHAIN_CERTS              = 82;
  SSL_CTRL_CLEAR_EXTRA_CHAIN_CERTS            = 83;
  SSL_CTRL_TLS_EXT_SEND_HEARTBEAT             = 85;
  SSL_CTRL_GET_TLS_EXT_HEARTBEAT_PENDING      = 86;
  SSL_CTRL_SET_TLS_EXT_HEARTBEAT_NO_REQUESTS  = 87;
  SSL_CTRL_CHAIN                              = 88;
  SSL_CTRL_CHAIN_CERT                         = 89;
  SSL_CTRL_GET_CURVES                         = 90;
  SSL_CTRL_SET_CURVES                         = 91;
  SSL_CTRL_SET_CURVES_LIST                    = 92;
  SSL_CTRL_GET_SHARED_CURVE                   = 93;
  SSL_CTRL_SET_ECDH_AUTO                      = 94;
  SSL_CTRL_SET_SIGALGS                        = 97;
  SSL_CTRL_SET_SIGALGS_LIST                   = 98;
  SSL_CTRL_CERT_FLAGS                         = 99;
  SSL_CTRL_CLEAR_CERT_FLAGS                   = 100;
  SSL_CTRL_SET_CLIENT_SIGALGS                 = 101;
  SSL_CTRL_SET_CLIENT_SIGALGS_LIST            = 102;
  SSL_CTRL_GET_CLIENT_CERT_TYPES              = 103;
  SSL_CTRL_SET_CLIENT_CERT_TYPES              = 104;
  SSL_CTRL_BUILD_CERT_CHAIN                   = 105;
  SSL_CTRL_SET_VERIFY_CERT_STORE              = 106;
  SSL_CTRL_SET_CHAIN_CERT_STORE               = 107;
  SSL_CTRL_GET_PEER_SIGNATURE_NID             = 108;
  SSL_CTRL_GET_SERVER_TMP_KEY                 = 109;
  SSL_CTRL_GET_RAW_CIPHERLIST                 = 110;
  SSL_CTRL_GET_EC_POINT_FORMATS               = 111;
  SSL_CTRL_GET_TLSA_RECORD                    = 112;
  SSL_CTRL_SET_TLSA_RECORD                    = 113;
  SSL_CTRL_PULL_TLSA_RECORD                   = 114;
  SSL_CTRL_GET_CHAIN_CERTS                    = 115;
  SSL_CTRL_SELECT_CURRENT_CERT                = 116;
  SSL_CTRL_CHANNEL_ID                         = 117;
  SSL_CTRL_GET_CHANNEL_ID                     = 118;
  SSL_CTRL_SET_CHANNEL_ID                     = 119;

  TLSEXT_TYPE_server_name = 0;
  TLSEXT_TYPE_max_fragment_length = 1;
  TLSEXT_TYPE_client_certificate_url = 2;
  TLSEXT_TYPE_trusted_ca_keys = 3;
  TLSEXT_TYPE_truncated_hmac = 4;
  TLSEXT_TYPE_status_request = 5;
  TLSEXT_TYPE_user_mapping = 6;
  TLSEXT_TYPE_client_authz = 7;
  TLSEXT_TYPE_server_authz = 8;
  TLSEXT_TYPE_cert_type = 9;
  TLSEXT_TYPE_elliptic_curves = 10;
  TLSEXT_TYPE_ec_point_formats = 11;
  TLSEXT_TYPE_srp = 12;
  TLSEXT_TYPE_signature_algorithms = 13;
  TLSEXT_TYPE_use_srtp = 14;
  TLSEXT_TYPE_heartbeat = 15;
  TLSEXT_TYPE_session_ticket = 35;
  TLSEXT_TYPE_renegotiate = $ff01;
  TLSEXT_TYPE_next_proto_neg = 13172;
  TLSEXT_NAMETYPE_host_name = 0;
  TLSEXT_STATUSTYPE_ocsp = 1;
  TLSEXT_ECPOINTFORMAT_first = 0;
  TLSEXT_ECPOINTFORMAT_uncompressed = 0;
  TLSEXT_ECPOINTFORMAT_ansiX962_compressed_prime = 1;
  TLSEXT_ECPOINTFORMAT_ansiX962_compressed_char2 = 2;
  TLSEXT_ECPOINTFORMAT_last = 2;
  TLSEXT_signature_anonymous = 0;
  TLSEXT_signature_rsa = 1;
  TLSEXT_signature_dsa = 2;
  TLSEXT_signature_ecdsa = 3;
  TLSEXT_hash_none = 0;
  TLSEXT_hash_md5 = 1;
  TLSEXT_hash_sha1 = 2;
  TLSEXT_hash_sha224 = 3;
  TLSEXT_hash_sha256 = 4;
  TLSEXT_hash_sha384 = 5;
  TLSEXT_hash_sha512 = 6;
  TLSEXT_MAXLEN_host_name = 255;

  SSL_TLSEXT_ERR_OK = 0;
  SSL_TLSEXT_ERR_ALERT_WARNING = 1;
  SSL_TLSEXT_ERR_ALERT_FATAL = 2;
  SSL_TLSEXT_ERR_NOACK = 3;

  SSL_FILETYPE_ASN1 = 2;
  SSL_FILETYPE_PEM = 1;
  EVP_PKEY_RSA = 6;

  // ASN1 values
  V_ASN1_EOC                     = 0;
  V_ASN1_BOOLEAN                 = 1;
  V_ASN1_INTEGER                 = 2;
  V_ASN1_BIT_STRING              = 3;
  V_ASN1_OCTET_STRING            = 4;
  V_ASN1_NULL                    = 5;
  V_ASN1_OBJECT                  = 6;
  V_ASN1_OBJECT_DESCRIPTOR       = 7;
  V_ASN1_EXTERNAL                = 8;
  V_ASN1_REAL                    = 9;
  V_ASN1_ENUMERATED              = 10;
  V_ASN1_UTF8STRING              = 12;
  V_ASN1_SEQUENCE                = 16;
  V_ASN1_SET                     = 17;
  V_ASN1_NUMERICSTRING           = 18;
  V_ASN1_PRINTABLESTRING         = 19;
  V_ASN1_T61STRING               = 20;
  V_ASN1_TELETEXSTRING           = 20;
  V_ASN1_VIDEOTEXSTRING          = 21;
  V_ASN1_IA5STRING               = 22;
  V_ASN1_UTCTIME                 = 23;
  V_ASN1_GENERALIZEDTIME         = 24;
  V_ASN1_GRAPHICSTRING           = 25;
  V_ASN1_ISO64STRING             = 26;
  V_ASN1_VISIBLESTRING           = 26;
  V_ASN1_GENERALSTRING           = 27;
  V_ASN1_UNIVERSALSTRING         = 28;
  V_ASN1_BMPSTRING               = 30;

//DES modes
  DES_ENCRYPT = 1;
  DES_DECRYPT = 0;

// Error codes for ECDH Function
  ECDH_F_ECDH_COMPUTE_KEY = 100;
  ECDH_F_ECDH_DATA_NEW_METHOD = 101;

// Error codes for ECDH Reason
  ECDH_R_NO_PRIVATE_VALUE = 100;
  ECDH_R_POINT_ARITHMETIC_FAILURE = 101;
  ECDH_R_KDF_FAILED = 102;

// SSL Constants:
const
  SSL_VERIFY_NONE = $00;
  SSL_VERIFY_PEER = $01;

type
  PASN1_BIT_STRING = Pointer;
  PPASN1_BIT_STRING = ^PASN1_BIT_STRING;


  PASN1_TYPE = Pointer;
  PASN1_ITEM = Pointer;
  PASN1_OBJECT = Pointer;
// SSLv23_client_method = TLS_client_method

function ASN1_TYPE_new(): PASN1_TYPE; external;
procedure ASN1_TYPE_free(asn1type: PASN1_TYPE); external;
function d2i_ASN1_TYPE(asn1type: PASN1_TYPE; in1: PPChar; Len: LongInt): PASN1_TYPE;  external;
function i2d_ASN1_TYPE(asn1type: PASN1_TYPE; out1: PPChar): LongInt;  external;
function ASN1_ANY_it(): PASN1_ITEM;  external;
function ASN1_TYPE_get(asn1type: PASN1_TYPE): LongInt;  external;
procedure ASN1_TYPE_set(asn1type: PASN1_TYPE; Type_: LongInt; Value: Pointer);  external;

function ASN1_OBJECT_new(): PASN1_OBJECT; external;
procedure ASN1_OBJECT_free(asn1object: PASN1_OBJECT); external;
function i2d_ASN1_OBJECT(asn1object: PASN1_OBJECT; PP: PChar): LongInt; external;
function d2i_ASN1_OBJECT(asn1object: PASN1_OBJECT; PP: PChar; Len: LongInt): LongInt; external;
function ASN1_OBJECT_it(): PASN1_ITEM; external;
function ASN1_STRING_new(): PASN1_STRING;  external;
procedure ASN1_STRING_free(asn1string: PASN1_STRING);  external;
function ASN1_STRING_dup(asn1strig: PASN1_STRING): PASN1_STRING; external;
function ASN1_STRING_type_new(Type_: LongInt): PASN1_STRING; external;
function ASN1_STRING_cmp(a: PASN1_STRING; b: PASN1_STRING): LongInt; external;
function ASN1_STRING_set(asn1strig: PASN1_STRING; Data: Pointer ; len: LongInt ): LongInt; external;
function ASN1_STRING_length(asn1strig: PASN1_STRING ): LongInt; external;
procedure ASN1_STRING_length_set(asn1strig: PASN1_STRING ; len: LongInt ); external;
function ASN1_STRING_type(asn1strig: PASN1_STRING ): LongInt; external;
function ASN1_STRING_data(asn1strig: PASN1_STRING ): PChar; external;
function ASN1_BIT_STRING_new(): PASN1_BIT_STRING; external;
procedure ASN1_BIT_STRING_free(s: PASN1_BIT_STRING ); external;
function d2i_ASN1_BIT_STRING(s: PPASN1_BIT_STRING ; in_: PPChar ; Len: LongInt ): PASN1_BIT_STRING; external;
function i2d_ASN1_BIT_STRING(s: PASN1_BIT_STRING ; Out_: PPChar ): LongInt; external;
function ASN1_BIT_STRING_it(): PASN1_ITEM; external;

function ASN1_BIT_STRING_set(s: PASN1_BIT_STRING ; data: PChar ; Len: LongInt ): LongInt; external;

function ASN1_UTCTIME_set_string(t : PASN1_UTCTIME ; S : PAnsiChar ): cint; external;


procedure ASN1_UTCTIME_free(a: PASN1_UTCTIME); external;
function ASN1_INTEGER_set(a: PASN1_INTEGER ; v: integer ): integer; external;
function ASN1_INTEGER_get(a: PASN1_INTEGER ): integer; external;

function BIO_ctrl_pending(b: PBIO ): cInt; external;
function BIO_new(b: PBIO_METHOD ): PBIO; external;
function BIO_read(bio: PBIO ; Data: Pointer ; Len: LongInt ): LongInt; external;
function BIO_write(b: PBIO ; Buf: PChar ; Len: cInt ): cInt; external;
function BIO_puts(bio: PBIO ; Buf: PAnsiChar ): LongInt; external;


function BIO_ctrl(bio: PBIO ; Cmd: LongInt ; larg: LongInt ; PArg: Pointer ): LongInt external;

procedure BIO_free_all(bio: PBIO ); external;
function BIO_s_mem: PBIO_METHOD; external;

function ERR_get_error: cInt; external;

procedure ERR_clear_error; external;
procedure ERR_error_string_n(e: cInt ; buf: PChar ; len: cInt ); external;

function EVP_get_digestbyname(Name: PChar ): PEVP_MD; external;
function EVP_PKEY_assign(pkey: PEVP_PKEY ; _type: cInt ; key: Prsa ): cInt; external;

function EVP_PKEY_new: PEVP_PKEY; external;
procedure EVP_PKEY_free(pk: PEVP_PKEY ); external;

function PEM_write_bio_X509(bp: pBIO ; x: PX509 ): integer; external;

procedure PKCS12_free(p12: SslPtr ); external;
function PKCS12_parse(p12: SslPtr ; pass: PChar ; var pkey: SslPtr ;  var cert: SslPtr ; var ca: SslPtr ): cInt; external;
function d2i_PKCS12_bio(b:PBIO ; Pkcs12: SslPtr ): SslPtr; external;

function BIO_new_ssl_connect(ctx: PSSL_CTX ): PBIO; external;


function SSL_CTX_set_cipher_list(arg0: PSSL_CTX ; str: PChar ):cInt; external;
function SSL_CTX_new(Meth: Pointer ): PSSL_CTX; external;
procedure SSL_CTX_free(a: PSSL_CTX ); external;

function SSL_get_current_cipher(s: PSSL ):SslPtr; external;
function SSL_CIPHER_get_bits(c: SslPtr ; alg_bits: PcInt ):cInt; external;
function SSL_CIPHER_get_name(c: Sslptr ):PChar; external;

function SSL_pending(ssl: PSSL ):cInt; external;
function SSL_set_fd(s: PSSL ; fd: cInt ):cInt; external;

function SSL_CTX_use_RSAPrivateKey_file(ctx: PSSL_CTX ; const _file: PChar ; _type: cInt ):cInt; external;
function SSL_CTX_use_certificate_file(ctx: PSSL_CTX ; const _file: PChar ; _type: cInt ):cInt; external;
function SSL_CTX_use_certificate_chain_file(ctx: PSSL_CTX ; const _file: PChar ):cInt; external;


function SSL_get_peer_certificate(ssl: PSSL ):PX509; external;

procedure SSL_CTX_set_verify(ctx: PSSL_CTX ; mode: LongInt ; arg2: Pointer ); external;
function SSL_CTX_use_PrivateKey(ctx: PSSL_CTX ; pkey: sslptr ):cInt; external;
function SSL_CTX_use_PrivateKey_ASN1(pk: cInt ; ctx: PSSL_CTX ; d: sslptr ; len: cInt ):cInt; external;
function SSL_CTX_use_certificate(ctx: PSSL_CTX ; x: SslPtr ):cInt; external;
function SSL_CTX_use_certificate_ASN1(ctx: PSSL_CTX ; len: cInt ; d: SslPtr ):cInt; external;

procedure SSL_CTX_set_default_passwd_cb(ctx: PSSL_CTX ; cb: SslPtr ); external;
procedure SSL_CTX_set_default_passwd_cb_userdata(ctx: PSSL_CTX ; u: SslPtr ); external;
function SSL_new(ctx: PSSL_CTX ):PSSL; external;
procedure SSL_free(ssl: PSSL ); external;
function SSL_accept(ssl: PSSL ):cInt; external;
function SSL_connect(ssl: PSSL ):cInt; external;
function SSL_read(ssl: PSSL ; buf: PChar ; num: cInt ):cInt; external;
function SSL_peek(ssl: PSSL ; buf: PChar ; num: cInt ):cInt; external;
function SSL_write(ssl: PSSL ; const buf: PChar ; num: cInt ):cInt; external;
function SSL_ctrl(ssl: PSSL ; cmd: cInt ; larg: clong ; parg: Pointer ): cLong; external;

function SSL_CTX_ctrl(ctx: PSSL_CTX ; cmd: cInt ; larg: clong ; parg: Pointer ): cLong; external;
procedure SSL_CTX_callback_ctrl(ctx: PSSL_CTX ; _type: cInt ; cb: PCallbackCb ); external;
function SSL_get_error(s: PSSL ; ret_code: cInt ):cInt; external;
function SSL_get_version(ssl: PSSL ):PChar; external;

function TLSv1_method:PSSL_METHOD; external;
function TLSv1_server_method:PSSL_METHOD; external;
function TLSv1_client_method:PSSL_METHOD; external;

function SSL_shutdown(ssl: PSSL ):cInt; external;

function SSL_CTX_load_verify_locations(ctx: PSSL_CTX ; const CAfile: PChar ; const CApath: PChar ):cInt; external;
function SSL_get_verify_result(ssl: PSSL ):cInt; external;

function X509_sign(x: PX509 ; pkey: PEVP_PKEY ; const md: PEVP_MD ): cInt; external;

function X509_digest(data: PX509 ; _type: PEVP_MD ; md: PChar ; len: PcInt ):cInt; external;

function i2d_X509_bio(b: PBIO ; x: PX509 ): cInt; external;
function i2d_PrivateKey_bio(b: PBIO ; pkey: PEVP_PKEY ): cInt; external;

function X509_new: PX509; external;
procedure X509_free(x: PX509); external;

function X509_NAME_oneline(a: PX509_NAME ; buf: PChar ; size: cInt ):PChar; external;

function X509_set_version(x: PX509 ; version: cInt ): cInt; external;

function X509_get_serialNumber(x: PX509 ): PASN1_cInt; external;

function X509_set_issuer_name(x: PX509 ; name: PX509_NAME ): cInt; external;
function X509_get_issuer_name(a: PX509 ):PX509_NAME; external;

function X509_get_subject_name(a: PX509 ):PX509_NAME; external;
function X509_set1_notBefore(x: PX509 ; tm: PASN1_UTCTIME ): cInt; external;
function X509_set1_notAfter(x: PX509 ; tm: PASN1_UTCTIME ): cInt; external;
function X509_set_pubkey(x: PX509 ; pkey: PEVP_PKEY ): cInt; external;

function X509_NAME_add_entry_by_txt(name: PX509_NAME ; field: PChar ; _type: cInt ; bytes: PChar ; len: cInt ; loc: cInt ; _set: cInt ): cInt; external;
function X509_NAME_hash(x: PX509_NAME ):cuLong; external;
function X509_print(b: PBIO ; a: PX509 ): cInt; external;

function RSA_generate_key(bits: cInt ; e: cInt ; callback: PFunction ; cb_arg: SslPtr ): PRSA; external;


function SSL_get_servername(ssl: PSSL ; _type: cInt ): PChar; external;
function SSL_set_SSL_CTX(ssl: PSSL ; ctx: PSSL_CTX ): PSSL; external;


function OPENSSL_init_ssl(Opts: QWord; Settings: Pointer ): LongInt; external;
function TLS_client_method(): PSSL_METHOD; external;
function TLS_method():PSSL_METHOD; external;

// inlines
function SSLv23_client_method(): Pointer; inline;

function Asn1UtctimeNew: PASN1_UTCTIME;

// shortcuts for strings and Tbytes access
function BioRead(b: PBIO; var Buf: String; Len: cInt): cInt; overload; inline;
function BioRead(b: PBIO; Buf: TBytes; Len: cInt): cInt; overload; inline;
function BioWrite(b: PBIO; Buf: String; Len: cInt): cInt; overload; inline;
function BioWrite(b: PBIO; Buf: TBytes; Len: cInt): cInt; overload; inline;

function SSLGetServername(ssl: PSSL; _type: cInt = TLSEXT_NAMETYPE_host_name): string; inline;

function X509NameAddEntryByTxt(name: PX509_NAME; field: string; _type: cInt; bytes: string; len, loc, _set: cInt): cInt; inline;
function X509NameOneline(a: PX509_NAME; var buf: String; size: cInt):String; inline;
function X509Digest(data: PX509; _type: PEVP_MD; md: String; var len: cInt):cInt; inline;

function EvpGetDigestByName(Name: String): PEVP_MD; inline;

function SslGetVersion(ssl: PSSL):String;
function SslCtxSetCipherList(arg0: PSSL_CTX; var str: String):cInt; inline;
procedure SslCtxSetVerify(ctx: PSSL_CTX; mode: cInt; arg2: TSSLCTXVerifyCallback); inline;
function SslCtxUsePrivateKeyASN1(pk: cInt; ctx: PSSL_CTX; d: String; len: cLong):cInt;overload; inline;
function SslCtxUsePrivateKeyASN1(pk: cInt; ctx: PSSL_CTX; b: TBytes; len: cLong):cInt;overload; inline;
function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: String; _type: cInt):cInt; inline;

function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: cLong; d: String):cInt; overload; inline;
function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: cLong; Buf: TBytes): cInt; overload; inline;
function SslCtxUseCertificateFile(ctx: PSSL_CTX; const _file: String; _type: cInt):cInt; inline;
function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: String):cInt; inline;
function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; const CAfile: String; const CApath: String):cInt; inline;

function PKCS12parse(p12: SslPtr; pass: string; var pkey, cert, ca: SslPtr): cInt; inline;

function SSLCipherGetName(c: SslPtr):String; inline;
function SSLCipherGetBits(c: SslPtr; var alg_bits: cInt):cInt; inline;

procedure ErrErrorString(e: cInt; var buf: string; len: cInt); inline;


const
// crypto
// Standard initialisation options
  OPENSSL_INIT_NO_LOAD_CRYPTO_STRINGS = $00000001;
  OPENSSL_INIT_LOAD_CRYPTO_STRINGS    = $00000002;
  OPENSSL_INIT_ADD_ALL_CIPHERS        = $00000004;
  OPENSSL_INIT_ADD_ALL_DIGESTS        = $00000008;
  OPENSSL_INIT_NO_ADD_ALL_CIPHERS     = $00000010;
  OPENSSL_INIT_NO_ADD_ALL_DIGESTS     = $00000020;
  OPENSSL_INIT_LOAD_CONFIG            = $00000040;
  OPENSSL_INIT_NO_LOAD_CONFIG         = $00000080;
  OPENSSL_INIT_ASYNC                  = $00000100;
  OPENSSL_INIT_ENGINE_RDRAND          = $00000200;
  OPENSSL_INIT_ENGINE_DYNAMIC         = $00000400;
  OPENSSL_INIT_ENGINE_OPENSSL         = $00000800;
  OPENSSL_INIT_ENGINE_CRYPTODEV       = $00001000;
  OPENSSL_INIT_ENGINE_CAPI            = $00002000;
  OPENSSL_INIT_ENGINE_PADLOCK         = $00004000;
  OPENSSL_INIT_ENGINE_AFALG           = $00008000;
// OPENSSL_INIT flag = $00010000 reserved for internal use
// OPENSSL_INIT flag range = $fff00000 reserved for OPENSSL_init_ssl()
// Max OPENSSL_INIT flag value is = $80000000

// OPENSSL_INIT flag 0x010000 reserved for internal use
  OPENSSL_INIT_NO_LOAD_SSL_STRINGS   = $00100000;
  OPENSSL_INIT_LOAD_SSL_STRINGS      = $00200000;

function SSLeay_add_ssl_algorithms(): LongInt;
function SSL_load_error_strings(): LongInt;



function BioCtrlPending(b: PBIO): cInt; external;
procedure BioFreeAll(b: PBIO); external;


var
  OpenSSL3Base: PLibrary external name 'OpenSSL3Base';

implementation

function SSLeay_add_ssl_algorithms(): LongInt;
begin
  SSLeay_add_ssl_algorithms := OPENSSL_init_ssl(0, nil);
end;

function SSL_load_error_strings(): LongInt;
begin
  SSL_load_error_strings := OPENSSL_init_SSL(OPENSSL_INIT_LOAD_SSL_STRINGS or OPENSSL_INIT_LOAD_CRYPTO_STRINGS, nil);
end;

function SSLv23_client_method(): Pointer;
begin
  SSLv23_client_method := TLS_client_method;
end;

function BioRead(b: PBIO; var Buf: String; Len: cInt): cInt;
begin
  Result := Bio_Read(b, PChar(Buf), Len)
end;

function BioRead(b: PBIO; Buf: TBytes; Len: cInt): cInt;
begin
  Result := BIO_read(b, PChar(Buf), Len);
end;

function SSLGetServername(ssl: PSSL; _type: cInt = TLSEXT_NAMETYPE_host_name): string;
begin
  Result := PChar(SSL_Get_Servername(ssl, _type));
end;

function X509NameAddEntryByTxt(name: PX509_NAME; field: string; _type: cInt; bytes: string; len, loc, _set: cInt): cInt;
begin
  Result := X509_NAME_add_entry_by_txt(name, PChar(field), _type, PChar(Bytes), len, loc, _set);
end;

function Asn1UtctimeNew: PASN1_UTCTIME;
begin
  Result:=PASN1_UTCTIME(ASN1_STRING_type_new(V_ASN1_UTCTIME));
end;

function EvpGetDigestByName(Name: String): PEVP_MD;
begin
  Result := EVP_get_digestbyname(PChar(Name));
end;

function SslCtxSetCipherList(arg0: PSSL_CTX; var str: String):cInt;
begin
  Result := SSL_CTX_set_cipher_list(arg0, PChar(str))
end;

procedure SslCtxSetVerify(ctx: PSSL_CTX; mode: cInt; arg2: TSSLCTXVerifyCallback);
begin
  SSL_CTX_set_verify(ctx, mode, @arg2);
end;

function SslCtxUsePrivateKeyASN1(pk: cInt; ctx: PSSL_CTX; d: String; len: cLong):cInt; overload;
begin
  Result := SSL_CTX_use_PrivateKey_ASN1(pk, ctx, Sslptr(d), len)
end;

function SslCtxUsePrivateKeyASN1(pk: cInt; ctx: PSSL_CTX; b: TBytes; len: cLong): cInt;overload;
begin
  Result := SSL_CTX_use_PrivateKey_ASN1(pk, ctx, Sslptr(b), len)
end;

function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: String; _type: cInt):cInt;
begin
  Result := SSL_CTX_use_RSAPrivateKey_file(ctx, PChar(_file), _type);
end;

function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: cLong; d: String):cInt;
begin
  Result := SSL_CTX_use_certificate_ASN1(ctx, len, SslPtr(d))
end;

function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: cLong; Buf: TBytes): cInt;
begin
  Result := SSL_CTX_use_certificate_ASN1(ctx, len, SslPtr(Buf))
end;

function SslCtxUseCertificateFile(ctx: PSSL_CTX; const _file: String; _type: cInt):cInt;
begin
  Result := SSL_CTX_use_certificate_file(ctx, PChar(_file), _type)
end;

function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: String):cInt;
begin
  Result := SSL_CTX_use_certificate_chain_file(ctx, PChar(_file))
end;

function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; const CAfile: String; const CApath: String):cInt;
begin
  Result := SSL_CTX_load_verify_locations(ctx, SslPtr(CAfile), SslPtr(CApath))
end;

function BioWrite(b: PBIO; Buf: String; Len: cInt): cInt;
begin
  Result := Bio_Write(b, PChar(Buf), Len)
end;

function BioWrite(b: PBIO; Buf: TBytes; Len: cInt): cInt;
begin
  Result := Bio_Write(b, PChar(Buf), Len)
end;

function PKCS12parse(p12: SslPtr; pass: string; var pkey, cert, ca: SslPtr): cInt;
begin
  Result := PKCS12_parse(p12, SslPtr(pass), pkey, cert, ca)
end;

function SslGetVersion(ssl: PSSL):String;
begin
  Result := SSL_get_version(ssl)
end;

function X509NameOneline(a: PX509_NAME; var buf: String; size: cInt):String;
begin
  Result := X509_NAME_oneline(a, PChar(buf),size)
end;

function X509Digest(data: PX509; _type: PEVP_MD; md: String; var len: cInt):cInt;
begin
  Result := X509_digest(data, _type, PChar(md), @len)
end;

function SSLCipherGetName(c: SslPtr):String;
begin
  Result := SSL_CIPHER_get_name(c)
end;

function SSLCipherGetBits(c: SslPtr; var alg_bits: cInt):cInt;
begin
  Result := SSL_CIPHER_get_bits(c, @alg_bits)
end;

procedure ErrErrorString(e: cInt; var buf: string; len: cInt);
begin
  ERR_error_string_n(e, @buf[1], len);
  buf := PChar(Buf);
end;

initialization
  OpenSSL3Base := OpenLibrary('openssl3.library', 1);
finalization
  if Assigned(OpenSSL3Base) then
    CloseLibrary(OpenSSL3Base);
end.
