{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Michael Van Canneyt and Silvio Clecio (silvioprog),
    members of the Free Pascal development team

    Low-level binding for GnuTLS library.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}

unit gnutls;

interface

uses ctypes;

{
  Automatically converted by H2Pas 1.0.0 from gnutls.c
  The following command line parameters were used:
    -D
    -l
    libgnutls.so
    -o
    gnutls.pp
    -u
    gnutls
    -T
    -P
    -p
    -c
    -e
    gnutls.c
}

const
{$IFDEF WINDOWS}
  LibGnuTLSExt = 'dll';
{$ELSE}
{$ifDEF DARWIN}   
  LibGnuTLSExt = 'dylib';
{$ELSE}
  LibGnuTLSExt = 'so';
{$ENDIF}
{$ENDIF}
  LibGnuTLS ='libgnutls.'+LibGnuTLSExt; {Setup as you need}


{ Converted enums}
Const
  // Tgnutls_cipher_algorithm
  
  GNUTLS_CIPHER_UNKNOWN = 0;
  GNUTLS_CIPHER_NULL = 1;
  GNUTLS_CIPHER_ARCFOUR_128 = 2;
  GNUTLS_CIPHER_3DES_CBC = 3;
  GNUTLS_CIPHER_AES_128_CBC = 4;
  GNUTLS_CIPHER_AES_256_CBC = 5;
  GNUTLS_CIPHER_ARCFOUR_40 = 6;
  GNUTLS_CIPHER_CAMELLIA_128_CBC = 7;
  GNUTLS_CIPHER_CAMELLIA_256_CBC = 8;
  GNUTLS_CIPHER_AES_192_CBC = 9;
  GNUTLS_CIPHER_AES_128_GCM = 10;
  GNUTLS_CIPHER_AES_256_GCM = 11;
  GNUTLS_CIPHER_CAMELLIA_192_CBC = 12;
  GNUTLS_CIPHER_SALSA20_256 = 13;
  GNUTLS_CIPHER_ESTREAM_SALSA20_256 = 14;
  GNUTLS_CIPHER_CAMELLIA_128_GCM = 15;
  GNUTLS_CIPHER_CAMELLIA_256_GCM = 16;
  GNUTLS_CIPHER_RC2_40_CBC = 17;
  GNUTLS_CIPHER_DES_CBC = 18;
  GNUTLS_CIPHER_AES_128_CCM = 19;
  GNUTLS_CIPHER_AES_256_CCM = 20;
  GNUTLS_CIPHER_AES_128_CCM_8 = 21;
  GNUTLS_CIPHER_AES_256_CCM_8 = 22;
  GNUTLS_CIPHER_CHACHA20_POLY1305 = 23;
  GNUTLS_CIPHER_IDEA_PGP_CFB = 200;
  GNUTLS_CIPHER_3DES_PGP_CFB = 201;
  GNUTLS_CIPHER_CAST5_PGP_CFB = 202;
  GNUTLS_CIPHER_BLOWFISH_PGP_CFB = 203;
  GNUTLS_CIPHER_SAFER_SK128_PGP_CFB = 204;
  GNUTLS_CIPHER_AES128_PGP_CFB = 205;
  GNUTLS_CIPHER_AES192_PGP_CFB = 206;
  GNUTLS_CIPHER_AES256_PGP_CFB = 207;
  GNUTLS_CIPHER_TWOFISH_PGP_CFB = 208;


  // gnutls_kx_algorithm_t ;

  GNUTLS_KX_UNKNOWN = 0;
  GNUTLS_KX_RSA = 1;
  GNUTLS_KX_DHE_DSS = 2;
  GNUTLS_KX_DHE_RSA = 3;
  GNUTLS_KX_ANON_DH = 4;
  GNUTLS_KX_SRP = 5;
  GNUTLS_KX_RSA_EXPORT = 6;
  GNUTLS_KX_SRP_RSA = 7;
  GNUTLS_KX_SRP_DSS = 8;
  GNUTLS_KX_PSK = 9;
  GNUTLS_KX_DHE_PSK = 10;
  GNUTLS_KX_ANON_ECDH = 11;
  GNUTLS_KX_ECDHE_RSA = 12;
  GNUTLS_KX_ECDHE_ECDSA = 13;
  GNUTLS_KX_ECDHE_PSK = 14;
  GNUTLS_KX_RSA_PSK = 15;

  // Init options
  GNUTLS_SERVER                = 1;
  GNUTLS_CLIENT                = (1 shl 1);
  GNUTLS_DATAGRAM              = (1 shl 2);
  GNUTLS_NONBLOCK              = (1 shl 3);
  GNUTLS_NO_EXTENSIONS         = (1 shl 4);
  GNUTLS_NO_REPLAY_PROTECTION  = (1 shl 5);
  GNUTLS_NO_SIGNAL             = (1 shl 6);

  GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT = cardinal(-1);

  // gnutls_params_type_t
  GNUTLS_PARAMS_RSA_EXPORT = 1;
  GNUTLS_PARAMS_DH = 2;
  GNUTLS_PARAMS_ECDH = 3;

  // gnutls_credentials_type_t
  GNUTLS_CRD_CERTIFICATE = 1;
  GNUTLS_CRD_ANON = 2;
  GNUTLS_CRD_SRP = 3;
  GNUTLS_CRD_PSK = 4;
  GNUTLS_CRD_IA = 5;

  // gnutls_mac_algorithm_t
  GNUTLS_MAC_UNKNOWN = 0;
  GNUTLS_MAC_NULL = 1;
  GNUTLS_MAC_MD5 = 2;
  GNUTLS_MAC_SHA1 = 3;
  GNUTLS_MAC_RMD160 = 4;
  GNUTLS_MAC_MD2 = 5;
  GNUTLS_MAC_SHA256 = 6;
  GNUTLS_MAC_SHA384 = 7;
  GNUTLS_MAC_SHA512 = 8;
  GNUTLS_MAC_SHA224 = 9;
  GNUTLS_MAC_AEAD = 200;
  GNUTLS_MAC_UMAC_96 = 201;
  GNUTLS_MAC_UMAC_128 = 202;

  // gnutls_digest_algorithm_t
  GNUTLS_DIG_UNKNOWN = GNUTLS_MAC_UNKNOWN;
  GNUTLS_DIG_NULL = GNUTLS_MAC_NULL;
  GNUTLS_DIG_MD5 = GNUTLS_MAC_MD5;
  GNUTLS_DIG_SHA1 = GNUTLS_MAC_SHA1;
  GNUTLS_DIG_RMD160 = GNUTLS_MAC_RMD160;
  GNUTLS_DIG_MD2 = GNUTLS_MAC_MD2;
  GNUTLS_DIG_SHA256 = GNUTLS_MAC_SHA256;
  GNUTLS_DIG_SHA384 = GNUTLS_MAC_SHA384;
  GNUTLS_DIG_SHA512 = GNUTLS_MAC_SHA512;
  GNUTLS_DIG_SHA224 = GNUTLS_MAC_SHA224;

  // gnutls_compression_method_t
  GNUTLS_COMP_UNKNOWN = 0;
  GNUTLS_COMP_NULL = 1;
  GNUTLS_COMP_DEFLATE = 2;
  GNUTLS_COMP_ZLIB = GNUTLS_COMP_DEFLATE;

  // gnutls_alert_level_t
  GNUTLS_AL_WARNING = 1;
  GNUTLS_AL_FATAL = 2;

  // gnutls_alert_description_t ;
  GNUTLS_A_CLOSE_NOTIFY = 0;
  GNUTLS_A_UNEXPECTED_MESSAGE = 10;
  GNUTLS_A_BAD_RECORD_MAC = 20;
  GNUTLS_A_DECRYPTION_FAILED = 21;
  GNUTLS_A_RECORD_OVERFLOW = 22;
  GNUTLS_A_DECOMPRESSION_FAILURE = 30;
  GNUTLS_A_HANDSHAKE_FAILURE = 40;
  GNUTLS_A_SSL3_NO_CERTIFICATE = 41;
  GNUTLS_A_BAD_CERTIFICATE = 42;
  GNUTLS_A_UNSUPPORTED_CERTIFICATE = 43;
  GNUTLS_A_CERTIFICATE_REVOKED = 44;
  GNUTLS_A_CERTIFICATE_EXPIRED = 45;
  GNUTLS_A_CERTIFICATE_UNKNOWN = 46;
  GNUTLS_A_ILLEGAL_PARAMETER = 47;
  GNUTLS_A_UNKNOWN_CA = 48;
  GNUTLS_A_ACCESS_DENIED = 49;
  GNUTLS_A_DECODE_ERROR = 50;
  GNUTLS_A_DECRYPT_ERROR = 51;
  GNUTLS_A_EXPORT_RESTRICTION = 60;
  GNUTLS_A_PROTOCOL_VERSION = 70;
  GNUTLS_A_INSUFFICIENT_SECURITY = 71;
  GNUTLS_A_INTERNAL_ERROR = 80;
  GNUTLS_A_INAPPROPRIATE_FALLBACK = 86;
  GNUTLS_A_USER_CANCELED = 90;
  GNUTLS_A_NO_RENEGOTIATION = 100;
  GNUTLS_A_UNSUPPORTED_EXTENSION = 110;
  GNUTLS_A_CERTIFICATE_UNOBTAINABLE = 111;
  GNUTLS_A_UNRECOGNIZED_NAME = 112;
  GNUTLS_A_UNKNOWN_PSK_IDENTITY = 115;
  GNUTLS_A_NO_APPLICATION_PROTOCOL = 120;

  // gnutls_handshake_description_t

  GNUTLS_HANDSHAKE_HELLO_REQUEST = 0;
  GNUTLS_HANDSHAKE_CLIENT_HELLO = 1;
  GNUTLS_HANDSHAKE_SERVER_HELLO = 2;
  GNUTLS_HANDSHAKE_HELLO_VERIFY_REQUEST = 3;
  GNUTLS_HANDSHAKE_NEW_SESSION_TICKET = 4;
  GNUTLS_HANDSHAKE_CERTIFICATE_PKT = 11;
  GNUTLS_HANDSHAKE_SERVER_KEY_EXCHANGE = 12;
  GNUTLS_HANDSHAKE_CERTIFICATE_REQUEST = 13;
  GNUTLS_HANDSHAKE_SERVER_HELLO_DONE = 14;
  GNUTLS_HANDSHAKE_CERTIFICATE_VERIFY = 15;
  GNUTLS_HANDSHAKE_CLIENT_KEY_EXCHANGE = 16;
  GNUTLS_HANDSHAKE_FINISHED = 20;
  GNUTLS_HANDSHAKE_CERTIFICATE_STATUS = 22;
  GNUTLS_HANDSHAKE_SUPPLEMENTAL = 23;
  GNUTLS_HANDSHAKE_CHANGE_CIPHER_SPEC = 254;
  GNUTLS_HANDSHAKE_CLIENT_HELLO_V2 = 1024;

  // gnutls_certificate_status_t
  GNUTLS_CERT_INVALID = 1 shl 1;
  GNUTLS_CERT_REVOKED = 1 shl 5;
  GNUTLS_CERT_SIGNER_NOT_FOUND = 1 shl 6;
  GNUTLS_CERT_SIGNER_NOT_CA = 1 shl 7;
  GNUTLS_CERT_INSECURE_ALGORITHM = 1 shl 8;
  GNUTLS_CERT_NOT_ACTIVATED = 1 shl 9;
  GNUTLS_CERT_EXPIRED = 1 shl 10;
  GNUTLS_CERT_SIGNATURE_FAILURE = 1 shl 11;
  GNUTLS_CERT_REVOCATION_DATA_SUPERSEDED = 1 shl 12;
  GNUTLS_CERT_UNEXPECTED_OWNER = 1 shl 14;
  GNUTLS_CERT_REVOCATION_DATA_ISSUED_IN_FUTURE = 1 shl 15;
  GNUTLS_CERT_SIGNER_CONSTRAINTS_FAILURE = 1 shl 16;
  GNUTLS_CERT_MISMATCH = 1 shl 17;
  GNUTLS_CERT_PURPOSE_MISMATCH = 1 shl 18;

  // gnutls_certificate_request_t
  GNUTLS_CERT_IGNORE = 0;
  GNUTLS_CERT_REQUEST = 1;
  GNUTLS_CERT_REQUIRE = 2;

  // gnutls_openpgp_crt_status_t
  GNUTLS_OPENPGP_CERT = 0;
  GNUTLS_OPENPGP_CERT_FINGERPRINT = 1;

  // gnutls_close_request_t
  GNUTLS_SHUT_RDWR = 0;
  GNUTLS_SHUT_WR = 1;

  // gnutls_protocol_t
  GNUTLS_SSL3 = 1;
  GNUTLS_TLS1_0 = 2;
  GNUTLS_TLS1 = GNUTLS_TLS1_0;
  GNUTLS_TLS1_1 = 3;
  GNUTLS_TLS1_2 = 4;
  GNUTLS_DTLS0_9 = 200;
  GNUTLS_DTLS1_0 = 201;
  GNUTLS_DTLS1_2 = 202;
  GNUTLS_DTLS_VERSION_MIN = GNUTLS_DTLS0_9;
  GNUTLS_DTLS_VERSION_MAX = GNUTLS_DTLS1_2;
  GNUTLS_TLS_VERSION_MAX = GNUTLS_TLS1_2;
  GNUTLS_VERSION_UNKNOWN = $ff;

  // gnutls_certificate_type_t
  GNUTLS_CRT_UNKNOWN = 0;
  GNUTLS_CRT_X509 = 1;
  GNUTLS_CRT_OPENPGP = 2;
  GNUTLS_CRT_RAW = 3;

  // gnutls_x509_crt_fmt_t
  GNUTLS_X509_FMT_DER = 0;
  GNUTLS_X509_FMT_PEM = 1;

  // gnutls_certificate_print_formats
  GNUTLS_CRT_PRINT_FULL = 0;
  GNUTLS_CRT_PRINT_ONELINE = 1;
  GNUTLS_CRT_PRINT_UNSIGNED_FULL = 2;
  GNUTLS_CRT_PRINT_COMPACT = 3;
  GNUTLS_CRT_PRINT_FULL_NUMBERS = 4;

  // gnutls_pk_algorithm_t
  GNUTLS_PK_UNKNOWN = 0;
  GNUTLS_PK_RSA = 1;
  GNUTLS_PK_DSA = 2;
  GNUTLS_PK_DH = 3;
  GNUTLS_PK_EC = 4;

  // gnutls_sign_algorithm_t
  GNUTLS_SIGN_UNKNOWN = 0;
  GNUTLS_SIGN_RSA_SHA1 = 1;
  GNUTLS_SIGN_RSA_SHA = GNUTLS_SIGN_RSA_SHA1;
  GNUTLS_SIGN_DSA_SHA1 = 2;
  GNUTLS_SIGN_DSA_SHA = GNUTLS_SIGN_DSA_SHA1;
  GNUTLS_SIGN_RSA_MD5 = 3;
  GNUTLS_SIGN_RSA_MD2 = 4;
  GNUTLS_SIGN_RSA_RMD160 = 5;
  GNUTLS_SIGN_RSA_SHA256 = 6;
  GNUTLS_SIGN_RSA_SHA384 = 7;
  GNUTLS_SIGN_RSA_SHA512 = 8;
  GNUTLS_SIGN_RSA_SHA224 = 9;
  GNUTLS_SIGN_DSA_SHA224 = 10;
  GNUTLS_SIGN_DSA_SHA256 = 11;
  GNUTLS_SIGN_ECDSA_SHA1 = 12;
  GNUTLS_SIGN_ECDSA_SHA224 = 13;
  GNUTLS_SIGN_ECDSA_SHA256 = 14;
  GNUTLS_SIGN_ECDSA_SHA384 = 15;
  GNUTLS_SIGN_ECDSA_SHA512 = 16;
  GNUTLS_SIGN_DSA_SHA384 = 17;
  GNUTLS_SIGN_DSA_SHA512 = 18;

  // gnutls_ecc_curve_t
  GNUTLS_ECC_CURVE_INVALID = 0;
  GNUTLS_ECC_CURVE_SECP224R1 = 1;
  GNUTLS_ECC_CURVE_SECP256R1 = 2;
  GNUTLS_ECC_CURVE_SECP384R1 = 3;
  GNUTLS_ECC_CURVE_SECP521R1 = 4;
  GNUTLS_ECC_CURVE_SECP192R1 = 5;

  // Tgnutls_sec_param_t
  GNUTLS_SEC_PARAM_UNKNOWN = 0;
  GNUTLS_SEC_PARAM_INSECURE = 5;
  GNUTLS_SEC_PARAM_EXPORT = 10;
  GNUTLS_SEC_PARAM_VERY_WEAK = 15;
  GNUTLS_SEC_PARAM_WEAK = 20;
  GNUTLS_SEC_PARAM_LOW = 25;
  GNUTLS_SEC_PARAM_LEGACY = 30;
  GNUTLS_SEC_PARAM_MEDIUM = 35;
  GNUTLS_SEC_PARAM_HIGH = 40;
  GNUTLS_SEC_PARAM_ULTRA = 45;
  GNUTLS_SEC_PARAM_FUTURE = 50;

  // gnutls_channel_binding_t
  GNUTLS_CB_TLS_UNIQUE = 0;

  // gnutls_server_name_type_t
  GNUTLS_NAME_DNS = 1;

  // gnutls_supplemental_data_format_type_t
  GNUTLS_SUPPLEMENTAL_UNKNOWN = 0;

  // gnutls_srtp_profile_t
  GNUTLS_SRTP_AES128_CM_HMAC_SHA1_80 = $0001;
  GNUTLS_SRTP_AES128_CM_HMAC_SHA1_32 = $0002;
  GNUTLS_SRTP_NULL_HMAC_SHA1_80 = $0005;
  GNUTLS_SRTP_NULL_HMAC_SHA1_32 = $0006;

  // gnutls_vdata_types_t
  GNUTLS_DT_UNKNOWN = 0;
  GNUTLS_DT_DNS_HOSTNAME = 1;
  GNUTLS_DT_KEY_PURPOSE_OID = 2;
  GNUTLS_DT_RFC822NAME = 3;

  // gnutls_certificate_flags
  GNUTLS_CERTIFICATE_SKIP_KEY_CERT_MATCH = 1;

  // gnutls_random_art
  GNUTLS_RANDOM_ART_OPENSSH = 1;

  // gnutls_psk_key_flags
  GNUTLS_PSK_KEY_RAW = 0;
  GNUTLS_PSK_KEY_HEX = 1;
  // gnutls_privkey_type_t
  GNUTLS_PRIVKEY_X509 = 0;
  GNUTLS_PRIVKEY_OPENPGP = 1;
  GNUTLS_PRIVKEY_PKCS11 = 2;
  GNUTLS_PRIVKEY_EXT = 3;

  // gnutls_x509_subject_alt_name_t
  GNUTLS_SAN_DNSNAME = 1;
  GNUTLS_SAN_RFC822NAME = 2;
  GNUTLS_SAN_URI = 3;
  GNUTLS_SAN_IPADDRESS = 4;
  GNUTLS_SAN_OTHERNAME = 5;
  GNUTLS_SAN_DN = 6;
  GNUTLS_SAN_OTHERNAME_XMPP = 1000;

  // gnutls_pin_flag_t
  GNUTLS_PIN_USER = 1 shl 0;
  GNUTLS_PIN_SO = 1 shl 1;
  GNUTLS_PIN_FINAL_TRY = 1 shl 2;
  GNUTLS_PIN_COUNT_LOW = 1 shl 3;
  GNUTLS_PIN_CONTEXT_SPECIFIC = 1 shl 4;
  GNUTLS_PIN_WRONG = 1 shl 5;

  // gnutls_ext_parse_type_t
  GNUTLS_EXT_ANY = 0;
  GNUTLS_EXT_APPLICATION = 1;
  GNUTLS_EXT_TLS = 2;
  GNUTLS_EXT_MANDATORY = 3;
  GNUTLS_EXT_NONE = 4;


  // GNU TLS Error codes
  GNUTLS_E_SUCCESS = 0;

  GNUTLS_E_UNKNOWN_COMPRESSION_ALGORITHM= -3;
  GNUTLS_E_UNKNOWN_CIPHER_TYPE =-6;
  GNUTLS_E_LARGE_PACKET =-7;
  GNUTLS_E_UNSUPPORTED_VERSION_PACKET =-8;
  GNUTLS_E_UNEXPECTED_PACKET_LENGTH =-9;
  GNUTLS_E_INVALID_SESSION =-10;
  GNUTLS_E_FATAL_ALERT_RECEIVED =-12;
  GNUTLS_E_UNEXPECTED_PACKET =-15;
  GNUTLS_E_WARNING_ALERT_RECEIVED= -16;
  GNUTLS_E_ERROR_IN_FINISHED_PACKET= -18;
  GNUTLS_E_UNEXPECTED_HANDSHAKE_PACKET= -19;
  GNUTLS_E_UNKNOWN_CIPHER_SUITE= -21;
  GNUTLS_E_UNWANTED_ALGORITHM= -22;
  GNUTLS_E_MPI_SCAN_FAILED= -23;
  GNUTLS_E_DECRYPTION_FAILED= -24;
  GNUTLS_E_MEMORY_ERROR =-25;
  GNUTLS_E_DECOMPRESSION_FAILED =-26;
  GNUTLS_E_COMPRESSION_FAILED= -27;
  GNUTLS_E_AGAIN= -28;
  GNUTLS_E_EXPIRED =-29;
  GNUTLS_E_DB_ERROR =-30;
  GNUTLS_E_SRP_PWD_ERROR =-31;
  GNUTLS_E_INSUFFICIENT_CREDENTIALS =-32;
  GNUTLS_E_INSUFICIENT_CREDENTIALS =GNUTLS_E_INSUFFICIENT_CREDENTIALS;
  GNUTLS_E_INSUFFICIENT_CRED =GNUTLS_E_INSUFFICIENT_CREDENTIALS;
  GNUTLS_E_INSUFICIENT_CRED =GNUTLS_E_INSUFFICIENT_CREDENTIALS;

  GNUTLS_E_HASH_FAILED =-33;
  GNUTLS_E_BASE64_DECODING_ERROR =-34;

  GNUTLS_E_MPI_PRINT_FAILED =-35;
  GNUTLS_E_REHANDSHAKE =-37;
  GNUTLS_E_GOT_APPLICATION_DATA =-38;
  GNUTLS_E_RECORD_LIMIT_REACHED =-39;
  GNUTLS_E_ENCRYPTION_FAILED= -40;

  GNUTLS_E_PK_ENCRYPTION_FAILED =-44;
  GNUTLS_E_PK_DECRYPTION_FAILED =-45;
  GNUTLS_E_PK_SIGN_FAILED =-46;
  GNUTLS_E_X509_UNSUPPORTED_CRITICAL_EXTENSION =-47;
  GNUTLS_E_KEY_USAGE_VIOLATION =-48;
  GNUTLS_E_NO_CERTIFICATE_FOUND =-49;
  GNUTLS_E_INVALID_REQUEST= -50;
  GNUTLS_E_SHORT_MEMORY_BUFFER =-51;
  GNUTLS_E_INTERRUPTED =-52;
  GNUTLS_E_PUSH_ERROR =-53;
  GNUTLS_E_PULL_ERROR =-54;
  GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER =-55;
  GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE =-56;
  GNUTLS_E_PKCS1_WRONG_PAD =-57;
  GNUTLS_E_RECEIVED_ILLEGAL_EXTENSION =-58;
  GNUTLS_E_INTERNAL_ERROR =-59;
  GNUTLS_E_DH_PRIME_UNACCEPTABLE =-63;
  GNUTLS_E_FILE_ERROR =-64;
  GNUTLS_E_TOO_MANY_EMPTY_PACKETS= -78;
  GNUTLS_E_UNKNOWN_PK_ALGORITHM =-80;
  GNUTLS_E_TOO_MANY_HANDSHAKE_PACKETS =-81;

  GNUTLS_E_NO_TEMPORARY_RSA_PARAMS = -(84);
  GNUTLS_E_NO_COMPRESSION_ALGORITHMS = -(86);
  GNUTLS_E_NO_CIPHER_SUITES = -(87);
  GNUTLS_E_OPENPGP_GETKEY_FAILED = -(88);
  GNUTLS_E_PK_SIG_VERIFY_FAILED = -(89);
  GNUTLS_E_ILLEGAL_SRP_USERNAME = -(90);
  GNUTLS_E_SRP_PWD_PARSING_ERROR = -(91);
  GNUTLS_E_NO_TEMPORARY_DH_PARAMS = -(93);

  { For certificate and key stuff }
  GNUTLS_E_ASN1_ELEMENT_NOT_FOUND = -(67);
  GNUTLS_E_ASN1_IDENTIFIER_NOT_FOUND = -(68);
  GNUTLS_E_ASN1_DER_ERROR = -(69);
  GNUTLS_E_ASN1_VALUE_NOT_FOUND = -(70);
  GNUTLS_E_ASN1_GENERIC_ERROR = -(71);
  GNUTLS_E_ASN1_VALUE_NOT_VALID = -(72);
  GNUTLS_E_ASN1_TAG_ERROR = -(73);
  GNUTLS_E_ASN1_TAG_IMPLICIT = -(74);
  GNUTLS_E_ASN1_TYPE_ANY_ERROR = -(75);
  GNUTLS_E_ASN1_SYNTAX_ERROR = -(76);
  GNUTLS_E_ASN1_DER_OVERFLOW = -(77);
  GNUTLS_E_OPENPGP_UID_REVOKED = -(79);
  GNUTLS_E_CERTIFICATE_ERROR = -(43);
  GNUTLS_E_X509_CERTIFICATE_ERROR = GNUTLS_E_CERTIFICATE_ERROR;
  GNUTLS_E_CERTIFICATE_KEY_MISMATCH = -(60);

  { GNUTLS_A_UNSUPPORTED_CERTIFICATE  }
  GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE = -(61);
  GNUTLS_E_X509_UNKNOWN_SAN = -(62);
  GNUTLS_E_OPENPGP_FINGERPRINT_UNSUPPORTED = -(94);
  GNUTLS_E_X509_UNSUPPORTED_ATTRIBUTE = -(95);
  GNUTLS_E_UNKNOWN_HASH_ALGORITHM = -(96);
  GNUTLS_E_UNKNOWN_PKCS_CONTENT_TYPE = -(97);
  GNUTLS_E_UNKNOWN_PKCS_BAG_TYPE = -(98);
  GNUTLS_E_INVALID_PASSWORD = -(99);

  { for PKCS #12 MAC  }
  GNUTLS_E_MAC_VERIFY_FAILED = -(100);
  GNUTLS_E_CONSTRAINT_ERROR = -(101);
  GNUTLS_E_WARNING_IA_IPHF_RECEIVED = -(102);
  GNUTLS_E_WARNING_IA_FPHF_RECEIVED = -(103);
  GNUTLS_E_IA_VERIFY_FAILED = -(104);
  GNUTLS_E_UNKNOWN_ALGORITHM = -(105);
  GNUTLS_E_UNSUPPORTED_SIGNATURE_ALGORITHM = -(106);
  GNUTLS_E_SAFE_RENEGOTIATION_FAILED = -(107);
  GNUTLS_E_UNSAFE_RENEGOTIATION_DENIED = -(108);
  GNUTLS_E_UNKNOWN_SRP_USERNAME = -(109);
  GNUTLS_E_PREMATURE_TERMINATION = -(110);
  GNUTLS_E_BASE64_ENCODING_ERROR = -(201);

  { obsolete  }
  GNUTLS_E_INCOMPATIBLE_GCRYPT_LIBRARY = -(202);
  GNUTLS_E_INCOMPATIBLE_CRYPTO_LIBRARY = -(202);
  GNUTLS_E_INCOMPATIBLE_LIBTASN1_LIBRARY = -(203);
  GNUTLS_E_OPENPGP_KEYRING_ERROR = -(204);
  GNUTLS_E_X509_UNSUPPORTED_OID = -(205);
  GNUTLS_E_RANDOM_FAILED = -(206);
  GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR = -(207);
  GNUTLS_E_OPENPGP_SUBKEY_ERROR = -(208);
  GNUTLS_E_ALREADY_REGISTERED = -(209);
  GNUTLS_E_CRYPTO_ALREADY_REGISTERED = GNUTLS_E_ALREADY_REGISTERED;
  GNUTLS_E_HANDSHAKE_TOO_LARGE = -(210);
  GNUTLS_E_CRYPTODEV_IOCTL_ERROR = -(211);
  GNUTLS_E_CRYPTODEV_DEVICE_ERROR = -(212);
  GNUTLS_E_CHANNEL_BINDING_NOT_AVAILABLE = -(213);
  GNUTLS_E_BAD_COOKIE = -(214);
  GNUTLS_E_OPENPGP_PREFERRED_KEY_ERROR = -(215);
  GNUTLS_E_INCOMPAT_DSA_KEY_WITH_TLS_PROTOCOL = -(216);
  GNUTLS_E_INSUFFICIENT_SECURITY = -(217);
  GNUTLS_E_HEARTBEAT_PONG_RECEIVED = -(292);
  GNUTLS_E_HEARTBEAT_PING_RECEIVED = -(293);

  { PKCS11 related  }
  GNUTLS_E_PKCS11_ERROR = -(300);
  GNUTLS_E_PKCS11_LOAD_ERROR = -(301);
  GNUTLS_E_PARSING_ERROR = -(302);
  GNUTLS_E_PKCS11_PIN_ERROR = -(303);
  GNUTLS_E_PKCS11_SLOT_ERROR = -(305);
  GNUTLS_E_LOCKING_ERROR = -(306);
  GNUTLS_E_PKCS11_ATTRIBUTE_ERROR = -(307);
  GNUTLS_E_PKCS11_DEVICE_ERROR = -(308);
  GNUTLS_E_PKCS11_DATA_ERROR = -(309);
  GNUTLS_E_PKCS11_UNSUPPORTED_FEATURE_ERROR = -(310);
  GNUTLS_E_PKCS11_KEY_ERROR = -(311);
  GNUTLS_E_PKCS11_PIN_EXPIRED = -(312);
  GNUTLS_E_PKCS11_PIN_LOCKED = -(313);
  GNUTLS_E_PKCS11_SESSION_ERROR = -(314);
  GNUTLS_E_PKCS11_SIGNATURE_ERROR = -(315);
  GNUTLS_E_PKCS11_TOKEN_ERROR = -(316);
  GNUTLS_E_PKCS11_USER_ERROR = -(317);
  GNUTLS_E_CRYPTO_INIT_FAILED = -(318);
  GNUTLS_E_TIMEDOUT = -(319);
  GNUTLS_E_USER_ERROR = -(320);
  GNUTLS_E_ECC_NO_SUPPORTED_CURVES = -(321);
  GNUTLS_E_ECC_UNSUPPORTED_CURVE = -(322);
  GNUTLS_E_PKCS11_REQUESTED_OBJECT_NOT_AVAILBLE = -(323);
  GNUTLS_E_CERTIFICATE_LIST_UNSORTED = -(324);
  GNUTLS_E_ILLEGAL_PARAMETER = -(325);
  GNUTLS_E_NO_PRIORITIES_WERE_SET = -(326);
  GNUTLS_E_X509_UNSUPPORTED_EXTENSION = -(327);
  GNUTLS_E_SESSION_EOF = -(328);
  GNUTLS_E_TPM_ERROR = -(329);
  GNUTLS_E_TPM_KEY_PASSWORD_ERROR = -(330);
  GNUTLS_E_TPM_SRK_PASSWORD_ERROR = -(331);
  GNUTLS_E_TPM_SESSION_ERROR = -(332);
  GNUTLS_E_TPM_KEY_NOT_FOUND = -(333);
  GNUTLS_E_TPM_UNINITIALIZED = -(334);
  GNUTLS_E_TPM_NO_LIB = -(335);
  GNUTLS_E_NO_CERTIFICATE_STATUS = -(340);
  GNUTLS_E_OCSP_RESPONSE_ERROR = -(341);
  GNUTLS_E_RANDOM_DEVICE_ERROR = -(342);
  GNUTLS_E_AUTH_ERROR = -(343);
  GNUTLS_E_NO_APPLICATION_PROTOCOL = -(344);
  GNUTLS_E_SOCKETS_INIT_ERROR = -(345);
  GNUTLS_E_KEY_IMPORT_FAILED = -(346);
  {GNUTLS_A_INAPPROPRIATE_FALLBACK }
  GNUTLS_E_INAPPROPRIATE_FALLBACK = -(347);
  GNUTLS_E_CERTIFICATE_VERIFICATION_ERROR = -(348);
  GNUTLS_E_SELF_TEST_ERROR = -(400);
  GNUTLS_E_NO_SELF_TEST = -(401);
  GNUTLS_E_LIB_IN_ERROR_STATE = -(402);
  GNUTLS_E_PK_GENERATION_ERROR = -(403);
  GNUTLS_E_IDNA_ERROR = -(404);
  GNUTLS_E_NEED_FALLBACK = -(405);
  GNUTLS_E_UNIMPLEMENTED_FEATURE = -(1250);
  GNUTLS_E_APPLICATION_ERROR_MAX = -(65000);
  GNUTLS_E_APPLICATION_ERROR_MIN = -(65500);
const
  GNUTLS_OID_X520_COUNTRY_NAME = '2.5.4.6';
  GNUTLS_OID_X520_ORGANIZATION_NAME = '2.5.4.10';
  GNUTLS_OID_X520_ORGANIZATIONAL_UNIT_NAME = '2.5.4.11';
  GNUTLS_OID_X520_COMMON_NAME = '2.5.4.3';
  GNUTLS_OID_X520_LOCALITY_NAME = '2.5.4.7';
  GNUTLS_OID_X520_STATE_OR_PROVINCE_NAME = '2.5.4.8';
  GNUTLS_OID_X520_INITIALS = '2.5.4.43';
  GNUTLS_OID_X520_GENERATION_QUALIFIER = '2.5.4.44';
  GNUTLS_OID_X520_SURNAME = '2.5.4.4';
  GNUTLS_OID_X520_GIVEN_NAME = '2.5.4.42';
  GNUTLS_OID_X520_TITLE = '2.5.4.12';
  GNUTLS_OID_X520_DN_QUALIFIER = '2.5.4.46';
  GNUTLS_OID_X520_PSEUDONYM = '2.5.4.65';
  GNUTLS_OID_X520_POSTALCODE = '2.5.4.17';
  GNUTLS_OID_X520_NAME = '2.5.4.41';
  GNUTLS_OID_LDAP_DC = '0.9.2342.19200300.100.1.25';
  GNUTLS_OID_LDAP_UID = '0.9.2342.19200300.100.1.1';
  GNUTLS_OID_PKCS9_EMAIL = '1.2.840.113549.1.9.1';
  GNUTLS_OID_PKIX_DATE_OF_BIRTH = '1.3.6.1.5.5.7.9.1';
  GNUTLS_OID_PKIX_PLACE_OF_BIRTH = '1.3.6.1.5.5.7.9.2';
  GNUTLS_OID_PKIX_GENDER = '1.3.6.1.5.5.7.9.3';
  GNUTLS_OID_PKIX_COUNTRY_OF_CITIZENSHIP = '1.3.6.1.5.5.7.9.4';
  GNUTLS_OID_PKIX_COUNTRY_OF_RESIDENCE = '1.3.6.1.5.5.7.9.5';
  GNUTLS_KP_TLS_WWW_SERVER = '1.3.6.1.5.5.7.3.1';
  GNUTLS_KP_TLS_WWW_CLIENT = '1.3.6.1.5.5.7.3.2';
  GNUTLS_KP_CODE_SIGNING = '1.3.6.1.5.5.7.3.3';
  GNUTLS_KP_MS_SMART_CARD_LOGON = '1.3.6.1.4.1.311.20.2.2';
  GNUTLS_KP_EMAIL_PROTECTION = '1.3.6.1.5.5.7.3.4';
  GNUTLS_KP_TIME_STAMPING = '1.3.6.1.5.5.7.3.8';
  GNUTLS_KP_OCSP_SIGNING = '1.3.6.1.5.5.7.3.9';
  GNUTLS_KP_IPSEC_IKE = '1.3.6.1.5.5.7.3.17';
  GNUTLS_KP_ANY = '2.5.29.37.0';
  GNUTLS_OID_AIA = '1.3.6.1.5.5.7.1.1';
  GNUTLS_OID_AD_OCSP = '1.3.6.1.5.5.7.48.1';
  GNUTLS_OID_AD_CAISSUERS = '1.3.6.1.5.5.7.48.2';
  GNUTLS_FSAN_SET = 0;
  GNUTLS_FSAN_APPEND = 1;
  GNUTLS_X509EXT_OID_SUBJECT_KEY_ID = '2.5.29.14';
  GNUTLS_X509EXT_OID_KEY_USAGE = '2.5.29.15';
  GNUTLS_X509EXT_OID_PRIVATE_KEY_USAGE_PERIOD = '2.5.29.16';
  GNUTLS_X509EXT_OID_SAN = '2.5.29.17';
  GNUTLS_X509EXT_OID_IAN = '2.5.29.18';
  GNUTLS_X509EXT_OID_BASIC_CONSTRAINTS = '2.5.29.19';
  GNUTLS_X509EXT_OID_NAME_CONSTRAINTS = '2.5.29.30';
  GNUTLS_X509EXT_OID_CRL_DIST_POINTS = '2.5.29.31';
  GNUTLS_X509EXT_OID_CRT_POLICY = '2.5.29.32';
  GNUTLS_X509EXT_OID_AUTHORITY_KEY_ID = '2.5.29.35';
  GNUTLS_X509EXT_OID_EXTENDED_KEY_USAGE = '2.5.29.37';
  GNUTLS_X509EXT_OID_AUTHORITY_INFO_ACCESS = '1.3.6.1.5.5.7.1.1';
  GNUTLS_X509EXT_OID_PROXY_CRT_INFO = '1.3.6.1.5.5.7.1.14';
  GNUTLS_NAME_CONSTRAINTS_FLAG_APPEND = 1;

{ was #define dname def_expr }
  GNUTLS_MAX_QUALIFIERS = 8;
  GNUTLS_X509_DN_OID_RETURN_OID = 1;
  GNUTLS_VFLAGS_PROFILE_MASK = $ff000000;

  GNUTLS_TL_VERIFY_CRL = 1;
  GNUTLS_TL_USE_IN_TLS = 1 shl 1;
  GNUTLS_TL_NO_DUPLICATES = 1 shl 2;
  GNUTLS_TL_NO_DUPLICATE_KEY = 1 shl 3;
  GNUTLS_TL_GET_COPY = 1 shl 4;

  GNUTLS_PKCS7_ATTR_ENCODE_OCTET_STRING = 1;

  //gnutls_info_access_what_t =  Longint;
  GNUTLS_IA_ACCESSMETHOD_OID = 1;
  GNUTLS_IA_ACCESSLOCATION_GENERALNAME_TYPE = 2;
  GNUTLS_IA_URI = 106;
  GNUTLS_IA_UNKNOWN = 10000;
  GNUTLS_IA_OCSP_URI = 10006;
  GNUTLS_IA_CAISSUERS_URI = 10106;

  // gnutls_x509_crl_reason_flags_t =  Longint;
  GNUTLS_CRL_REASON_UNSPECIFIED = 0;
  GNUTLS_CRL_REASON_PRIVILEGE_WITHDRAWN = 1;
  GNUTLS_CRL_REASON_CERTIFICATE_HOLD = 2;
  GNUTLS_CRL_REASON_CESSATION_OF_OPERATION = 4;
  GNUTLS_CRL_REASON_SUPERSEDED = 8;
  GNUTLS_CRL_REASON_AFFILIATION_CHANGED = 16;
  GNUTLS_CRL_REASON_CA_COMPROMISE = 32;
  GNUTLS_CRL_REASON_KEY_COMPROMISE = 64;
  GNUTLS_CRL_REASON_UNUSED = 128;
  GNUTLS_CRL_REASON_AA_COMPROMISE = 32768;
  GNUTLS_CRL_REASON_SUPERSEEDED = GNUTLS_CRL_REASON_SUPERSEDED;

  // gnutls_x509_qualifier_t =  Longint;
  GNUTLS_X509_QUALIFIER_UNKNOWN = 0;
  GNUTLS_X509_QUALIFIER_URI = 1;
  GNUTLS_X509_QUALIFIER_NOTICE = 2;

  // gnutls_certificate_verify_flags
  GNUTLS_VERIFY_DISABLE_CA_SIGN = 1 shl 0;
  GNUTLS_VERIFY_DO_NOT_ALLOW_SAME = 1 shl 2;
  GNUTLS_VERIFY_ALLOW_ANY_X509_V1_CA_CRT = 1 shl 3;
  GNUTLS_VERIFY_ALLOW_SIGN_RSA_MD2 = 1 shl 4;
  GNUTLS_VERIFY_ALLOW_SIGN_RSA_MD5 = 1 shl 5;
  GNUTLS_VERIFY_DISABLE_TIME_CHECKS = 1 shl 6;
  GNUTLS_VERIFY_DISABLE_TRUSTED_TIME_CHECKS = 1 shl 7;
  GNUTLS_VERIFY_DO_NOT_ALLOW_X509_V1_CA_CRT = 1 shl 8;
  GNUTLS_VERIFY_DISABLE_CRL_CHECKS = 1 shl 9;
  GNUTLS_VERIFY_ALLOW_UNSORTED_CHAIN = 1 shl 10;
  GNUTLS_VERIFY_DO_NOT_ALLOW_UNSORTED_CHAIN = 1 shl 11;
  GNUTLS_VERIFY_DO_NOT_ALLOW_WILDCARDS = 1 shl 12;
  GNUTLS_VERIFY_USE_TLS1_RSA = 1 shl 13;

  GNUTLS_VERIFY_ALLOW_BROKEN = GNUTLS_VERIFY_ALLOW_SIGN_RSA_MD2 or GNUTLS_VERIFY_ALLOW_SIGN_RSA_MD5;

  // gnutls_certificate_verification_profiles_t

  GNUTLS_PROFILE_VERY_WEAK = 1;
  GNUTLS_PROFILE_LOW = 2;
  GNUTLS_PROFILE_LEGACY = 4;
  GNUTLS_PROFILE_MEDIUM = 5;
  GNUTLS_PROFILE_HIGH = 6;
  GNUTLS_PROFILE_ULTRA = 7;
  GNUTLS_PROFILE_SUITEB128 = 32;
  GNUTLS_PROFILE_SUITEB192 = 33;

  // gnutls_pkcs7_sign_flags

  GNUTLS_PKCS7_EMBED_DATA = 1;
  GNUTLS_PKCS7_INCLUDE_TIME = 1 shl 1;
  GNUTLS_PKCS7_INCLUDE_CERT = 1 shl 2;
  GNUTLS_PKCS7_WRITE_SPKI = 1 shl 3;

  // gnutls_pkcs_encrypt_flags_t

  GNUTLS_PKCS_PLAIN = 1;
  GNUTLS_PKCS_PKCS12_3DES = 1 shl 1;
  GNUTLS_PKCS_PKCS12_ARCFOUR = 1 shl 2;
  GNUTLS_PKCS_PKCS12_RC2_40 = 1 shl 3;
  GNUTLS_PKCS_PBES2_3DES = 1 shl 4;
  GNUTLS_PKCS_PBES2_AES_128 = 1 shl 5;
  GNUTLS_PKCS_PBES2_AES_192 = 1 shl 6;
  GNUTLS_PKCS_PBES2_AES_256 = 1 shl 7;
  GNUTLS_PKCS_NULL_PASSWORD = 1 shl 8;
  GNUTLS_PKCS_PBES2_DES = 1 shl 9;
  GNUTLS_PKCS8_PLAIN = GNUTLS_PKCS_PLAIN;

  // gnutls_certificate_import_flags
  GNUTLS_X509_CRT_LIST_IMPORT_FAIL_IF_EXCEED = 1;
  GNUTLS_X509_CRT_LIST_FAIL_IF_UNSORTED = 1 shl 1;
  GNUTLS_X509_CRT_LIST_SORT = 1 shl 2;

  // gnutls_keyid_flags_t

  GNUTLS_KEYID_USE_SHA1 = 0;
  GNUTLS_KEYID_USE_SHA256 = 1 shl 0;
  GNUTLS_KEYID_USE_BEST_KNOWN = 1 shl 30;

  GNUTLS_PKCS8_USE_PKCS12_3DES = GNUTLS_PKCS_PKCS12_3DES;
  GNUTLS_PKCS8_USE_PKCS12_ARCFOUR = GNUTLS_PKCS_PKCS12_ARCFOUR;
  GNUTLS_PKCS8_USE_PKCS12_RC2_40 = GNUTLS_PKCS_PKCS12_RC2_40;

  GNUTLS_PKCS_USE_PKCS12_3DES = GNUTLS_PKCS_PKCS12_3DES;
  GNUTLS_PKCS_USE_PKCS12_ARCFOUR = GNUTLS_PKCS_PKCS12_ARCFOUR;
  GNUTLS_PKCS_USE_PKCS12_RC2_40 = GNUTLS_PKCS_PKCS12_RC2_40;
  GNUTLS_PKCS_USE_PBES2_3DES = GNUTLS_PKCS_PBES2_3DES;
  GNUTLS_PKCS_USE_PBES2_AES_128 = GNUTLS_PKCS_PBES2_AES_128;
  GNUTLS_PKCS_USE_PBES2_AES_192 = GNUTLS_PKCS_PBES2_AES_192;
  GNUTLS_PKCS_USE_PBES2_AES_256 = GNUTLS_PKCS_PBES2_AES_256;

  // when the key is to be used for signing:
  GNUTLS_KEY_DIGITAL_SIGNATURE = 128;
  GNUTLS_KEY_NON_REPUDIATION = 64;
  // when the key is to be used for encryption:
  GNUTLS_KEY_KEY_ENCIPHERMENT = 32;
  GNUTLS_KEY_DATA_ENCIPHERMENT = 16;
  GNUTLS_KEY_KEY_AGREEMENT = 8;
  GNUTLS_KEY_KEY_CERT_SIGN = 4;
  GNUTLS_KEY_CRL_SIGN = 2;
  GNUTLS_KEY_ENCIPHER_ONLY = 1;
  GNUTLS_KEY_DECIPHER_ONLY = 32768;

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;
  Tsize_t = cint;
  Tssize_t = cint;
  Ttime_t = int64; // Used as such in gnutls
  PPdword = ^PDWord;

Type
  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

  Pgiovec_t  = ^Tgiovec_t;
  Pgnutls_alert_description_t  = ^Tgnutls_alert_description_t;
  Pgnutls_alert_level_t  = ^Tgnutls_alert_level_t;
  Pgnutls_alloc_function  = ^Tgnutls_alloc_function;
  Pgnutls_anon_client_credentials_st  = ^Tgnutls_anon_client_credentials_st;
  Pgnutls_anon_client_credentials_t  = ^Tgnutls_anon_client_credentials_t;
  Pgnutls_anon_server_credentials_st  = ^Tgnutls_anon_server_credentials_st;
  Pgnutls_anon_server_credentials_t  = ^Tgnutls_anon_server_credentials_t;
  Pgnutls_buffer_st  = ^Tgnutls_buffer_st;
  Pgnutls_buffer_t  = ^Tgnutls_buffer_t;
  Pgnutls_calloc_function  = ^Tgnutls_calloc_function;
  Pgnutls_certificate_client_credentials  = ^Tgnutls_certificate_client_credentials;
  Pgnutls_certificate_credentials_st  = ^Tgnutls_certificate_credentials_st;
  Pgnutls_certificate_credentials_t  = ^Tgnutls_certificate_credentials_t;
  Pgnutls_certificate_flags  = ^Tgnutls_certificate_flags;
  Pgnutls_certificate_print_formats  = ^Tgnutls_certificate_print_formats;
  Pgnutls_certificate_print_formats_t  = ^Tgnutls_certificate_print_formats_t;
  Pgnutls_certificate_request_t  = ^Tgnutls_certificate_request_t;
  Pgnutls_certificate_retrieve_function  = ^Tgnutls_certificate_retrieve_function;
  Pgnutls_certificate_server_credentials  = ^Tgnutls_certificate_server_credentials;
  Pgnutls_certificate_status_t  = ^Tgnutls_certificate_status_t;
  Pgnutls_certificate_type_t  = ^Tgnutls_certificate_type_t;
  Pgnutls_certificate_verify_function  = ^Tgnutls_certificate_verify_function;
  Pgnutls_channel_binding_t  = ^Tgnutls_channel_binding_t;
  Pgnutls_cipher_algorithm  = ^Tgnutls_cipher_algorithm;
  Pgnutls_cipher_algorithm_t  = ^Tgnutls_cipher_algorithm_t;
  Pgnutls_close_request_t  = ^Tgnutls_close_request_t;
  Pgnutls_compression_method_t  = ^Tgnutls_compression_method_t;
  Pgnutls_credentials_type_t  = ^Tgnutls_credentials_type_t;
  Pgnutls_datum_t  = ^Tgnutls_datum_t;
  Pgnutls_dh_params_int  = ^Tgnutls_dh_params_int;
  Pgnutls_dh_params_t  = ^Tgnutls_dh_params_t;
  Pgnutls_digest_algorithm_t  = ^Tgnutls_digest_algorithm_t;
  Pgnutls_ecc_curve_t  = ^Tgnutls_ecc_curve_t;
  Pgnutls_ext_parse_type_t  = ^Tgnutls_ext_parse_type_t;
  Pgnutls_ext_priv_data_t  = ^Tgnutls_ext_priv_data_t;
  Pgnutls_handshake_description_t  = ^Tgnutls_handshake_description_t;
  Pgnutls_kx_algorithm_t  = ^Tgnutls_kx_algorithm_t;
  Pgnutls_mac_algorithm_t  = ^Tgnutls_mac_algorithm_t;
  Pgnutls_openpgp_crt_int  = ^Tgnutls_openpgp_crt_int;
  Pgnutls_openpgp_crt_status_t  = ^Tgnutls_openpgp_crt_status_t;
  Pgnutls_openpgp_crt_t  = ^Tgnutls_openpgp_crt_t;
  Pgnutls_openpgp_keyring_int  = ^Tgnutls_openpgp_keyring_int;
  Pgnutls_openpgp_keyring_t  = ^Tgnutls_openpgp_keyring_t;
  Pgnutls_openpgp_privkey_int  = ^Tgnutls_openpgp_privkey_int;
  Pgnutls_openpgp_privkey_t  = ^Tgnutls_openpgp_privkey_t;
  Pgnutls_packet_t  = ^Tgnutls_packet_t;
  Pgnutls_params_function  = ^Tgnutls_params_function;
  Pgnutls_params_st  = ^Tgnutls_params_st;
  Pgnutls_params_type_t  = ^Tgnutls_params_type_t;
  Pgnutls_pin_flag_t  = ^Tgnutls_pin_flag_t;
  Pgnutls_pk_algorithm_t  = ^Tgnutls_pk_algorithm_t;
  Pgnutls_pkcs11_privkey_st  = ^Tgnutls_pkcs11_privkey_st;
  Pgnutls_pkcs11_privkey_t  = ^Tgnutls_pkcs11_privkey_t;
  Pgnutls_priority_st  = ^Tgnutls_priority_st;
  Pgnutls_priority_t  = ^Tgnutls_priority_t;
  Pgnutls_privkey_st  = ^Tgnutls_privkey_st;
  Pgnutls_privkey_t  = ^Tgnutls_privkey_t;
  Pgnutls_privkey_type_t  = ^Tgnutls_privkey_type_t;
  Pgnutls_protocol_t  = ^Tgnutls_protocol_t;
  Pgnutls_psk_client_credentials_function  = ^Tgnutls_psk_client_credentials_function;
  Pgnutls_psk_client_credentials_st  = ^Tgnutls_psk_client_credentials_st;
  Pgnutls_psk_client_credentials_t  = ^Tgnutls_psk_client_credentials_t;
  Pgnutls_psk_key_flags  = ^Tgnutls_psk_key_flags;
  Pgnutls_psk_server_credentials_function  = ^Tgnutls_psk_server_credentials_function;
  Pgnutls_psk_server_credentials_st  = ^Tgnutls_psk_server_credentials_st;
  Pgnutls_psk_server_credentials_t  = ^Tgnutls_psk_server_credentials_t;
  Pgnutls_pubkey_st  = ^Tgnutls_pubkey_st;
  Pgnutls_pubkey_t  = ^Tgnutls_pubkey_t;
  Pgnutls_random_art  = ^Tgnutls_random_art;
  Pgnutls_random_art_t  = ^Tgnutls_random_art_t;
  Pgnutls_range_st  = ^Tgnutls_range_st;
  Pgnutls_realloc_function  = ^Tgnutls_realloc_function;
  Pgnutls_retr2_st  = ^Tgnutls_retr2_st;
  Pgnutls_rsa_params_t  = ^Tgnutls_rsa_params_t;
  Pgnutls_sec_param_t  = ^Tgnutls_sec_param_t;
  Pgnutls_server_name_type_t  = ^Tgnutls_server_name_type_t;
  Pgnutls_session_int  = ^Tgnutls_session_int;
  Pgnutls_session_t  = ^Tgnutls_session_t;
  Pgnutls_sign_algorithm_t  = ^Tgnutls_sign_algorithm_t;
  Pgnutls_srp_client_credentials_function  = ^Tgnutls_srp_client_credentials_function;
  Pgnutls_srp_client_credentials_st  = ^Tgnutls_srp_client_credentials_st;
  Pgnutls_srp_client_credentials_t  = ^Tgnutls_srp_client_credentials_t;
  Pgnutls_srp_server_credentials_function  = ^Tgnutls_srp_server_credentials_function;
  Pgnutls_srp_server_credentials_st  = ^Tgnutls_srp_server_credentials_st;
  Pgnutls_srp_server_credentials_t  = ^Tgnutls_srp_server_credentials_t;
  Pgnutls_srtp_profile_t  = ^Tgnutls_srtp_profile_t;
  Pgnutls_supplemental_data_format_type_t  = ^Tgnutls_supplemental_data_format_type_t;
  Pgnutls_tdb_int  = ^Tgnutls_tdb_int;
  Pgnutls_tdb_t  = ^Tgnutls_tdb_t;
  Pgnutls_transport_ptr_t  = ^Tgnutls_transport_ptr_t;
  Pgnutls_typed_vdata_st  = ^Tgnutls_typed_vdata_st;
  Pgnutls_vdata_types_t  = ^Tgnutls_vdata_types_t;
  Pgnutls_x509_crl_int  = ^Tgnutls_x509_crl_int;
  Pgnutls_x509_crl_t  = ^Tgnutls_x509_crl_t;
  Pgnutls_x509_crq_int  = ^Tgnutls_x509_crq_int;
  Pgnutls_x509_crq_t  = ^Tgnutls_x509_crq_t;
  Pgnutls_x509_crt_fmt_t  = ^Tgnutls_x509_crt_fmt_t;
  Pgnutls_x509_crt_int  = ^Tgnutls_x509_crt_int;
  Pgnutls_x509_crt_t  = ^Tgnutls_x509_crt_t;
  Pgnutls_x509_privkey_int  = ^Tgnutls_x509_privkey_int;
  Pgnutls_x509_privkey_t  = ^Tgnutls_x509_privkey_t;
  Pgnutls_x509_subject_alt_name_t  = ^Tgnutls_x509_subject_alt_name_t;
  Pmbuffer_st  = ^Tmbuffer_st;
  Psize_t  = ^Tsize_t;
  Ptime_t  = ^Ttime_t;


  Tgnutls_cipher_algorithm =  Longint;
  Tgnutls_cipher_algorithm_t = Tgnutls_cipher_algorithm;
  Tgnutls_kx_algorithm_t =  Longint;
  Tgnutls_params_type_t =  Longint;
  Tgnutls_credentials_type_t =  Longint;
  Tgnutls_mac_algorithm_t =  Longint;
  Tgnutls_digest_algorithm_t =  Longint;
  Tgnutls_compression_method_t =  Longint;
  Tgnutls_alert_level_t =  Longint;
  Tgnutls_alert_description_t =  Longint;
  Tgnutls_handshake_description_t =  Longint;
  Tgnutls_certificate_status_t =  Longint;
  Tgnutls_certificate_request_t =  Longint;
  Tgnutls_openpgp_crt_status_t =  Longint;
  Tgnutls_close_request_t =  Longint;
  Tgnutls_protocol_t =  Longint;
  Tgnutls_certificate_type_t =  Longint;
  Tgnutls_x509_crt_fmt_t =  Longint;
  Tgnutls_certificate_print_formats =  Longint;
  Tgnutls_certificate_print_formats_t =  Tgnutls_certificate_print_formats;
  Tgnutls_pk_algorithm_t =  Longint;
  Tgnutls_sign_algorithm_t =  Longint;
  Tgnutls_ecc_curve_t =  Longint;
  Tgnutls_sec_param_t =  Longint;
  Tgnutls_channel_binding_t =  Longint;

  PPgnutls_x509_crt_t = ^Pgnutls_x509_crt_t;

  Tgnutls_transport_ptr_t = pointer;

  Tgnutls_session_int = record
      {undefined structure}
  end;
  Tgnutls_session_t = Pgnutls_session_int;
  Tgnutls_dh_params_int = record
      {undefined structure}
  end;


  Tgnutls_dh_params_t = Pgnutls_dh_params_int;
  Tgnutls_x509_privkey_int = record
      {undefined structure}
    end;


  Tgnutls_rsa_params_t = Pgnutls_x509_privkey_int;
  Tgnutls_priority_st = record
      {undefined structure}
    end;

  Tgnutls_priority_t = Pgnutls_priority_st;

  Tgnutls_datum_t = record
      data : Pbyte;
      size : dword;
    end;

  Tgnutls_params_st = record
      _type : Tgnutls_params_type_t;
      params : record
          case longint of
            0 : ( dh : Tgnutls_dh_params_t );
            1 : ( rsa_export : Tgnutls_rsa_params_t );
          end;
      deinit : longint;
    end;


  Tgnutls_range_st = record
      low : Tsize_t;
      high : Tsize_t;
    end;


  Tgnutls_packet_t = Pmbuffer_st;
  Tgnutls_server_name_type_t =  Longint;
  Tgnutls_supplemental_data_format_type_t =  Longint;
  Tgnutls_srtp_profile_t =  Longint;
  Tgnutls_vdata_types_t =  Longint;
  Tgnutls_typed_vdata_st = record
      _type : Tgnutls_vdata_types_t;
      data : Pbyte;
      size : dword;
    end;

  Tgnutls_pubkey_st = record
      {undefined structure}
    end;


  Tgnutls_pubkey_t = Pgnutls_pubkey_st;
  Tgnutls_privkey_st = record
      {undefined structure}
    end;


  Tgnutls_privkey_t = Pgnutls_privkey_st;

  Tgnutls_x509_privkey_t = Pgnutls_x509_privkey_int;
  Tgnutls_x509_crl_int = record
      {undefined structure}
    end;


  Tgnutls_x509_crl_t = Pgnutls_x509_crl_int;
  Tgnutls_x509_crt_int = record
      {undefined structure}
    end;


  Tgnutls_x509_crt_t = Pgnutls_x509_crt_int;
  Tgnutls_x509_crq_int = record
      {undefined structure}
    end;


  Tgnutls_x509_crq_t = Pgnutls_x509_crq_int;
  Tgnutls_openpgp_keyring_int = record
      {undefined structure}
    end;


  Tgnutls_openpgp_keyring_t = Pgnutls_openpgp_keyring_int;
  Tgnutls_certificate_credentials_st = record
      {undefined structure}
    end;


  Tgnutls_certificate_credentials_t = Pgnutls_certificate_credentials_st;
  Tgnutls_certificate_server_credentials = Tgnutls_certificate_credentials_t;
  Tgnutls_certificate_client_credentials = Tgnutls_certificate_credentials_t;
  Tgnutls_anon_server_credentials_t = Pgnutls_anon_server_credentials_st;
  Tgnutls_anon_client_credentials_t = Pgnutls_anon_client_credentials_st;

  tgnutls_anon_client_credentials_st = record
  end;
  tgnutls_anon_server_credentials_st = record
  end;
  tgnutls_psk_client_credentials_st = record
  end;
  tgnutls_psk_server_credentials_st = record
  end;
  tgnutls_srp_client_credentials_st = record
  end;
  tgnutls_srp_server_credentials_st = record
  end;
  tmbuffer_st = record
  end;

  Tgnutls_certificate_flags =  Longint;

  Tgiovec_t = record
    iov_base : pointer;
    iov_len : Tsize_t;
  end;


  Tgnutls_random_art =  Longint;
  Tgnutls_random_art_t = Tgnutls_random_art;

  Tgnutls_srp_server_credentials_t = Pgnutls_srp_server_credentials_st;
  Tgnutls_srp_client_credentials_t = Pgnutls_srp_client_credentials_st;
  Tgnutls_psk_server_credentials_t = Pgnutls_psk_server_credentials_st;
  Tgnutls_psk_client_credentials_t = Pgnutls_psk_client_credentials_st;

  Tgnutls_psk_key_flags =  Longint;

  Tgnutls_x509_subject_alt_name_t =  Longint;

  Tgnutls_openpgp_crt_int = record
      {undefined structure}
    end;


  Tgnutls_openpgp_crt_t = Pgnutls_openpgp_crt_int;
  Tgnutls_openpgp_privkey_int = record
      {undefined structure}
    end;


  Tgnutls_openpgp_privkey_t = Pgnutls_openpgp_privkey_int;
  Tgnutls_pkcs11_privkey_st = record
      {undefined structure}
    end;

  tgnutls_buffer_st = record
  end;
  Tgnutls_pkcs11_privkey_t = Pgnutls_pkcs11_privkey_st;

  Tgnutls_privkey_type_t =  Longint;

  Tgnutls_retr2_st = record
      cert_type : Tgnutls_certificate_type_t;
      key_type : Tgnutls_privkey_type_t;
      cert : record
          case longint of
            0 : ( x509 : Pgnutls_x509_crt_t );
            1 : ( pgp : Tgnutls_openpgp_crt_t );
          end;
      ncerts : dword;
      key : record
          case longint of
            0 : ( x509 : Tgnutls_x509_privkey_t );
            1 : ( pgp : Tgnutls_openpgp_privkey_t );
            2 : ( pkcs11 : Tgnutls_pkcs11_privkey_t );
          end;
      deinit_all : dword;
    end;

  tgnutls_pkcs_encrypt_flags_t = longint;

  Tgnutls_tdb_int = record
      {undefined structure}
    end;
  Tgnutls_tdb_t = Pgnutls_tdb_int;

  Tgnutls_pin_flag_t =  Longint;

  Tgnutls_buffer_t = Pgnutls_buffer_st;
  Tgnutls_ext_priv_data_t = pointer;
  Tgnutls_ext_parse_type_t =  Longint;
  tgnutls_certificate_verify_flags = Longint;

  Tgnutls_pin_callback_t = function (userdata:pointer; attempt:longint; token_url:Pchar; token_label:Pchar; flags:dword; pin:Pchar; pin_max:Tsize_t):longint;cdecl;
  Tgnutls_supp_recv_func = function (session:Tgnutls_session_t; data:Pbyte; data_size:Tsize_t):longint;cdecl;
  Tgnutls_supp_send_func = function (session:Tgnutls_session_t; buf:Tgnutls_buffer_t):longint;cdecl;
  Tgnutls_ext_recv_func = function (session:Tgnutls_session_t; data:Pbyte; len:Tsize_t):longint;cdecl;
  Tgnutls_ext_send_func = function (session:Tgnutls_session_t; extdata:Tgnutls_buffer_t):longint;cdecl;
  Tgnutls_ext_deinit_data_func = procedure (data:Tgnutls_ext_priv_data_t);cdecl;
  Tgnutls_ext_pack_func = function (data:Tgnutls_ext_priv_data_t; packed_data:Tgnutls_buffer_t):longint;cdecl;
  Tgnutls_ext_unpack_func = function (packed_data:Tgnutls_buffer_t; data:Pgnutls_ext_priv_data_t):longint;cdecl;
  Tgnutls_tdb_store_func = function (db_name:Pchar; host:Pchar; service:Pchar; expiration:Ttime_t; pubkey:Pgnutls_datum_t):longint;cdecl;
  Tgnutls_tdb_store_commitment_func = function (db_name:Pchar; host:Pchar; service:Pchar; expiration:Ttime_t; hash_algo:Tgnutls_digest_algorithm_t;  hash:Pgnutls_datum_t):longint;cdecl;
  Tgnutls_tdb_verify_func = function (db_name:Pchar; host:Pchar; service:Pchar; pubkey:Pgnutls_datum_t):longint;cdecl;
  Tgnutls_pull_func = function (para1:Tgnutls_transport_ptr_t; para2:pointer; para3:Tsize_t):Tssize_t;cdecl;
  Tgnutls_push_func = function (para1:Tgnutls_transport_ptr_t; para2:pointer; para3:Tsize_t):Tssize_t;cdecl;
  Tgnutls_pull_timeout_func = function (para1:Tgnutls_transport_ptr_t; ms:dword):longint;cdecl;
  Tgnutls_vec_push_func = function (para1:Tgnutls_transport_ptr_t; iov:Pgiovec_t; iovcnt:longint):Tssize_t;cdecl;
  Tgnutls_errno_func = function (para1:Tgnutls_transport_ptr_t):longint;cdecl;
  Tgnutls_time_func = function (t:Ptime_t):Ttime_t;cdecl;
  Tmutex_init_func = function (mutex:Ppointer):longint;cdecl;
  Tmutex_lock_func = function (mutex:Ppointer):longint;cdecl;
  Tmutex_unlock_func = function (mutex:Ppointer):longint;cdecl;
  Tmutex_deinit_func = function (mutex:Ppointer):longint;cdecl;
  Tgnutls_alloc_function = function (para1:Tsize_t):pointer;cdecl;
  Tgnutls_calloc_function = function (para1:Tsize_t; para2:Tsize_t):pointer;cdecl;
  Tgnutls_is_secure_function = function (para1:pointer):longint;cdecl;
  Tgnutls_free_function = procedure (para1:pointer);cdecl;
  Tgnutls_realloc_function = function (para1:pointer; para2:Tsize_t):pointer;cdecl;
  Tgnutls_log_func = procedure (para1:longint; para2:Pchar);cdecl;
  Tgnutls_audit_log_func = procedure (para1:Tgnutls_session_t; para2:Pchar);cdecl;

  // Callback typedefs
    Tgnutls_status_request_ocsp_func = function (session:Tgnutls_session_t; ptr:pointer; ocsp_response:Pgnutls_datum_t):longint;cdecl;
  Tgnutls_params_function =  function(a : tgnutls_session_t; b: tgnutls_params_type_t; c : tgnutls_params_st ) : cInt; cdecl;
  Tgnutls_srp_server_credentials_function = Function (a :tgnutls_session_t; username : pchar; salt : pgnutls_datum_t; verifier : pgnutls_datum_t; generator : pgnutls_datum_t; prime : pgnutls_datum_t) : cint ; cdecl;
  Tgnutls_psk_server_credentials_function = Function (a : tgnutls_session_t; username : pchar; key : pgnutls_datum_t) : cInt; cdecl;
  Tgnutls_psk_client_credentials_function = Function (a : tgnutls_session_t; username : ppchar; key : pgnutls_datum_t) : cint; cdecl;
  Tgnutls_srp_client_credentials_function = Function (a : tgnutls_session_t; b : ppchar; c : ppchar): cint; cdecl;
  Tgnutls_certificate_verify_function = function (a : tgnutls_session_t) : cint; cdecl;
  Tgnutls_db_store_func = function (para1:pointer; key:Tgnutls_datum_t; data:Tgnutls_datum_t):longint;cdecl;
  Tgnutls_db_remove_func = function (para1:pointer; key:Tgnutls_datum_t):longint;cdecl;
  Tgnutls_db_retr_func = function (para1:pointer; key:Tgnutls_datum_t):Tgnutls_datum_t;cdecl;
  tgnutls_certificate_retrieve_function = function (a: tgnutls_session_t;  req_ca_rdn : pgnutls_datum_t; nreqs : cint; pk_algos : pgnutls_pk_algorithm_t; pk_algos_length : cint ; st: pgnutls_retr2_st) : cint; cdecl;
  Tgnutls_handshake_hook_func = function (para1:Tgnutls_session_t; htype:dword; post:dword; incoming:dword; msg:Pgnutls_datum_t):longint;cdecl;
  Tgnutls_handshake_post_client_hello_func = function (para1:Tgnutls_session_t):longint;cdecl;

  TSeqArray = array[0..7] of byte;

  Pgnutls_name_constraints_st = ^gnutls_name_constraints_st;
  gnutls_name_constraints_st = record
  end;
  PPgnutls_x509_crl_t = ^Pgnutls_x509_crl_t;

  Pgnutls_certificate_import_flags = ^Tgnutls_certificate_import_flags;
  Tgnutls_certificate_import_flags =  Longint;
  Pgnutls_keyid_flags_t = ^Tgnutls_keyid_flags_t;
  Tgnutls_keyid_flags_t =  Longint;
  Pgnutls_info_access_what_t = ^Tgnutls_info_access_what_t;
  Tgnutls_info_access_what_t =  Longint;
  Pgnutls_x509_name_constraints_t = ^Tgnutls_x509_name_constraints_t;
  Tgnutls_x509_name_constraints_t = Pgnutls_name_constraints_st;
  Pgnutls_x509_crl_reason_flags_t = ^Tgnutls_x509_crl_reason_flags_t;
  Tgnutls_x509_crl_reason_flags_t =  Longint;
  Pgnutls_x509_qualifier_t = ^Tgnutls_x509_qualifier_t;
  Tgnutls_x509_qualifier_t =  Longint;
  Pgnutls_x509_policy_st = ^Tgnutls_x509_policy_st;
  Tgnutls_x509_policy_st = record
      oid : Pchar;
      qualifiers : dword;
      qualifier : array[0..7] of record
          _type : Tgnutls_x509_qualifier_t;
          data : Pchar;
          size : dword;
        end;
    end;
  Pgnutls_x509_dn_st = ^gnutls_x509_dn_st;
  gnutls_x509_dn_st = record
  end;

  Pgnutls_x509_dn_t = ^Tgnutls_x509_dn_t;
  Tgnutls_x509_dn_t = Pgnutls_x509_dn_st;

  Pgnutls_x509_ava_st = ^Tgnutls_x509_ava_st;
  Tgnutls_x509_ava_st = record
      oid : Tgnutls_datum_t;
      value : Tgnutls_datum_t;
      value_tag : dword;
    end;
  Pgnutls_x509_crl_iter = ^tgnutls_x509_crl_iter;
  tgnutls_x509_crl_iter = record
      end;
  Pgnutls_x509_crl_iter_t = ^Tgnutls_x509_crl_iter_t;
  Tgnutls_x509_crl_iter_t = Pgnutls_x509_crl_iter;
  Pgnutls_certificate_verify_flags = ^Tgnutls_certificate_verify_flags;
  Pgnutls_pkcs_encrypt_flags_t = ^Tgnutls_pkcs_encrypt_flags_t;

  Pgnutls_certificate_verification_profiles_t = ^Tgnutls_certificate_verification_profiles_t;
  Tgnutls_certificate_verification_profiles_t =  Longint;
  Pgnutls_x509_trust_list_t = ^Tgnutls_x509_trust_list_t;
  Pgnutls_x509_trust_list_st = ^Tgnutls_x509_trust_list_st;
  Tgnutls_x509_trust_list_st = record
  end;
  Tgnutls_x509_trust_list_t = Pgnutls_x509_trust_list_st;
  Pgnutls_x509_trust_list_iter_t = ^Tgnutls_x509_trust_list_iter_t;
  Pgnutls_x509_trust_list_iter = ^Tgnutls_x509_trust_list_iter;
  tgnutls_x509_trust_list_iter  = record
  end;
  Tgnutls_x509_trust_list_iter_t = Pgnutls_x509_trust_list_iter;

  Tgnutls_verify_output_function = function(cert : tgnutls_x509_crt_t; issuer :tgnutls_x509_crt_t; crl : tgnutls_x509_crl_t; verification_output : dword) : cint; cdecl;

  Pgnutls_x509_ext_st = ^Tgnutls_x509_ext_st;
  Tgnutls_x509_ext_st = record
      oid : Pchar;
      critical : dword;
      data : Tgnutls_datum_t;
  end;
  Pgnutls_pkcs7_int = ^Tgnutls_pkcs7_int;
  Tgnutls_pkcs7_int = record
      {undefined structure}
  end;

  Pgnutls_pkcs7_t = ^Tgnutls_pkcs7_t;
  Tgnutls_pkcs7_t = Pgnutls_pkcs7_int;
  Pgnutls_pkcs7_attrs_st = ^Tgnutls_pkcs7_attrs_st;
  Tgnutls_pkcs7_attrs_st = record
  end;

  Pgnutls_pkcs7_attrs_t = ^Tgnutls_pkcs7_attrs_t;
  Tgnutls_pkcs7_attrs_t = Pgnutls_pkcs7_attrs_st;

  Pgnutls_pkcs7_signature_info_st = ^Tgnutls_pkcs7_signature_info_st;
  Tgnutls_pkcs7_signature_info_st = record
      algo : Tgnutls_sign_algorithm_t;
      sig : Tgnutls_datum_t;
      issuer_dn : Tgnutls_datum_t;
      signer_serial : Tgnutls_datum_t;
      issuer_keyid : Tgnutls_datum_t;
      signing_time : Ttime_t;
      signed_attrs : Tgnutls_pkcs7_attrs_t;
      unsigned_attrs : Tgnutls_pkcs7_attrs_t;
      pad : array[0..63] of char;
    end;
  Pgnutls_pkcs7_sign_flags = ^Tgnutls_pkcs7_sign_flags;
  Tgnutls_pkcs7_sign_flags =  Longint;

Var
  gnutls_malloc : Tgnutls_alloc_function;cvar;external;
  gnutls_realloc : Tgnutls_realloc_function;cvar;external;
  gnutls_calloc : Tgnutls_calloc_function;cvar;external;
  gnutls_free : Tgnutls_free_function;cvar;external;
  gnutls_strdup : function (para1:Pchar):Pchar;cvar;external;
  gnutls_srp_4096_group_prime : Tgnutls_datum_t;cvar;external;
  gnutls_srp_4096_group_generator : Tgnutls_datum_t;cvar;external;
  gnutls_srp_3072_group_prime : Tgnutls_datum_t;cvar;external;
  gnutls_srp_3072_group_generator : Tgnutls_datum_t;cvar;external;
  gnutls_srp_2048_group_prime : Tgnutls_datum_t;cvar;external;
  gnutls_srp_2048_group_generator : Tgnutls_datum_t;cvar;external;
  gnutls_srp_1536_group_prime : Tgnutls_datum_t;cvar;external;
  gnutls_srp_1536_group_generator : Tgnutls_datum_t;cvar;external;
  gnutls_srp_1024_group_prime : Tgnutls_datum_t;cvar;external;
  gnutls_srp_1024_group_generator : Tgnutls_datum_t;cvar;external;


var
  gnutls_handshake_description_get_name : function(_type:Tgnutls_handshake_description_t):Pchar;cdecl;
  gnutls_pk_algorithm_get_name : function(algorithm:Tgnutls_pk_algorithm_t):Pchar;cdecl;
  gnutls_init : function(session:Pgnutls_session_t; flags:dword):longint;cdecl;
  gnutls_deinit : procedure(session:Tgnutls_session_t);cdecl;
  gnutls_bye : function(session:Tgnutls_session_t; how:Tgnutls_close_request_t):longint;cdecl;
  gnutls_handshake : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_handshake_set_timeout : procedure(session:Tgnutls_session_t; ms:dword);cdecl;
  gnutls_rehandshake : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_alert_get : function(session:Tgnutls_session_t):Tgnutls_alert_description_t;cdecl;
  gnutls_alert_send : function(session:Tgnutls_session_t; level:Tgnutls_alert_level_t; desc:Tgnutls_alert_description_t):longint;cdecl;
  gnutls_alert_send_appropriate : function(session:Tgnutls_session_t; err:longint):longint;cdecl;
  gnutls_alert_get_name : function(alert:Tgnutls_alert_description_t):Pchar;cdecl;
  gnutls_alert_get_strname : function(alert:Tgnutls_alert_description_t):Pchar;cdecl;
  gnutls_pk_bits_to_sec_param : function(algo:Tgnutls_pk_algorithm_t; bits:dword):Tgnutls_sec_param_t;cdecl;
  gnutls_sec_param_get_name : function(param:Tgnutls_sec_param_t):Pchar;cdecl;
  gnutls_sec_param_to_pk_bits : function(algo:Tgnutls_pk_algorithm_t; param:Tgnutls_sec_param_t):dword;cdecl;
  gnutls_sec_param_to_symmetric_bits : function(param:Tgnutls_sec_param_t):dword;cdecl;
  gnutls_ecc_curve_get_name : function(curve:Tgnutls_ecc_curve_t):Pchar;cdecl;
  gnutls_ecc_curve_get_oid : function(curve:Tgnutls_ecc_curve_t):Pchar;cdecl;
  gnutls_ecc_curve_get_size : function(curve:Tgnutls_ecc_curve_t):longint;cdecl;
  gnutls_ecc_curve_get : function(session:Tgnutls_session_t):Tgnutls_ecc_curve_t;cdecl;
  gnutls_cipher_get : function(session:Tgnutls_session_t):Tgnutls_cipher_algorithm_t;cdecl;
  gnutls_kx_get : function(session:Tgnutls_session_t):Tgnutls_kx_algorithm_t;cdecl;
  gnutls_mac_get : function(session:Tgnutls_session_t):Tgnutls_mac_algorithm_t;cdecl;
  gnutls_compression_get : function(session:Tgnutls_session_t):Tgnutls_compression_method_t;cdecl;
  gnutls_certificate_type_get : function(session:Tgnutls_session_t):Tgnutls_certificate_type_t;cdecl;
  gnutls_sign_algorithm_get : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_sign_algorithm_get_client : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_sign_algorithm_get_requested : function(session:Tgnutls_session_t; indx:Tsize_t; algo:Pgnutls_sign_algorithm_t):longint;cdecl;
  gnutls_cipher_get_name : function(algorithm:Tgnutls_cipher_algorithm_t):Pchar;cdecl;
  gnutls_mac_get_name : function(algorithm:Tgnutls_mac_algorithm_t):Pchar;cdecl;
  gnutls_digest_get_name : function(algorithm:Tgnutls_digest_algorithm_t):Pchar;cdecl;
  gnutls_digest_get_oid : function(algorithm:Tgnutls_digest_algorithm_t):Pchar;cdecl;
  gnutls_compression_get_name : function(algorithm:Tgnutls_compression_method_t):Pchar;cdecl;
  gnutls_kx_get_name : function(algorithm:Tgnutls_kx_algorithm_t):Pchar;cdecl;
  gnutls_certificate_type_get_name : function(_type:Tgnutls_certificate_type_t):Pchar;cdecl;
  gnutls_pk_get_name : function(algorithm:Tgnutls_pk_algorithm_t):Pchar;cdecl;
  gnutls_pk_get_oid : function(algorithm:Tgnutls_pk_algorithm_t):Pchar;cdecl;
  gnutls_sign_get_name : function(algorithm:Tgnutls_sign_algorithm_t):Pchar;cdecl;
  gnutls_sign_get_oid : function(algorithm:Tgnutls_sign_algorithm_t):Pchar;cdecl;
  gnutls_cipher_get_key_size : function(algorithm:Tgnutls_cipher_algorithm_t):Tsize_t;cdecl;
  gnutls_mac_get_key_size : function(algorithm:Tgnutls_mac_algorithm_t):Tsize_t;cdecl;
  gnutls_sign_is_secure : function(algorithm:Tgnutls_sign_algorithm_t):longint;cdecl;
  gnutls_sign_get_hash_algorithm : function(sign:Tgnutls_sign_algorithm_t):Tgnutls_digest_algorithm_t;cdecl;
  gnutls_sign_get_pk_algorithm : function(sign:Tgnutls_sign_algorithm_t):Tgnutls_pk_algorithm_t;cdecl;
  gnutls_pk_to_sign : function(pk:Tgnutls_pk_algorithm_t; hash:Tgnutls_digest_algorithm_t):Tgnutls_sign_algorithm_t;cdecl;
  gnutls_mac_get_id : function(name:Pchar):Tgnutls_mac_algorithm_t;cdecl;
  gnutls_digest_get_id : function(name:Pchar):Tgnutls_digest_algorithm_t;cdecl;
  gnutls_compression_get_id : function(name:Pchar):Tgnutls_compression_method_t;cdecl;
  gnutls_cipher_get_id : function(name:Pchar):Tgnutls_cipher_algorithm_t;cdecl;
  gnutls_kx_get_id : function(name:Pchar):Tgnutls_kx_algorithm_t;cdecl;
  gnutls_protocol_get_id : function(name:Pchar):Tgnutls_protocol_t;cdecl;
  gnutls_certificate_type_get_id : function(name:Pchar):Tgnutls_certificate_type_t;cdecl;
  gnutls_pk_get_id : function(name:Pchar):Tgnutls_pk_algorithm_t;cdecl;
  gnutls_sign_get_id : function(name:Pchar):Tgnutls_sign_algorithm_t;cdecl;
  gnutls_ecc_curve_get_id : function(name:Pchar):Tgnutls_ecc_curve_t;cdecl;
  gnutls_oid_to_digest : function(oid:Pchar):Tgnutls_digest_algorithm_t;cdecl;
  gnutls_oid_to_pk : function(oid:Pchar):Tgnutls_pk_algorithm_t;cdecl;
  gnutls_oid_to_sign : function(oid:Pchar):Tgnutls_sign_algorithm_t;cdecl;
  gnutls_oid_to_ecc_curve : function(oid:Pchar):Tgnutls_ecc_curve_t;cdecl;
  gnutls_ecc_curve_list : function:Pgnutls_ecc_curve_t;cdecl;
  gnutls_cipher_list : function:Pgnutls_cipher_algorithm_t;cdecl;
  gnutls_mac_list : function:Pgnutls_mac_algorithm_t;cdecl;
  gnutls_digest_list : function:Pgnutls_digest_algorithm_t;cdecl;
  gnutls_compression_list : function:Pgnutls_compression_method_t;cdecl;
  gnutls_protocol_list : function:Pgnutls_protocol_t;cdecl;
  gnutls_certificate_type_list : function:Pgnutls_certificate_type_t;cdecl;
  gnutls_kx_list : function:Pgnutls_kx_algorithm_t;cdecl;
  gnutls_pk_list : function:Pgnutls_pk_algorithm_t;cdecl;
  gnutls_sign_list : function:Pgnutls_sign_algorithm_t;cdecl;
  gnutls_cipher_suite_info : function(idx:Tsize_t; cs_id:Pbyte; kx:Pgnutls_kx_algorithm_t; cipher:Pgnutls_cipher_algorithm_t; mac:Pgnutls_mac_algorithm_t; min_version:Pgnutls_protocol_t):Pchar;cdecl;
  gnutls_error_is_fatal : function(error:longint):longint;cdecl;
  gnutls_error_to_alert : function(err:longint; level:Plongint):longint;cdecl;
  gnutls_perror : procedure(error:longint);cdecl;
  gnutls_strerror : function(error:longint):Pchar;cdecl;
  gnutls_strerror_name : function(error:longint):Pchar;cdecl;
  gnutls_handshake_set_private_extensions : procedure(session:Tgnutls_session_t; allow:longint);cdecl;
  gnutls_handshake_set_random : function(session:Tgnutls_session_t; random:Pgnutls_datum_t):longint;cdecl;
  gnutls_handshake_get_last_out : function(session:Tgnutls_session_t):Tgnutls_handshake_description_t;cdecl;
  gnutls_handshake_get_last_in : function(session:Tgnutls_session_t):Tgnutls_handshake_description_t;cdecl;
  gnutls_heartbeat_ping : function(session:Tgnutls_session_t; data_size:Tsize_t; max_tries:dword; flags:dword):longint;cdecl;
  gnutls_heartbeat_pong : function(session:Tgnutls_session_t; flags:dword):longint;cdecl;
  gnutls_record_set_timeout : procedure(session:Tgnutls_session_t; ms:dword);cdecl;
  gnutls_record_disable_padding : procedure(session:Tgnutls_session_t);cdecl;
  gnutls_record_cork : procedure(session:Tgnutls_session_t);cdecl;
  gnutls_record_uncork : function(session:Tgnutls_session_t; flags:dword):longint;cdecl;
  gnutls_record_discard_queued : function(session:Tgnutls_session_t):Tsize_t;cdecl;
  gnutls_record_get_state : function(session:Tgnutls_session_t; read:dword; mac_key:Pgnutls_datum_t; IV:Pgnutls_datum_t; cipher_key:Pgnutls_datum_t; seq_number: tseqarray):longint;cdecl;
  gnutls_record_set_state : function(session:Tgnutls_session_t; read:dword; seq_number: TSeqArray):longint;cdecl;
  gnutls_range_split : function(session:Tgnutls_session_t; orig:Pgnutls_range_st; small_range:Pgnutls_range_st; rem_range:Pgnutls_range_st):longint;cdecl;
  gnutls_record_send : function(session:Tgnutls_session_t; data:pointer; data_size:Tsize_t):Tssize_t;cdecl;
  gnutls_record_send_range : function(session:Tgnutls_session_t; data:pointer; data_size:Tsize_t; range:Pgnutls_range_st):Tssize_t;cdecl;
  gnutls_record_recv : function(session:Tgnutls_session_t; data:pointer; data_size:Tsize_t):Tssize_t;cdecl;
  gnutls_record_recv_packet : function(session:Tgnutls_session_t; packet:Pgnutls_packet_t):Tssize_t;cdecl;
  gnutls_packet_get : procedure(packet:Tgnutls_packet_t; data:Pgnutls_datum_t; sequence:Pbyte);cdecl;
  gnutls_packet_deinit : procedure(packet:Tgnutls_packet_t);cdecl;
  gnutls_record_recv_seq : function(session:Tgnutls_session_t; data:pointer; data_size:Tsize_t; seq:Pbyte):Tssize_t;cdecl;
  gnutls_record_overhead_size : function(session:Tgnutls_session_t):Tsize_t;cdecl;
  gnutls_est_record_overhead_size : function(version:Tgnutls_protocol_t; cipher:Tgnutls_cipher_algorithm_t; mac:Tgnutls_mac_algorithm_t; comp:Tgnutls_compression_method_t; flags:dword):Tsize_t;cdecl;
  gnutls_session_enable_compatibility_mode : procedure(session:Tgnutls_session_t);cdecl;
  gnutls_record_can_use_length_hiding : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_record_get_direction : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_record_get_max_size : function(session:Tgnutls_session_t):Tsize_t;cdecl;
  gnutls_record_set_max_size : function(session:Tgnutls_session_t; size:Tsize_t):Tssize_t;cdecl;
  gnutls_record_check_pending : function(session:Tgnutls_session_t):Tsize_t;cdecl;
  gnutls_record_check_corked : function(session:Tgnutls_session_t):Tsize_t;cdecl;
  gnutls_session_force_valid : procedure(session:Tgnutls_session_t);cdecl;
  gnutls_prf : function(session:Tgnutls_session_t; label_size:Tsize_t; _label:Pchar; server_random_first:longint; extra_size:Tsize_t;  extra:Pchar; outsize:Tsize_t; outs :Pchar):longint;cdecl;
  gnutls_prf_rfc5705 : function(session:Tgnutls_session_t; label_size:Tsize_t; _label:Pchar; context_size:Tsize_t; context:Pchar; outsize:Tsize_t; outs:Pchar):longint;cdecl;
  gnutls_prf_raw : function(session:Tgnutls_session_t; label_size:Tsize_t; _label:Pchar; seed_size:Tsize_t; seed:Pchar;  outsize:Tsize_t; outs:Pchar):longint;cdecl;
  gnutls_server_name_set : function(session:Tgnutls_session_t; _type:Tgnutls_server_name_type_t; name:pointer; name_length:Tsize_t):longint;cdecl;
  gnutls_server_name_get : function(session:Tgnutls_session_t; data:pointer; data_length:Psize_t; _type:Pdword; indx:dword):longint;cdecl;
  gnutls_heartbeat_get_timeout : function(session:Tgnutls_session_t):dword;cdecl;
  gnutls_heartbeat_set_timeouts : procedure(session:Tgnutls_session_t; retrans_timeout:dword; total_timeout:dword);cdecl;
  gnutls_heartbeat_enable : procedure(session:Tgnutls_session_t; _type:dword);cdecl;
  gnutls_heartbeat_allowed : function(session:Tgnutls_session_t; _type:dword):longint;cdecl;
  gnutls_safe_renegotiation_status : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_session_ext_master_secret_status : function(session:Tgnutls_session_t):dword;cdecl;
  gnutls_session_etm_status : function(session:Tgnutls_session_t):dword;cdecl;
  gnutls_supplemental_get_name : function(_type:Tgnutls_supplemental_data_format_type_t):Pchar;cdecl;
  gnutls_session_ticket_key_generate : function(key:Pgnutls_datum_t):longint;cdecl;
  gnutls_session_ticket_enable_client : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_session_ticket_enable_server : function(session:Tgnutls_session_t; key:Pgnutls_datum_t):longint;cdecl;
  gnutls_srtp_set_profile : function(session:Tgnutls_session_t; profile:Tgnutls_srtp_profile_t):longint;cdecl;
  gnutls_srtp_set_profile_direct : function(session:Tgnutls_session_t; profiles:Pchar; err_pos:PPchar):longint;cdecl;
  gnutls_srtp_get_selected_profile : function(session:Tgnutls_session_t; profile:Pgnutls_srtp_profile_t):longint;cdecl;
  gnutls_srtp_get_profile_name : function(profile:Tgnutls_srtp_profile_t):Pchar;cdecl;
  gnutls_srtp_get_profile_id : function(name:Pchar; profile:Pgnutls_srtp_profile_t):longint;cdecl;
  gnutls_srtp_get_keys : function(session:Tgnutls_session_t; key_material:pointer; key_material_size:dword; client_key:Pgnutls_datum_t; client_salt:Pgnutls_datum_t;  server_key:Pgnutls_datum_t; server_salt:Pgnutls_datum_t):longint;cdecl;
  gnutls_srtp_set_mki : function(session:Tgnutls_session_t; mki:Pgnutls_datum_t):longint;cdecl;
  gnutls_srtp_get_mki : function(session:Tgnutls_session_t; mki:Pgnutls_datum_t):longint;cdecl;
  gnutls_alpn_get_selected_protocol : function(session:Tgnutls_session_t; protocol:Pgnutls_datum_t):longint;cdecl;
  gnutls_alpn_set_protocols : function(session:Tgnutls_session_t; protocols:Pgnutls_datum_t; protocols_size:dword; flags:dword):longint;cdecl;
  gnutls_key_generate : function(key:Pgnutls_datum_t; key_size:dword):longint;cdecl;
  gnutls_priority_init : function(priority_cache:Pgnutls_priority_t; priorities:Pchar; err_pos:PPchar):longint;cdecl;
  gnutls_priority_deinit : procedure(priority_cache:Tgnutls_priority_t);cdecl;
  gnutls_priority_get_cipher_suite_index : function(pcache:Tgnutls_priority_t; idx:dword; sidx:Pdword):longint;cdecl;
  gnutls_priority_string_list : function(iter:dword; flags:dword):Pchar;cdecl;
  gnutls_priority_set : function(session:Tgnutls_session_t; priority:Tgnutls_priority_t):longint;cdecl;
  gnutls_priority_set_direct : function(session:Tgnutls_session_t; priorities:Pchar; err_pos:PPchar):longint;cdecl;
  gnutls_priority_certificate_type_list : function(pcache:Tgnutls_priority_t; list:PPdword):longint;cdecl;
  gnutls_priority_sign_list : function(pcache:Tgnutls_priority_t; list:PPdword):longint;cdecl;
  gnutls_priority_protocol_list : function(pcache:Tgnutls_priority_t; list:PPdword):longint;cdecl;
  gnutls_priority_compression_list : function(pcache:Tgnutls_priority_t; list:PPdword):longint;cdecl;
  gnutls_priority_ecc_curve_list : function(pcache:Tgnutls_priority_t; list:PPdword):longint;cdecl;
  gnutls_priority_kx_list : function(pcache:Tgnutls_priority_t; list:PPdword):longint;cdecl;
  gnutls_priority_cipher_list : function(pcache:Tgnutls_priority_t; list:PPdword):longint;cdecl;
  gnutls_priority_mac_list : function(pcache:Tgnutls_priority_t; list:PPdword):longint;cdecl;
  gnutls_set_default_priority : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_cipher_suite_get_name : function(kx_algorithm:Tgnutls_kx_algorithm_t; cipher_algorithm:Tgnutls_cipher_algorithm_t; mac_algorithm:Tgnutls_mac_algorithm_t):Pchar;cdecl;
  gnutls_protocol_get_version : function(session:Tgnutls_session_t):Tgnutls_protocol_t;cdecl;
  gnutls_protocol_get_name : function(version:Tgnutls_protocol_t):Pchar;cdecl;
  gnutls_session_set_data : function(session:Tgnutls_session_t; session_data:pointer; session_data_size:Tsize_t):longint;cdecl;
  gnutls_session_get_data : function(session:Tgnutls_session_t; session_data:pointer; session_data_size:Psize_t):longint;cdecl;
  gnutls_session_get_data2 : function(session:Tgnutls_session_t; data:Pgnutls_datum_t):longint;cdecl;
  gnutls_session_get_random : procedure(session:Tgnutls_session_t; client:Pgnutls_datum_t; server:Pgnutls_datum_t);cdecl;
  gnutls_session_get_desc : function(session:Tgnutls_session_t):Pchar;cdecl;
  gnutls_session_set_verify_function : procedure(session:Tgnutls_session_t; func:tgnutls_certificate_verify_function);cdecl;
  gnutls_session_set_verify_cert : procedure(session:Tgnutls_session_t; hostname:Pchar; flags:dword);cdecl;
  gnutls_session_set_verify_cert2 : procedure(session:Tgnutls_session_t; data:Pgnutls_typed_vdata_st; elements:dword; flags:dword);cdecl;
  gnutls_session_get_verify_cert_status : function(para1:Tgnutls_session_t):dword;cdecl;
  gnutls_session_set_premaster : function(session:Tgnutls_session_t; entity:dword; version:Tgnutls_protocol_t; kx:Tgnutls_kx_algorithm_t; cipher:Tgnutls_cipher_algorithm_t;
      mac:Tgnutls_mac_algorithm_t; comp:Tgnutls_compression_method_t; master:Pgnutls_datum_t; session_id:Pgnutls_datum_t):longint;cdecl;
  gnutls_session_get_id : function(session:Tgnutls_session_t; session_id:pointer; session_id_size:Psize_t):longint;cdecl;
  gnutls_session_get_id2 : function(session:Tgnutls_session_t; session_id:Pgnutls_datum_t):longint;cdecl;
  gnutls_session_set_id : function(session:Tgnutls_session_t; sid:Pgnutls_datum_t):longint;cdecl;
  gnutls_session_channel_binding : function(session:Tgnutls_session_t; cbtype:Tgnutls_channel_binding_t; cb:Pgnutls_datum_t):longint;cdecl;
  gnutls_session_is_resumed : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_session_resumption_requested : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_db_set_cache_expiration : procedure(session:Tgnutls_session_t; seconds:longint);cdecl;
  gnutls_db_get_default_cache_expiration : function:dword;cdecl;
  gnutls_db_remove_session : procedure(session:Tgnutls_session_t);cdecl;
  gnutls_db_set_retrieve_function : procedure(session:Tgnutls_session_t; retr_func:Tgnutls_db_retr_func);cdecl;
  gnutls_db_set_remove_function : procedure(session:Tgnutls_session_t; rem_func:Tgnutls_db_remove_func);cdecl;
  gnutls_db_set_store_function : procedure(session:Tgnutls_session_t; store_func:Tgnutls_db_store_func);cdecl;
  gnutls_db_set_ptr : procedure(session:Tgnutls_session_t; ptr:pointer);cdecl;
  gnutls_db_get_ptr : function(session:Tgnutls_session_t):pointer;cdecl;
  gnutls_db_check_entry : function(session:Tgnutls_session_t; session_entry:Tgnutls_datum_t):longint;cdecl;
  gnutls_db_check_entry_time : function(entry:Pgnutls_datum_t):Ttime_t;cdecl;
  gnutls_handshake_set_hook_function : procedure(session:Tgnutls_session_t; htype:dword; post:longint; func:Tgnutls_handshake_hook_func);cdecl;
  gnutls_handshake_set_post_client_hello_function : procedure(session:Tgnutls_session_t; func:Tgnutls_handshake_post_client_hello_func);cdecl;
  gnutls_handshake_set_max_packet_length : procedure(session:Tgnutls_session_t; max:Tsize_t);cdecl;
  gnutls_check_version : function(req_version:Pchar):Pchar;cdecl;
  gnutls_credentials_clear : procedure(session:Tgnutls_session_t);cdecl;
  gnutls_credentials_set : function(session:Tgnutls_session_t; _type:Tgnutls_credentials_type_t; cred:pointer):longint;cdecl;
  gnutls_credentials_get : function(session:Tgnutls_session_t; _type:Tgnutls_credentials_type_t; cred:Ppointer):longint;cdecl;
  gnutls_anon_free_server_credentials : procedure(sc:Tgnutls_anon_server_credentials_t);cdecl;
  gnutls_anon_allocate_server_credentials : function(sc:Pgnutls_anon_server_credentials_t):longint;cdecl;
  gnutls_anon_set_server_dh_params : procedure(res:Tgnutls_anon_server_credentials_t; dh_params:Tgnutls_dh_params_t);cdecl;
  gnutls_anon_set_server_params_function : procedure(res:Tgnutls_anon_server_credentials_t; func: tgnutls_params_function);cdecl;
  gnutls_anon_free_client_credentials : procedure(sc:Tgnutls_anon_client_credentials_t);cdecl;
  gnutls_anon_allocate_client_credentials : function(sc:Pgnutls_anon_client_credentials_t):longint;cdecl;
  gnutls_certificate_free_credentials : procedure(sc:Tgnutls_certificate_credentials_t);cdecl;
  gnutls_certificate_allocate_credentials : function(res:Pgnutls_certificate_credentials_t):longint;cdecl;
  gnutls_certificate_get_issuer : function(sc:Tgnutls_certificate_credentials_t; cert:Tgnutls_x509_crt_t; issuer:Pgnutls_x509_crt_t; flags:dword):longint;cdecl;
  gnutls_certificate_get_crt_raw : function(sc:Tgnutls_certificate_credentials_t; idx1:dword; idx2:dword; cert:Pgnutls_datum_t):longint;cdecl;
  gnutls_certificate_get_x509_crt : function(res:Tgnutls_certificate_credentials_t; index:dword; crt_list:PPgnutls_x509_crt_t; crt_list_size:Pdword):longint;cdecl;
  gnutls_certificate_get_x509_key : function(res:Tgnutls_certificate_credentials_t; index:dword; key:Pgnutls_x509_privkey_t):longint;cdecl;
  gnutls_certificate_free_keys : procedure(sc:Tgnutls_certificate_credentials_t);cdecl;
  gnutls_certificate_free_cas : procedure(sc:Tgnutls_certificate_credentials_t);cdecl;
  gnutls_certificate_free_ca_names : procedure(sc:Tgnutls_certificate_credentials_t);cdecl;
  gnutls_certificate_free_crls : procedure(sc:Tgnutls_certificate_credentials_t);cdecl;
  gnutls_certificate_set_dh_params : procedure(res:Tgnutls_certificate_credentials_t; dh_params:Tgnutls_dh_params_t);cdecl;
  gnutls_certificate_set_verify_flags : procedure(res:Tgnutls_certificate_credentials_t; flags:dword);cdecl;
  gnutls_certificate_get_verify_flags : function(res:Tgnutls_certificate_credentials_t):dword;cdecl;
  gnutls_certificate_set_flags : procedure(para1:Tgnutls_certificate_credentials_t; flags:dword);cdecl;
  gnutls_certificate_set_verify_limits : procedure(res:Tgnutls_certificate_credentials_t; max_bits:dword; max_depth:dword);cdecl;
  gnutls_certificate_set_x509_system_trust : function(cred:Tgnutls_certificate_credentials_t):longint;cdecl;
  gnutls_certificate_set_x509_trust_file : function(cred:Tgnutls_certificate_credentials_t; cafile:Pchar; _type:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_certificate_set_x509_trust_dir : function(cred:Tgnutls_certificate_credentials_t; ca_dir:Pchar; _type:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_certificate_set_x509_trust_mem : function(res:Tgnutls_certificate_credentials_t; ca:Pgnutls_datum_t; _type:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_certificate_set_x509_crl_file : function(res:Tgnutls_certificate_credentials_t; crlfile:Pchar; _type:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_certificate_set_x509_crl_mem : function(res:Tgnutls_certificate_credentials_t; CRL:Pgnutls_datum_t; _type:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_certificate_set_x509_key_file : function(res:Tgnutls_certificate_credentials_t; certfile:Pchar; keyfile:Pchar; _type:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_certificate_set_x509_key_file2 : function(res:Tgnutls_certificate_credentials_t; certfile:Pchar; keyfile:Pchar; _type:Tgnutls_x509_crt_fmt_t; pass:Pchar;
      flags:dword):longint;cdecl;
  gnutls_certificate_set_x509_key_mem : function(res:Tgnutls_certificate_credentials_t; cert:Pgnutls_datum_t; key:Pgnutls_datum_t; _type:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_certificate_set_x509_key_mem2 : function(res:Tgnutls_certificate_credentials_t; cert:Pgnutls_datum_t; key:Pgnutls_datum_t; _type:Tgnutls_x509_crt_fmt_t; pass:Pchar;
      flags:dword):longint;cdecl;
  gnutls_certificate_send_x509_rdn_sequence : procedure(session:Tgnutls_session_t; status:longint);cdecl;
  gnutls_certificate_set_x509_simple_pkcs12_file : function(res:Tgnutls_certificate_credentials_t; pkcs12file:Pchar; _type:Tgnutls_x509_crt_fmt_t; password:Pchar):longint;cdecl;
  gnutls_certificate_set_x509_simple_pkcs12_mem : function(res:Tgnutls_certificate_credentials_t; p12blob:Pgnutls_datum_t; _type:Tgnutls_x509_crt_fmt_t; password:Pchar):longint;cdecl;
  gnutls_certificate_set_x509_key : function(res:Tgnutls_certificate_credentials_t; cert_list:Pgnutls_x509_crt_t; cert_list_size:longint; key:Tgnutls_x509_privkey_t):longint;cdecl;
  gnutls_certificate_set_x509_trust : function(res:Tgnutls_certificate_credentials_t; ca_list:Pgnutls_x509_crt_t; ca_list_size:longint):longint;cdecl;
  gnutls_certificate_set_x509_crl : function(res:Tgnutls_certificate_credentials_t; crl_list:Pgnutls_x509_crl_t; crl_list_size:longint):longint;cdecl;
  gnutls_certificate_set_ocsp_status_request_function : procedure(res:Tgnutls_certificate_credentials_t; ocsp_func:Tgnutls_status_request_ocsp_func; ptr:pointer);cdecl;
  gnutls_certificate_set_ocsp_status_request_file : function(res:Tgnutls_certificate_credentials_t; response_file:Pchar; flags:dword):longint;cdecl;
  gnutls_ocsp_status_request_enable_client : function(session:Tgnutls_session_t; responder_id:Pgnutls_datum_t; responder_id_size:Tsize_t; request_extensions:Pgnutls_datum_t):longint;cdecl;
  gnutls_ocsp_status_request_get : function(session:Tgnutls_session_t; response:Pgnutls_datum_t):longint;cdecl;
  gnutls_ocsp_status_request_is_checked : function(session:Tgnutls_session_t; flags:dword):longint;cdecl;
  gnutls_global_init : function:longint;cdecl;
  gnutls_global_deinit : procedure;cdecl;
  gnutls_global_set_mutex : procedure(init:Tmutex_init_func; deinit:Tmutex_deinit_func; lock:Tmutex_lock_func; unlock:Tmutex_unlock_func);cdecl;
  gnutls_global_set_time_function : procedure(time_func:Tgnutls_time_func);cdecl;
  gnutls_memset : procedure(data:pointer; c:longint; size:Tsize_t);cdecl;
  gnutls_memcmp : function(s1:pointer; s2:pointer; n:Tsize_t):longint;cdecl;
  gnutls_global_set_log_function : procedure(log_func:Tgnutls_log_func);cdecl;
  gnutls_global_set_audit_log_function : procedure(log_func:Tgnutls_audit_log_func);cdecl;
  gnutls_global_set_log_level : procedure(level:longint);cdecl;
  gnutls_dh_params_init : function(dh_params:Pgnutls_dh_params_t):longint;cdecl;
  gnutls_dh_params_deinit : procedure(dh_params:Tgnutls_dh_params_t);cdecl;
  gnutls_dh_params_import_raw : function(dh_params:Tgnutls_dh_params_t; prime:Pgnutls_datum_t; generator:Pgnutls_datum_t):longint;cdecl;
  gnutls_dh_params_import_raw2 : function(dh_params:Tgnutls_dh_params_t; prime:Pgnutls_datum_t; generator:Pgnutls_datum_t; key_bits:dword):longint;cdecl;
  gnutls_dh_params_import_pkcs3 : function(params:Tgnutls_dh_params_t; pkcs3_params:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_dh_params_generate2 : function(params:Tgnutls_dh_params_t; bits:dword):longint;cdecl;
  gnutls_dh_params_export_pkcs3 : function(params:Tgnutls_dh_params_t; format:Tgnutls_x509_crt_fmt_t; params_data:Pbyte; params_data_size:Psize_t):longint;cdecl;
  gnutls_dh_params_export2_pkcs3 : function(params:Tgnutls_dh_params_t; format:Tgnutls_x509_crt_fmt_t; outd :Pgnutls_datum_t):longint;cdecl;
  gnutls_dh_params_export_raw : function(params:Tgnutls_dh_params_t; prime:Pgnutls_datum_t; generator:Pgnutls_datum_t; bits:Pdword):longint;cdecl;
  gnutls_dh_params_cpy : function(dst:Tgnutls_dh_params_t; src:Tgnutls_dh_params_t):longint;cdecl;
  gnutls_system_recv_timeout : function(ptr:Tgnutls_transport_ptr_t; ms:dword):longint;cdecl;
  gnutls_transport_set_int2 : procedure(session:Tgnutls_session_t; r:longint; s:longint);cdecl;
  gnutls_transport_get_int2 : procedure(session:Tgnutls_session_t; r:Plongint; s:Plongint);cdecl;
  gnutls_transport_get_int : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_transport_set_ptr : procedure(session:Tgnutls_session_t; ptr:Tgnutls_transport_ptr_t);cdecl;
  gnutls_transport_set_ptr2 : procedure(session:Tgnutls_session_t; recv_ptr:Tgnutls_transport_ptr_t; send_ptr:Tgnutls_transport_ptr_t);cdecl;
  gnutls_transport_get_ptr : function(session:Tgnutls_session_t):Tgnutls_transport_ptr_t;cdecl;
  gnutls_transport_get_ptr2 : procedure(session:Tgnutls_session_t; recv_ptr:Pgnutls_transport_ptr_t; send_ptr:Pgnutls_transport_ptr_t);cdecl;
  gnutls_transport_set_vec_push_function : procedure(session:Tgnutls_session_t; vec_func:Tgnutls_vec_push_func);cdecl;
  gnutls_transport_set_push_function : procedure(session:Tgnutls_session_t; push_func:Tgnutls_push_func);cdecl;
  gnutls_transport_set_pull_function : procedure(session:Tgnutls_session_t; pull_func:Tgnutls_pull_func);cdecl;
  gnutls_transport_set_pull_timeout_function : procedure(session:Tgnutls_session_t; func:Tgnutls_pull_timeout_func);cdecl;
  gnutls_transport_set_errno_function : procedure(session:Tgnutls_session_t; errno_func:Tgnutls_errno_func);cdecl;
  gnutls_transport_set_errno : procedure(session:Tgnutls_session_t; err:longint);cdecl;
  gnutls_session_set_ptr : procedure(session:Tgnutls_session_t; ptr:pointer);cdecl;
  gnutls_session_get_ptr : function(session:Tgnutls_session_t):pointer;cdecl;
  gnutls_openpgp_send_cert : procedure(session:Tgnutls_session_t; status:Tgnutls_openpgp_crt_status_t);cdecl;
  gnutls_fingerprint : function(algo:Tgnutls_digest_algorithm_t; data:Pgnutls_datum_t; result:pointer; result_size:Psize_t):longint;cdecl;
  gnutls_random_art : function(_type:Tgnutls_random_art_t; key_type:Pchar; key_size:dword; fpr:pointer; fpr_size:Tsize_t;  art:Pgnutls_datum_t):longint;cdecl;
  gnutls_srp_free_client_credentials : procedure(sc:Tgnutls_srp_client_credentials_t);cdecl;
  gnutls_srp_allocate_client_credentials : function(sc:Pgnutls_srp_client_credentials_t):longint;cdecl;
  gnutls_srp_set_client_credentials : function(res:Tgnutls_srp_client_credentials_t; username:Pchar; password:Pchar):longint;cdecl;
  gnutls_srp_free_server_credentials : procedure(sc:Tgnutls_srp_server_credentials_t);cdecl;
  gnutls_srp_allocate_server_credentials : function(sc:Pgnutls_srp_server_credentials_t):longint;cdecl;
  gnutls_srp_set_server_credentials_file : function(res:Tgnutls_srp_server_credentials_t; password_file:Pchar; password_conf_file:Pchar):longint;cdecl;
  gnutls_srp_server_get_username : function(session:Tgnutls_session_t):Pchar;cdecl;
  gnutls_srp_set_prime_bits : procedure(session:Tgnutls_session_t; bits:dword);cdecl;
  gnutls_srp_verifier : function(username:Pchar; password:Pchar; salt:Pgnutls_datum_t; generator:Pgnutls_datum_t; prime:Pgnutls_datum_t; res:Pgnutls_datum_t):longint;cdecl;
  gnutls_srp_set_server_credentials_function : procedure(cred:Tgnutls_srp_server_credentials_t; func:tgnutls_srp_server_credentials_function);cdecl;
  gnutls_srp_set_client_credentials_function : procedure(cred:Tgnutls_srp_client_credentials_t; func:tgnutls_srp_client_credentials_function);cdecl;
  gnutls_srp_base64_encode : function(data:Pgnutls_datum_t; result:Pchar; result_size:Psize_t):longint;cdecl;
  gnutls_srp_base64_encode2 : function(data:Pgnutls_datum_t; result:Pgnutls_datum_t):longint;cdecl;
  gnutls_srp_base64_decode : function(b64_data:Pgnutls_datum_t; result:Pchar; result_size:Psize_t):longint;cdecl;
  gnutls_srp_base64_decode2 : function(b64_data:Pgnutls_datum_t; result:Pgnutls_datum_t):longint;cdecl;
  gnutls_srp_set_server_fake_salt_seed : procedure(sc:Tgnutls_srp_server_credentials_t; seed:Pgnutls_datum_t; salt_length:dword);cdecl;
  gnutls_psk_free_client_credentials : procedure(sc:Tgnutls_psk_client_credentials_t);cdecl;
  gnutls_psk_allocate_client_credentials : function(sc:Pgnutls_psk_client_credentials_t):longint;cdecl;
  gnutls_psk_set_client_credentials : function(res:Tgnutls_psk_client_credentials_t; username:Pchar; key:Pgnutls_datum_t; flags:Tgnutls_psk_key_flags):longint;cdecl;
  gnutls_psk_free_server_credentials : procedure(sc:Tgnutls_psk_server_credentials_t);cdecl;
  gnutls_psk_allocate_server_credentials : function(sc:Pgnutls_psk_server_credentials_t):longint;cdecl;
  gnutls_psk_set_server_credentials_file : function(res:Tgnutls_psk_server_credentials_t; password_file:Pchar):longint;cdecl;
  gnutls_psk_set_server_credentials_hint : function(res:Tgnutls_psk_server_credentials_t; hint:Pchar):longint;cdecl;
  gnutls_psk_server_get_username : function(session:Tgnutls_session_t):Pchar;cdecl;
  gnutls_psk_client_get_hint : function(session:Tgnutls_session_t):Pchar;cdecl;
  gnutls_psk_set_server_credentials_function : procedure(cred:Tgnutls_psk_server_credentials_t; func:Tgnutls_psk_server_credentials_function);cdecl;
  gnutls_psk_set_client_credentials_function : procedure(cred:Tgnutls_psk_client_credentials_t; func:Tgnutls_psk_client_credentials_function);cdecl;
  gnutls_hex_encode : function(data:Pgnutls_datum_t; result:Pchar; result_size:Psize_t):longint;cdecl;
  gnutls_hex_decode : function(hex_data:Pgnutls_datum_t; result:pointer; result_size:Psize_t):longint;cdecl;
  gnutls_hex_encode2 : function(data:Pgnutls_datum_t; result:Pgnutls_datum_t):longint;cdecl;
  gnutls_hex_decode2 : function(data:Pgnutls_datum_t; result:Pgnutls_datum_t):longint;cdecl;
  gnutls_psk_set_server_dh_params : procedure(res:Tgnutls_psk_server_credentials_t; dh_params:Tgnutls_dh_params_t);cdecl;
  gnutls_psk_set_server_params_function : procedure(res:Tgnutls_psk_server_credentials_t; func: tgnutls_params_function);cdecl;
  gnutls_auth_get_type : function(session:Tgnutls_session_t):Tgnutls_credentials_type_t;cdecl;
  gnutls_auth_server_get_type : function(session:Tgnutls_session_t):Tgnutls_credentials_type_t;cdecl;
  gnutls_auth_client_get_type : function(session:Tgnutls_session_t):Tgnutls_credentials_type_t;cdecl;
  gnutls_dh_set_prime_bits : procedure(session:Tgnutls_session_t; bits:dword);cdecl;
  gnutls_dh_get_secret_bits : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_dh_get_peers_public_bits : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_dh_get_prime_bits : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_dh_get_group : function(session:Tgnutls_session_t; raw_gen:Pgnutls_datum_t; raw_prime:Pgnutls_datum_t):longint;cdecl;
  gnutls_dh_get_pubkey : function(session:Tgnutls_session_t; raw_key:Pgnutls_datum_t):longint;cdecl;
  gnutls_certificate_set_retrieve_function : procedure(cred:Tgnutls_certificate_credentials_t; func:Pgnutls_certificate_retrieve_function);cdecl;
  gnutls_certificate_set_verify_function : procedure(cred:Tgnutls_certificate_credentials_t; func:Pgnutls_certificate_verify_function);cdecl;
  gnutls_certificate_server_set_request : procedure(session:Tgnutls_session_t; req:Tgnutls_certificate_request_t);cdecl;
  gnutls_certificate_get_peers : function(session:Tgnutls_session_t; list_size:Pdword):Pgnutls_datum_t;cdecl;
  gnutls_certificate_get_ours : function(session:Tgnutls_session_t):Pgnutls_datum_t;cdecl;
  gnutls_certificate_get_peers_subkey_id : function(session:Tgnutls_session_t; id:Pgnutls_datum_t):longint;cdecl;
  gnutls_certificate_activation_time_peers : function(session:Tgnutls_session_t):Ttime_t;cdecl;
  gnutls_certificate_expiration_time_peers : function(session:Tgnutls_session_t):Ttime_t;cdecl;
  gnutls_certificate_client_get_request_status : function(session:Tgnutls_session_t):longint;cdecl;
  gnutls_certificate_verify_peers2 : function(session:Tgnutls_session_t; status:Pdword):longint;cdecl;
  gnutls_certificate_verify_peers3 : function(session:Tgnutls_session_t; hostname:Pchar; status:Pdword):longint;cdecl;
  gnutls_certificate_verify_peers : function(session:Tgnutls_session_t; data:Pgnutls_typed_vdata_st; elements:dword; status:Pdword):longint;cdecl;
  gnutls_certificate_verification_status_print : function(status:dword; _type:Tgnutls_certificate_type_t; outa:Pgnutls_datum_t; flags:dword):longint;cdecl;
  gnutls_pem_base64_encode : function(msg:Pchar; data:Pgnutls_datum_t; result:Pchar; result_size:Psize_t):longint;cdecl;
  gnutls_pem_base64_decode : function(header:Pchar; b64_data:Pgnutls_datum_t; result:Pbyte; result_size:Psize_t):longint;cdecl;
  gnutls_pem_base64_encode2 : function(msg:Pchar; data:Pgnutls_datum_t; result:Pgnutls_datum_t):longint;cdecl;
  gnutls_pem_base64_decode2 : function(header:Pchar; b64_data:Pgnutls_datum_t; result:Pgnutls_datum_t):longint;cdecl;
  gnutls_certificate_set_params_function : procedure(res:Tgnutls_certificate_credentials_t; func:Pgnutls_params_function);cdecl;
  gnutls_anon_set_params_function : procedure(res:Tgnutls_anon_server_credentials_t; func:Pgnutls_params_function);cdecl;
  gnutls_psk_set_params_function : procedure(res:Tgnutls_psk_server_credentials_t; func:Pgnutls_params_function);cdecl;
  gnutls_hex2bin : function(hex_data:Pchar; hex_size:Tsize_t; bin_data:pointer; bin_size:Psize_t):longint;cdecl;
  gnutls_tdb_init : function(tdb:Pgnutls_tdb_t):longint;cdecl;
  gnutls_tdb_set_store_func : procedure(tdb:Tgnutls_tdb_t; store:Tgnutls_tdb_store_func);cdecl;
  gnutls_tdb_set_store_commitment_func : procedure(tdb:Tgnutls_tdb_t; cstore:Tgnutls_tdb_store_commitment_func);cdecl;
  gnutls_tdb_set_verify_func : procedure(tdb:Tgnutls_tdb_t; verify:Tgnutls_tdb_verify_func);cdecl;
  gnutls_tdb_deinit : procedure(tdb:Tgnutls_tdb_t);cdecl;
  gnutls_verify_stored_pubkey : function(db_name:Pchar; tdb:Tgnutls_tdb_t; host:Pchar; service:Pchar; cert_type:Tgnutls_certificate_type_t;
      cert:Pgnutls_datum_t; flags:dword):longint;cdecl;
  gnutls_store_commitment : function(db_name:Pchar; tdb:Tgnutls_tdb_t; host:Pchar; service:Pchar; hash_algo:Tgnutls_digest_algorithm_t;
      hash:Pgnutls_datum_t; expiration:Ttime_t; flags:dword):longint;cdecl;
  gnutls_store_pubkey : function(db_name:Pchar; tdb:Tgnutls_tdb_t; host:Pchar; service:Pchar; cert_type:Tgnutls_certificate_type_t;
      cert:Pgnutls_datum_t; expiration:Ttime_t; flags:dword):longint;cdecl;
  gnutls_load_file : function(filename:Pchar; data:Pgnutls_datum_t):longint;cdecl;
  gnutls_url_is_supported : function(url:Pchar):longint;cdecl;
  gnutls_certificate_set_pin_function : procedure(para1:Tgnutls_certificate_credentials_t; fn:Tgnutls_pin_callback_t; userdata:pointer);cdecl;
    gnutls_buffer_append_data : function(para1:Tgnutls_buffer_t; data:pointer; data_size:Tsize_t):longint;cdecl;
  gnutls_ext_set_data : procedure(session:Tgnutls_session_t; _type:dword; para3:Tgnutls_ext_priv_data_t);cdecl;
  gnutls_ext_get_data : function(session:Tgnutls_session_t; _type:dword; para3:Pgnutls_ext_priv_data_t):longint;cdecl;
  gnutls_ext_register : function(name:Pchar; _type:longint; parse_type:Tgnutls_ext_parse_type_t; recv_func:Tgnutls_ext_recv_func; send_func:Tgnutls_ext_send_func;
      deinit_func:Tgnutls_ext_deinit_data_func; pack_func:Tgnutls_ext_pack_func; unpack_func:Tgnutls_ext_unpack_func):longint;cdecl;
  gnutls_supplemental_register : function(name:Pchar; _type:Tgnutls_supplemental_data_format_type_t; supp_recv_func:Tgnutls_supp_recv_func; supp_send_func:Tgnutls_supp_send_func):longint;cdecl;
  gnutls_supplemental_recv : procedure(session:Tgnutls_session_t; do_recv_supplemental:dword);cdecl;
  gnutls_supplemental_send : procedure(session:Tgnutls_session_t; do_send_supplemental:dword);cdecl;
  gnutls_fips140_mode_enabled : function:longint;cdecl;
var
  gnutls_x509_crt_init : function(cert:Pgnutls_x509_crt_t):longint;cdecl;
  gnutls_x509_crt_deinit : procedure(cert:Tgnutls_x509_crt_t);cdecl;
  gnutls_x509_crt_import : function(cert:Tgnutls_x509_crt_t; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_x509_crt_list_import2 : function(certs:PPgnutls_x509_crt_t; size:Pdword; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t; flags:dword):longint;cdecl;
  gnutls_x509_crt_list_import : function(certs:Pgnutls_x509_crt_t; cert_max:Pdword; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t; flags:dword):longint;cdecl;
  gnutls_x509_crt_import_url : function(crt:Tgnutls_x509_crt_t; url:Pchar; flags:dword):longint;cdecl;
  gnutls_x509_crt_export : function(cert:Tgnutls_x509_crt_t; format:Tgnutls_x509_crt_fmt_t; output_data:pointer; output_data_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_export2 : function(cert:Tgnutls_x509_crt_t; format:Tgnutls_x509_crt_fmt_t; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_get_private_key_usage_period : function(cert:Tgnutls_x509_crt_t; activation:Ptime_t; expiration:Ptime_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_issuer_dn : function(cert:Tgnutls_x509_crt_t; buf:Pchar; buf_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_issuer_dn2 : function(cert:Tgnutls_x509_crt_t; dn:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_get_issuer_dn_oid : function(cert:Tgnutls_x509_crt_t; indx:longint; oid:pointer; oid_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_issuer_dn_by_oid : function(cert:Tgnutls_x509_crt_t; oid:Pchar; indx:longint; raw_flag:dword; buf:pointer;
      buf_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_dn : function(cert:Tgnutls_x509_crt_t; buf:Pchar; buf_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_dn2 : function(cert:Tgnutls_x509_crt_t; dn:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_get_dn_oid : function(cert:Tgnutls_x509_crt_t; indx:longint; oid:pointer; oid_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_dn_by_oid : function(cert:Tgnutls_x509_crt_t; oid:Pchar; indx:longint; raw_flag:dword; buf:pointer;
      buf_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_check_hostname : function(cert:Tgnutls_x509_crt_t; hostname:Pchar):longint;cdecl;
  gnutls_x509_crt_check_hostname2 : function(cert:Tgnutls_x509_crt_t; hostname:Pchar; flags:dword):longint;cdecl;
  gnutls_x509_crt_check_email : function(cert:Tgnutls_x509_crt_t; email:Pchar; flags:dword):longint;cdecl;
  gnutls_x509_crt_get_signature_algorithm : function(cert:Tgnutls_x509_crt_t):longint;cdecl;
  gnutls_x509_crt_get_signature : function(cert:Tgnutls_x509_crt_t; sig:Pchar; sizeof_sig:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_version : function(cert:Tgnutls_x509_crt_t):longint;cdecl;
  gnutls_x509_crt_get_key_id : function(crt:Tgnutls_x509_crt_t; flags:dword; output_data:Pbyte; output_data_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_set_private_key_usage_period : function(crt:Tgnutls_x509_crt_t; activation:Ttime_t; expiration:Ttime_t):longint;cdecl;
  gnutls_x509_crt_set_authority_key_id : function(cert:Tgnutls_x509_crt_t; id:pointer; id_size:Tsize_t):longint;cdecl;
  gnutls_x509_crt_get_authority_key_id : function(cert:Tgnutls_x509_crt_t; id:pointer; id_size:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_authority_key_gn_serial : function(cert:Tgnutls_x509_crt_t; seq:dword; alt:pointer; alt_size:Psize_t; alt_type:Pdword;
      serial:pointer; serial_size:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_subject_key_id : function(cert:Tgnutls_x509_crt_t; ret:pointer; ret_size:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_subject_unique_id : function(crt:Tgnutls_x509_crt_t; buf:Pchar; buf_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_issuer_unique_id : function(crt:Tgnutls_x509_crt_t; buf:Pchar; buf_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_set_pin_function : procedure(crt:Tgnutls_x509_crt_t; fn:Tgnutls_pin_callback_t; userdata:pointer);cdecl;
  gnutls_x509_crt_get_authority_info_access : function(crt:Tgnutls_x509_crt_t; seq:dword; what:longint; data:Pgnutls_datum_t; critical:Pdword):longint;cdecl;
  gnutls_x509_name_constraints_check : function(nc:Tgnutls_x509_name_constraints_t; _type:Tgnutls_x509_subject_alt_name_t; name:Pgnutls_datum_t):dword;cdecl;
  gnutls_x509_name_constraints_check_crt : function(nc:Tgnutls_x509_name_constraints_t; _type:Tgnutls_x509_subject_alt_name_t; crt:Tgnutls_x509_crt_t):dword;cdecl;
  gnutls_x509_name_constraints_init : function(nc:Pgnutls_x509_name_constraints_t):longint;cdecl;
  gnutls_x509_name_constraints_deinit : procedure(nc:Tgnutls_x509_name_constraints_t);cdecl;
  gnutls_x509_crt_get_name_constraints : function(crt:Tgnutls_x509_crt_t; nc:Tgnutls_x509_name_constraints_t; flags:dword; critical:Pdword):longint;cdecl;
  gnutls_x509_name_constraints_add_permitted : function(nc:Tgnutls_x509_name_constraints_t; _type:Tgnutls_x509_subject_alt_name_t; name:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_name_constraints_add_excluded : function(nc:Tgnutls_x509_name_constraints_t; _type:Tgnutls_x509_subject_alt_name_t; name:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_set_name_constraints : function(crt:Tgnutls_x509_crt_t; nc:Tgnutls_x509_name_constraints_t; critical:dword):longint;cdecl;
  gnutls_x509_name_constraints_get_permitted : function(nc:Tgnutls_x509_name_constraints_t; idx:dword; _type:Pdword; name:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_name_constraints_get_excluded : function(nc:Tgnutls_x509_name_constraints_t; idx:dword; _type:Pdword; name:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_get_crl_dist_points : function(cert:Tgnutls_x509_crt_t; seq:dword; ret:pointer; ret_size:Psize_t; reason_flags:Pdword;
      critical:Pdword):longint;cdecl;
  gnutls_x509_crt_set_crl_dist_points2 : function(crt:Tgnutls_x509_crt_t; _type:Tgnutls_x509_subject_alt_name_t; data:pointer; data_size:dword; reason_flags:dword):longint;cdecl;
  gnutls_x509_crt_set_crl_dist_points : function(crt:Tgnutls_x509_crt_t; _type:Tgnutls_x509_subject_alt_name_t; data_string:pointer; reason_flags:dword):longint;cdecl;
  gnutls_x509_crt_cpy_crl_dist_points : function(dst:Tgnutls_x509_crt_t; src:Tgnutls_x509_crt_t):longint;cdecl;
  gnutls_x509_crl_sign2 : function(crl:Tgnutls_x509_crl_t; issuer:Tgnutls_x509_crt_t; issuer_key:Tgnutls_x509_privkey_t; dig:Tgnutls_digest_algorithm_t; flags:dword):longint;cdecl;
  gnutls_x509_crt_get_activation_time : function(cert:Tgnutls_x509_crt_t):Ttime_t;cdecl;
  gnutls_x509_crt_get_expiration_time : function(cert:Tgnutls_x509_crt_t):Ttime_t;cdecl;
  gnutls_x509_crt_get_serial : function(cert:Tgnutls_x509_crt_t; result:pointer; result_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_pk_algorithm : function(cert:Tgnutls_x509_crt_t; bits:Pdword):longint;cdecl;
  gnutls_x509_crt_get_pk_rsa_raw : function(crt:Tgnutls_x509_crt_t; m:Pgnutls_datum_t; e:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_get_pk_dsa_raw : function(crt:Tgnutls_x509_crt_t; p:Pgnutls_datum_t; q:Pgnutls_datum_t; g:Pgnutls_datum_t; y:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_get_pk_ecc_raw : function(crt:Tgnutls_x509_crt_t; curve:Pgnutls_ecc_curve_t; x:Pgnutls_datum_t; y:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_get_subject_alt_name : function(cert:Tgnutls_x509_crt_t; seq:dword; san:pointer; san_size:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_subject_alt_name2 : function(cert:Tgnutls_x509_crt_t; seq:dword; san:pointer; san_size:Psize_t; san_type:Pdword;
      critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_subject_alt_othername_oid : function(cert:Tgnutls_x509_crt_t; seq:dword; oid:pointer; oid_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_issuer_alt_name : function(cert:Tgnutls_x509_crt_t; seq:dword; ian:pointer; ian_size:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_issuer_alt_name2 : function(cert:Tgnutls_x509_crt_t; seq:dword; ian:pointer; ian_size:Psize_t; ian_type:Pdword;
      critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_issuer_alt_othername_oid : function(cert:Tgnutls_x509_crt_t; seq:dword; ret:pointer; ret_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_ca_status : function(cert:Tgnutls_x509_crt_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_basic_constraints : function(cert:Tgnutls_x509_crt_t; critical:Pdword; ca:Pdword; pathlen:Plongint):longint;cdecl;
  gnutls_x509_crt_get_key_usage : function(cert:Tgnutls_x509_crt_t; key_usage:Pdword; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_set_key_usage : function(crt:Tgnutls_x509_crt_t; usage:dword):longint;cdecl;
  gnutls_x509_crt_set_authority_info_access : function(crt:Tgnutls_x509_crt_t; what:longint; data:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_get_proxy : function(cert:Tgnutls_x509_crt_t; critical:Pdword; pathlen:Plongint; policyLanguage:PPchar; policy:PPchar;
      sizeof_policy:Psize_t):longint;cdecl;
  gnutls_x509_policy_release : procedure(policy:Pgnutls_x509_policy_st);cdecl;
  gnutls_x509_crt_get_policy : function(crt:Tgnutls_x509_crt_t; indx:longint; policy:Pgnutls_x509_policy_st; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_set_policy : function(crt:Tgnutls_x509_crt_t; policy:Pgnutls_x509_policy_st; critical:dword):longint;cdecl;
  gnutls_x509_dn_oid_known : function(oid:Pchar):longint;cdecl;
  gnutls_x509_dn_oid_name : function(oid:Pchar; flags:dword):Pchar;cdecl;
  gnutls_x509_crt_get_extension_oid : function(cert:Tgnutls_x509_crt_t; indx:longint; oid:pointer; oid_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_extension_by_oid : function(cert:Tgnutls_x509_crt_t; oid:Pchar; indx:longint; buf:pointer; buf_size:Psize_t;
      critical:Pdword):longint;cdecl;
  gnutls_x509_crq_get_signature_algorithm : function(crq:Tgnutls_x509_crq_t):longint;cdecl;
  gnutls_x509_crq_get_extension_by_oid2 : function(crq:Tgnutls_x509_crq_t; oid:Pchar; indx:longint; output:Pgnutls_datum_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_extension_info : function(cert:Tgnutls_x509_crt_t; indx:longint; oid:pointer; oid_size:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_extension_data : function(cert:Tgnutls_x509_crt_t; indx:longint; data:pointer; sizeof_data:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_extension_data2 : function(cert:Tgnutls_x509_crt_t; indx:dword; data:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_set_extension_by_oid : function(crt:Tgnutls_x509_crt_t; oid:Pchar; buf:pointer; sizeof_buf:Tsize_t; critical:dword):longint;cdecl;
  gnutls_x509_crt_set_dn : function(crt:Tgnutls_x509_crt_t; dn:Pchar; err:PPchar):longint;cdecl;
  gnutls_x509_crt_set_dn_by_oid : function(crt:Tgnutls_x509_crt_t; oid:Pchar; raw_flag:dword; name:pointer; sizeof_name:dword):longint;cdecl;
  gnutls_x509_crt_set_issuer_dn_by_oid : function(crt:Tgnutls_x509_crt_t; oid:Pchar; raw_flag:dword; name:pointer; sizeof_name:dword):longint;cdecl;
  gnutls_x509_crt_set_issuer_dn : function(crt:Tgnutls_x509_crt_t; dn:Pchar; err:PPchar):longint;cdecl;
  gnutls_x509_crt_set_version : function(crt:Tgnutls_x509_crt_t; version:dword):longint;cdecl;
  gnutls_x509_crt_set_key : function(crt:Tgnutls_x509_crt_t; key:Tgnutls_x509_privkey_t):longint;cdecl;
  gnutls_x509_crt_set_ca_status : function(crt:Tgnutls_x509_crt_t; ca:dword):longint;cdecl;
  gnutls_x509_crt_set_basic_constraints : function(crt:Tgnutls_x509_crt_t; ca:dword; pathLenConstraint:longint):longint;cdecl;
  gnutls_x509_crt_set_subject_unique_id : function(cert:Tgnutls_x509_crt_t; id:pointer; id_size:Tsize_t):longint;cdecl;
  gnutls_x509_crt_set_issuer_unique_id : function(cert:Tgnutls_x509_crt_t; id:pointer; id_size:Tsize_t):longint;cdecl;
  gnutls_x509_crt_set_subject_alternative_name : function(crt:Tgnutls_x509_crt_t; _type:Tgnutls_x509_subject_alt_name_t; data_string:Pchar):longint;cdecl;
  gnutls_x509_crt_set_subject_alt_name : function(crt:Tgnutls_x509_crt_t; _type:Tgnutls_x509_subject_alt_name_t; data:pointer; data_size:dword; flags:dword):longint;cdecl;
  gnutls_x509_crt_set_issuer_alt_name : function(crt:Tgnutls_x509_crt_t; _type:Tgnutls_x509_subject_alt_name_t; data:pointer; data_size:dword; flags:dword):longint;cdecl;
  gnutls_x509_crt_sign : function(crt:Tgnutls_x509_crt_t; issuer:Tgnutls_x509_crt_t; issuer_key:Tgnutls_x509_privkey_t):longint;cdecl;
  gnutls_x509_crt_sign2 : function(crt:Tgnutls_x509_crt_t; issuer:Tgnutls_x509_crt_t; issuer_key:Tgnutls_x509_privkey_t; dig:Tgnutls_digest_algorithm_t; flags:dword):longint;cdecl;
  gnutls_x509_crt_set_activation_time : function(cert:Tgnutls_x509_crt_t; act_time:Ttime_t):longint;cdecl;
  gnutls_x509_crt_set_expiration_time : function(cert:Tgnutls_x509_crt_t; exp_time:Ttime_t):longint;cdecl;
  gnutls_x509_crt_set_serial : function(cert:Tgnutls_x509_crt_t; serial:pointer; serial_size:Tsize_t):longint;cdecl;
  gnutls_x509_crt_set_subject_key_id : function(cert:Tgnutls_x509_crt_t; id:pointer; id_size:Tsize_t):longint;cdecl;
  gnutls_x509_crt_set_proxy_dn : function(crt:Tgnutls_x509_crt_t; eecrt:Tgnutls_x509_crt_t; raw_flag:dword; name:pointer; sizeof_name:dword):longint;cdecl;
  gnutls_x509_crt_set_proxy : function(crt:Tgnutls_x509_crt_t; pathLenConstraint:longint; policyLanguage:Pchar; policy:Pchar; sizeof_policy:Tsize_t):longint;cdecl;
  gnutls_x509_crt_print : function(cert:Tgnutls_x509_crt_t; format:Tgnutls_certificate_print_formats_t; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crl_print : function(crl:Tgnutls_x509_crl_t; format:Tgnutls_certificate_print_formats_t; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_get_raw_issuer_dn : function(cert:Tgnutls_x509_crt_t; start:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_get_raw_dn : function(cert:Tgnutls_x509_crt_t; start:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_rdn_get : function(idn:Pgnutls_datum_t; buf:Pchar; sizeof_buf:Psize_t):longint;cdecl;
  gnutls_x509_rdn_get_oid : function(idn:Pgnutls_datum_t; indx:longint; buf:pointer; sizeof_buf:Psize_t):longint;cdecl;
  gnutls_x509_rdn_get_by_oid : function(idn:Pgnutls_datum_t; oid:Pchar; indx:longint; raw_flag:dword; buf:pointer;
      sizeof_buf:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_subject : function(cert:Tgnutls_x509_crt_t; dn:Pgnutls_x509_dn_t):longint;cdecl;
  gnutls_x509_crt_get_issuer : function(cert:Tgnutls_x509_crt_t; dn:Pgnutls_x509_dn_t):longint;cdecl;
  gnutls_x509_dn_get_rdn_ava : function(dn:Tgnutls_x509_dn_t; irdn:longint; iava:longint; ava:Pgnutls_x509_ava_st):longint;cdecl;
  gnutls_x509_dn_get_str : function(dn:Tgnutls_x509_dn_t; str:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_dn_init : function(dn:Pgnutls_x509_dn_t):longint;cdecl;
  gnutls_x509_dn_import : function(dn:Tgnutls_x509_dn_t; data:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_dn_export : function(dn:Tgnutls_x509_dn_t; format:Tgnutls_x509_crt_fmt_t; output_data:pointer; output_data_size:Psize_t):longint;cdecl;
  gnutls_x509_dn_export2 : function(dn:Tgnutls_x509_dn_t; format:Tgnutls_x509_crt_fmt_t; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_dn_deinit : procedure(dn:Tgnutls_x509_dn_t);cdecl;
  gnutls_x509_crl_init : function(crl:Pgnutls_x509_crl_t):longint;cdecl;
  gnutls_x509_crl_deinit : procedure(crl:Tgnutls_x509_crl_t);cdecl;
  gnutls_x509_crl_import : function(crl:Tgnutls_x509_crl_t; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_x509_crl_export : function(crl:Tgnutls_x509_crl_t; format:Tgnutls_x509_crt_fmt_t; output_data:pointer; output_data_size:Psize_t):longint;cdecl;
  gnutls_x509_crl_export2 : function(crl:Tgnutls_x509_crl_t; format:Tgnutls_x509_crt_fmt_t; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crl_get_raw_issuer_dn : function(crl:Tgnutls_x509_crl_t; dn:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crl_get_issuer_dn : function(crl:Tgnutls_x509_crl_t; buf:Pchar; sizeof_buf:Psize_t):longint;cdecl;
  gnutls_x509_crl_get_issuer_dn2 : function(crl:Tgnutls_x509_crl_t; dn:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crl_get_issuer_dn_by_oid : function(crl:Tgnutls_x509_crl_t; oid:Pchar; indx:longint; raw_flag:dword; buf:pointer;
      sizeof_buf:Psize_t):longint;cdecl;
  gnutls_x509_crl_get_dn_oid : function(crl:Tgnutls_x509_crl_t; indx:longint; oid:pointer; sizeof_oid:Psize_t):longint;cdecl;
  gnutls_x509_crl_get_signature_algorithm : function(crl:Tgnutls_x509_crl_t):longint;cdecl;
  gnutls_x509_crl_get_signature : function(crl:Tgnutls_x509_crl_t; sig:Pchar; sizeof_sig:Psize_t):longint;cdecl;
  gnutls_x509_crl_get_version : function(crl:Tgnutls_x509_crl_t):longint;cdecl;
  gnutls_x509_crl_get_this_update : function(crl:Tgnutls_x509_crl_t):Ttime_t;cdecl;
  gnutls_x509_crl_get_next_update : function(crl:Tgnutls_x509_crl_t):Ttime_t;cdecl;
  gnutls_x509_crl_get_crt_count : function(crl:Tgnutls_x509_crl_t):longint;cdecl;
  gnutls_x509_crl_get_crt_serial : function(crl:Tgnutls_x509_crl_t; indx:longint; serial:Pbyte; serial_size:Psize_t; t:Ptime_t):longint;cdecl;
  gnutls_x509_crl_iter_crt_serial : function(crl:Tgnutls_x509_crl_t; para2:Pgnutls_x509_crl_iter_t; serial:Pbyte; serial_size:Psize_t; t:Ptime_t):longint;cdecl;
  gnutls_x509_crl_iter_deinit : procedure(para1:Tgnutls_x509_crl_iter_t);cdecl;
  gnutls_x509_crl_check_issuer : function(crl:Tgnutls_x509_crl_t; issuer:Tgnutls_x509_crt_t):longint;cdecl;
  gnutls_x509_crl_list_import2 : function(crls:PPgnutls_x509_crl_t; size:Pdword; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t; flags:dword):longint;cdecl;
  gnutls_x509_crl_list_import : function(crls:Pgnutls_x509_crl_t; crl_max:Pdword; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t; flags:dword):longint;cdecl;
  gnutls_x509_crl_set_version : function(crl:Tgnutls_x509_crl_t; version:dword):longint;cdecl;
  gnutls_x509_crl_set_this_update : function(crl:Tgnutls_x509_crl_t; act_time:Ttime_t):longint;cdecl;
  gnutls_x509_crl_set_next_update : function(crl:Tgnutls_x509_crl_t; exp_time:Ttime_t):longint;cdecl;
  gnutls_x509_crl_set_crt_serial : function(crl:Tgnutls_x509_crl_t; serial:pointer; serial_size:Tsize_t; revocation_time:Ttime_t):longint;cdecl;
  gnutls_x509_crl_set_crt : function(crl:Tgnutls_x509_crl_t; crt:Tgnutls_x509_crt_t; revocation_time:Ttime_t):longint;cdecl;
  gnutls_x509_crl_get_authority_key_id : function(crl:Tgnutls_x509_crl_t; id:pointer; id_size:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crl_get_authority_key_gn_serial : function(crl:Tgnutls_x509_crl_t; seq:dword; alt:pointer; alt_size:Psize_t; alt_type:Pdword;
      serial:pointer; serial_size:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crl_get_number : function(crl:Tgnutls_x509_crl_t; ret:pointer; ret_size:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crl_get_extension_oid : function(crl:Tgnutls_x509_crl_t; indx:longint; oid:pointer; sizeof_oid:Psize_t):longint;cdecl;
  gnutls_x509_crl_get_extension_info : function(crl:Tgnutls_x509_crl_t; indx:longint; oid:pointer; sizeof_oid:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crl_get_extension_data : function(crl:Tgnutls_x509_crl_t; indx:longint; data:pointer; sizeof_data:Psize_t):longint;cdecl;
  gnutls_x509_crl_get_extension_data2 : function(crl:Tgnutls_x509_crl_t; indx:dword; data:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crl_set_authority_key_id : function(crl:Tgnutls_x509_crl_t; id:pointer; id_size:Tsize_t):longint;cdecl;
  gnutls_x509_crl_set_number : function(crl:Tgnutls_x509_crl_t; nr:pointer; nr_size:Tsize_t):longint;cdecl;
  gnutls_x509_crt_check_issuer : function(cert:Tgnutls_x509_crt_t; issuer:Tgnutls_x509_crt_t):longint;cdecl;
  gnutls_x509_crt_list_verify : function(cert_list:Pgnutls_x509_crt_t; cert_list_length:longint; CA_list:Pgnutls_x509_crt_t; CA_list_length:longint; CRL_list:Pgnutls_x509_crl_t;
      CRL_list_length:longint; flags:dword; verify:Pdword):longint;cdecl;
  gnutls_x509_crt_verify : function(cert:Tgnutls_x509_crt_t; CA_list:Pgnutls_x509_crt_t; CA_list_length:longint; flags:dword; verify:Pdword):longint;cdecl;
  gnutls_x509_crl_verify : function(crl:Tgnutls_x509_crl_t; CA_list:Pgnutls_x509_crt_t; CA_list_length:longint; flags:dword; verify:Pdword):longint;cdecl;
  gnutls_x509_crt_verify_data2 : function(crt:Tgnutls_x509_crt_t; algo:Tgnutls_sign_algorithm_t; flags:dword; data:Pgnutls_datum_t; signature:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_check_revocation : function(cert:Tgnutls_x509_crt_t; crl_list:Pgnutls_x509_crl_t; crl_list_length:longint):longint;cdecl;
  gnutls_x509_crt_get_fingerprint : function(cert:Tgnutls_x509_crt_t; algo:Tgnutls_digest_algorithm_t; buf:pointer; buf_size:Psize_t):longint;cdecl;
  gnutls_x509_crt_get_key_purpose_oid : function(cert:Tgnutls_x509_crt_t; indx:longint; oid:pointer; oid_size:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crt_set_key_purpose_oid : function(cert:Tgnutls_x509_crt_t; oid:pointer; critical:dword):longint;cdecl;
  gnutls_pkcs_schema_get_name : function(schema:dword):Pchar;cdecl;
  gnutls_pkcs_schema_get_oid : function(schema:dword):Pchar;cdecl;
  gnutls_x509_privkey_init : function(key:Pgnutls_x509_privkey_t):longint;cdecl;
  gnutls_x509_privkey_deinit : procedure(key:Tgnutls_x509_privkey_t);cdecl;
  gnutls_x509_privkey_sec_param : function(key:Tgnutls_x509_privkey_t):Tgnutls_sec_param_t;cdecl;
  gnutls_x509_privkey_set_pin_function : procedure(key:Tgnutls_x509_privkey_t; fn:Tgnutls_pin_callback_t; userdata:pointer);cdecl;
  gnutls_x509_privkey_cpy : function(dst:Tgnutls_x509_privkey_t; src:Tgnutls_x509_privkey_t):longint;cdecl;
  gnutls_x509_privkey_import : function(key:Tgnutls_x509_privkey_t; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_x509_privkey_import_pkcs8 : function(key:Tgnutls_x509_privkey_t; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t; password:Pchar; flags:dword):longint;cdecl;
  gnutls_x509_privkey_import_openssl : function(key:Tgnutls_x509_privkey_t; data:Pgnutls_datum_t; password:Pchar):longint;cdecl;
  gnutls_pkcs8_info : function(data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t; schema:Pdword; cipher:Pdword; salt:pointer;
      salt_size:Pdword; iter_count:Pdword; oid:PPchar):longint;cdecl;
  gnutls_x509_privkey_import2 : function(key:Tgnutls_x509_privkey_t; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t; password:Pchar; flags:dword):longint;cdecl;
  gnutls_x509_privkey_import_rsa_raw : function(key:Tgnutls_x509_privkey_t; m:Pgnutls_datum_t; e:Pgnutls_datum_t; d:Pgnutls_datum_t; p:Pgnutls_datum_t;
      q:Pgnutls_datum_t; u:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_privkey_import_rsa_raw2 : function(key:Tgnutls_x509_privkey_t; m:Pgnutls_datum_t; e:Pgnutls_datum_t; d:Pgnutls_datum_t; p:Pgnutls_datum_t;
      q:Pgnutls_datum_t; u:Pgnutls_datum_t; e1:Pgnutls_datum_t; e2:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_privkey_import_ecc_raw : function(key:Tgnutls_x509_privkey_t; curve:Tgnutls_ecc_curve_t; x:Pgnutls_datum_t; y:Pgnutls_datum_t; k:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_privkey_fix : function(key:Tgnutls_x509_privkey_t):longint;cdecl;
  gnutls_x509_privkey_export_dsa_raw : function(key:Tgnutls_x509_privkey_t; p:Pgnutls_datum_t; q:Pgnutls_datum_t; g:Pgnutls_datum_t; y:Pgnutls_datum_t;
      x:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_privkey_import_dsa_raw : function(key:Tgnutls_x509_privkey_t; p:Pgnutls_datum_t; q:Pgnutls_datum_t; g:Pgnutls_datum_t; y:Pgnutls_datum_t;
      x:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_privkey_get_pk_algorithm : function(key:Tgnutls_x509_privkey_t):longint;cdecl;
  gnutls_x509_privkey_get_pk_algorithm2 : function(key:Tgnutls_x509_privkey_t; bits:Pdword):longint;cdecl;
  gnutls_x509_privkey_get_key_id : function(key:Tgnutls_x509_privkey_t; flags:dword; output_data:Pbyte; output_data_size:Psize_t):longint;cdecl;
  gnutls_x509_privkey_generate : function(key:Tgnutls_x509_privkey_t; algo:Tgnutls_pk_algorithm_t; bits:dword; flags:dword):longint;cdecl;
  gnutls_x509_privkey_verify_params : function(key:Tgnutls_x509_privkey_t):longint;cdecl;
  gnutls_x509_privkey_export : function(key:Tgnutls_x509_privkey_t; format:Tgnutls_x509_crt_fmt_t; output_data:pointer; output_data_size:Psize_t):longint;cdecl;
  gnutls_x509_privkey_export2 : function(key:Tgnutls_x509_privkey_t; format:Tgnutls_x509_crt_fmt_t; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_privkey_export_pkcs8 : function(key:Tgnutls_x509_privkey_t; format:Tgnutls_x509_crt_fmt_t; password:Pchar; flags:dword; output_data:pointer;
      output_data_size:Psize_t):longint;cdecl;
  gnutls_x509_privkey_export2_pkcs8 : function(key:Tgnutls_x509_privkey_t; format:Tgnutls_x509_crt_fmt_t; password:Pchar; flags:dword; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_privkey_export_rsa_raw2 : function(key:Tgnutls_x509_privkey_t; m:Pgnutls_datum_t; e:Pgnutls_datum_t; d:Pgnutls_datum_t; p:Pgnutls_datum_t;
      q:Pgnutls_datum_t; u:Pgnutls_datum_t; e1:Pgnutls_datum_t; e2:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_privkey_export_rsa_raw : function(key:Tgnutls_x509_privkey_t; m:Pgnutls_datum_t; e:Pgnutls_datum_t; d:Pgnutls_datum_t; p:Pgnutls_datum_t;
      q:Pgnutls_datum_t; u:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_privkey_export_ecc_raw : function(key:Tgnutls_x509_privkey_t; curve:Pgnutls_ecc_curve_t; x:Pgnutls_datum_t; y:Pgnutls_datum_t; k:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_privkey_sign_data : function(key:Tgnutls_x509_privkey_t; digest:Tgnutls_digest_algorithm_t; flags:dword; data:Pgnutls_datum_t; signature:pointer;
      signature_size:Psize_t):longint;cdecl;
  gnutls_x509_crq_sign2 : function(crq:Tgnutls_x509_crq_t; key:Tgnutls_x509_privkey_t; dig:Tgnutls_digest_algorithm_t; flags:dword):longint;cdecl;
  gnutls_x509_crq_print : function(crq:Tgnutls_x509_crq_t; format:Tgnutls_certificate_print_formats_t; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crq_verify : function(crq:Tgnutls_x509_crq_t; flags:dword):longint;cdecl;
  gnutls_x509_crq_init : function(crq:Pgnutls_x509_crq_t):longint;cdecl;
  gnutls_x509_crq_deinit : procedure(crq:Tgnutls_x509_crq_t);cdecl;
  gnutls_x509_crq_import : function(crq:Tgnutls_x509_crq_t; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_x509_crq_get_private_key_usage_period : function(cert:Tgnutls_x509_crq_t; activation:Ptime_t; expiration:Ptime_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crq_get_dn : function(crq:Tgnutls_x509_crq_t; buf:Pchar; sizeof_buf:Psize_t):longint;cdecl;
  gnutls_x509_crq_get_dn2 : function(crq:Tgnutls_x509_crq_t; dn:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crq_get_dn_oid : function(crq:Tgnutls_x509_crq_t; indx:longint; oid:pointer; sizeof_oid:Psize_t):longint;cdecl;
  gnutls_x509_crq_get_dn_by_oid : function(crq:Tgnutls_x509_crq_t; oid:Pchar; indx:longint; raw_flag:dword; buf:pointer;
      sizeof_buf:Psize_t):longint;cdecl;
  gnutls_x509_crq_set_dn : function(crq:Tgnutls_x509_crq_t; dn:Pchar; err:PPchar):longint;cdecl;
  gnutls_x509_crq_set_dn_by_oid : function(crq:Tgnutls_x509_crq_t; oid:Pchar; raw_flag:dword; data:pointer; sizeof_data:dword):longint;cdecl;
  gnutls_x509_crq_set_version : function(crq:Tgnutls_x509_crq_t; version:dword):longint;cdecl;
  gnutls_x509_crq_get_version : function(crq:Tgnutls_x509_crq_t):longint;cdecl;
  gnutls_x509_crq_set_key : function(crq:Tgnutls_x509_crq_t; key:Tgnutls_x509_privkey_t):longint;cdecl;
  gnutls_x509_crq_set_challenge_password : function(crq:Tgnutls_x509_crq_t; pass:Pchar):longint;cdecl;
  gnutls_x509_crq_get_challenge_password : function(crq:Tgnutls_x509_crq_t; pass:Pchar; sizeof_pass:Psize_t):longint;cdecl;
  gnutls_x509_crq_set_attribute_by_oid : function(crq:Tgnutls_x509_crq_t; oid:Pchar; buf:pointer; sizeof_buf:Tsize_t):longint;cdecl;
  gnutls_x509_crq_get_attribute_by_oid : function(crq:Tgnutls_x509_crq_t; oid:Pchar; indx:longint; buf:pointer; sizeof_buf:Psize_t):longint;cdecl;
  gnutls_x509_crq_export : function(crq:Tgnutls_x509_crq_t; format:Tgnutls_x509_crt_fmt_t; output_data:pointer; output_data_size:Psize_t):longint;cdecl;
  gnutls_x509_crq_export2 : function(crq:Tgnutls_x509_crq_t; format:Tgnutls_x509_crt_fmt_t; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crt_set_crq : function(crt:Tgnutls_x509_crt_t; crq:Tgnutls_x509_crq_t):longint;cdecl;
  gnutls_x509_crt_set_crq_extensions : function(crt:Tgnutls_x509_crt_t; crq:Tgnutls_x509_crq_t):longint;cdecl;
  gnutls_x509_crq_set_private_key_usage_period : function(crq:Tgnutls_x509_crq_t; activation:Ttime_t; expiration:Ttime_t):longint;cdecl;
  gnutls_x509_crq_set_key_rsa_raw : function(crq:Tgnutls_x509_crq_t; m:Pgnutls_datum_t; e:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crq_set_subject_alt_name : function(crq:Tgnutls_x509_crq_t; nt:Tgnutls_x509_subject_alt_name_t; data:pointer; data_size:dword; flags:dword):longint;cdecl;
  gnutls_x509_crq_set_key_usage : function(crq:Tgnutls_x509_crq_t; usage:dword):longint;cdecl;
  gnutls_x509_crq_set_basic_constraints : function(crq:Tgnutls_x509_crq_t; ca:dword; pathLenConstraint:longint):longint;cdecl;
  gnutls_x509_crq_set_key_purpose_oid : function(crq:Tgnutls_x509_crq_t; oid:pointer; critical:dword):longint;cdecl;
  gnutls_x509_crq_get_key_purpose_oid : function(crq:Tgnutls_x509_crq_t; indx:longint; oid:pointer; sizeof_oid:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crq_get_extension_data : function(crq:Tgnutls_x509_crq_t; indx:longint; data:pointer; sizeof_data:Psize_t):longint;cdecl;
  gnutls_x509_crq_get_extension_data2 : function(crq:Tgnutls_x509_crq_t; indx:dword; data:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crq_get_extension_info : function(crq:Tgnutls_x509_crq_t; indx:longint; oid:pointer; sizeof_oid:Psize_t; critical:Pdword):longint;cdecl;
  gnutls_x509_crq_get_attribute_data : function(crq:Tgnutls_x509_crq_t; indx:longint; data:pointer; sizeof_data:Psize_t):longint;cdecl;
  gnutls_x509_crq_get_attribute_info : function(crq:Tgnutls_x509_crq_t; indx:longint; oid:pointer; sizeof_oid:Psize_t):longint;cdecl;
  gnutls_x509_crq_get_pk_algorithm : function(crq:Tgnutls_x509_crq_t; bits:Pdword):longint;cdecl;
  gnutls_x509_crq_get_key_id : function(crq:Tgnutls_x509_crq_t; flags:dword; output_data:Pbyte; output_data_size:Psize_t):longint;cdecl;
  gnutls_x509_crq_get_key_rsa_raw : function(crq:Tgnutls_x509_crq_t; m:Pgnutls_datum_t; e:Pgnutls_datum_t):longint;cdecl;
  gnutls_x509_crq_get_key_usage : function(crq:Tgnutls_x509_crq_t; key_usage:Pdword; critical:Pdword):longint;cdecl;
  gnutls_x509_crq_get_basic_constraints : function(crq:Tgnutls_x509_crq_t; critical:Pdword; ca:Pdword; pathlen:Plongint):longint;cdecl;
  gnutls_x509_crq_get_subject_alt_name : function(crq:Tgnutls_x509_crq_t; seq:dword; ret:pointer; ret_size:Psize_t; ret_type:Pdword;
      critical:Pdword):longint;cdecl;
  gnutls_x509_crq_get_subject_alt_othername_oid : function(crq:Tgnutls_x509_crq_t; seq:dword; ret:pointer; ret_size:Psize_t):longint;cdecl;
  gnutls_x509_crq_get_extension_by_oid : function(crq:Tgnutls_x509_crq_t; oid:Pchar; indx:longint; buf:pointer; sizeof_buf:Psize_t;
      critical:Pdword):longint;cdecl;
  gnutls_x509_crt_get_extension_by_oid2 : function(cert:Tgnutls_x509_crt_t; oid:Pchar; indx:longint; output:Pgnutls_datum_t; critical:Pdword):longint;cdecl;
  gnutls_x509_trust_list_init : function(list:Pgnutls_x509_trust_list_t; size:dword):longint;cdecl;
  gnutls_x509_trust_list_deinit : procedure(list:Tgnutls_x509_trust_list_t; all:dword);cdecl;
  gnutls_x509_trust_list_get_issuer : function(list:Tgnutls_x509_trust_list_t; cert:Tgnutls_x509_crt_t; issuer:Pgnutls_x509_crt_t; flags:dword):longint;cdecl;
  gnutls_x509_trust_list_get_issuer_by_dn : function(list:Tgnutls_x509_trust_list_t; dn:Pgnutls_datum_t; issuer:Pgnutls_x509_crt_t; flags:dword):longint;cdecl;
  gnutls_x509_trust_list_get_issuer_by_subject_key_id : function(list:Tgnutls_x509_trust_list_t; dn:Pgnutls_datum_t; spki:Pgnutls_datum_t; issuer:Pgnutls_x509_crt_t; flags:dword):longint;cdecl;
  gnutls_x509_trust_list_add_cas : function(list:Tgnutls_x509_trust_list_t; clist:Pgnutls_x509_crt_t; clist_size:dword; flags:dword):longint;cdecl;
  gnutls_x509_trust_list_remove_cas : function(list:Tgnutls_x509_trust_list_t; clist:Pgnutls_x509_crt_t; clist_size:longint):longint;cdecl;
  gnutls_x509_trust_list_add_named_crt : function(list:Tgnutls_x509_trust_list_t; cert:Tgnutls_x509_crt_t; name:pointer; name_size:Tsize_t; flags:dword):longint;cdecl;
  gnutls_x509_trust_list_add_crls : function(list:Tgnutls_x509_trust_list_t; crl_list:Pgnutls_x509_crl_t; crl_size:longint; flags:dword; verification_flags:dword):longint;cdecl;
  gnutls_x509_trust_list_iter_get_ca : function(list:Tgnutls_x509_trust_list_t; iter:Pgnutls_x509_trust_list_iter_t; crt:Pgnutls_x509_crt_t):longint;cdecl;
  gnutls_x509_trust_list_iter_deinit : procedure(iter:Tgnutls_x509_trust_list_iter_t);cdecl;
    gnutls_x509_trust_list_verify_named_crt : function(list:Tgnutls_x509_trust_list_t; cert:Tgnutls_x509_crt_t; name:pointer; name_size:Tsize_t; flags:dword;
      verify:Pdword; func:Tgnutls_verify_output_function):longint;cdecl;
  gnutls_x509_trust_list_verify_crt2 : function(list:Tgnutls_x509_trust_list_t; cert_list:Pgnutls_x509_crt_t; cert_list_size:dword; data:Pgnutls_typed_vdata_st; elements:dword;
      flags:dword; voutput:Pdword; func:Tgnutls_verify_output_function):longint;cdecl;
  gnutls_x509_trust_list_verify_crt : function(list:Tgnutls_x509_trust_list_t; cert_list:Pgnutls_x509_crt_t; cert_list_size:dword; flags:dword; verify:Pdword;
      func:Tgnutls_verify_output_function):longint;cdecl;
  gnutls_x509_trust_list_add_trust_mem : function(list:Tgnutls_x509_trust_list_t; cas:Pgnutls_datum_t; crls:Pgnutls_datum_t; _type:Tgnutls_x509_crt_fmt_t; tl_flags:dword;
      tl_vflags:dword):longint;cdecl;
  gnutls_x509_trust_list_add_trust_file : function(list:Tgnutls_x509_trust_list_t; ca_file:Pchar; crl_file:Pchar; _type:Tgnutls_x509_crt_fmt_t; tl_flags:dword;
      tl_vflags:dword):longint;cdecl;
  gnutls_x509_trust_list_add_trust_dir : function(list:Tgnutls_x509_trust_list_t; ca_dir:Pchar; crl_dir:Pchar; _type:Tgnutls_x509_crt_fmt_t; tl_flags:dword;
      tl_vflags:dword):longint;cdecl;
  gnutls_x509_trust_list_remove_trust_file : function(list:Tgnutls_x509_trust_list_t; ca_file:Pchar; _type:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_x509_trust_list_remove_trust_mem : function(list:Tgnutls_x509_trust_list_t; cas:Pgnutls_datum_t; _type:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_x509_trust_list_add_system_trust : function(list:Tgnutls_x509_trust_list_t; tl_flags:dword; tl_vflags:dword):longint;cdecl;
  gnutls_certificate_set_trust_list : procedure(res:Tgnutls_certificate_credentials_t; tlist:Tgnutls_x509_trust_list_t; flags:dword);cdecl;
  gnutls_certificate_get_trust_list : procedure(res:Tgnutls_certificate_credentials_t; tlist:Pgnutls_x509_trust_list_t);cdecl;
  gnutls_x509_ext_deinit : procedure(ext:Pgnutls_x509_ext_st);cdecl;
  gnutls_x509_ext_print : function(exts:Pgnutls_x509_ext_st; exts_size:dword; format:Tgnutls_certificate_print_formats_t; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_pkcs7_init : function(pkcs7:Pgnutls_pkcs7_t):longint;cdecl;
  gnutls_pkcs7_deinit : procedure(pkcs7:Tgnutls_pkcs7_t);cdecl;
  gnutls_pkcs7_import : function(pkcs7:Tgnutls_pkcs7_t; data:Pgnutls_datum_t; format:Tgnutls_x509_crt_fmt_t):longint;cdecl;
  gnutls_pkcs7_export : function(pkcs7:Tgnutls_pkcs7_t; format:Tgnutls_x509_crt_fmt_t; output_data:pointer; output_data_size:Psize_t):longint;cdecl;
  gnutls_pkcs7_export2 : function(pkcs7:Tgnutls_pkcs7_t; format:Tgnutls_x509_crt_fmt_t; outd:Pgnutls_datum_t):longint;cdecl;
  gnutls_pkcs7_get_signature_count : function(pkcs7:Tgnutls_pkcs7_t):longint;cdecl;
  gnutls_pkcs7_get_embedded_data : function(pkcs7:Tgnutls_pkcs7_t; idx:dword; data:Pgnutls_datum_t):longint;cdecl;
  gnutls_pkcs7_get_crt_count : function(pkcs7:Tgnutls_pkcs7_t):longint;cdecl;
  gnutls_pkcs7_get_crt_raw : function(pkcs7:Tgnutls_pkcs7_t; indx:longint; certificate:pointer; certificate_size:Psize_t):longint;cdecl;
  gnutls_pkcs7_set_crt_raw : function(pkcs7:Tgnutls_pkcs7_t; crt:Pgnutls_datum_t):longint;cdecl;
  gnutls_pkcs7_set_crt : function(pkcs7:Tgnutls_pkcs7_t; crt:Tgnutls_x509_crt_t):longint;cdecl;
  gnutls_pkcs7_delete_crt : function(pkcs7:Tgnutls_pkcs7_t; indx:longint):longint;cdecl;
  gnutls_pkcs7_get_crl_raw : function(pkcs7:Tgnutls_pkcs7_t; indx:longint; crl:pointer; crl_size:Psize_t):longint;cdecl;
  gnutls_pkcs7_get_crl_count : function(pkcs7:Tgnutls_pkcs7_t):longint;cdecl;
  gnutls_pkcs7_set_crl_raw : function(pkcs7:Tgnutls_pkcs7_t; crl:Pgnutls_datum_t):longint;cdecl;
  gnutls_pkcs7_set_crl : function(pkcs7:Tgnutls_pkcs7_t; crl:Tgnutls_x509_crl_t):longint;cdecl;
  gnutls_pkcs7_delete_crl : function(pkcs7:Tgnutls_pkcs7_t; indx:longint):longint;cdecl;
  gnutls_pkcs7_signature_info_deinit : procedure(info:Pgnutls_pkcs7_signature_info_st);cdecl;
  gnutls_pkcs7_get_signature_info : function(pkcs7:Tgnutls_pkcs7_t; idx:dword; info:Pgnutls_pkcs7_signature_info_st):longint;cdecl;
  gnutls_pkcs7_verify_direct : function(pkcs7:Tgnutls_pkcs7_t; signer:Tgnutls_x509_crt_t; idx:dword; data:Pgnutls_datum_t; flags:dword):longint;cdecl;
  gnutls_pkcs7_verify : function(pkcs7:Tgnutls_pkcs7_t; tl:Tgnutls_x509_trust_list_t; vdata:Pgnutls_typed_vdata_st; vdata_size:dword; idx:dword;
      data:Pgnutls_datum_t; flags:dword):longint;cdecl;
  gnutls_pkcs7_add_attr : function(list:Pgnutls_pkcs7_attrs_t; oid:Pchar; data:Pgnutls_datum_t; flags:dword):longint;cdecl;
  gnutls_pkcs7_attrs_deinit : procedure(list:Tgnutls_pkcs7_attrs_t);cdecl;
  gnutls_pkcs7_get_attr : function(list:Tgnutls_pkcs7_attrs_t; idx:dword; oid:PPchar; data:Pgnutls_datum_t; flags:dword):longint;cdecl;
  gnutls_pkcs7_sign : function(pkcs7:Tgnutls_pkcs7_t; signer:Tgnutls_x509_crt_t; signer_key:Tgnutls_privkey_t; data:Pgnutls_datum_t; signed_attrs:Tgnutls_pkcs7_attrs_t;
      unsigned_attrs:Tgnutls_pkcs7_attrs_t; dig:Tgnutls_digest_algorithm_t; flags:dword):longint;cdecl;
  gnutls_pkcs7_get_crt_raw2 : function(pkcs7:Tgnutls_pkcs7_t; indx:longint; cert:Pgnutls_datum_t):longint;cdecl;
  gnutls_pkcs7_get_crl_raw2 : function(pkcs7:Tgnutls_pkcs7_t; indx:longint; crl:Pgnutls_datum_t):longint;cdecl;
  gnutls_pkcs7_print : function(pkcs7:Tgnutls_pkcs7_t; format:Tgnutls_certificate_print_formats_t; outd:Pgnutls_datum_t):longint;cdecl;



function GNUTLS_X509_NO_WELL_DEFINED_EXPIRATION : Ttime_t;
function gnutls_x509_crl_get_certificate_count(crl:Tgnutls_x509_crl_t)  : cint;
function gnutls_x509_crl_get_certificate (crl:Tgnutls_x509_crl_t; indx:longint; serial:Pbyte; serial_size:Psize_t; t:Ptime_t):longint;


procedure gnutls_transport_set_int(s : tgnutls_session_t; r : cint);

procedure LoadGnuTLS(alib : String = '');
procedure FreeGnuTLS;
Function GnuTLSloaded : Boolean;


implementation

uses
  SysUtils, dynlibs;

{ was #define dname def_expr }
function GNUTLS_X509_NO_WELL_DEFINED_EXPIRATION : Ttime_t;
  begin
    GNUTLS_X509_NO_WELL_DEFINED_EXPIRATION:=Ttime_t(4294197631);
  end;

function gnutls_x509_crl_get_certificate (crl:Tgnutls_x509_crl_t; indx:longint; serial:Pbyte; serial_size:Psize_t; t:Ptime_t):longint;

begin
  Result:=gnutls_x509_crl_get_crt_serial(crl,indx,serial,serial_size,t);
end;

function gnutls_x509_crl_get_certificate_count(crl:Tgnutls_x509_crl_t)  : cint;

begin
  Result:=gnutls_x509_crl_get_crt_count(crl);
end;

procedure gnutls_transport_set_int(s : tgnutls_session_t; r : cint);

begin
  gnutls_transport_set_int2(s,r,r);
end;

var
  hlib : tlibhandle;
  LoadedLibName : String;

Function GnuTLSloaded : Boolean;

begin
  Result:=hlib<>NilHandle;
end;

procedure FreeGnuTLS;

begin
  if Not GnuTLSLoaded then
    Exit;
  FreeLibrary(hlib);
  hlib:=NilHandle;
  LoadedLibName:='';
  gnutls_handshake_description_get_name:=nil;
  gnutls_pk_algorithm_get_name:=nil;
  gnutls_init:=nil;
  gnutls_deinit:=nil;
  gnutls_bye:=nil;
  gnutls_handshake:=nil;
  gnutls_handshake_set_timeout:=nil;
  gnutls_rehandshake:=nil;
  gnutls_alert_get:=nil;
  gnutls_alert_send:=nil;
  gnutls_alert_send_appropriate:=nil;
  gnutls_alert_get_name:=nil;
  gnutls_alert_get_strname:=nil;
  gnutls_pk_bits_to_sec_param:=nil;
  gnutls_sec_param_get_name:=nil;
  gnutls_sec_param_to_pk_bits:=nil;
  gnutls_sec_param_to_symmetric_bits:=nil;
  gnutls_ecc_curve_get_name:=nil;
  gnutls_ecc_curve_get_oid:=nil;
  gnutls_ecc_curve_get_size:=nil;
  gnutls_ecc_curve_get:=nil;
  gnutls_cipher_get:=nil;
  gnutls_kx_get:=nil;
  gnutls_mac_get:=nil;
  gnutls_compression_get:=nil;
  gnutls_certificate_type_get:=nil;
  gnutls_sign_algorithm_get:=nil;
  gnutls_sign_algorithm_get_client:=nil;
  gnutls_sign_algorithm_get_requested:=nil;
  gnutls_cipher_get_name:=nil;
  gnutls_mac_get_name:=nil;
  gnutls_digest_get_name:=nil;
  gnutls_digest_get_oid:=nil;
  gnutls_compression_get_name:=nil;
  gnutls_kx_get_name:=nil;
  gnutls_certificate_type_get_name:=nil;
  gnutls_pk_get_name:=nil;
  gnutls_pk_get_oid:=nil;
  gnutls_sign_get_name:=nil;
  gnutls_sign_get_oid:=nil;
  gnutls_cipher_get_key_size:=nil;
  gnutls_mac_get_key_size:=nil;
  gnutls_sign_is_secure:=nil;
  gnutls_sign_get_hash_algorithm:=nil;
  gnutls_sign_get_pk_algorithm:=nil;
  gnutls_pk_to_sign:=nil;
  gnutls_mac_get_id:=nil;
  gnutls_digest_get_id:=nil;
  gnutls_compression_get_id:=nil;
  gnutls_cipher_get_id:=nil;
  gnutls_kx_get_id:=nil;
  gnutls_protocol_get_id:=nil;
  gnutls_certificate_type_get_id:=nil;
  gnutls_pk_get_id:=nil;
  gnutls_sign_get_id:=nil;
  gnutls_ecc_curve_get_id:=nil;
  gnutls_oid_to_digest:=nil;
  gnutls_oid_to_pk:=nil;
  gnutls_oid_to_sign:=nil;
  gnutls_oid_to_ecc_curve:=nil;
  gnutls_ecc_curve_list:=nil;
  gnutls_cipher_list:=nil;
  gnutls_mac_list:=nil;
  gnutls_digest_list:=nil;
  gnutls_compression_list:=nil;
  gnutls_protocol_list:=nil;
  gnutls_certificate_type_list:=nil;
  gnutls_kx_list:=nil;
  gnutls_pk_list:=nil;
  gnutls_sign_list:=nil;
  gnutls_cipher_suite_info:=nil;
  gnutls_error_is_fatal:=nil;
  gnutls_error_to_alert:=nil;
  gnutls_perror:=nil;
  gnutls_strerror:=nil;
  gnutls_strerror_name:=nil;
  gnutls_handshake_set_private_extensions:=nil;
  gnutls_handshake_set_random:=nil;
  gnutls_handshake_get_last_out:=nil;
  gnutls_handshake_get_last_in:=nil;
  gnutls_heartbeat_ping:=nil;
  gnutls_heartbeat_pong:=nil;
  gnutls_record_set_timeout:=nil;
  gnutls_record_disable_padding:=nil;
  gnutls_record_cork:=nil;
  gnutls_record_uncork:=nil;
  gnutls_record_discard_queued:=nil;
  gnutls_record_get_state:=nil;
  gnutls_record_set_state:=nil;
  gnutls_range_split:=nil;
  gnutls_record_send:=nil;
  gnutls_record_send_range:=nil;
  gnutls_record_recv:=nil;
  gnutls_record_recv_packet:=nil;
  gnutls_packet_get:=nil;
  gnutls_packet_deinit:=nil;
  gnutls_record_recv_seq:=nil;
  gnutls_record_overhead_size:=nil;
  gnutls_est_record_overhead_size:=nil;
  gnutls_session_enable_compatibility_mode:=nil;
  gnutls_record_can_use_length_hiding:=nil;
  gnutls_record_get_direction:=nil;
  gnutls_record_get_max_size:=nil;
  gnutls_record_set_max_size:=nil;
  gnutls_record_check_pending:=nil;
  gnutls_record_check_corked:=nil;
  gnutls_session_force_valid:=nil;
  gnutls_prf:=nil;
  gnutls_prf_rfc5705:=nil;
  gnutls_prf_raw:=nil;
  gnutls_server_name_set:=nil;
  gnutls_server_name_get:=nil;
  gnutls_heartbeat_get_timeout:=nil;
  gnutls_heartbeat_set_timeouts:=nil;
  gnutls_heartbeat_enable:=nil;
  gnutls_heartbeat_allowed:=nil;
  gnutls_safe_renegotiation_status:=nil;
  gnutls_session_ext_master_secret_status:=nil;
  gnutls_session_etm_status:=nil;
  gnutls_supplemental_get_name:=nil;
  gnutls_session_ticket_key_generate:=nil;
  gnutls_session_ticket_enable_client:=nil;
  gnutls_session_ticket_enable_server:=nil;
  gnutls_srtp_set_profile:=nil;
  gnutls_srtp_set_profile_direct:=nil;
  gnutls_srtp_get_selected_profile:=nil;
  gnutls_srtp_get_profile_name:=nil;
  gnutls_srtp_get_profile_id:=nil;
  gnutls_srtp_get_keys:=nil;
  gnutls_srtp_set_mki:=nil;
  gnutls_srtp_get_mki:=nil;
  gnutls_alpn_get_selected_protocol:=nil;
  gnutls_alpn_set_protocols:=nil;
  gnutls_key_generate:=nil;
  gnutls_priority_init:=nil;
  gnutls_priority_deinit:=nil;
  gnutls_priority_get_cipher_suite_index:=nil;
  gnutls_priority_string_list:=nil;
  gnutls_priority_set:=nil;
  gnutls_priority_set_direct:=nil;
  gnutls_priority_certificate_type_list:=nil;
  gnutls_priority_sign_list:=nil;
  gnutls_priority_protocol_list:=nil;
  gnutls_priority_compression_list:=nil;
  gnutls_priority_ecc_curve_list:=nil;
  gnutls_priority_kx_list:=nil;
  gnutls_priority_cipher_list:=nil;
  gnutls_priority_mac_list:=nil;
  gnutls_set_default_priority:=nil;
  gnutls_cipher_suite_get_name:=nil;
  gnutls_protocol_get_version:=nil;
  gnutls_protocol_get_name:=nil;
  gnutls_session_set_data:=nil;
  gnutls_session_get_data:=nil;
  gnutls_session_get_data2:=nil;
  gnutls_session_get_random:=nil;
  gnutls_session_get_desc:=nil;
  gnutls_session_set_verify_function:=nil;
  gnutls_session_set_verify_cert:=nil;
  gnutls_session_set_verify_cert2:=nil;
  gnutls_session_get_verify_cert_status:=nil;
  gnutls_session_set_premaster:=nil;
  gnutls_session_get_id:=nil;
  gnutls_session_get_id2:=nil;
  gnutls_session_set_id:=nil;
  gnutls_session_channel_binding:=nil;
  gnutls_session_is_resumed:=nil;
  gnutls_session_resumption_requested:=nil;
  gnutls_db_set_cache_expiration:=nil;
  gnutls_db_get_default_cache_expiration:=nil;
  gnutls_db_remove_session:=nil;
  gnutls_db_set_retrieve_function:=nil;
  gnutls_db_set_remove_function:=nil;
  gnutls_db_set_store_function:=nil;
  gnutls_db_set_ptr:=nil;
  gnutls_db_get_ptr:=nil;
  gnutls_db_check_entry:=nil;
  gnutls_db_check_entry_time:=nil;
  gnutls_handshake_set_hook_function:=nil;
  gnutls_handshake_set_post_client_hello_function:=nil;
  gnutls_handshake_set_max_packet_length:=nil;
  gnutls_check_version:=nil;
  gnutls_credentials_clear:=nil;
  gnutls_credentials_set:=nil;
  gnutls_credentials_get:=nil;
  gnutls_anon_free_server_credentials:=nil;
  gnutls_anon_allocate_server_credentials:=nil;
  gnutls_anon_set_server_dh_params:=nil;
  gnutls_anon_set_server_params_function:=nil;
  gnutls_anon_free_client_credentials:=nil;
  gnutls_anon_allocate_client_credentials:=nil;
  gnutls_certificate_free_credentials:=nil;
  gnutls_certificate_allocate_credentials:=nil;
  gnutls_certificate_get_issuer:=nil;
  gnutls_certificate_get_crt_raw:=nil;
  gnutls_certificate_get_x509_crt:=nil;
  gnutls_certificate_get_x509_key:=nil;
  gnutls_certificate_free_keys:=nil;
  gnutls_certificate_free_cas:=nil;
  gnutls_certificate_free_ca_names:=nil;
  gnutls_certificate_free_crls:=nil;
  gnutls_certificate_set_dh_params:=nil;
  gnutls_certificate_set_verify_flags:=nil;
  gnutls_certificate_get_verify_flags:=nil;
  gnutls_certificate_set_flags:=nil;
  gnutls_certificate_set_verify_limits:=nil;
  gnutls_certificate_get_verify_flags:=nil;
  gnutls_certificate_set_x509_system_trust:=nil;
  gnutls_certificate_set_x509_trust_file:=nil;
  gnutls_certificate_set_x509_trust_dir:=nil;
  gnutls_certificate_set_x509_trust_mem:=nil;
  gnutls_certificate_set_x509_crl_file:=nil;
  gnutls_certificate_set_x509_crl_mem:=nil;
  gnutls_certificate_set_x509_key_file:=nil;
  gnutls_certificate_set_x509_key_file2:=nil;
  gnutls_certificate_set_x509_key_mem:=nil;
  gnutls_certificate_set_x509_key_mem2:=nil;
  gnutls_certificate_send_x509_rdn_sequence:=nil;
  gnutls_certificate_set_x509_simple_pkcs12_file:=nil;
  gnutls_certificate_set_x509_simple_pkcs12_mem:=nil;
  gnutls_certificate_set_x509_key:=nil;
  gnutls_certificate_set_x509_trust:=nil;
  gnutls_certificate_set_x509_crl:=nil;
  gnutls_certificate_get_x509_key:=nil;
  gnutls_certificate_get_x509_crt:=nil;
  gnutls_certificate_set_ocsp_status_request_function:=nil;
  gnutls_certificate_set_ocsp_status_request_file:=nil;
  gnutls_ocsp_status_request_enable_client:=nil;
  gnutls_ocsp_status_request_get:=nil;
  gnutls_ocsp_status_request_is_checked:=nil;
  gnutls_global_init:=nil;
  gnutls_global_deinit:=nil;
  gnutls_global_set_mutex:=nil;
  gnutls_global_set_time_function:=nil;
  gnutls_memset:=nil;
  gnutls_memcmp:=nil;
  gnutls_global_set_log_function:=nil;
  gnutls_global_set_audit_log_function:=nil;
  gnutls_global_set_log_level:=nil;
  gnutls_dh_params_init:=nil;
  gnutls_dh_params_deinit:=nil;
  gnutls_dh_params_import_raw:=nil;
  gnutls_dh_params_import_raw2:=nil;
  gnutls_dh_params_import_pkcs3:=nil;
  gnutls_dh_params_generate2:=nil;
  gnutls_dh_params_export_pkcs3:=nil;
  gnutls_dh_params_export2_pkcs3:=nil;
  gnutls_dh_params_export_raw:=nil;
  gnutls_dh_params_cpy:=nil;
  gnutls_system_recv_timeout:=nil;
  gnutls_transport_set_int2:=nil;
  gnutls_transport_get_int2:=nil;
  gnutls_transport_get_int:=nil;
  gnutls_transport_set_ptr:=nil;
  gnutls_transport_set_ptr2:=nil;
  gnutls_transport_get_ptr:=nil;
  gnutls_transport_get_ptr2:=nil;
  gnutls_transport_set_vec_push_function:=nil;
  gnutls_transport_set_push_function:=nil;
  gnutls_transport_set_pull_function:=nil;
  gnutls_transport_set_pull_timeout_function:=nil;
  gnutls_transport_set_errno_function:=nil;
  gnutls_transport_set_errno:=nil;
  gnutls_session_set_ptr:=nil;
  gnutls_session_get_ptr:=nil;
  gnutls_openpgp_send_cert:=nil;
  gnutls_fingerprint:=nil;
  gnutls_random_art:=nil;
  gnutls_srp_free_client_credentials:=nil;
  gnutls_srp_allocate_client_credentials:=nil;
  gnutls_srp_set_client_credentials:=nil;
  gnutls_srp_free_server_credentials:=nil;
  gnutls_srp_allocate_server_credentials:=nil;
  gnutls_srp_set_server_credentials_file:=nil;
  gnutls_srp_server_get_username:=nil;
  gnutls_srp_set_prime_bits:=nil;
  gnutls_srp_verifier:=nil;
  gnutls_srp_set_server_credentials_function:=nil;
  gnutls_srp_set_client_credentials_function:=nil;
  gnutls_srp_base64_encode:=nil;
  gnutls_srp_base64_encode2:=nil;
  gnutls_srp_base64_decode:=nil;
  gnutls_srp_base64_decode2:=nil;
  gnutls_srp_set_server_fake_salt_seed:=nil;
  gnutls_psk_free_client_credentials:=nil;
  gnutls_psk_allocate_client_credentials:=nil;
  gnutls_psk_set_client_credentials:=nil;
  gnutls_psk_free_server_credentials:=nil;
  gnutls_psk_allocate_server_credentials:=nil;
  gnutls_psk_set_server_credentials_file:=nil;
  gnutls_psk_set_server_credentials_hint:=nil;
  gnutls_psk_server_get_username:=nil;
  gnutls_psk_client_get_hint:=nil;
  gnutls_psk_set_server_credentials_function:=nil;
  gnutls_psk_set_client_credentials_function:=nil;
  gnutls_hex_encode:=nil;
  gnutls_hex_decode:=nil;
  gnutls_hex_encode2:=nil;
  gnutls_hex_decode2:=nil;
  gnutls_psk_set_server_dh_params:=nil;
  gnutls_psk_set_server_params_function:=nil;
  gnutls_auth_get_type:=nil;
  gnutls_auth_server_get_type:=nil;
  gnutls_auth_client_get_type:=nil;
  gnutls_dh_set_prime_bits:=nil;
  gnutls_dh_get_secret_bits:=nil;
  gnutls_dh_get_peers_public_bits:=nil;
  gnutls_dh_get_prime_bits:=nil;
  gnutls_dh_get_group:=nil;
  gnutls_dh_get_pubkey:=nil;
  gnutls_certificate_set_retrieve_function:=nil;
  gnutls_certificate_set_verify_function:=nil;
  gnutls_certificate_server_set_request:=nil;
  gnutls_certificate_get_peers:=nil;
  gnutls_certificate_get_ours:=nil;
  gnutls_certificate_get_peers_subkey_id:=nil;
  gnutls_certificate_activation_time_peers:=nil;
  gnutls_certificate_expiration_time_peers:=nil;
  gnutls_certificate_client_get_request_status:=nil;
  gnutls_certificate_verify_peers2:=nil;
  gnutls_certificate_verify_peers3:=nil;
  gnutls_certificate_verify_peers:=nil;
  gnutls_certificate_verification_status_print:=nil;
  gnutls_pem_base64_encode:=nil;
  gnutls_pem_base64_decode:=nil;
  gnutls_pem_base64_encode2:=nil;
  gnutls_pem_base64_decode2:=nil;
  gnutls_certificate_set_params_function:=nil;
  gnutls_anon_set_params_function:=nil;
  gnutls_psk_set_params_function:=nil;
  gnutls_hex2bin:=nil;
  gnutls_tdb_init:=nil;
  gnutls_tdb_set_store_func:=nil;
  gnutls_tdb_set_store_commitment_func:=nil;
  gnutls_tdb_set_verify_func:=nil;
  gnutls_tdb_deinit:=nil;
  gnutls_verify_stored_pubkey:=nil;
  gnutls_store_commitment:=nil;
  gnutls_store_pubkey:=nil;
  gnutls_load_file:=nil;
  gnutls_url_is_supported:=nil;
  gnutls_certificate_set_pin_function:=nil;
  gnutls_buffer_append_data:=nil;
  gnutls_ext_set_data:=nil;
  gnutls_ext_get_data:=nil;
  gnutls_ext_register:=nil;
  gnutls_supplemental_register:=nil;
  gnutls_supplemental_recv:=nil;
  gnutls_supplemental_send:=nil;
  gnutls_fips140_mode_enabled:=nil;
  gnutls_x509_crt_init:=nil;
  gnutls_x509_crt_deinit:=nil;
  gnutls_x509_crt_import:=nil;
  gnutls_x509_crt_list_import2:=nil;
  gnutls_x509_crt_list_import:=nil;
  gnutls_x509_crt_import_url:=nil;
  gnutls_x509_crt_export:=nil;
  gnutls_x509_crt_export2:=nil;
  gnutls_x509_crt_get_private_key_usage_period:=nil;
  gnutls_x509_crt_get_issuer_dn:=nil;
  gnutls_x509_crt_get_issuer_dn2:=nil;
  gnutls_x509_crt_get_issuer_dn_oid:=nil;
  gnutls_x509_crt_get_issuer_dn_by_oid:=nil;
  gnutls_x509_crt_get_dn:=nil;
  gnutls_x509_crt_get_dn2:=nil;
  gnutls_x509_crt_get_dn_oid:=nil;
  gnutls_x509_crt_get_dn_by_oid:=nil;
  gnutls_x509_crt_check_hostname:=nil;
  gnutls_x509_crt_check_hostname2:=nil;
  gnutls_x509_crt_check_email:=nil;
  gnutls_x509_crt_get_signature_algorithm:=nil;
  gnutls_x509_crt_get_signature:=nil;
  gnutls_x509_crt_get_version:=nil;
  gnutls_x509_crt_get_key_id:=nil;
  gnutls_x509_crt_set_private_key_usage_period:=nil;
  gnutls_x509_crt_set_authority_key_id:=nil;
  gnutls_x509_crt_get_authority_key_id:=nil;
  gnutls_x509_crt_get_authority_key_gn_serial:=nil;
  gnutls_x509_crt_get_subject_key_id:=nil;
  gnutls_x509_crt_get_subject_unique_id:=nil;
  gnutls_x509_crt_get_issuer_unique_id:=nil;
  gnutls_x509_crt_set_pin_function:=nil;
  gnutls_x509_crt_get_authority_info_access:=nil;
  gnutls_x509_name_constraints_check:=nil;
  gnutls_x509_name_constraints_check_crt:=nil;
  gnutls_x509_name_constraints_init:=nil;
  gnutls_x509_name_constraints_deinit:=nil;
  gnutls_x509_crt_get_name_constraints:=nil;
  gnutls_x509_name_constraints_add_permitted:=nil;
  gnutls_x509_name_constraints_add_excluded:=nil;
  gnutls_x509_crt_set_name_constraints:=nil;
  gnutls_x509_name_constraints_get_permitted:=nil;
  gnutls_x509_name_constraints_get_excluded:=nil;
  gnutls_x509_crt_get_crl_dist_points:=nil;
  gnutls_x509_crt_set_crl_dist_points2:=nil;
  gnutls_x509_crt_set_crl_dist_points:=nil;
  gnutls_x509_crt_cpy_crl_dist_points:=nil;
  gnutls_x509_crl_sign2:=nil;
  gnutls_x509_crt_get_activation_time:=nil;
  gnutls_x509_crt_get_expiration_time:=nil;
  gnutls_x509_crt_get_serial:=nil;
  gnutls_x509_crt_get_pk_algorithm:=nil;
  gnutls_x509_crt_get_pk_rsa_raw:=nil;
  gnutls_x509_crt_get_pk_dsa_raw:=nil;
  gnutls_x509_crt_get_pk_ecc_raw:=nil;
  gnutls_x509_crt_get_subject_alt_name:=nil;
  gnutls_x509_crt_get_subject_alt_name2:=nil;
  gnutls_x509_crt_get_subject_alt_othername_oid:=nil;
  gnutls_x509_crt_get_issuer_alt_name:=nil;
  gnutls_x509_crt_get_issuer_alt_name2:=nil;
  gnutls_x509_crt_get_issuer_alt_othername_oid:=nil;
  gnutls_x509_crt_get_ca_status:=nil;
  gnutls_x509_crt_get_basic_constraints:=nil;
  gnutls_x509_crt_get_key_usage:=nil;
  gnutls_x509_crt_set_key_usage:=nil;
  gnutls_x509_crt_set_authority_info_access:=nil;
  gnutls_x509_crt_get_proxy:=nil;
  gnutls_x509_policy_release:=nil;
  gnutls_x509_crt_get_policy:=nil;
  gnutls_x509_crt_set_policy:=nil;
  gnutls_x509_dn_oid_known:=nil;
  gnutls_x509_dn_oid_name:=nil;
  gnutls_x509_crt_get_extension_oid:=nil;
  gnutls_x509_crt_get_extension_by_oid:=nil;
  gnutls_x509_crq_get_signature_algorithm:=nil;
  gnutls_x509_crq_get_extension_by_oid2:=nil;
  gnutls_x509_crt_get_extension_info:=nil;
  gnutls_x509_crt_get_extension_data:=nil;
  gnutls_x509_crt_get_extension_data2:=nil;
  gnutls_x509_crt_set_extension_by_oid:=nil;
  gnutls_x509_crt_set_dn:=nil;
  gnutls_x509_crt_set_dn_by_oid:=nil;
  gnutls_x509_crt_set_issuer_dn_by_oid:=nil;
  gnutls_x509_crt_set_issuer_dn:=nil;
  gnutls_x509_crt_set_version:=nil;
  gnutls_x509_crt_set_key:=nil;
  gnutls_x509_crt_set_ca_status:=nil;
  gnutls_x509_crt_set_basic_constraints:=nil;
  gnutls_x509_crt_set_subject_unique_id:=nil;
  gnutls_x509_crt_set_issuer_unique_id:=nil;
  gnutls_x509_crt_set_subject_alternative_name:=nil;
  gnutls_x509_crt_set_subject_alt_name:=nil;
  gnutls_x509_crt_set_issuer_alt_name:=nil;
  gnutls_x509_crt_sign:=nil;
  gnutls_x509_crt_sign2:=nil;
  gnutls_x509_crt_set_activation_time:=nil;
  gnutls_x509_crt_set_expiration_time:=nil;
  gnutls_x509_crt_set_serial:=nil;
  gnutls_x509_crt_set_subject_key_id:=nil;
  gnutls_x509_crt_set_proxy_dn:=nil;
  gnutls_x509_crt_set_proxy:=nil;
  gnutls_x509_crt_print:=nil;
  gnutls_x509_crl_print:=nil;
  gnutls_x509_crt_get_raw_issuer_dn:=nil;
  gnutls_x509_crt_get_raw_dn:=nil;
  gnutls_x509_rdn_get:=nil;
  gnutls_x509_rdn_get_oid:=nil;
  gnutls_x509_rdn_get_by_oid:=nil;
  gnutls_x509_crt_get_subject:=nil;
  gnutls_x509_crt_get_issuer:=nil;
  gnutls_x509_dn_get_rdn_ava:=nil;
  gnutls_x509_dn_get_str:=nil;
  gnutls_x509_dn_init:=nil;
  gnutls_x509_dn_import:=nil;
  gnutls_x509_dn_export:=nil;
  gnutls_x509_dn_export2:=nil;
  gnutls_x509_dn_deinit:=nil;
  gnutls_x509_crl_init:=nil;
  gnutls_x509_crl_deinit:=nil;
  gnutls_x509_crl_import:=nil;
  gnutls_x509_crl_export:=nil;
  gnutls_x509_crl_export2:=nil;
  gnutls_x509_crl_get_raw_issuer_dn:=nil;
  gnutls_x509_crl_get_issuer_dn:=nil;
  gnutls_x509_crl_get_issuer_dn2:=nil;
  gnutls_x509_crl_get_issuer_dn_by_oid:=nil;
  gnutls_x509_crl_get_dn_oid:=nil;
  gnutls_x509_crl_get_signature_algorithm:=nil;
  gnutls_x509_crl_get_signature:=nil;
  gnutls_x509_crl_get_version:=nil;
  gnutls_x509_crl_get_this_update:=nil;
  gnutls_x509_crl_get_next_update:=nil;
  gnutls_x509_crl_get_crt_count:=nil;
  gnutls_x509_crl_get_crt_serial:=nil;
  gnutls_x509_crl_iter_crt_serial:=nil;
  gnutls_x509_crl_iter_deinit:=nil;
  gnutls_x509_crl_check_issuer:=nil;
  gnutls_x509_crl_list_import2:=nil;
  gnutls_x509_crl_list_import:=nil;
  gnutls_x509_crl_set_version:=nil;
  gnutls_x509_crl_set_this_update:=nil;
  gnutls_x509_crl_set_next_update:=nil;
  gnutls_x509_crl_set_crt_serial:=nil;
  gnutls_x509_crl_set_crt:=nil;
  gnutls_x509_crl_get_authority_key_id:=nil;
  gnutls_x509_crl_get_authority_key_gn_serial:=nil;
  gnutls_x509_crl_get_number:=nil;
  gnutls_x509_crl_get_extension_oid:=nil;
  gnutls_x509_crl_get_extension_info:=nil;
  gnutls_x509_crl_get_extension_data:=nil;
  gnutls_x509_crl_get_extension_data2:=nil;
  gnutls_x509_crl_set_authority_key_id:=nil;
  gnutls_x509_crl_set_number:=nil;
  gnutls_x509_crt_check_issuer:=nil;
  gnutls_x509_crt_list_verify:=nil;
  gnutls_x509_crt_verify:=nil;
  gnutls_x509_crl_verify:=nil;
  gnutls_x509_crt_verify_data2:=nil;
  gnutls_x509_crt_check_revocation:=nil;
  gnutls_x509_crt_get_fingerprint:=nil;
  gnutls_x509_crt_get_key_purpose_oid:=nil;
  gnutls_x509_crt_set_key_purpose_oid:=nil;
  gnutls_pkcs_schema_get_name:=nil;
  gnutls_pkcs_schema_get_oid:=nil;
  gnutls_x509_privkey_init:=nil;
  gnutls_x509_privkey_deinit:=nil;
  gnutls_x509_privkey_sec_param:=nil;
  gnutls_x509_privkey_set_pin_function:=nil;
  gnutls_x509_privkey_cpy:=nil;
  gnutls_x509_privkey_import:=nil;
  gnutls_x509_privkey_import_pkcs8:=nil;
  gnutls_x509_privkey_import_openssl:=nil;
  gnutls_pkcs8_info:=nil;
  gnutls_x509_privkey_import2:=nil;
  gnutls_x509_privkey_import_rsa_raw:=nil;
  gnutls_x509_privkey_import_rsa_raw2:=nil;
  gnutls_x509_privkey_import_ecc_raw:=nil;
  gnutls_x509_privkey_fix:=nil;
  gnutls_x509_privkey_export_dsa_raw:=nil;
  gnutls_x509_privkey_import_dsa_raw:=nil;
  gnutls_x509_privkey_get_pk_algorithm:=nil;
  gnutls_x509_privkey_get_pk_algorithm2:=nil;
  gnutls_x509_privkey_get_key_id:=nil;
  gnutls_x509_privkey_generate:=nil;
  gnutls_x509_privkey_verify_params:=nil;
  gnutls_x509_privkey_export:=nil;
  gnutls_x509_privkey_export2:=nil;
  gnutls_x509_privkey_export_pkcs8:=nil;
  gnutls_x509_privkey_export2_pkcs8:=nil;
  gnutls_x509_privkey_export_rsa_raw2:=nil;
  gnutls_x509_privkey_export_rsa_raw:=nil;
  gnutls_x509_privkey_export_ecc_raw:=nil;
  gnutls_x509_privkey_sign_data:=nil;
  gnutls_x509_crq_sign2:=nil;
  gnutls_x509_crq_print:=nil;
  gnutls_x509_crq_verify:=nil;
  gnutls_x509_crq_init:=nil;
  gnutls_x509_crq_deinit:=nil;
  gnutls_x509_crq_import:=nil;
  gnutls_x509_crq_get_private_key_usage_period:=nil;
  gnutls_x509_crq_get_dn:=nil;
  gnutls_x509_crq_get_dn2:=nil;
  gnutls_x509_crq_get_dn_oid:=nil;
  gnutls_x509_crq_get_dn_by_oid:=nil;
  gnutls_x509_crq_set_dn:=nil;
  gnutls_x509_crq_set_dn_by_oid:=nil;
  gnutls_x509_crq_set_version:=nil;
  gnutls_x509_crq_get_version:=nil;
  gnutls_x509_crq_set_key:=nil;
  gnutls_x509_crq_set_challenge_password:=nil;
  gnutls_x509_crq_get_challenge_password:=nil;
  gnutls_x509_crq_set_attribute_by_oid:=nil;
  gnutls_x509_crq_get_attribute_by_oid:=nil;
  gnutls_x509_crq_export:=nil;
  gnutls_x509_crq_export2:=nil;
  gnutls_x509_crt_set_crq:=nil;
  gnutls_x509_crt_set_crq_extensions:=nil;
  gnutls_x509_crq_set_private_key_usage_period:=nil;
  gnutls_x509_crq_set_key_rsa_raw:=nil;
  gnutls_x509_crq_set_subject_alt_name:=nil;
  gnutls_x509_crq_set_key_usage:=nil;
  gnutls_x509_crq_set_basic_constraints:=nil;
  gnutls_x509_crq_set_key_purpose_oid:=nil;
  gnutls_x509_crq_get_key_purpose_oid:=nil;
  gnutls_x509_crq_get_extension_data:=nil;
  gnutls_x509_crq_get_extension_data2:=nil;
  gnutls_x509_crq_get_extension_info:=nil;
  gnutls_x509_crq_get_attribute_data:=nil;
  gnutls_x509_crq_get_attribute_info:=nil;
  gnutls_x509_crq_get_pk_algorithm:=nil;
  gnutls_x509_crq_get_key_id:=nil;
  gnutls_x509_crq_get_key_rsa_raw:=nil;
  gnutls_x509_crq_get_key_usage:=nil;
  gnutls_x509_crq_get_basic_constraints:=nil;
  gnutls_x509_crq_get_subject_alt_name:=nil;
  gnutls_x509_crq_get_subject_alt_othername_oid:=nil;
  gnutls_x509_crq_get_extension_by_oid:=nil;
  gnutls_x509_crt_get_extension_by_oid2:=nil;
  gnutls_x509_trust_list_init:=nil;
  gnutls_x509_trust_list_deinit:=nil;
  gnutls_x509_trust_list_get_issuer:=nil;
  gnutls_x509_trust_list_get_issuer_by_dn:=nil;
  gnutls_x509_trust_list_get_issuer_by_subject_key_id:=nil;
  gnutls_x509_trust_list_add_cas:=nil;
  gnutls_x509_trust_list_remove_cas:=nil;
  gnutls_x509_trust_list_add_named_crt:=nil;
  gnutls_x509_trust_list_add_crls:=nil;
  gnutls_x509_trust_list_iter_get_ca:=nil;
  gnutls_x509_trust_list_iter_deinit:=nil;
  gnutls_x509_trust_list_verify_named_crt:=nil;
  gnutls_x509_trust_list_verify_crt2:=nil;
  gnutls_x509_trust_list_verify_crt:=nil;
  gnutls_x509_trust_list_add_trust_mem:=nil;
  gnutls_x509_trust_list_add_trust_file:=nil;
  gnutls_x509_trust_list_add_trust_dir:=nil;
  gnutls_x509_trust_list_remove_trust_file:=nil;
  gnutls_x509_trust_list_remove_trust_mem:=nil;
  gnutls_x509_trust_list_add_system_trust:=nil;
  gnutls_certificate_set_trust_list:=nil;
  gnutls_certificate_get_trust_list:=nil;
  gnutls_x509_ext_deinit:=nil;
  gnutls_x509_ext_print:=nil;
  gnutls_pkcs7_init:=nil;
  gnutls_pkcs7_deinit:=nil;
  gnutls_pkcs7_import:=nil;
  gnutls_pkcs7_export:=nil;
  gnutls_pkcs7_export2:=nil;
  gnutls_pkcs7_get_signature_count:=nil;
  gnutls_pkcs7_get_embedded_data:=nil;
  gnutls_pkcs7_get_crt_count:=nil;
  gnutls_pkcs7_get_crt_raw:=nil;
  gnutls_pkcs7_set_crt_raw:=nil;
  gnutls_pkcs7_set_crt:=nil;
  gnutls_pkcs7_delete_crt:=nil;
  gnutls_pkcs7_get_crl_raw:=nil;
  gnutls_pkcs7_get_crl_count:=nil;
  gnutls_pkcs7_set_crl_raw:=nil;
  gnutls_pkcs7_set_crl:=nil;
  gnutls_pkcs7_delete_crl:=nil;
  gnutls_pkcs7_signature_info_deinit:=nil;
  gnutls_pkcs7_get_signature_info:=nil;
  gnutls_pkcs7_verify_direct:=nil;
  gnutls_pkcs7_verify:=nil;
  gnutls_pkcs7_add_attr:=nil;
  gnutls_pkcs7_attrs_deinit:=nil;
  gnutls_pkcs7_get_attr:=nil;
  gnutls_pkcs7_sign:=nil;
  gnutls_pkcs7_get_crt_raw2:=nil;
  gnutls_pkcs7_get_crl_raw2:=nil;
  gnutls_pkcs7_print:=nil;
end;


procedure LoadGnuTLS(alib : String = '');

  Function GPA(aName : string) : Pointer;

  begin
    Result:=GetProcAddress(hlib,aName);
    // For debugging purposes
    if Result=Nil then
      Writeln('Failed to get address for '+AName);
  end;

begin
  // Default if needed.
  if ALib='' then
    aLib:=LibGnuTLS;
  // If it is a different one, unload current.
  if GnuTLSloaded and (aLib<>LoadedLibName) then
    FreeGnuTLS;
  // Bail out on error.
  if GnuTLSloaded then
    exit;
  // Load !
  hlib:=LoadLibrary(alib);
  if (hlib=NilHandle) then
    raise Exception.Create(format('Could not load library: %s',[alib]));
  pointer(gnutls_handshake_description_get_name):=GPA('gnutls_handshake_description_get_name');
  pointer(gnutls_pk_algorithm_get_name):=GPA('gnutls_pk_algorithm_get_name');
  pointer(gnutls_init):=GPA('gnutls_init');
  pointer(gnutls_deinit):=GPA('gnutls_deinit');
  pointer(gnutls_bye):=GPA('gnutls_bye');
  pointer(gnutls_handshake):=GPA('gnutls_handshake');
  pointer(gnutls_handshake_set_timeout):=GPA('gnutls_handshake_set_timeout');
  pointer(gnutls_rehandshake):=GPA('gnutls_rehandshake');
  pointer(gnutls_alert_get):=GPA('gnutls_alert_get');
  pointer(gnutls_alert_send):=GPA('gnutls_alert_send');
  pointer(gnutls_alert_send_appropriate):=GPA('gnutls_alert_send_appropriate');
  pointer(gnutls_alert_get_name):=GPA('gnutls_alert_get_name');
  pointer(gnutls_alert_get_strname):=GPA('gnutls_alert_get_strname');
  pointer(gnutls_pk_bits_to_sec_param):=GPA('gnutls_pk_bits_to_sec_param');
  pointer(gnutls_sec_param_get_name):=GPA('gnutls_sec_param_get_name');
  pointer(gnutls_sec_param_to_pk_bits):=GPA('gnutls_sec_param_to_pk_bits');
  pointer(gnutls_sec_param_to_symmetric_bits):=GPA('gnutls_sec_param_to_symmetric_bits');
  pointer(gnutls_ecc_curve_get_name):=GPA('gnutls_ecc_curve_get_name');
  pointer(gnutls_ecc_curve_get_oid):=GPA('gnutls_ecc_curve_get_oid');
  pointer(gnutls_ecc_curve_get_size):=GPA('gnutls_ecc_curve_get_size');
  pointer(gnutls_ecc_curve_get):=GPA('gnutls_ecc_curve_get');
  pointer(gnutls_cipher_get):=GPA('gnutls_cipher_get');
  pointer(gnutls_kx_get):=GPA('gnutls_kx_get');
  pointer(gnutls_mac_get):=GPA('gnutls_mac_get');
  pointer(gnutls_compression_get):=GPA('gnutls_compression_get');
  pointer(gnutls_certificate_type_get):=GPA('gnutls_certificate_type_get');
  pointer(gnutls_sign_algorithm_get):=GPA('gnutls_sign_algorithm_get');
  pointer(gnutls_sign_algorithm_get_client):=GPA('gnutls_sign_algorithm_get_client');
  pointer(gnutls_sign_algorithm_get_requested):=GPA('gnutls_sign_algorithm_get_requested');
  pointer(gnutls_cipher_get_name):=GPA('gnutls_cipher_get_name');
  pointer(gnutls_mac_get_name):=GPA('gnutls_mac_get_name');
  pointer(gnutls_digest_get_name):=GPA('gnutls_digest_get_name');
  pointer(gnutls_digest_get_oid):=GPA('gnutls_digest_get_oid');
  pointer(gnutls_compression_get_name):=GPA('gnutls_compression_get_name');
  pointer(gnutls_kx_get_name):=GPA('gnutls_kx_get_name');
  pointer(gnutls_certificate_type_get_name):=GPA('gnutls_certificate_type_get_name');
  pointer(gnutls_pk_get_name):=GPA('gnutls_pk_get_name');
  pointer(gnutls_pk_get_oid):=GPA('gnutls_pk_get_oid');
  pointer(gnutls_sign_get_name):=GPA('gnutls_sign_get_name');
  pointer(gnutls_sign_get_oid):=GPA('gnutls_sign_get_oid');
  pointer(gnutls_cipher_get_key_size):=GPA('gnutls_cipher_get_key_size');
  pointer(gnutls_mac_get_key_size):=GPA('gnutls_mac_get_key_size');
  pointer(gnutls_sign_is_secure):=GPA('gnutls_sign_is_secure');
  pointer(gnutls_sign_get_hash_algorithm):=GPA('gnutls_sign_get_hash_algorithm');
  pointer(gnutls_sign_get_pk_algorithm):=GPA('gnutls_sign_get_pk_algorithm');
  pointer(gnutls_pk_to_sign):=GPA('gnutls_pk_to_sign');
  pointer(gnutls_mac_get_id):=GPA('gnutls_mac_get_id');
  pointer(gnutls_digest_get_id):=GPA('gnutls_digest_get_id');
  pointer(gnutls_compression_get_id):=GPA('gnutls_compression_get_id');
  pointer(gnutls_cipher_get_id):=GPA('gnutls_cipher_get_id');
  pointer(gnutls_kx_get_id):=GPA('gnutls_kx_get_id');
  pointer(gnutls_protocol_get_id):=GPA('gnutls_protocol_get_id');
  pointer(gnutls_certificate_type_get_id):=GPA('gnutls_certificate_type_get_id');
  pointer(gnutls_pk_get_id):=GPA('gnutls_pk_get_id');
  pointer(gnutls_sign_get_id):=GPA('gnutls_sign_get_id');
  pointer(gnutls_ecc_curve_get_id):=GPA('gnutls_ecc_curve_get_id');
  pointer(gnutls_oid_to_digest):=GPA('gnutls_oid_to_digest');
  pointer(gnutls_oid_to_pk):=GPA('gnutls_oid_to_pk');
  pointer(gnutls_oid_to_sign):=GPA('gnutls_oid_to_sign');
  pointer(gnutls_oid_to_ecc_curve):=GPA('gnutls_oid_to_ecc_curve');
  pointer(gnutls_ecc_curve_list):=GPA('gnutls_ecc_curve_list');
  pointer(gnutls_cipher_list):=GPA('gnutls_cipher_list');
  pointer(gnutls_mac_list):=GPA('gnutls_mac_list');
  pointer(gnutls_digest_list):=GPA('gnutls_digest_list');
  pointer(gnutls_compression_list):=GPA('gnutls_compression_list');
  pointer(gnutls_protocol_list):=GPA('gnutls_protocol_list');
  pointer(gnutls_certificate_type_list):=GPA('gnutls_certificate_type_list');
  pointer(gnutls_kx_list):=GPA('gnutls_kx_list');
  pointer(gnutls_pk_list):=GPA('gnutls_pk_list');
  pointer(gnutls_sign_list):=GPA('gnutls_sign_list');
  pointer(gnutls_cipher_suite_info):=GPA('gnutls_cipher_suite_info');
  pointer(gnutls_error_is_fatal):=GPA('gnutls_error_is_fatal');
  pointer(gnutls_error_to_alert):=GPA('gnutls_error_to_alert');
  pointer(gnutls_perror):=GPA('gnutls_perror');
  pointer(gnutls_strerror):=GPA('gnutls_strerror');
  pointer(gnutls_strerror_name):=GPA('gnutls_strerror_name');
  pointer(gnutls_handshake_set_private_extensions):=GPA('gnutls_handshake_set_private_extensions');
  pointer(gnutls_handshake_set_random):=GPA('gnutls_handshake_set_random');
  pointer(gnutls_handshake_get_last_out):=GPA('gnutls_handshake_get_last_out');
  pointer(gnutls_handshake_get_last_in):=GPA('gnutls_handshake_get_last_in');
  pointer(gnutls_heartbeat_ping):=GPA('gnutls_heartbeat_ping');
  pointer(gnutls_heartbeat_pong):=GPA('gnutls_heartbeat_pong');
  pointer(gnutls_record_set_timeout):=GPA('gnutls_record_set_timeout');
  pointer(gnutls_record_disable_padding):=GPA('gnutls_record_disable_padding');
  pointer(gnutls_record_cork):=GPA('gnutls_record_cork');
  pointer(gnutls_record_uncork):=GPA('gnutls_record_uncork');
  pointer(gnutls_record_discard_queued):=GPA('gnutls_record_discard_queued');
  pointer(gnutls_record_get_state):=GPA('gnutls_record_get_state');
  pointer(gnutls_record_set_state):=GPA('gnutls_record_set_state');
  pointer(gnutls_range_split):=GPA('gnutls_range_split');
  pointer(gnutls_record_send):=GPA('gnutls_record_send');
  pointer(gnutls_record_send_range):=GPA('gnutls_record_send_range');
  pointer(gnutls_record_recv):=GPA('gnutls_record_recv');
  pointer(gnutls_record_recv_packet):=GPA('gnutls_record_recv_packet');
  pointer(gnutls_packet_get):=GPA('gnutls_packet_get');
  pointer(gnutls_packet_deinit):=GPA('gnutls_packet_deinit');
  pointer(gnutls_record_recv_seq):=GPA('gnutls_record_recv_seq');
  pointer(gnutls_record_overhead_size):=GPA('gnutls_record_overhead_size');
  pointer(gnutls_est_record_overhead_size):=GPA('gnutls_est_record_overhead_size');
  pointer(gnutls_session_enable_compatibility_mode):=GPA('gnutls_session_enable_compatibility_mode');
  pointer(gnutls_record_can_use_length_hiding):=GPA('gnutls_record_can_use_length_hiding');
  pointer(gnutls_record_get_direction):=GPA('gnutls_record_get_direction');
  pointer(gnutls_record_get_max_size):=GPA('gnutls_record_get_max_size');
  pointer(gnutls_record_set_max_size):=GPA('gnutls_record_set_max_size');
  pointer(gnutls_record_check_pending):=GPA('gnutls_record_check_pending');
  pointer(gnutls_record_check_corked):=GPA('gnutls_record_check_corked');
  pointer(gnutls_session_force_valid):=GPA('gnutls_session_force_valid');
  pointer(gnutls_prf):=GPA('gnutls_prf');
  pointer(gnutls_prf_rfc5705):=GPA('gnutls_prf_rfc5705');
  pointer(gnutls_prf_raw):=GPA('gnutls_prf_raw');
  pointer(gnutls_server_name_set):=GPA('gnutls_server_name_set');
  pointer(gnutls_server_name_get):=GPA('gnutls_server_name_get');
  pointer(gnutls_heartbeat_get_timeout):=GPA('gnutls_heartbeat_get_timeout');
  pointer(gnutls_heartbeat_set_timeouts):=GPA('gnutls_heartbeat_set_timeouts');
  pointer(gnutls_heartbeat_enable):=GPA('gnutls_heartbeat_enable');
  pointer(gnutls_heartbeat_allowed):=GPA('gnutls_heartbeat_allowed');
  pointer(gnutls_safe_renegotiation_status):=GPA('gnutls_safe_renegotiation_status');
  pointer(gnutls_session_ext_master_secret_status):=GPA('gnutls_session_ext_master_secret_status');
  pointer(gnutls_session_etm_status):=GPA('gnutls_session_etm_status');
  pointer(gnutls_supplemental_get_name):=GPA('gnutls_supplemental_get_name');
  pointer(gnutls_session_ticket_key_generate):=GPA('gnutls_session_ticket_key_generate');
  pointer(gnutls_session_ticket_enable_client):=GPA('gnutls_session_ticket_enable_client');
  pointer(gnutls_session_ticket_enable_server):=GPA('gnutls_session_ticket_enable_server');
  pointer(gnutls_srtp_set_profile):=GPA('gnutls_srtp_set_profile');
  pointer(gnutls_srtp_set_profile_direct):=GPA('gnutls_srtp_set_profile_direct');
  pointer(gnutls_srtp_get_selected_profile):=GPA('gnutls_srtp_get_selected_profile');
  pointer(gnutls_srtp_get_profile_name):=GPA('gnutls_srtp_get_profile_name');
  pointer(gnutls_srtp_get_profile_id):=GPA('gnutls_srtp_get_profile_id');
  pointer(gnutls_srtp_get_keys):=GPA('gnutls_srtp_get_keys');
  pointer(gnutls_srtp_set_mki):=GPA('gnutls_srtp_set_mki');
  pointer(gnutls_srtp_get_mki):=GPA('gnutls_srtp_get_mki');
  pointer(gnutls_alpn_get_selected_protocol):=GPA('gnutls_alpn_get_selected_protocol');
  pointer(gnutls_alpn_set_protocols):=GPA('gnutls_alpn_set_protocols');
  pointer(gnutls_key_generate):=GPA('gnutls_key_generate');
  pointer(gnutls_priority_init):=GPA('gnutls_priority_init');
  pointer(gnutls_priority_deinit):=GPA('gnutls_priority_deinit');
  pointer(gnutls_priority_get_cipher_suite_index):=GPA('gnutls_priority_get_cipher_suite_index');
  pointer(gnutls_priority_string_list):=GPA('gnutls_priority_string_list');
  pointer(gnutls_priority_set):=GPA('gnutls_priority_set');
  pointer(gnutls_priority_set_direct):=GPA('gnutls_priority_set_direct');
  pointer(gnutls_priority_certificate_type_list):=GPA('gnutls_priority_certificate_type_list');
  pointer(gnutls_priority_sign_list):=GPA('gnutls_priority_sign_list');
  pointer(gnutls_priority_protocol_list):=GPA('gnutls_priority_protocol_list');
  pointer(gnutls_priority_compression_list):=GPA('gnutls_priority_compression_list');
  pointer(gnutls_priority_ecc_curve_list):=GPA('gnutls_priority_ecc_curve_list');
  pointer(gnutls_priority_kx_list):=GPA('gnutls_priority_kx_list');
  pointer(gnutls_priority_cipher_list):=GPA('gnutls_priority_cipher_list');
  pointer(gnutls_priority_mac_list):=GPA('gnutls_priority_mac_list');
  pointer(gnutls_set_default_priority):=GPA('gnutls_set_default_priority');
  pointer(gnutls_cipher_suite_get_name):=GPA('gnutls_cipher_suite_get_name');
  pointer(gnutls_protocol_get_version):=GPA('gnutls_protocol_get_version');
  pointer(gnutls_protocol_get_name):=GPA('gnutls_protocol_get_name');
  pointer(gnutls_session_set_data):=GPA('gnutls_session_set_data');
  pointer(gnutls_session_get_data):=GPA('gnutls_session_get_data');
  pointer(gnutls_session_get_data2):=GPA('gnutls_session_get_data2');
  pointer(gnutls_session_get_random):=GPA('gnutls_session_get_random');
  pointer(gnutls_session_get_desc):=GPA('gnutls_session_get_desc');
  pointer(gnutls_session_set_verify_function):=GPA('gnutls_session_set_verify_function');
  pointer(gnutls_session_set_verify_cert):=GPA('gnutls_session_set_verify_cert');
  pointer(gnutls_session_set_verify_cert2):=GPA('gnutls_session_set_verify_cert2');
  pointer(gnutls_session_get_verify_cert_status):=GPA('gnutls_session_get_verify_cert_status');
  pointer(gnutls_session_set_premaster):=GPA('gnutls_session_set_premaster');
  pointer(gnutls_session_get_id):=GPA('gnutls_session_get_id');
  pointer(gnutls_session_get_id2):=GPA('gnutls_session_get_id2');
  pointer(gnutls_session_set_id):=GPA('gnutls_session_set_id');
  pointer(gnutls_session_channel_binding):=GPA('gnutls_session_channel_binding');
  pointer(gnutls_session_is_resumed):=GPA('gnutls_session_is_resumed');
  pointer(gnutls_session_resumption_requested):=GPA('gnutls_session_resumption_requested');
  pointer(gnutls_db_set_cache_expiration):=GPA('gnutls_db_set_cache_expiration');
  pointer(gnutls_db_get_default_cache_expiration):=GPA('gnutls_db_get_default_cache_expiration');
  pointer(gnutls_db_remove_session):=GPA('gnutls_db_remove_session');
  pointer(gnutls_db_set_retrieve_function):=GPA('gnutls_db_set_retrieve_function');
  pointer(gnutls_db_set_remove_function):=GPA('gnutls_db_set_remove_function');
  pointer(gnutls_db_set_store_function):=GPA('gnutls_db_set_store_function');
  pointer(gnutls_db_set_ptr):=GPA('gnutls_db_set_ptr');
  pointer(gnutls_db_get_ptr):=GPA('gnutls_db_get_ptr');
  pointer(gnutls_db_check_entry):=GPA('gnutls_db_check_entry');
  pointer(gnutls_db_check_entry_time):=GPA('gnutls_db_check_entry_time');
  pointer(gnutls_handshake_set_hook_function):=GPA('gnutls_handshake_set_hook_function');
  pointer(gnutls_handshake_set_post_client_hello_function):=GPA('gnutls_handshake_set_post_client_hello_function');
  pointer(gnutls_handshake_set_max_packet_length):=GPA('gnutls_handshake_set_max_packet_length');
  pointer(gnutls_check_version):=GPA('gnutls_check_version');
  pointer(gnutls_credentials_clear):=GPA('gnutls_credentials_clear');
  pointer(gnutls_credentials_set):=GPA('gnutls_credentials_set');
  pointer(gnutls_credentials_get):=GPA('gnutls_credentials_get');
  pointer(gnutls_anon_free_server_credentials):=GPA('gnutls_anon_free_server_credentials');
  pointer(gnutls_anon_allocate_server_credentials):=GPA('gnutls_anon_allocate_server_credentials');
  pointer(gnutls_anon_set_server_dh_params):=GPA('gnutls_anon_set_server_dh_params');
  pointer(gnutls_anon_set_server_params_function):=GPA('gnutls_anon_set_server_params_function');
  pointer(gnutls_anon_free_client_credentials):=GPA('gnutls_anon_free_client_credentials');
  pointer(gnutls_anon_allocate_client_credentials):=GPA('gnutls_anon_allocate_client_credentials');
  pointer(gnutls_certificate_free_credentials):=GPA('gnutls_certificate_free_credentials');
  pointer(gnutls_certificate_allocate_credentials):=GPA('gnutls_certificate_allocate_credentials');
  pointer(gnutls_certificate_get_issuer):=GPA('gnutls_certificate_get_issuer');
  pointer(gnutls_certificate_get_crt_raw):=GPA('gnutls_certificate_get_crt_raw');
  pointer(gnutls_certificate_get_x509_crt):=GPA('gnutls_certificate_get_x509_crt');
  pointer(gnutls_certificate_get_x509_key):=GPA('gnutls_certificate_get_x509_key');
  pointer(gnutls_certificate_free_keys):=GPA('gnutls_certificate_free_keys');
  pointer(gnutls_certificate_free_cas):=GPA('gnutls_certificate_free_cas');
  pointer(gnutls_certificate_free_ca_names):=GPA('gnutls_certificate_free_ca_names');
  pointer(gnutls_certificate_free_crls):=GPA('gnutls_certificate_free_crls');
  pointer(gnutls_certificate_set_dh_params):=GPA('gnutls_certificate_set_dh_params');
  pointer(gnutls_certificate_set_verify_flags):=GPA('gnutls_certificate_set_verify_flags');
  pointer(gnutls_certificate_get_verify_flags):=GPA('gnutls_certificate_get_verify_flags');
  pointer(gnutls_certificate_set_flags):=GPA('gnutls_certificate_set_flags');
  pointer(gnutls_certificate_set_verify_limits):=GPA('gnutls_certificate_set_verify_limits');
  pointer(gnutls_certificate_get_verify_flags):=GPA('gnutls_certificate_get_verify_flags');
  pointer(gnutls_certificate_set_x509_system_trust):=GPA('gnutls_certificate_set_x509_system_trust');
  pointer(gnutls_certificate_set_x509_trust_file):=GPA('gnutls_certificate_set_x509_trust_file');
  pointer(gnutls_certificate_set_x509_trust_dir):=GPA('gnutls_certificate_set_x509_trust_dir');
  pointer(gnutls_certificate_set_x509_trust_mem):=GPA('gnutls_certificate_set_x509_trust_mem');
  pointer(gnutls_certificate_set_x509_crl_file):=GPA('gnutls_certificate_set_x509_crl_file');
  pointer(gnutls_certificate_set_x509_crl_mem):=GPA('gnutls_certificate_set_x509_crl_mem');
  pointer(gnutls_certificate_set_x509_key_file):=GPA('gnutls_certificate_set_x509_key_file');
  pointer(gnutls_certificate_set_x509_key_file2):=GPA('gnutls_certificate_set_x509_key_file2');
  pointer(gnutls_certificate_set_x509_key_mem):=GPA('gnutls_certificate_set_x509_key_mem');
  pointer(gnutls_certificate_set_x509_key_mem2):=GPA('gnutls_certificate_set_x509_key_mem2');
  pointer(gnutls_certificate_send_x509_rdn_sequence):=GPA('gnutls_certificate_send_x509_rdn_sequence');
  pointer(gnutls_certificate_set_x509_simple_pkcs12_file):=GPA('gnutls_certificate_set_x509_simple_pkcs12_file');
  pointer(gnutls_certificate_set_x509_simple_pkcs12_mem):=GPA('gnutls_certificate_set_x509_simple_pkcs12_mem');
  pointer(gnutls_certificate_set_x509_key):=GPA('gnutls_certificate_set_x509_key');
  pointer(gnutls_certificate_set_x509_trust):=GPA('gnutls_certificate_set_x509_trust');
  pointer(gnutls_certificate_set_x509_crl):=GPA('gnutls_certificate_set_x509_crl');
  pointer(gnutls_certificate_get_x509_key):=GPA('gnutls_certificate_get_x509_key');
  pointer(gnutls_certificate_get_x509_crt):=GPA('gnutls_certificate_get_x509_crt');
  pointer(gnutls_certificate_set_ocsp_status_request_function):=GPA('gnutls_certificate_set_ocsp_status_request_function');
  pointer(gnutls_certificate_set_ocsp_status_request_file):=GPA('gnutls_certificate_set_ocsp_status_request_file');
  pointer(gnutls_ocsp_status_request_enable_client):=GPA('gnutls_ocsp_status_request_enable_client');
  pointer(gnutls_ocsp_status_request_get):=GPA('gnutls_ocsp_status_request_get');
  pointer(gnutls_ocsp_status_request_is_checked):=GPA('gnutls_ocsp_status_request_is_checked');
  pointer(gnutls_global_init):=GPA('gnutls_global_init');
  pointer(gnutls_global_deinit):=GPA('gnutls_global_deinit');
  pointer(gnutls_global_set_mutex):=GPA('gnutls_global_set_mutex');
  pointer(gnutls_global_set_time_function):=GPA('gnutls_global_set_time_function');
  pointer(gnutls_memset):=GPA('gnutls_memset');
  pointer(gnutls_memcmp):=GPA('gnutls_memcmp');
  pointer(gnutls_global_set_log_function):=GPA('gnutls_global_set_log_function');
  pointer(gnutls_global_set_audit_log_function):=GPA('gnutls_global_set_audit_log_function');
  pointer(gnutls_global_set_log_level):=GPA('gnutls_global_set_log_level');
  pointer(gnutls_dh_params_init):=GPA('gnutls_dh_params_init');
  pointer(gnutls_dh_params_deinit):=GPA('gnutls_dh_params_deinit');
  pointer(gnutls_dh_params_import_raw):=GPA('gnutls_dh_params_import_raw');
  pointer(gnutls_dh_params_import_raw2):=GPA('gnutls_dh_params_import_raw2');
  pointer(gnutls_dh_params_import_pkcs3):=GPA('gnutls_dh_params_import_pkcs3');
  pointer(gnutls_dh_params_generate2):=GPA('gnutls_dh_params_generate2');
  pointer(gnutls_dh_params_export_pkcs3):=GPA('gnutls_dh_params_export_pkcs3');
  pointer(gnutls_dh_params_export2_pkcs3):=GPA('gnutls_dh_params_export2_pkcs3');
  pointer(gnutls_dh_params_export_raw):=GPA('gnutls_dh_params_export_raw');
  pointer(gnutls_dh_params_cpy):=GPA('gnutls_dh_params_cpy');
  pointer(gnutls_system_recv_timeout):=GPA('gnutls_system_recv_timeout');
  pointer(gnutls_transport_set_int2):=GPA('gnutls_transport_set_int2');
  pointer(gnutls_transport_get_int2):=GPA('gnutls_transport_get_int2');
  pointer(gnutls_transport_get_int):=GPA('gnutls_transport_get_int');
  pointer(gnutls_transport_set_ptr):=GPA('gnutls_transport_set_ptr');
  pointer(gnutls_transport_set_ptr2):=GPA('gnutls_transport_set_ptr2');
  pointer(gnutls_transport_get_ptr):=GPA('gnutls_transport_get_ptr');
  pointer(gnutls_transport_get_ptr2):=GPA('gnutls_transport_get_ptr2');
  pointer(gnutls_transport_set_vec_push_function):=GPA('gnutls_transport_set_vec_push_function');
  pointer(gnutls_transport_set_push_function):=GPA('gnutls_transport_set_push_function');
  pointer(gnutls_transport_set_pull_function):=GPA('gnutls_transport_set_pull_function');
  pointer(gnutls_transport_set_pull_timeout_function):=GPA('gnutls_transport_set_pull_timeout_function');
  pointer(gnutls_transport_set_errno_function):=GPA('gnutls_transport_set_errno_function');
  pointer(gnutls_transport_set_errno):=GPA('gnutls_transport_set_errno');
  pointer(gnutls_session_set_ptr):=GPA('gnutls_session_set_ptr');
  pointer(gnutls_session_get_ptr):=GPA('gnutls_session_get_ptr');
  pointer(gnutls_openpgp_send_cert):=GPA('gnutls_openpgp_send_cert');
  pointer(gnutls_fingerprint):=GPA('gnutls_fingerprint');
  pointer(gnutls_random_art):=GPA('gnutls_random_art');
  pointer(gnutls_srp_free_client_credentials):=GPA('gnutls_srp_free_client_credentials');
  pointer(gnutls_srp_allocate_client_credentials):=GPA('gnutls_srp_allocate_client_credentials');
  pointer(gnutls_srp_set_client_credentials):=GPA('gnutls_srp_set_client_credentials');
  pointer(gnutls_srp_free_server_credentials):=GPA('gnutls_srp_free_server_credentials');
  pointer(gnutls_srp_allocate_server_credentials):=GPA('gnutls_srp_allocate_server_credentials');
  pointer(gnutls_srp_set_server_credentials_file):=GPA('gnutls_srp_set_server_credentials_file');
  pointer(gnutls_srp_server_get_username):=GPA('gnutls_srp_server_get_username');
  pointer(gnutls_srp_set_prime_bits):=GPA('gnutls_srp_set_prime_bits');
  pointer(gnutls_srp_verifier):=GPA('gnutls_srp_verifier');
  pointer(gnutls_srp_set_server_credentials_function):=GPA('gnutls_srp_set_server_credentials_function');
  pointer(gnutls_srp_set_client_credentials_function):=GPA('gnutls_srp_set_client_credentials_function');
  pointer(gnutls_srp_base64_encode):=GPA('gnutls_srp_base64_encode');
  pointer(gnutls_srp_base64_encode2):=GPA('gnutls_srp_base64_encode2');
  pointer(gnutls_srp_base64_decode):=GPA('gnutls_srp_base64_decode');
  pointer(gnutls_srp_base64_decode2):=GPA('gnutls_srp_base64_decode2');
  pointer(gnutls_srp_set_server_fake_salt_seed):=GPA('gnutls_srp_set_server_fake_salt_seed');
  pointer(gnutls_psk_free_client_credentials):=GPA('gnutls_psk_free_client_credentials');
  pointer(gnutls_psk_allocate_client_credentials):=GPA('gnutls_psk_allocate_client_credentials');
  pointer(gnutls_psk_set_client_credentials):=GPA('gnutls_psk_set_client_credentials');
  pointer(gnutls_psk_free_server_credentials):=GPA('gnutls_psk_free_server_credentials');
  pointer(gnutls_psk_allocate_server_credentials):=GPA('gnutls_psk_allocate_server_credentials');
  pointer(gnutls_psk_set_server_credentials_file):=GPA('gnutls_psk_set_server_credentials_file');
  pointer(gnutls_psk_set_server_credentials_hint):=GPA('gnutls_psk_set_server_credentials_hint');
  pointer(gnutls_psk_server_get_username):=GPA('gnutls_psk_server_get_username');
  pointer(gnutls_psk_client_get_hint):=GPA('gnutls_psk_client_get_hint');
  pointer(gnutls_psk_set_server_credentials_function):=GPA('gnutls_psk_set_server_credentials_function');
  pointer(gnutls_psk_set_client_credentials_function):=GPA('gnutls_psk_set_client_credentials_function');
  pointer(gnutls_hex_encode):=GPA('gnutls_hex_encode');
  pointer(gnutls_hex_decode):=GPA('gnutls_hex_decode');
  pointer(gnutls_hex_encode2):=GPA('gnutls_hex_encode2');
  pointer(gnutls_hex_decode2):=GPA('gnutls_hex_decode2');
  pointer(gnutls_psk_set_server_dh_params):=GPA('gnutls_psk_set_server_dh_params');
  pointer(gnutls_psk_set_server_params_function):=GPA('gnutls_psk_set_server_params_function');
  pointer(gnutls_auth_get_type):=GPA('gnutls_auth_get_type');
  pointer(gnutls_auth_server_get_type):=GPA('gnutls_auth_server_get_type');
  pointer(gnutls_auth_client_get_type):=GPA('gnutls_auth_client_get_type');
  pointer(gnutls_dh_set_prime_bits):=GPA('gnutls_dh_set_prime_bits');
  pointer(gnutls_dh_get_secret_bits):=GPA('gnutls_dh_get_secret_bits');
  pointer(gnutls_dh_get_peers_public_bits):=GPA('gnutls_dh_get_peers_public_bits');
  pointer(gnutls_dh_get_prime_bits):=GPA('gnutls_dh_get_prime_bits');
  pointer(gnutls_dh_get_group):=GPA('gnutls_dh_get_group');
  pointer(gnutls_dh_get_pubkey):=GPA('gnutls_dh_get_pubkey');
  pointer(gnutls_certificate_set_retrieve_function):=GPA('gnutls_certificate_set_retrieve_function');
  pointer(gnutls_certificate_set_verify_function):=GPA('gnutls_certificate_set_verify_function');
  pointer(gnutls_certificate_server_set_request):=GPA('gnutls_certificate_server_set_request');
  pointer(gnutls_certificate_get_peers):=GPA('gnutls_certificate_get_peers');
  pointer(gnutls_certificate_get_ours):=GPA('gnutls_certificate_get_ours');
  pointer(gnutls_certificate_get_peers_subkey_id):=GPA('gnutls_certificate_get_peers_subkey_id');
  pointer(gnutls_certificate_activation_time_peers):=GPA('gnutls_certificate_activation_time_peers');
  pointer(gnutls_certificate_expiration_time_peers):=GPA('gnutls_certificate_expiration_time_peers');
  pointer(gnutls_certificate_client_get_request_status):=GPA('gnutls_certificate_client_get_request_status');
  pointer(gnutls_certificate_verify_peers2):=GPA('gnutls_certificate_verify_peers2');
  pointer(gnutls_certificate_verify_peers3):=GPA('gnutls_certificate_verify_peers3');
  pointer(gnutls_certificate_verify_peers):=GPA('gnutls_certificate_verify_peers');
  pointer(gnutls_certificate_verification_status_print):=GPA('gnutls_certificate_verification_status_print');
  pointer(gnutls_pem_base64_encode):=GPA('gnutls_pem_base64_encode');
  pointer(gnutls_pem_base64_decode):=GPA('gnutls_pem_base64_decode');
  pointer(gnutls_pem_base64_encode2):=GPA('gnutls_pem_base64_encode2');
  pointer(gnutls_pem_base64_decode2):=GPA('gnutls_pem_base64_decode2');
  pointer(gnutls_certificate_set_params_function):=GPA('gnutls_certificate_set_params_function');
  pointer(gnutls_anon_set_params_function):=GPA('gnutls_anon_set_params_function');
  pointer(gnutls_psk_set_params_function):=GPA('gnutls_psk_set_params_function');
  pointer(gnutls_hex2bin):=GPA('gnutls_hex2bin');
  pointer(gnutls_tdb_init):=GPA('gnutls_tdb_init');
  pointer(gnutls_tdb_set_store_func):=GPA('gnutls_tdb_set_store_func');
  pointer(gnutls_tdb_set_store_commitment_func):=GPA('gnutls_tdb_set_store_commitment_func');
  pointer(gnutls_tdb_set_verify_func):=GPA('gnutls_tdb_set_verify_func');
  pointer(gnutls_tdb_deinit):=GPA('gnutls_tdb_deinit');
  pointer(gnutls_verify_stored_pubkey):=GPA('gnutls_verify_stored_pubkey');
  pointer(gnutls_store_commitment):=GPA('gnutls_store_commitment');
  pointer(gnutls_store_pubkey):=GPA('gnutls_store_pubkey');
  pointer(gnutls_load_file):=GPA('gnutls_load_file');
  pointer(gnutls_url_is_supported):=GPA('gnutls_url_is_supported');
  pointer(gnutls_certificate_set_pin_function):=GPA('gnutls_certificate_set_pin_function');
  pointer(gnutls_buffer_append_data):=GPA('gnutls_buffer_append_data');
  pointer(gnutls_ext_set_data):=GPA('gnutls_ext_set_data');
  pointer(gnutls_ext_get_data):=GPA('gnutls_ext_get_data');
  pointer(gnutls_ext_register):=GPA('gnutls_ext_register');
  pointer(gnutls_supplemental_register):=GPA('gnutls_supplemental_register');
  pointer(gnutls_supplemental_recv):=GPA('gnutls_supplemental_recv');
  pointer(gnutls_supplemental_send):=GPA('gnutls_supplemental_send');
  pointer(gnutls_fips140_mode_enabled):=GPA('gnutls_fips140_mode_enabled');
  pointer(gnutls_x509_crt_init):=GPA('gnutls_x509_crt_init');
  pointer(gnutls_x509_crt_deinit):=GPA('gnutls_x509_crt_deinit');
  pointer(gnutls_x509_crt_import):=GPA('gnutls_x509_crt_import');
  pointer(gnutls_x509_crt_list_import2):=GPA('gnutls_x509_crt_list_import2');
  pointer(gnutls_x509_crt_list_import):=GPA('gnutls_x509_crt_list_import');
  pointer(gnutls_x509_crt_import_url):=GPA('gnutls_x509_crt_import_url');
  pointer(gnutls_x509_crt_export):=GPA('gnutls_x509_crt_export');
  pointer(gnutls_x509_crt_export2):=GPA('gnutls_x509_crt_export2');
  pointer(gnutls_x509_crt_get_private_key_usage_period):=GPA('gnutls_x509_crt_get_private_key_usage_period');
  pointer(gnutls_x509_crt_get_issuer_dn):=GPA('gnutls_x509_crt_get_issuer_dn');
  pointer(gnutls_x509_crt_get_issuer_dn2):=GPA('gnutls_x509_crt_get_issuer_dn2');
  pointer(gnutls_x509_crt_get_issuer_dn_oid):=GPA('gnutls_x509_crt_get_issuer_dn_oid');
  pointer(gnutls_x509_crt_get_issuer_dn_by_oid):=GPA('gnutls_x509_crt_get_issuer_dn_by_oid');
  pointer(gnutls_x509_crt_get_dn):=GPA('gnutls_x509_crt_get_dn');
  pointer(gnutls_x509_crt_get_dn2):=GPA('gnutls_x509_crt_get_dn2');
  pointer(gnutls_x509_crt_get_dn_oid):=GPA('gnutls_x509_crt_get_dn_oid');
  pointer(gnutls_x509_crt_get_dn_by_oid):=GPA('gnutls_x509_crt_get_dn_by_oid');
  pointer(gnutls_x509_crt_check_hostname):=GPA('gnutls_x509_crt_check_hostname');
  pointer(gnutls_x509_crt_check_hostname2):=GPA('gnutls_x509_crt_check_hostname2');
  pointer(gnutls_x509_crt_check_email):=GPA('gnutls_x509_crt_check_email');
  pointer(gnutls_x509_crt_get_signature_algorithm):=GPA('gnutls_x509_crt_get_signature_algorithm');
  pointer(gnutls_x509_crt_get_signature):=GPA('gnutls_x509_crt_get_signature');
  pointer(gnutls_x509_crt_get_version):=GPA('gnutls_x509_crt_get_version');
  pointer(gnutls_x509_crt_get_key_id):=GPA('gnutls_x509_crt_get_key_id');
  pointer(gnutls_x509_crt_set_private_key_usage_period):=GPA('gnutls_x509_crt_set_private_key_usage_period');
  pointer(gnutls_x509_crt_set_authority_key_id):=GPA('gnutls_x509_crt_set_authority_key_id');
  pointer(gnutls_x509_crt_get_authority_key_id):=GPA('gnutls_x509_crt_get_authority_key_id');
  pointer(gnutls_x509_crt_get_authority_key_gn_serial):=GPA('gnutls_x509_crt_get_authority_key_gn_serial');
  pointer(gnutls_x509_crt_get_subject_key_id):=GPA('gnutls_x509_crt_get_subject_key_id');
  pointer(gnutls_x509_crt_get_subject_unique_id):=GPA('gnutls_x509_crt_get_subject_unique_id');
  pointer(gnutls_x509_crt_get_issuer_unique_id):=GPA('gnutls_x509_crt_get_issuer_unique_id');
  pointer(gnutls_x509_crt_set_pin_function):=GPA('gnutls_x509_crt_set_pin_function');
  pointer(gnutls_x509_crt_get_authority_info_access):=GPA('gnutls_x509_crt_get_authority_info_access');
  pointer(gnutls_x509_name_constraints_check):=GPA('gnutls_x509_name_constraints_check');
  pointer(gnutls_x509_name_constraints_check_crt):=GPA('gnutls_x509_name_constraints_check_crt');
  pointer(gnutls_x509_name_constraints_init):=GPA('gnutls_x509_name_constraints_init');
  pointer(gnutls_x509_name_constraints_deinit):=GPA('gnutls_x509_name_constraints_deinit');
  pointer(gnutls_x509_crt_get_name_constraints):=GPA('gnutls_x509_crt_get_name_constraints');
  pointer(gnutls_x509_name_constraints_add_permitted):=GPA('gnutls_x509_name_constraints_add_permitted');
  pointer(gnutls_x509_name_constraints_add_excluded):=GPA('gnutls_x509_name_constraints_add_excluded');
  pointer(gnutls_x509_crt_set_name_constraints):=GPA('gnutls_x509_crt_set_name_constraints');
  pointer(gnutls_x509_name_constraints_get_permitted):=GPA('gnutls_x509_name_constraints_get_permitted');
  pointer(gnutls_x509_name_constraints_get_excluded):=GPA('gnutls_x509_name_constraints_get_excluded');
  pointer(gnutls_x509_crt_get_crl_dist_points):=GPA('gnutls_x509_crt_get_crl_dist_points');
  pointer(gnutls_x509_crt_set_crl_dist_points2):=GPA('gnutls_x509_crt_set_crl_dist_points2');
  pointer(gnutls_x509_crt_set_crl_dist_points):=GPA('gnutls_x509_crt_set_crl_dist_points');
  pointer(gnutls_x509_crt_cpy_crl_dist_points):=GPA('gnutls_x509_crt_cpy_crl_dist_points');
  pointer(gnutls_x509_crl_sign2):=GPA('gnutls_x509_crl_sign2');
  pointer(gnutls_x509_crt_get_activation_time):=GPA('gnutls_x509_crt_get_activation_time');
  pointer(gnutls_x509_crt_get_expiration_time):=GPA('gnutls_x509_crt_get_expiration_time');
  pointer(gnutls_x509_crt_get_serial):=GPA('gnutls_x509_crt_get_serial');
  pointer(gnutls_x509_crt_get_pk_algorithm):=GPA('gnutls_x509_crt_get_pk_algorithm');
  pointer(gnutls_x509_crt_get_pk_rsa_raw):=GPA('gnutls_x509_crt_get_pk_rsa_raw');
  pointer(gnutls_x509_crt_get_pk_dsa_raw):=GPA('gnutls_x509_crt_get_pk_dsa_raw');
  pointer(gnutls_x509_crt_get_pk_ecc_raw):=GPA('gnutls_x509_crt_get_pk_ecc_raw');
  pointer(gnutls_x509_crt_get_subject_alt_name):=GPA('gnutls_x509_crt_get_subject_alt_name');
  pointer(gnutls_x509_crt_get_subject_alt_name2):=GPA('gnutls_x509_crt_get_subject_alt_name2');
  pointer(gnutls_x509_crt_get_subject_alt_othername_oid):=GPA('gnutls_x509_crt_get_subject_alt_othername_oid');
  pointer(gnutls_x509_crt_get_issuer_alt_name):=GPA('gnutls_x509_crt_get_issuer_alt_name');
  pointer(gnutls_x509_crt_get_issuer_alt_name2):=GPA('gnutls_x509_crt_get_issuer_alt_name2');
  pointer(gnutls_x509_crt_get_issuer_alt_othername_oid):=GPA('gnutls_x509_crt_get_issuer_alt_othername_oid');
  pointer(gnutls_x509_crt_get_ca_status):=GPA('gnutls_x509_crt_get_ca_status');
  pointer(gnutls_x509_crt_get_basic_constraints):=GPA('gnutls_x509_crt_get_basic_constraints');
  pointer(gnutls_x509_crt_get_key_usage):=GPA('gnutls_x509_crt_get_key_usage');
  pointer(gnutls_x509_crt_set_key_usage):=GPA('gnutls_x509_crt_set_key_usage');
  pointer(gnutls_x509_crt_set_authority_info_access):=GPA('gnutls_x509_crt_set_authority_info_access');
  pointer(gnutls_x509_crt_get_proxy):=GPA('gnutls_x509_crt_get_proxy');
  pointer(gnutls_x509_policy_release):=GPA('gnutls_x509_policy_release');
  pointer(gnutls_x509_crt_get_policy):=GPA('gnutls_x509_crt_get_policy');
  pointer(gnutls_x509_crt_set_policy):=GPA('gnutls_x509_crt_set_policy');
  pointer(gnutls_x509_dn_oid_known):=GPA('gnutls_x509_dn_oid_known');
  pointer(gnutls_x509_dn_oid_name):=GPA('gnutls_x509_dn_oid_name');
  pointer(gnutls_x509_crt_get_extension_oid):=GPA('gnutls_x509_crt_get_extension_oid');
  pointer(gnutls_x509_crt_get_extension_by_oid):=GPA('gnutls_x509_crt_get_extension_by_oid');
  pointer(gnutls_x509_crq_get_signature_algorithm):=GPA('gnutls_x509_crq_get_signature_algorithm');
  pointer(gnutls_x509_crq_get_extension_by_oid2):=GPA('gnutls_x509_crq_get_extension_by_oid2');
  pointer(gnutls_x509_crt_get_extension_info):=GPA('gnutls_x509_crt_get_extension_info');
  pointer(gnutls_x509_crt_get_extension_data):=GPA('gnutls_x509_crt_get_extension_data');
  pointer(gnutls_x509_crt_get_extension_data2):=GPA('gnutls_x509_crt_get_extension_data2');
  pointer(gnutls_x509_crt_set_extension_by_oid):=GPA('gnutls_x509_crt_set_extension_by_oid');
  pointer(gnutls_x509_crt_set_dn):=GPA('gnutls_x509_crt_set_dn');
  pointer(gnutls_x509_crt_set_dn_by_oid):=GPA('gnutls_x509_crt_set_dn_by_oid');
  pointer(gnutls_x509_crt_set_issuer_dn_by_oid):=GPA('gnutls_x509_crt_set_issuer_dn_by_oid');
  pointer(gnutls_x509_crt_set_issuer_dn):=GPA('gnutls_x509_crt_set_issuer_dn');
  pointer(gnutls_x509_crt_set_version):=GPA('gnutls_x509_crt_set_version');
  pointer(gnutls_x509_crt_set_key):=GPA('gnutls_x509_crt_set_key');
  pointer(gnutls_x509_crt_set_ca_status):=GPA('gnutls_x509_crt_set_ca_status');
  pointer(gnutls_x509_crt_set_basic_constraints):=GPA('gnutls_x509_crt_set_basic_constraints');
  pointer(gnutls_x509_crt_set_subject_unique_id):=GPA('gnutls_x509_crt_set_subject_unique_id');
  pointer(gnutls_x509_crt_set_issuer_unique_id):=GPA('gnutls_x509_crt_set_issuer_unique_id');
  pointer(gnutls_x509_crt_set_subject_alternative_name):=GPA('gnutls_x509_crt_set_subject_alternative_name');
  pointer(gnutls_x509_crt_set_subject_alt_name):=GPA('gnutls_x509_crt_set_subject_alt_name');
  pointer(gnutls_x509_crt_set_issuer_alt_name):=GPA('gnutls_x509_crt_set_issuer_alt_name');
  pointer(gnutls_x509_crt_sign):=GPA('gnutls_x509_crt_sign');
  pointer(gnutls_x509_crt_sign2):=GPA('gnutls_x509_crt_sign2');
  pointer(gnutls_x509_crt_set_activation_time):=GPA('gnutls_x509_crt_set_activation_time');
  pointer(gnutls_x509_crt_set_expiration_time):=GPA('gnutls_x509_crt_set_expiration_time');
  pointer(gnutls_x509_crt_set_serial):=GPA('gnutls_x509_crt_set_serial');
  pointer(gnutls_x509_crt_set_subject_key_id):=GPA('gnutls_x509_crt_set_subject_key_id');
  pointer(gnutls_x509_crt_set_proxy_dn):=GPA('gnutls_x509_crt_set_proxy_dn');
  pointer(gnutls_x509_crt_set_proxy):=GPA('gnutls_x509_crt_set_proxy');
  pointer(gnutls_x509_crt_print):=GPA('gnutls_x509_crt_print');
  pointer(gnutls_x509_crl_print):=GPA('gnutls_x509_crl_print');
  pointer(gnutls_x509_crt_get_raw_issuer_dn):=GPA('gnutls_x509_crt_get_raw_issuer_dn');
  pointer(gnutls_x509_crt_get_raw_dn):=GPA('gnutls_x509_crt_get_raw_dn');
  pointer(gnutls_x509_rdn_get):=GPA('gnutls_x509_rdn_get');
  pointer(gnutls_x509_rdn_get_oid):=GPA('gnutls_x509_rdn_get_oid');
  pointer(gnutls_x509_rdn_get_by_oid):=GPA('gnutls_x509_rdn_get_by_oid');
  pointer(gnutls_x509_crt_get_subject):=GPA('gnutls_x509_crt_get_subject');
  pointer(gnutls_x509_crt_get_issuer):=GPA('gnutls_x509_crt_get_issuer');
  pointer(gnutls_x509_dn_get_rdn_ava):=GPA('gnutls_x509_dn_get_rdn_ava');
  pointer(gnutls_x509_dn_get_str):=GPA('gnutls_x509_dn_get_str');
  pointer(gnutls_x509_dn_init):=GPA('gnutls_x509_dn_init');
  pointer(gnutls_x509_dn_import):=GPA('gnutls_x509_dn_import');
  pointer(gnutls_x509_dn_export):=GPA('gnutls_x509_dn_export');
  pointer(gnutls_x509_dn_export2):=GPA('gnutls_x509_dn_export2');
  pointer(gnutls_x509_dn_deinit):=GPA('gnutls_x509_dn_deinit');
  pointer(gnutls_x509_crl_init):=GPA('gnutls_x509_crl_init');
  pointer(gnutls_x509_crl_deinit):=GPA('gnutls_x509_crl_deinit');
  pointer(gnutls_x509_crl_import):=GPA('gnutls_x509_crl_import');
  pointer(gnutls_x509_crl_export):=GPA('gnutls_x509_crl_export');
  pointer(gnutls_x509_crl_export2):=GPA('gnutls_x509_crl_export2');
  pointer(gnutls_x509_crl_get_raw_issuer_dn):=GPA('gnutls_x509_crl_get_raw_issuer_dn');
  pointer(gnutls_x509_crl_get_issuer_dn):=GPA('gnutls_x509_crl_get_issuer_dn');
  pointer(gnutls_x509_crl_get_issuer_dn2):=GPA('gnutls_x509_crl_get_issuer_dn2');
  pointer(gnutls_x509_crl_get_issuer_dn_by_oid):=GPA('gnutls_x509_crl_get_issuer_dn_by_oid');
  pointer(gnutls_x509_crl_get_dn_oid):=GPA('gnutls_x509_crl_get_dn_oid');
  pointer(gnutls_x509_crl_get_signature_algorithm):=GPA('gnutls_x509_crl_get_signature_algorithm');
  pointer(gnutls_x509_crl_get_signature):=GPA('gnutls_x509_crl_get_signature');
  pointer(gnutls_x509_crl_get_version):=GPA('gnutls_x509_crl_get_version');
  pointer(gnutls_x509_crl_get_this_update):=GPA('gnutls_x509_crl_get_this_update');
  pointer(gnutls_x509_crl_get_next_update):=GPA('gnutls_x509_crl_get_next_update');
  pointer(gnutls_x509_crl_get_crt_count):=GPA('gnutls_x509_crl_get_crt_count');
  pointer(gnutls_x509_crl_get_crt_serial):=GPA('gnutls_x509_crl_get_crt_serial');
  pointer(gnutls_x509_crl_iter_crt_serial):=GPA('gnutls_x509_crl_iter_crt_serial');
  pointer(gnutls_x509_crl_iter_deinit):=GPA('gnutls_x509_crl_iter_deinit');
  pointer(gnutls_x509_crl_check_issuer):=GPA('gnutls_x509_crl_check_issuer');
  pointer(gnutls_x509_crl_list_import2):=GPA('gnutls_x509_crl_list_import2');
  pointer(gnutls_x509_crl_list_import):=GPA('gnutls_x509_crl_list_import');
  pointer(gnutls_x509_crl_set_version):=GPA('gnutls_x509_crl_set_version');
  pointer(gnutls_x509_crl_set_this_update):=GPA('gnutls_x509_crl_set_this_update');
  pointer(gnutls_x509_crl_set_next_update):=GPA('gnutls_x509_crl_set_next_update');
  pointer(gnutls_x509_crl_set_crt_serial):=GPA('gnutls_x509_crl_set_crt_serial');
  pointer(gnutls_x509_crl_set_crt):=GPA('gnutls_x509_crl_set_crt');
  pointer(gnutls_x509_crl_get_authority_key_id):=GPA('gnutls_x509_crl_get_authority_key_id');
  pointer(gnutls_x509_crl_get_authority_key_gn_serial):=GPA('gnutls_x509_crl_get_authority_key_gn_serial');
  pointer(gnutls_x509_crl_get_number):=GPA('gnutls_x509_crl_get_number');
  pointer(gnutls_x509_crl_get_extension_oid):=GPA('gnutls_x509_crl_get_extension_oid');
  pointer(gnutls_x509_crl_get_extension_info):=GPA('gnutls_x509_crl_get_extension_info');
  pointer(gnutls_x509_crl_get_extension_data):=GPA('gnutls_x509_crl_get_extension_data');
  pointer(gnutls_x509_crl_get_extension_data2):=GPA('gnutls_x509_crl_get_extension_data2');
  pointer(gnutls_x509_crl_set_authority_key_id):=GPA('gnutls_x509_crl_set_authority_key_id');
  pointer(gnutls_x509_crl_set_number):=GPA('gnutls_x509_crl_set_number');
  pointer(gnutls_x509_crt_check_issuer):=GPA('gnutls_x509_crt_check_issuer');
  pointer(gnutls_x509_crt_list_verify):=GPA('gnutls_x509_crt_list_verify');
  pointer(gnutls_x509_crt_verify):=GPA('gnutls_x509_crt_verify');
  pointer(gnutls_x509_crl_verify):=GPA('gnutls_x509_crl_verify');
  pointer(gnutls_x509_crt_verify_data2):=GPA('gnutls_x509_crt_verify_data2');
  pointer(gnutls_x509_crt_check_revocation):=GPA('gnutls_x509_crt_check_revocation');
  pointer(gnutls_x509_crt_get_fingerprint):=GPA('gnutls_x509_crt_get_fingerprint');
  pointer(gnutls_x509_crt_get_key_purpose_oid):=GPA('gnutls_x509_crt_get_key_purpose_oid');
  pointer(gnutls_x509_crt_set_key_purpose_oid):=GPA('gnutls_x509_crt_set_key_purpose_oid');
  pointer(gnutls_pkcs_schema_get_name):=GPA('gnutls_pkcs_schema_get_name');
  pointer(gnutls_pkcs_schema_get_oid):=GPA('gnutls_pkcs_schema_get_oid');
  pointer(gnutls_x509_privkey_init):=GPA('gnutls_x509_privkey_init');
  pointer(gnutls_x509_privkey_deinit):=GPA('gnutls_x509_privkey_deinit');
  pointer(gnutls_x509_privkey_sec_param):=GPA('gnutls_x509_privkey_sec_param');
  pointer(gnutls_x509_privkey_set_pin_function):=GPA('gnutls_x509_privkey_set_pin_function');
  pointer(gnutls_x509_privkey_cpy):=GPA('gnutls_x509_privkey_cpy');
  pointer(gnutls_x509_privkey_import):=GPA('gnutls_x509_privkey_import');
  pointer(gnutls_x509_privkey_import_pkcs8):=GPA('gnutls_x509_privkey_import_pkcs8');
  pointer(gnutls_x509_privkey_import_openssl):=GPA('gnutls_x509_privkey_import_openssl');
  pointer(gnutls_pkcs8_info):=GPA('gnutls_pkcs8_info');
  pointer(gnutls_x509_privkey_import2):=GPA('gnutls_x509_privkey_import2');
  pointer(gnutls_x509_privkey_import_rsa_raw):=GPA('gnutls_x509_privkey_import_rsa_raw');
  pointer(gnutls_x509_privkey_import_rsa_raw2):=GPA('gnutls_x509_privkey_import_rsa_raw2');
  pointer(gnutls_x509_privkey_import_ecc_raw):=GPA('gnutls_x509_privkey_import_ecc_raw');
  pointer(gnutls_x509_privkey_fix):=GPA('gnutls_x509_privkey_fix');
  pointer(gnutls_x509_privkey_export_dsa_raw):=GPA('gnutls_x509_privkey_export_dsa_raw');
  pointer(gnutls_x509_privkey_import_dsa_raw):=GPA('gnutls_x509_privkey_import_dsa_raw');
  pointer(gnutls_x509_privkey_get_pk_algorithm):=GPA('gnutls_x509_privkey_get_pk_algorithm');
  pointer(gnutls_x509_privkey_get_pk_algorithm2):=GPA('gnutls_x509_privkey_get_pk_algorithm2');
  pointer(gnutls_x509_privkey_get_key_id):=GPA('gnutls_x509_privkey_get_key_id');
  pointer(gnutls_x509_privkey_generate):=GPA('gnutls_x509_privkey_generate');
  pointer(gnutls_x509_privkey_verify_params):=GPA('gnutls_x509_privkey_verify_params');
  pointer(gnutls_x509_privkey_export):=GPA('gnutls_x509_privkey_export');
  pointer(gnutls_x509_privkey_export2):=GPA('gnutls_x509_privkey_export2');
  pointer(gnutls_x509_privkey_export_pkcs8):=GPA('gnutls_x509_privkey_export_pkcs8');
  pointer(gnutls_x509_privkey_export2_pkcs8):=GPA('gnutls_x509_privkey_export2_pkcs8');
  pointer(gnutls_x509_privkey_export_rsa_raw2):=GPA('gnutls_x509_privkey_export_rsa_raw2');
  pointer(gnutls_x509_privkey_export_rsa_raw):=GPA('gnutls_x509_privkey_export_rsa_raw');
  pointer(gnutls_x509_privkey_export_ecc_raw):=GPA('gnutls_x509_privkey_export_ecc_raw');
  pointer(gnutls_x509_privkey_sign_data):=GPA('gnutls_x509_privkey_sign_data');
  pointer(gnutls_x509_crq_sign2):=GPA('gnutls_x509_crq_sign2');
  pointer(gnutls_x509_crq_print):=GPA('gnutls_x509_crq_print');
  pointer(gnutls_x509_crq_verify):=GPA('gnutls_x509_crq_verify');
  pointer(gnutls_x509_crq_init):=GPA('gnutls_x509_crq_init');
  pointer(gnutls_x509_crq_deinit):=GPA('gnutls_x509_crq_deinit');
  pointer(gnutls_x509_crq_import):=GPA('gnutls_x509_crq_import');
  pointer(gnutls_x509_crq_get_private_key_usage_period):=GPA('gnutls_x509_crq_get_private_key_usage_period');
  pointer(gnutls_x509_crq_get_dn):=GPA('gnutls_x509_crq_get_dn');
  pointer(gnutls_x509_crq_get_dn2):=GPA('gnutls_x509_crq_get_dn2');
  pointer(gnutls_x509_crq_get_dn_oid):=GPA('gnutls_x509_crq_get_dn_oid');
  pointer(gnutls_x509_crq_get_dn_by_oid):=GPA('gnutls_x509_crq_get_dn_by_oid');
  pointer(gnutls_x509_crq_set_dn):=GPA('gnutls_x509_crq_set_dn');
  pointer(gnutls_x509_crq_set_dn_by_oid):=GPA('gnutls_x509_crq_set_dn_by_oid');
  pointer(gnutls_x509_crq_set_version):=GPA('gnutls_x509_crq_set_version');
  pointer(gnutls_x509_crq_get_version):=GPA('gnutls_x509_crq_get_version');
  pointer(gnutls_x509_crq_set_key):=GPA('gnutls_x509_crq_set_key');
  pointer(gnutls_x509_crq_set_challenge_password):=GPA('gnutls_x509_crq_set_challenge_password');
  pointer(gnutls_x509_crq_get_challenge_password):=GPA('gnutls_x509_crq_get_challenge_password');
  pointer(gnutls_x509_crq_set_attribute_by_oid):=GPA('gnutls_x509_crq_set_attribute_by_oid');
  pointer(gnutls_x509_crq_get_attribute_by_oid):=GPA('gnutls_x509_crq_get_attribute_by_oid');
  pointer(gnutls_x509_crq_export):=GPA('gnutls_x509_crq_export');
  pointer(gnutls_x509_crq_export2):=GPA('gnutls_x509_crq_export2');
  pointer(gnutls_x509_crt_set_crq):=GPA('gnutls_x509_crt_set_crq');
  pointer(gnutls_x509_crt_set_crq_extensions):=GPA('gnutls_x509_crt_set_crq_extensions');
  pointer(gnutls_x509_crq_set_private_key_usage_period):=GPA('gnutls_x509_crq_set_private_key_usage_period');
  pointer(gnutls_x509_crq_set_key_rsa_raw):=GPA('gnutls_x509_crq_set_key_rsa_raw');
  pointer(gnutls_x509_crq_set_subject_alt_name):=GPA('gnutls_x509_crq_set_subject_alt_name');
  pointer(gnutls_x509_crq_set_key_usage):=GPA('gnutls_x509_crq_set_key_usage');
  pointer(gnutls_x509_crq_set_basic_constraints):=GPA('gnutls_x509_crq_set_basic_constraints');
  pointer(gnutls_x509_crq_set_key_purpose_oid):=GPA('gnutls_x509_crq_set_key_purpose_oid');
  pointer(gnutls_x509_crq_get_key_purpose_oid):=GPA('gnutls_x509_crq_get_key_purpose_oid');
  pointer(gnutls_x509_crq_get_extension_data):=GPA('gnutls_x509_crq_get_extension_data');
  pointer(gnutls_x509_crq_get_extension_data2):=GPA('gnutls_x509_crq_get_extension_data2');
  pointer(gnutls_x509_crq_get_extension_info):=GPA('gnutls_x509_crq_get_extension_info');
  pointer(gnutls_x509_crq_get_attribute_data):=GPA('gnutls_x509_crq_get_attribute_data');
  pointer(gnutls_x509_crq_get_attribute_info):=GPA('gnutls_x509_crq_get_attribute_info');
  pointer(gnutls_x509_crq_get_pk_algorithm):=GPA('gnutls_x509_crq_get_pk_algorithm');
  pointer(gnutls_x509_crq_get_key_id):=GPA('gnutls_x509_crq_get_key_id');
  pointer(gnutls_x509_crq_get_key_rsa_raw):=GPA('gnutls_x509_crq_get_key_rsa_raw');
  pointer(gnutls_x509_crq_get_key_usage):=GPA('gnutls_x509_crq_get_key_usage');
  pointer(gnutls_x509_crq_get_basic_constraints):=GPA('gnutls_x509_crq_get_basic_constraints');
  pointer(gnutls_x509_crq_get_subject_alt_name):=GPA('gnutls_x509_crq_get_subject_alt_name');
  pointer(gnutls_x509_crq_get_subject_alt_othername_oid):=GPA('gnutls_x509_crq_get_subject_alt_othername_oid');
  pointer(gnutls_x509_crq_get_extension_by_oid):=GPA('gnutls_x509_crq_get_extension_by_oid');
  pointer(gnutls_x509_crt_get_extension_by_oid2):=GPA('gnutls_x509_crt_get_extension_by_oid2');
  pointer(gnutls_x509_trust_list_init):=GPA('gnutls_x509_trust_list_init');
  pointer(gnutls_x509_trust_list_deinit):=GPA('gnutls_x509_trust_list_deinit');
  pointer(gnutls_x509_trust_list_get_issuer):=GPA('gnutls_x509_trust_list_get_issuer');
  pointer(gnutls_x509_trust_list_get_issuer_by_dn):=GPA('gnutls_x509_trust_list_get_issuer_by_dn');
  pointer(gnutls_x509_trust_list_get_issuer_by_subject_key_id):=GPA('gnutls_x509_trust_list_get_issuer_by_subject_key_id');
  pointer(gnutls_x509_trust_list_add_cas):=GPA('gnutls_x509_trust_list_add_cas');
  pointer(gnutls_x509_trust_list_remove_cas):=GPA('gnutls_x509_trust_list_remove_cas');
  pointer(gnutls_x509_trust_list_add_named_crt):=GPA('gnutls_x509_trust_list_add_named_crt');
  pointer(gnutls_x509_trust_list_add_crls):=GPA('gnutls_x509_trust_list_add_crls');
  pointer(gnutls_x509_trust_list_iter_get_ca):=GPA('gnutls_x509_trust_list_iter_get_ca');
  pointer(gnutls_x509_trust_list_iter_deinit):=GPA('gnutls_x509_trust_list_iter_deinit');
  pointer(gnutls_x509_trust_list_verify_named_crt):=GPA('gnutls_x509_trust_list_verify_named_crt');
  pointer(gnutls_x509_trust_list_verify_crt2):=GPA('gnutls_x509_trust_list_verify_crt2');
  pointer(gnutls_x509_trust_list_verify_crt):=GPA('gnutls_x509_trust_list_verify_crt');
  pointer(gnutls_x509_trust_list_add_trust_mem):=GPA('gnutls_x509_trust_list_add_trust_mem');
  pointer(gnutls_x509_trust_list_add_trust_file):=GPA('gnutls_x509_trust_list_add_trust_file');
  pointer(gnutls_x509_trust_list_add_trust_dir):=GPA('gnutls_x509_trust_list_add_trust_dir');
  pointer(gnutls_x509_trust_list_remove_trust_file):=GPA('gnutls_x509_trust_list_remove_trust_file');
  pointer(gnutls_x509_trust_list_remove_trust_mem):=GPA('gnutls_x509_trust_list_remove_trust_mem');
  pointer(gnutls_x509_trust_list_add_system_trust):=GPA('gnutls_x509_trust_list_add_system_trust');
  pointer(gnutls_certificate_set_trust_list):=GPA('gnutls_certificate_set_trust_list');
  pointer(gnutls_certificate_get_trust_list):=GPA('gnutls_certificate_get_trust_list');
  pointer(gnutls_x509_ext_deinit):=GPA('gnutls_x509_ext_deinit');
  pointer(gnutls_x509_ext_print):=GPA('gnutls_x509_ext_print');
  pointer(gnutls_pkcs7_init):=GPA('gnutls_pkcs7_init');
  pointer(gnutls_pkcs7_deinit):=GPA('gnutls_pkcs7_deinit');
  pointer(gnutls_pkcs7_import):=GPA('gnutls_pkcs7_import');
  pointer(gnutls_pkcs7_export):=GPA('gnutls_pkcs7_export');
  pointer(gnutls_pkcs7_export2):=GPA('gnutls_pkcs7_export2');
  pointer(gnutls_pkcs7_get_signature_count):=GPA('gnutls_pkcs7_get_signature_count');
  pointer(gnutls_pkcs7_get_embedded_data):=GPA('gnutls_pkcs7_get_embedded_data');
  pointer(gnutls_pkcs7_get_crt_count):=GPA('gnutls_pkcs7_get_crt_count');
  pointer(gnutls_pkcs7_get_crt_raw):=GPA('gnutls_pkcs7_get_crt_raw');
  pointer(gnutls_pkcs7_set_crt_raw):=GPA('gnutls_pkcs7_set_crt_raw');
  pointer(gnutls_pkcs7_set_crt):=GPA('gnutls_pkcs7_set_crt');
  pointer(gnutls_pkcs7_delete_crt):=GPA('gnutls_pkcs7_delete_crt');
  pointer(gnutls_pkcs7_get_crl_raw):=GPA('gnutls_pkcs7_get_crl_raw');
  pointer(gnutls_pkcs7_get_crl_count):=GPA('gnutls_pkcs7_get_crl_count');
  pointer(gnutls_pkcs7_set_crl_raw):=GPA('gnutls_pkcs7_set_crl_raw');
  pointer(gnutls_pkcs7_set_crl):=GPA('gnutls_pkcs7_set_crl');
  pointer(gnutls_pkcs7_delete_crl):=GPA('gnutls_pkcs7_delete_crl');
  pointer(gnutls_pkcs7_signature_info_deinit):=GPA('gnutls_pkcs7_signature_info_deinit');
  pointer(gnutls_pkcs7_get_signature_info):=GPA('gnutls_pkcs7_get_signature_info');
  pointer(gnutls_pkcs7_verify_direct):=GPA('gnutls_pkcs7_verify_direct');
  pointer(gnutls_pkcs7_verify):=GPA('gnutls_pkcs7_verify');
  pointer(gnutls_pkcs7_add_attr):=GPA('gnutls_pkcs7_add_attr');
  pointer(gnutls_pkcs7_attrs_deinit):=GPA('gnutls_pkcs7_attrs_deinit');
  pointer(gnutls_pkcs7_get_attr):=GPA('gnutls_pkcs7_get_attr');
  pointer(gnutls_pkcs7_sign):=GPA('gnutls_pkcs7_sign');
  pointer(gnutls_pkcs7_get_crt_raw2):=GPA('gnutls_pkcs7_get_crt_raw2');
  pointer(gnutls_pkcs7_get_crl_raw2):=GPA('gnutls_pkcs7_get_crl_raw2');
  pointer(gnutls_pkcs7_print):=GPA('gnutls_pkcs7_print');
  LoadedLibName:=aLib;
end;

end.
