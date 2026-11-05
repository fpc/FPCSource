{ This file is part of the Free Pascal run time library.
  Translation of bcrypt.h: Cryptography API: Next Generation (CNG) in bcrypt.dll.
  Needs Windows Vista or later; declarations that need a later version say so.
  See the file COPYING.FPC, included in this distribution, for details about the copyright. }
{$IFNDEF FPC_DOTTEDUNITS}
unit BCrypt;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}
{$PACKRECORDS C}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
uses
  Windows;
{$ENDIF FPC_DOTTEDUNITS}

const
  BCryptDLL = 'bcrypt.dll';

type
  NTSTATUS = LongInt;
  PNTSTATUS = ^NTSTATUS;
  PPUCHAR = ^PUCHAR;

const
  { NTSTATUS values returned by the BCrypt functions }
  STATUS_SUCCESS = NTSTATUS($00000000);
  STATUS_INVALID_HANDLE = NTSTATUS($C0000008);
  STATUS_INVALID_PARAMETER = NTSTATUS($C000000D);
  STATUS_NO_MEMORY = NTSTATUS($C0000017);
  STATUS_BUFFER_TOO_SMALL = NTSTATUS($C0000023);
  STATUS_DATA_ERROR = NTSTATUS($C000003E);
  STATUS_NOT_SUPPORTED = NTSTATUS($C00000BB);
  STATUS_INVALID_BUFFER_SIZE = NTSTATUS($C0000206);
  STATUS_NOT_FOUND = NTSTATUS($C0000225);
  STATUS_INVALID_SIGNATURE = NTSTATUS($C000A000);
  STATUS_AUTH_TAG_MISMATCH = NTSTATUS($C000A002);

  BCRYPT_OBJECT_ALIGNMENT = 16;

  { Key derivation functions }
  BCRYPT_KDF_HASH: PWideChar = 'HASH';
  BCRYPT_KDF_HMAC: PWideChar = 'HMAC';
  BCRYPT_KDF_TLS_PRF: PWideChar = 'TLS_PRF';
  BCRYPT_KDF_SP80056A_CONCAT: PWideChar = 'SP800_56A_CONCAT';
  BCRYPT_KDF_RAW_SECRET: PWideChar = 'TRUNCATE';       // Windows 8.1
  BCRYPT_KDF_HKDF: PWideChar = 'HKDF';                 // Windows 10 1803

  { BCryptBuffer.BufferType values for key derivation }
  KDF_HASH_ALGORITHM = $0;
  KDF_SECRET_PREPEND = $1;
  KDF_SECRET_APPEND = $2;
  KDF_HMAC_KEY = $3;
  KDF_TLS_PRF_LABEL = $4;
  KDF_TLS_PRF_SEED = $5;
  KDF_SECRET_HANDLE = $6;
  KDF_TLS_PRF_PROTOCOL = $7;                           // Windows 7
  KDF_ALGORITHMID = $8;
  KDF_PARTYUINFO = $9;
  KDF_PARTYVINFO = $a;
  KDF_SUPPPUBINFO = $b;
  KDF_SUPPPRIVINFO = $c;
  KDF_LABEL = $d;                                      // Windows 8
  KDF_CONTEXT = $e;
  KDF_SALT = $f;
  KDF_ITERATION_COUNT = $10;
  KDF_GENERIC_PARAMETER = $11;
  KDF_KEYBITLENGTH = $12;
  KDF_HKDF_SALT = $13;                                 // Windows 10 1803
  KDF_HKDF_INFO = $14;

  KDF_USE_SECRET_AS_HMAC_KEY_FLAG = 1;

  BCRYPT_AUTHENTICATED_CIPHER_MODE_INFO_VERSION = 1;

  BCRYPT_AUTH_MODE_CHAIN_CALLS_FLAG = $00000001;
  BCRYPT_AUTH_MODE_IN_PROGRESS_FLAG = $00000002;

  { Symmetric key blob types }
  BCRYPT_OPAQUE_KEY_BLOB: PWideChar = 'OpaqueKeyBlob';
  BCRYPT_KEY_DATA_BLOB: PWideChar = 'KeyDataBlob';
  BCRYPT_AES_WRAP_KEY_BLOB: PWideChar = 'Rfc3565KeyWrapBlob';  // Windows 7

  { Property names }
  BCRYPT_ALGORITHM_NAME: PWideChar = 'AlgorithmName';
  BCRYPT_AUTH_TAG_LENGTH: PWideChar = 'AuthTagLength';
  BCRYPT_BLOCK_LENGTH: PWideChar = 'BlockLength';
  BCRYPT_BLOCK_SIZE_LIST: PWideChar = 'BlockSizeList';
  BCRYPT_CHAINING_MODE: PWideChar = 'ChainingMode';
  BCRYPT_CHAIN_MODE_CBC: PWideChar = 'ChainingModeCBC';
  BCRYPT_CHAIN_MODE_CCM: PWideChar = 'ChainingModeCCM';
  BCRYPT_CHAIN_MODE_CFB: PWideChar = 'ChainingModeCFB';
  BCRYPT_CHAIN_MODE_ECB: PWideChar = 'ChainingModeECB';
  BCRYPT_CHAIN_MODE_GCM: PWideChar = 'ChainingModeGCM';
  BCRYPT_CHAIN_MODE_NA: PWideChar = 'ChainingModeN/A';
  BCRYPT_EFFECTIVE_KEY_LENGTH: PWideChar = 'EffectiveKeyLength';
  BCRYPT_HASH_BLOCK_LENGTH: PWideChar = 'HashBlockLength';
  BCRYPT_HASH_LENGTH: PWideChar = 'HashDigestLength';
  BCRYPT_HASH_OID_LIST: PWideChar = 'HashOIDList';
  BCRYPT_INITIALIZATION_VECTOR: PWideChar = 'IV';
  BCRYPT_IS_KEYED_HASH: PWideChar = 'IsKeyedHash';
  BCRYPT_IS_REUSABLE_HASH: PWideChar = 'IsReusableHash';
  BCRYPT_KEY_LENGTH: PWideChar = 'KeyLength';
  BCRYPT_KEY_LENGTHS: PWideChar = 'KeyLengths';
  BCRYPT_KEY_OBJECT_LENGTH: PWideChar = 'KeyObjectLength';
  BCRYPT_KEY_STRENGTH: PWideChar = 'KeyStrength';
  BCRYPT_MESSAGE_BLOCK_LENGTH: PWideChar = 'MessageBlockLength';
  BCRYPT_OBJECT_LENGTH: PWideChar = 'ObjectLength';
  BCRYPT_PADDING_SCHEMES: PWideChar = 'PaddingSchemes';
  BCRYPT_PCP_PLATFORM_TYPE_PROPERTY: PWideChar = 'PCP_PLATFORM_TYPE';
  BCRYPT_PCP_PROVIDER_VERSION_PROPERTY: PWideChar = 'PCP_PROVIDER_VERSION';
  BCRYPT_PRIMITIVE_TYPE: PWideChar = 'PrimitiveType';
  BCRYPT_PROVIDER_HANDLE: PWideChar = 'ProviderHandle';
  BCRYPT_PUBLIC_KEY_LENGTH: PWideChar = 'PublicKeyLength';
  BCRYPT_SIGNATURE_LENGTH: PWideChar = 'SignatureLength';
  BCRYPT_MULTI_OBJECT_LENGTH: PWideChar = 'MultiObjectLength';      // Windows 8.1
  BCRYPT_IS_IFX_TPM_WEAK_KEY: PWideChar = 'IsIfxTpmWeakKey';        // Windows 10 1803
  BCRYPT_HKDF_HASH_ALGORITHM: PWideChar = 'HkdfHashAlgorithm';
  BCRYPT_HKDF_SALT_AND_FINALIZE: PWideChar = 'HkdfSaltAndFinalize';
  BCRYPT_HKDF_PRK_AND_FINALIZE: PWideChar = 'HkdfPrkAndFinalize';

  { BCRYPT_PADDING_SCHEMES values }
  BCRYPT_SUPPORTED_PAD_ROUTER = $00000001;
  BCRYPT_SUPPORTED_PAD_PKCS1_ENC = $00000002;
  BCRYPT_SUPPORTED_PAD_PKCS1_SIG = $00000004;
  BCRYPT_SUPPORTED_PAD_OAEP = $00000008;
  BCRYPT_SUPPORTED_PAD_PSS = $00000010;

  BCRYPT_PROV_DISPATCH = $00000001;

  BCRYPT_BLOCK_PADDING = $00000001;

  { Padding flags for asymmetric encryption and signatures }
  BCRYPT_PAD_NONE = $00000001;
  BCRYPT_PAD_PKCS1 = $00000002;
  BCRYPT_PAD_OAEP = $00000004;
  BCRYPT_PAD_PSS = $00000008;
  BCRYPT_PAD_PKCS1_OPTIONAL_HASH_OID = $00000010;     // Windows 8.1

  BCRYPTBUFFER_VERSION = 0;

type
  BCRYPT_KEY_LENGTHS_STRUCT = record
    dwMinLength: ULONG;
    dwMaxLength: ULONG;
    dwIncrement: ULONG;
  end;
  PBCRYPT_KEY_LENGTHS_STRUCT = ^BCRYPT_KEY_LENGTHS_STRUCT;

  BCRYPT_AUTH_TAG_LENGTHS_STRUCT = BCRYPT_KEY_LENGTHS_STRUCT;
  PBCRYPT_AUTH_TAG_LENGTHS_STRUCT = ^BCRYPT_AUTH_TAG_LENGTHS_STRUCT;

  BCRYPT_OID = record
    cbOID: ULONG;
    pbOID: PUCHAR;
  end;
  PBCRYPT_OID = ^BCRYPT_OID;

  BCRYPT_OID_LIST = record
    dwOIDCount: ULONG;
    pOIDs: PBCRYPT_OID;
  end;
  PBCRYPT_OID_LIST = ^BCRYPT_OID_LIST;

  BCRYPT_PKCS1_PADDING_INFO = record
    pszAlgId: LPCWSTR;
  end;
  PBCRYPT_PKCS1_PADDING_INFO = ^BCRYPT_PKCS1_PADDING_INFO;

  BCRYPT_PSS_PADDING_INFO = record
    pszAlgId: LPCWSTR;
    cbSalt: ULONG;
  end;
  PBCRYPT_PSS_PADDING_INFO = ^BCRYPT_PSS_PADDING_INFO;

  BCRYPT_OAEP_PADDING_INFO = record
    pszAlgId: LPCWSTR;
    pbLabel: PUCHAR;
    cbLabel: ULONG;
  end;
  PBCRYPT_OAEP_PADDING_INFO = ^BCRYPT_OAEP_PADDING_INFO;

  BCRYPT_AUTHENTICATED_CIPHER_MODE_INFO = record
    cbSize: ULONG;
    dwInfoVersion: ULONG;
    pbNonce: PUCHAR;
    cbNonce: ULONG;
    pbAuthData: PUCHAR;
    cbAuthData: ULONG;
    pbTag: PUCHAR;
    cbTag: ULONG;
    pbMacContext: PUCHAR;
    cbMacContext: ULONG;
    cbAAD: ULONG;
    cbData: ULONGLONG;
    dwFlags: ULONG;
  end;
  PBCRYPT_AUTHENTICATED_CIPHER_MODE_INFO = ^BCRYPT_AUTHENTICATED_CIPHER_MODE_INFO;

  BCryptBuffer = record
    cbBuffer: ULONG;
    BufferType: ULONG;
    pvBuffer: PVOID;
  end;
  PBCryptBuffer = ^BCryptBuffer;

  BCryptBufferDesc = record
    ulVersion: ULONG;
    cBuffers: ULONG;
    pBuffers: PBCryptBuffer;
  end;
  PBCryptBufferDesc = ^BCryptBufferDesc;

const
  { Asymmetric key blob types }
  BCRYPT_PUBLIC_KEY_BLOB: PWideChar = 'PUBLICBLOB';
  BCRYPT_PRIVATE_KEY_BLOB: PWideChar = 'PRIVATEBLOB';

  BCRYPT_RSAPUBLIC_BLOB: PWideChar = 'RSAPUBLICBLOB';
  BCRYPT_RSAPRIVATE_BLOB: PWideChar = 'RSAPRIVATEBLOB';
  LEGACY_RSAPUBLIC_BLOB: PWideChar = 'CAPIPUBLICBLOB';
  LEGACY_RSAPRIVATE_BLOB: PWideChar = 'CAPIPRIVATEBLOB';

  BCRYPT_RSAPUBLIC_MAGIC = $31415352;
  BCRYPT_RSAPRIVATE_MAGIC = $32415352;

  BCRYPT_RSAFULLPRIVATE_BLOB: PWideChar = 'RSAFULLPRIVATEBLOB';

  BCRYPT_RSAFULLPRIVATE_MAGIC = $33415352;

  BCRYPT_GLOBAL_PARAMETERS: PWideChar = 'SecretAgreementParam';     // Windows 8
  BCRYPT_PRIVATE_KEY: PWideChar = 'PrivKeyVal';

  BCRYPT_ECCPUBLIC_BLOB: PWideChar = 'ECCPUBLICBLOB';
  BCRYPT_ECCPRIVATE_BLOB: PWideChar = 'ECCPRIVATEBLOB';

  BCRYPT_ECCFULLPUBLIC_BLOB: PWideChar = 'ECCFULLPUBLICBLOB';       // Windows 10
  BCRYPT_ECCFULLPRIVATE_BLOB: PWideChar = 'ECCFULLPRIVATEBLOB';
  SSL_ECCPUBLIC_BLOB: PWideChar = 'SSLECCPUBLICBLOB';

  BCRYPT_ECDH_PUBLIC_P256_MAGIC = $314b4345;
  BCRYPT_ECDH_PRIVATE_P256_MAGIC = $324b4345;
  BCRYPT_ECDH_PUBLIC_P384_MAGIC = $334b4345;
  BCRYPT_ECDH_PRIVATE_P384_MAGIC = $344b4345;
  BCRYPT_ECDH_PUBLIC_P521_MAGIC = $354b4345;
  BCRYPT_ECDH_PRIVATE_P521_MAGIC = $364b4345;
  BCRYPT_ECDH_PUBLIC_GENERIC_MAGIC = $504B4345;                     // Windows 10
  BCRYPT_ECDH_PRIVATE_GENERIC_MAGIC = $564B4345;

  BCRYPT_ECDSA_PUBLIC_P256_MAGIC = $31534345;
  BCRYPT_ECDSA_PRIVATE_P256_MAGIC = $32534345;
  BCRYPT_ECDSA_PUBLIC_P384_MAGIC = $33534345;
  BCRYPT_ECDSA_PRIVATE_P384_MAGIC = $34534345;
  BCRYPT_ECDSA_PUBLIC_P521_MAGIC = $35534345;
  BCRYPT_ECDSA_PRIVATE_P521_MAGIC = $36534345;
  BCRYPT_ECDSA_PUBLIC_GENERIC_MAGIC = $50444345;                    // Windows 10
  BCRYPT_ECDSA_PRIVATE_GENERIC_MAGIC = $56444345;

  BCRYPT_DH_PUBLIC_BLOB: PWideChar = 'DHPUBLICBLOB';
  BCRYPT_DH_PRIVATE_BLOB: PWideChar = 'DHPRIVATEBLOB';
  LEGACY_DH_PUBLIC_BLOB: PWideChar = 'CAPIDHPUBLICBLOB';
  LEGACY_DH_PRIVATE_BLOB: PWideChar = 'CAPIDHPRIVATEBLOB';

  BCRYPT_DH_PUBLIC_MAGIC = $42504844;
  BCRYPT_DH_PRIVATE_MAGIC = $56504844;

  BCRYPT_DH_PARAMETERS: PWideChar = 'DHParameters';
  BCRYPT_DH_PARAMETERS_MAGIC = $4d504844;

  BCRYPT_DSA_PUBLIC_BLOB: PWideChar = 'DSAPUBLICBLOB';
  BCRYPT_DSA_PRIVATE_BLOB: PWideChar = 'DSAPRIVATEBLOB';
  LEGACY_DSA_PUBLIC_BLOB: PWideChar = 'CAPIDSAPUBLICBLOB';
  LEGACY_DSA_PRIVATE_BLOB: PWideChar = 'CAPIDSAPRIVATEBLOB';
  LEGACY_DSA_V2_PUBLIC_BLOB: PWideChar = 'V2CAPIDSAPUBLICBLOB';
  LEGACY_DSA_V2_PRIVATE_BLOB: PWideChar = 'V2CAPIDSAPRIVATEBLOB';

  BCRYPT_DSA_PUBLIC_MAGIC = $42505344;
  BCRYPT_DSA_PRIVATE_MAGIC = $56505344;
  BCRYPT_DSA_PUBLIC_MAGIC_V2 = $32425044;
  BCRYPT_DSA_PRIVATE_MAGIC_V2 = $32565044;

  BCRYPT_KEY_DATA_BLOB_MAGIC = $4d42444b;
  BCRYPT_KEY_DATA_BLOB_VERSION1 = $1;

  BCRYPT_DSA_PARAMETERS: PWideChar = 'DSAParameters';
  BCRYPT_DSA_PARAMETERS_MAGIC = $4d505344;
  BCRYPT_DSA_PARAMETERS_MAGIC_V2 = $324d5044;

  { Provider names }
  MS_PRIMITIVE_PROVIDER: PWideChar = 'Microsoft Primitive Provider';
  MS_PLATFORM_CRYPTO_PROVIDER: PWideChar = 'Microsoft Platform Crypto Provider';

  { Algorithm identifiers }
  BCRYPT_RSA_ALGORITHM: PWideChar = 'RSA';
  BCRYPT_RSA_SIGN_ALGORITHM: PWideChar = 'RSA_SIGN';
  BCRYPT_DH_ALGORITHM: PWideChar = 'DH';
  BCRYPT_DSA_ALGORITHM: PWideChar = 'DSA';
  BCRYPT_RC2_ALGORITHM: PWideChar = 'RC2';
  BCRYPT_RC4_ALGORITHM: PWideChar = 'RC4';
  BCRYPT_AES_ALGORITHM: PWideChar = 'AES';
  BCRYPT_DES_ALGORITHM: PWideChar = 'DES';
  BCRYPT_DESX_ALGORITHM: PWideChar = 'DESX';
  BCRYPT_3DES_ALGORITHM: PWideChar = '3DES';
  BCRYPT_3DES_112_ALGORITHM: PWideChar = '3DES_112';
  BCRYPT_MD2_ALGORITHM: PWideChar = 'MD2';
  BCRYPT_MD4_ALGORITHM: PWideChar = 'MD4';
  BCRYPT_MD5_ALGORITHM: PWideChar = 'MD5';
  BCRYPT_SHA1_ALGORITHM: PWideChar = 'SHA1';
  BCRYPT_SHA256_ALGORITHM: PWideChar = 'SHA256';
  BCRYPT_SHA384_ALGORITHM: PWideChar = 'SHA384';
  BCRYPT_SHA512_ALGORITHM: PWideChar = 'SHA512';
  BCRYPT_AES_GMAC_ALGORITHM: PWideChar = 'AES-GMAC';
  BCRYPT_AES_CMAC_ALGORITHM: PWideChar = 'AES-CMAC';
  BCRYPT_ECDSA_P256_ALGORITHM: PWideChar = 'ECDSA_P256';
  BCRYPT_ECDSA_P384_ALGORITHM: PWideChar = 'ECDSA_P384';
  BCRYPT_ECDSA_P521_ALGORITHM: PWideChar = 'ECDSA_P521';
  BCRYPT_ECDH_P256_ALGORITHM: PWideChar = 'ECDH_P256';
  BCRYPT_ECDH_P384_ALGORITHM: PWideChar = 'ECDH_P384';
  BCRYPT_ECDH_P521_ALGORITHM: PWideChar = 'ECDH_P521';
  BCRYPT_RNG_ALGORITHM: PWideChar = 'RNG';
  BCRYPT_RNG_FIPS186_DSA_ALGORITHM: PWideChar = 'FIPS186DSARNG';
  BCRYPT_RNG_DUAL_EC_ALGORITHM: PWideChar = 'DUALECRNG';
  BCRYPT_SP800108_CTR_HMAC_ALGORITHM: PWideChar = 'SP800_108_CTR_HMAC';
  BCRYPT_SP80056A_CONCAT_ALGORITHM: PWideChar = 'SP800_56A_CONCAT';
  BCRYPT_PBKDF2_ALGORITHM: PWideChar = 'PBKDF2';
  BCRYPT_CAPI_KDF_ALGORITHM: PWideChar = 'CAPI_KDF';

  { Interface identifiers }
  BCRYPT_CIPHER_INTERFACE = $00000001;
  BCRYPT_HASH_INTERFACE = $00000002;
  BCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE = $00000003;
  BCRYPT_SECRET_AGREEMENT_INTERFACE = $00000004;
  BCRYPT_SIGNATURE_INTERFACE = $00000005;
  BCRYPT_RNG_INTERFACE = $00000006;
  BCRYPT_KEY_DERIVATION_INTERFACE = $00000007;

  { BCryptOpenAlgorithmProvider flags }
  BCRYPT_ALG_HANDLE_HMAC_FLAG = $00000008;
  BCRYPT_CAPI_AES_FLAG = $00000010;
  BCRYPT_HASH_REUSABLE_FLAG = $00000020;

  BCRYPT_BUFFERS_LOCKED_FLAG = $00000040;

  { BCryptEnumAlgorithms operation classes }
  BCRYPT_CIPHER_OPERATION = $00000001;
  BCRYPT_HASH_OPERATION = $00000002;
  BCRYPT_ASYMMETRIC_ENCRYPTION_OPERATION = $00000004;
  BCRYPT_SECRET_AGREEMENT_OPERATION = $00000008;
  BCRYPT_SIGNATURE_OPERATION = $00000010;
  BCRYPT_RNG_OPERATION = $00000020;
  BCRYPT_KEY_DERIVATION_OPERATION = $00000040;

  BCRYPT_PUBLIC_KEY_FLAG = $00000001;
  BCRYPT_PRIVATE_KEY_FLAG = $00000002;

  BCRYPT_NO_KEY_VALIDATION = $00000008;

  { BCryptGenRandom flags }
  BCRYPT_RNG_USE_ENTROPY_IN_BUFFER = $00000001;
  BCRYPT_USE_SYSTEM_PREFERRED_RNG = $00000002;

  { Configuration functions }
  CRYPT_MIN_DEPENDENCIES = $00000001;
  CRYPT_PROCESS_ISOLATE = $00010000;

  CRYPT_UM = $00000001;
  CRYPT_KM = $00000002;
  CRYPT_MM = $00000003;
  CRYPT_ANY = $00000004;

  CRYPT_OVERWRITE = $00000001;

  CRYPT_LOCAL = $00000001;
  CRYPT_DOMAIN = $00000002;

  CRYPT_EXCLUSIVE = $00000001;
  CRYPT_OVERRIDE = $00010000;

  CRYPT_ALL_FUNCTIONS = $00000001;
  CRYPT_ALL_PROVIDERS = $00000002;

  CRYPT_PRIORITY_TOP = $00000000;
  CRYPT_PRIORITY_BOTTOM = $ffffffff;

  CRYPT_DEFAULT_CONTEXT: PWideChar = 'Default';

type
  BCRYPT_HANDLE = PVOID;
  BCRYPT_ALG_HANDLE = PVOID;
  BCRYPT_KEY_HANDLE = PVOID;
  BCRYPT_HASH_HANDLE = PVOID;
  BCRYPT_SECRET_HANDLE = PVOID;
  PBCRYPT_HANDLE = ^BCRYPT_HANDLE;
  PBCRYPT_ALG_HANDLE = ^BCRYPT_ALG_HANDLE;
  PBCRYPT_KEY_HANDLE = ^BCRYPT_KEY_HANDLE;
  PBCRYPT_HASH_HANDLE = ^BCRYPT_HASH_HANDLE;
  PBCRYPT_SECRET_HANDLE = ^BCRYPT_SECRET_HANDLE;

  BCRYPT_KEY_BLOB = record
    Magic: ULONG;
  end;
  PBCRYPT_KEY_BLOB = ^BCRYPT_KEY_BLOB;

  BCRYPT_RSAKEY_BLOB = record
    Magic: ULONG;
    BitLength: ULONG;
    cbPublicExp: ULONG;
    cbModulus: ULONG;
    cbPrime1: ULONG;
    cbPrime2: ULONG;
  end;
  PBCRYPT_RSAKEY_BLOB = ^BCRYPT_RSAKEY_BLOB;

  BCRYPT_ECCKEY_BLOB = record
    dwMagic: ULONG;
    cbKey: ULONG;
  end;
  PBCRYPT_ECCKEY_BLOB = ^BCRYPT_ECCKEY_BLOB;

  BCRYPT_DH_KEY_BLOB = record
    dwMagic: ULONG;
    cbKey: ULONG;
  end;
  PBCRYPT_DH_KEY_BLOB = ^BCRYPT_DH_KEY_BLOB;

  BCRYPT_DH_PARAMETER_HEADER = record
    cbLength: ULONG;
    dwMagic: ULONG;
    cbKeyLength: ULONG;
  end;
  PBCRYPT_DH_PARAMETER_HEADER = ^BCRYPT_DH_PARAMETER_HEADER;

  BCRYPT_DSA_KEY_BLOB = record
    dwMagic: ULONG;
    cbKey: ULONG;
    Count: array[0..3] of UCHAR;
    Seed: array[0..19] of UCHAR;
    q: array[0..19] of UCHAR;
  end;
  PBCRYPT_DSA_KEY_BLOB = ^BCRYPT_DSA_KEY_BLOB;

  HASHALGORITHM_ENUM = DWORD;

const
  DSA_HASH_ALGORITHM_SHA1 = 0;
  DSA_HASH_ALGORITHM_SHA256 = 1;
  DSA_HASH_ALGORITHM_SHA512 = 2;

type
  DSAFIPSVERSION_ENUM = DWORD;

const
  DSA_FIPS186_2 = 0;
  DSA_FIPS186_3 = 1;

type
  BCRYPT_DSA_KEY_BLOB_V2 = record
    dwMagic: ULONG;
    cbKey: ULONG;
    hashAlgorithm: HASHALGORITHM_ENUM;
    standardVersion: DSAFIPSVERSION_ENUM;
    cbSeedLength: ULONG;
    cbGroupSize: ULONG;
    Count: array[0..3] of UCHAR;
  end;
  PBCRYPT_DSA_KEY_BLOB_V2 = ^BCRYPT_DSA_KEY_BLOB_V2;

  BCRYPT_KEY_DATA_BLOB_HEADER = record
    dwMagic: ULONG;
    dwVersion: ULONG;
    cbKeyData: ULONG;
  end;
  PBCRYPT_KEY_DATA_BLOB_HEADER = ^BCRYPT_KEY_DATA_BLOB_HEADER;

  BCRYPT_DSA_PARAMETER_HEADER = record
    cbLength: ULONG;
    dwMagic: ULONG;
    cbKeyLength: ULONG;
    Count: array[0..3] of UCHAR;
    Seed: array[0..19] of UCHAR;
    q: array[0..19] of UCHAR;
  end;
  PBCRYPT_DSA_PARAMETER_HEADER = ^BCRYPT_DSA_PARAMETER_HEADER;

  BCRYPT_DSA_PARAMETER_HEADER_V2 = record
    cbLength: ULONG;
    dwMagic: ULONG;
    cbKeyLength: ULONG;
    hashAlgorithm: HASHALGORITHM_ENUM;
    standardVersion: DSAFIPSVERSION_ENUM;
    cbSeedLength: ULONG;
    cbGroupSize: ULONG;
    Count: array[0..3] of UCHAR;
  end;
  PBCRYPT_DSA_PARAMETER_HEADER_V2 = ^BCRYPT_DSA_PARAMETER_HEADER_V2;

  BCRYPT_ECC_CURVE_NAMES = record
    dwEccCurveNames: ULONG;
    pEccCurveNames: ^LPWSTR;
  end;
  PBCRYPT_ECC_CURVE_NAMES = ^BCRYPT_ECC_CURVE_NAMES;

  BCRYPT_HASH_OPERATION_TYPE = DWORD;

const
  BCRYPT_HASH_OPERATION_HASH_DATA = 1;
  BCRYPT_HASH_OPERATION_FINISH_HASH = 2;

type
  BCRYPT_MULTI_HASH_OPERATION = record
    iHash: ULONG;
    hashOperation: BCRYPT_HASH_OPERATION_TYPE;
    pbBuffer: PUCHAR;
    cbBuffer: ULONG;
  end;
  PBCRYPT_MULTI_HASH_OPERATION = ^BCRYPT_MULTI_HASH_OPERATION;

  BCRYPT_MULTI_OPERATION_TYPE = DWORD;

const
  BCRYPT_OPERATION_TYPE_HASH = 1;

type
  BCRYPT_MULTI_OBJECT_LENGTH_STRUCT = record
    cbPerObject: ULONG;
    cbPerElement: ULONG;
  end;
  PBCRYPT_MULTI_OBJECT_LENGTH_STRUCT = ^BCRYPT_MULTI_OBJECT_LENGTH_STRUCT;

  BCRYPT_ALGORITHM_IDENTIFIER = record
    pszName: LPWSTR;
    dwClass: ULONG;
    dwFlags: ULONG;
  end;
  PBCRYPT_ALGORITHM_IDENTIFIER = ^BCRYPT_ALGORITHM_IDENTIFIER;
  PPBCRYPT_ALGORITHM_IDENTIFIER = ^PBCRYPT_ALGORITHM_IDENTIFIER;

  BCRYPT_PROVIDER_NAME = record
    pszProviderName: LPWSTR;
  end;
  PBCRYPT_PROVIDER_NAME = ^BCRYPT_PROVIDER_NAME;
  PPBCRYPT_PROVIDER_NAME = ^PBCRYPT_PROVIDER_NAME;

  BCRYPT_INTERFACE_VERSION = record
    MajorVersion: USHORT;
    MinorVersion: USHORT;
  end;
  PBCRYPT_INTERFACE_VERSION = ^BCRYPT_INTERFACE_VERSION;

  PCRYPT_INTERFACE_REG = ^CRYPT_INTERFACE_REG;
  CRYPT_INTERFACE_REG = record
    dwInterface: ULONG;
    dwFlags: ULONG;
    cFunctions: ULONG;
    rgpszFunctions: ^PWSTR;
  end;

  PCRYPT_IMAGE_REG = ^CRYPT_IMAGE_REG;
  CRYPT_IMAGE_REG = record
    pszImage: PWSTR;
    cInterfaces: ULONG;
    rgpInterfaces: ^PCRYPT_INTERFACE_REG;
  end;

  PCRYPT_PROVIDER_REG = ^CRYPT_PROVIDER_REG;
  PPCRYPT_PROVIDER_REG = ^PCRYPT_PROVIDER_REG;
  CRYPT_PROVIDER_REG = record
    cAliases: ULONG;
    rgpszAliases: ^PWSTR;
    pUM: PCRYPT_IMAGE_REG;
    pKM: PCRYPT_IMAGE_REG;
  end;

  PCRYPT_PROVIDERS = ^CRYPT_PROVIDERS;
  PPCRYPT_PROVIDERS = ^PCRYPT_PROVIDERS;
  CRYPT_PROVIDERS = record
    cProviders: ULONG;
    rgpszProviders: ^PWSTR;
  end;

  PCRYPT_CONTEXT_CONFIG = ^CRYPT_CONTEXT_CONFIG;
  PPCRYPT_CONTEXT_CONFIG = ^PCRYPT_CONTEXT_CONFIG;
  CRYPT_CONTEXT_CONFIG = record
    dwFlags: ULONG;
    dwReserved: ULONG;
  end;

  PCRYPT_CONTEXT_FUNCTION_CONFIG = ^CRYPT_CONTEXT_FUNCTION_CONFIG;
  PPCRYPT_CONTEXT_FUNCTION_CONFIG = ^PCRYPT_CONTEXT_FUNCTION_CONFIG;
  CRYPT_CONTEXT_FUNCTION_CONFIG = record
    dwFlags: ULONG;
    dwReserved: ULONG;
  end;

  PCRYPT_CONTEXTS = ^CRYPT_CONTEXTS;
  PPCRYPT_CONTEXTS = ^PCRYPT_CONTEXTS;
  CRYPT_CONTEXTS = record
    cContexts: ULONG;
    rgpszContexts: ^PWSTR;
  end;

  PCRYPT_CONTEXT_FUNCTIONS = ^CRYPT_CONTEXT_FUNCTIONS;
  PPCRYPT_CONTEXT_FUNCTIONS = ^PCRYPT_CONTEXT_FUNCTIONS;
  CRYPT_CONTEXT_FUNCTIONS = record
    cFunctions: ULONG;
    rgpszFunctions: ^PWSTR;
  end;

  PCRYPT_CONTEXT_FUNCTION_PROVIDERS = ^CRYPT_CONTEXT_FUNCTION_PROVIDERS;
  PPCRYPT_CONTEXT_FUNCTION_PROVIDERS = ^PCRYPT_CONTEXT_FUNCTION_PROVIDERS;
  CRYPT_CONTEXT_FUNCTION_PROVIDERS = record
    cProviders: ULONG;
    rgpszProviders: ^PWSTR;
  end;

  PCRYPT_PROPERTY_REF = ^CRYPT_PROPERTY_REF;
  CRYPT_PROPERTY_REF = record
    pszProperty: PWSTR;
    cbValue: ULONG;
    pbValue: PUCHAR;
  end;

  PCRYPT_IMAGE_REF = ^CRYPT_IMAGE_REF;
  CRYPT_IMAGE_REF = record
    pszImage: PWSTR;
    dwFlags: ULONG;
  end;

  PCRYPT_PROVIDER_REF = ^CRYPT_PROVIDER_REF;
  CRYPT_PROVIDER_REF = record
    dwInterface: ULONG;
    pszFunction: PWSTR;
    pszProvider: PWSTR;
    cProperties: ULONG;
    rgpProperties: ^PCRYPT_PROPERTY_REF;
    pUM: PCRYPT_IMAGE_REF;
    pKM: PCRYPT_IMAGE_REF;
  end;

  PCRYPT_PROVIDER_REFS = ^CRYPT_PROVIDER_REFS;
  PPCRYPT_PROVIDER_REFS = ^PCRYPT_PROVIDER_REFS;
  CRYPT_PROVIDER_REFS = record
    cProviders: ULONG;
    rgpProviders: ^PCRYPT_PROVIDER_REF;
  end;

// Return True if aStatus is a success or informational NTSTATUS value
function BCRYPT_SUCCESS(aStatus: NTSTATUS): Boolean; inline;
// Prepare aInfo for an authenticated cipher mode call
procedure BCRYPT_INIT_AUTH_MODE_INFO(out aInfo: BCRYPT_AUTHENTICATED_CIPHER_MODE_INFO);

function BCryptOpenAlgorithmProvider(phAlgorithm: PBCRYPT_ALG_HANDLE; pszAlgId: LPCWSTR; pszImplementation: LPCWSTR; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptEnumAlgorithms(dwAlgOperations: ULONG; pAlgCount: PULONG; ppAlgList: PPBCRYPT_ALGORITHM_IDENTIFIER; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptEnumProviders(pszAlgId: LPCWSTR; pImplCount: PULONG; ppImplList: PPBCRYPT_PROVIDER_NAME; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptGetProperty(hObject: BCRYPT_HANDLE; pszProperty: LPCWSTR; pbOutput: PUCHAR; cbOutput: ULONG; pcbResult: PULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptSetProperty(hObject: BCRYPT_HANDLE; pszProperty: LPCWSTR; pbInput: PUCHAR; cbInput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptCloseAlgorithmProvider(hAlgorithm: BCRYPT_ALG_HANDLE; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
procedure BCryptFreeBuffer(pvBuffer: PVOID); stdcall; external BCryptDLL;
function BCryptGenerateSymmetricKey(hAlgorithm: BCRYPT_ALG_HANDLE; phKey: PBCRYPT_KEY_HANDLE; pbKeyObject: PUCHAR; cbKeyObject: ULONG; pbSecret: PUCHAR; cbSecret: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptGenerateKeyPair(hAlgorithm: BCRYPT_ALG_HANDLE; phKey: PBCRYPT_KEY_HANDLE; dwLength: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptEncrypt(hKey: BCRYPT_KEY_HANDLE; pbInput: PUCHAR; cbInput: ULONG; pPaddingInfo: PVOID; pbIV: PUCHAR; cbIV: ULONG; pbOutput: PUCHAR; cbOutput: ULONG; pcbResult: PULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptDecrypt(hKey: BCRYPT_KEY_HANDLE; pbInput: PUCHAR; cbInput: ULONG; pPaddingInfo: PVOID; pbIV: PUCHAR; cbIV: ULONG; pbOutput: PUCHAR; cbOutput: ULONG; pcbResult: PULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptExportKey(hKey: BCRYPT_KEY_HANDLE; hExportKey: BCRYPT_KEY_HANDLE; pszBlobType: LPCWSTR; pbOutput: PUCHAR; cbOutput: ULONG; pcbResult: PULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptImportKey(hAlgorithm: BCRYPT_ALG_HANDLE; hImportKey: BCRYPT_KEY_HANDLE; pszBlobType: LPCWSTR; phKey: PBCRYPT_KEY_HANDLE; pbKeyObject: PUCHAR; cbKeyObject: ULONG; pbInput: PUCHAR; cbInput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptImportKeyPair(hAlgorithm: BCRYPT_ALG_HANDLE; hImportKey: BCRYPT_KEY_HANDLE; pszBlobType: LPCWSTR; phKey: PBCRYPT_KEY_HANDLE; pbInput: PUCHAR; cbInput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptDuplicateKey(hKey: BCRYPT_KEY_HANDLE; phNewKey: PBCRYPT_KEY_HANDLE; pbKeyObject: PUCHAR; cbKeyObject: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptFinalizeKeyPair(hKey: BCRYPT_KEY_HANDLE; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptDestroyKey(hKey: BCRYPT_KEY_HANDLE): NTSTATUS; stdcall; external BCryptDLL;
function BCryptDestroySecret(hSecret: BCRYPT_SECRET_HANDLE): NTSTATUS; stdcall; external BCryptDLL;
function BCryptSignHash(hKey: BCRYPT_KEY_HANDLE; pPaddingInfo: PVOID; pbInput: PUCHAR; cbInput: ULONG; pbOutput: PUCHAR; cbOutput: ULONG; pcbResult: PULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptVerifySignature(hKey: BCRYPT_KEY_HANDLE; pPaddingInfo: PVOID; pbHash: PUCHAR; cbHash: ULONG; pbSignature: PUCHAR; cbSignature: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptSecretAgreement(hPrivKey: BCRYPT_KEY_HANDLE; hPubKey: BCRYPT_KEY_HANDLE; phAgreedSecret: PBCRYPT_SECRET_HANDLE; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptDeriveKey(hSharedSecret: BCRYPT_SECRET_HANDLE; pwszKDF: LPCWSTR; pParameterList: PBCryptBufferDesc; pbDerivedKey: PUCHAR; cbDerivedKey: ULONG; pcbResult: PULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
// Windows 8
function BCryptKeyDerivation(hKey: BCRYPT_KEY_HANDLE; pParameterList: PBCryptBufferDesc; pbDerivedKey: PUCHAR; cbDerivedKey: ULONG; pcbResult: PULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptCreateHash(hAlgorithm: BCRYPT_ALG_HANDLE; phHash: PBCRYPT_HASH_HANDLE; pbHashObject: PUCHAR; cbHashObject: ULONG; pbSecret: PUCHAR; cbSecret: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptHashData(hHash: BCRYPT_HASH_HANDLE; pbInput: PUCHAR; cbInput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptFinishHash(hHash: BCRYPT_HASH_HANDLE; pbOutput: PUCHAR; cbOutput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
// Windows 8.1
function BCryptCreateMultiHash(hAlgorithm: BCRYPT_ALG_HANDLE; phHash: PBCRYPT_HASH_HANDLE; nHashes: ULONG; pbHashObject: PUCHAR; cbHashObject: ULONG; pbSecret: PUCHAR; cbSecret: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
// Windows 8.1
function BCryptProcessMultiOperations(hObject: BCRYPT_HANDLE; operationType: BCRYPT_MULTI_OPERATION_TYPE; pOperations: PVOID; cbOperations: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptDuplicateHash(hHash: BCRYPT_HASH_HANDLE; phNewHash: PBCRYPT_HASH_HANDLE; pbHashObject: PUCHAR; cbHashObject: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptDestroyHash(hHash: BCRYPT_HASH_HANDLE): NTSTATUS; stdcall; external BCryptDLL;
// Windows 10
function BCryptHash(hAlgorithm: BCRYPT_ALG_HANDLE; pbSecret: PUCHAR; cbSecret: ULONG; pbInput: PUCHAR; cbInput: ULONG; pbOutput: PUCHAR; cbOutput: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptGenRandom(hAlgorithm: BCRYPT_ALG_HANDLE; pbBuffer: PUCHAR; cbBuffer: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptDeriveKeyCapi(hHash: BCRYPT_HASH_HANDLE; hTargetAlg: BCRYPT_ALG_HANDLE; pbDerivedKey: PUCHAR; cbDerivedKey: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
// Windows 7
function BCryptDeriveKeyPBKDF2(hPrf: BCRYPT_ALG_HANDLE; pbPassword: PUCHAR; cbPassword: ULONG; pbSalt: PUCHAR; cbSalt: ULONG; cIterations: ULONGLONG; pbDerivedKey: PUCHAR; cbDerivedKey: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptResolveProviders(pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR; pszProvider: LPCWSTR; dwMode: ULONG; dwFlags: ULONG; pcbBuffer: PULONG; ppBuffer: PPCRYPT_PROVIDER_REFS): NTSTATUS; stdcall; external BCryptDLL;
function BCryptGetFipsAlgorithmMode(pfEnabled: PByteBool): NTSTATUS; stdcall; external BCryptDLL;

function BCryptQueryProviderRegistration(pszProvider: LPCWSTR; dwMode: ULONG; dwInterface: ULONG; pcbBuffer: PULONG; ppBuffer: PPCRYPT_PROVIDER_REG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptEnumRegisteredProviders(pcbBuffer: PULONG; ppBuffer: PPCRYPT_PROVIDERS): NTSTATUS; stdcall; external BCryptDLL;
function BCryptCreateContext(dwTable: ULONG; pszContext: LPCWSTR; pConfig: PCRYPT_CONTEXT_CONFIG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptDeleteContext(dwTable: ULONG; pszContext: LPCWSTR): NTSTATUS; stdcall; external BCryptDLL;
function BCryptEnumContexts(dwTable: ULONG; pcbBuffer: PULONG; ppBuffer: PPCRYPT_CONTEXTS): NTSTATUS; stdcall; external BCryptDLL;
function BCryptConfigureContext(dwTable: ULONG; pszContext: LPCWSTR; pConfig: PCRYPT_CONTEXT_CONFIG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptQueryContextConfiguration(dwTable: ULONG; pszContext: LPCWSTR; pcbBuffer: PULONG; ppBuffer: PPCRYPT_CONTEXT_CONFIG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptAddContextFunction(dwTable: ULONG; pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR; dwPosition: ULONG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptRemoveContextFunction(dwTable: ULONG; pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR): NTSTATUS; stdcall; external BCryptDLL;
function BCryptEnumContextFunctions(dwTable: ULONG; pszContext: LPCWSTR; dwInterface: ULONG; pcbBuffer: PULONG; ppBuffer: PPCRYPT_CONTEXT_FUNCTIONS): NTSTATUS; stdcall; external BCryptDLL;
function BCryptConfigureContextFunction(dwTable: ULONG; pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR; pConfig: PCRYPT_CONTEXT_FUNCTION_CONFIG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptQueryContextFunctionConfiguration(dwTable: ULONG; pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR; pcbBuffer: PULONG; ppBuffer: PPCRYPT_CONTEXT_FUNCTION_CONFIG): NTSTATUS; stdcall; external BCryptDLL;
function BCryptEnumContextFunctionProviders(dwTable: ULONG; pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR; pcbBuffer: PULONG; ppBuffer: PPCRYPT_CONTEXT_FUNCTION_PROVIDERS): NTSTATUS; stdcall; external BCryptDLL;
function BCryptSetContextFunctionProperty(dwTable: ULONG; pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR; pszProperty: LPCWSTR; cbValue: ULONG; pbValue: PUCHAR): NTSTATUS; stdcall; external BCryptDLL;
function BCryptQueryContextFunctionProperty(dwTable: ULONG; pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR; pszProperty: LPCWSTR; pcbValue: PULONG; ppbValue: PPUCHAR): NTSTATUS; stdcall; external BCryptDLL;
function BCryptRegisterConfigChangeNotify(phEvent: PHANDLE): NTSTATUS; stdcall; external BCryptDLL;
function BCryptUnregisterConfigChangeNotify(hEvent: HANDLE): NTSTATUS; stdcall; external BCryptDLL;

implementation

function BCRYPT_SUCCESS(aStatus: NTSTATUS): Boolean;

begin
  Result:=aStatus>=0;
end;


procedure BCRYPT_INIT_AUTH_MODE_INFO(out aInfo: BCRYPT_AUTHENTICATED_CIPHER_MODE_INFO);

begin
  aInfo:=Default(BCRYPT_AUTHENTICATED_CIPHER_MODE_INFO);
  aInfo.cbSize:=SizeOf(BCRYPT_AUTHENTICATED_CIPHER_MODE_INFO);
  aInfo.dwInfoVersion:=BCRYPT_AUTHENTICATED_CIPHER_MODE_INFO_VERSION;
end;


end.
