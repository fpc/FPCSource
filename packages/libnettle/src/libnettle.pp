{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2022 Michael Van Canneyt

    Import unit for libnettle.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
unit libnettle;

interface


uses
  ctypes, libgmp;

{
  Automatically converted by H2Pas 1.0.0 from libnettle.c
  The following command line parameters were used:
    -D
    -C
    -c
    -l
    libnettle
    -o
    libnettle.pp
    -P
    -T
    -u
    libnettle
    -p
    libnettle.c
}

const
  NettleExternal_library='nettle';
  HogweedExternal_library='hogweed';

{$ifdef linux}
  NettleLibraryFileName = 'lib'+NettleExternal_library+'.so';
  HogweedLibraryFileName = 'lib'+HogweedExternal_library+'.so';
{$else}
{$ifdef windows}
  NettleLibraryFileName = NettleExternal_library+'.dll';
  HogweedLibraryFileName = HogweedExternal_library+'.dll';
{$endif}
{$endif}

Const
  AES_BLOCK_SIZE = 16;
  AES128_KEY_SIZE = 16;
  AES192_KEY_SIZE = 24;
  AES256_KEY_SIZE = 32;
  _AES128_ROUNDS = 10;
  _AES192_ROUNDS = 12;
  _AES256_ROUNDS = 14;
  AES_MIN_KEY_SIZE = AES128_KEY_SIZE;
  AES_MAX_KEY_SIZE = AES256_KEY_SIZE;
  AES_KEY_SIZE = 32;
  ARCFOUR_MIN_KEY_SIZE = 1;
  ARCFOUR_MAX_KEY_SIZE = 256;
  ARCFOUR_KEY_SIZE = 16;
  ARCFOUR128_KEY_SIZE = 16;
  ARCTWO_BLOCK_SIZE = 8;
  ARCTWO_MIN_KEY_SIZE = 1;
  ARCTWO_MAX_KEY_SIZE = 128;
  ARCTWO_KEY_SIZE = 8;
  BASE64_BINARY_BLOCK_SIZE = 3;
  BASE64_TEXT_BLOCK_SIZE = 4;
  BASE64_ENCODE_FINAL_LENGTH = 3;
  BLOWFISH_BLOCK_SIZE = 8;
  BLOWFISH_MIN_KEY_SIZE = 8;
  BLOWFISH_MAX_KEY_SIZE = 56;
  BLOWFISH_KEY_SIZE = 16;
  BLOWFISH128_KEY_SIZE = 16;
  _BLOWFISH_ROUNDS = 16;
  CAMELLIA_BLOCK_SIZE = 16;
  CAMELLIA128_KEY_SIZE = 16;
  CAMELLIA192_KEY_SIZE = 24;
  CAMELLIA256_KEY_SIZE = 32;
  _CAMELLIA128_NKEYS = 24;
  _CAMELLIA256_NKEYS = 32;
  CAST128_BLOCK_SIZE = 8;
  CAST5_MIN_KEY_SIZE = 5;
  CAST5_MAX_KEY_SIZE = 16;
  CCM_BLOCK_SIZE = 16;
  CCM_DIGEST_SIZE = 16;
  CCM_MIN_NONCE_SIZE = 7;
  CCM_MAX_NONCE_SIZE = 14;
  CHACHA_KEY_SIZE = 32;
  CHACHA_BLOCK_SIZE = 64;
  CHACHA_NONCE_SIZE = 8;
  CHACHA_NONCE96_SIZE = 12;
  _CHACHA_STATE_LENGTH = 16;
  CHACHA_POLY1305_BLOCK_SIZE = 64;
  CHACHA_POLY1305_KEY_SIZE = 32;
  CHACHA_POLY1305_NONCE_SIZE = CHACHA_NONCE96_SIZE;
  CHACHA_POLY1305_DIGEST_SIZE = 16;
  CURVE25519_SIZE = 32;
  DES_KEY_SIZE = 8;
  DES_BLOCK_SIZE = 8;
  _DES_KEY_LENGTH = 32;
  DES3_KEY_SIZE = 24;
  DES3_BLOCK_SIZE = DES_BLOCK_SIZE;
  DSA_SHA1_MIN_P_BITS = 512;
  DSA_SHA1_Q_OCTETS = 20;
  DSA_SHA1_Q_BITS = 160;
  DSA_SHA256_MIN_P_BITS = 1024;
  DSA_SHA256_Q_OCTETS = 32;
  DSA_SHA256_Q_BITS = 256;
  EAX_BLOCK_SIZE = 16;
  EAX_DIGEST_SIZE = 16;
  EAX_IV_SIZE = 16;
  ED25519_KEY_SIZE = 32;
  ED25519_SIGNATURE_SIZE = 64;
  GCM_BLOCK_SIZE = 16;
  GCM_IV_SIZE = GCM_BLOCK_SIZE-4;
  GCM_DIGEST_SIZE = 16;
  GCM_TABLE_BITS = 8;
  GOSTHASH94_BLOCK_SIZE = 32;
  GOSTHASH94_DIGEST_SIZE = 32;
  GOSTHASH94_DATA_SIZE = GOSTHASH94_BLOCK_SIZE;
  _KNUTH_LFIB_KK = 100;
  MD2_DIGEST_SIZE = 16;
  MD2_BLOCK_SIZE = 16;
  MD2_DATA_SIZE = MD2_BLOCK_SIZE;
  MD4_DIGEST_SIZE = 16;
  MD4_BLOCK_SIZE = 64;
  MD4_DATA_SIZE = MD4_BLOCK_SIZE;
  MD5_DIGEST_SIZE = 16;
  MD5_BLOCK_SIZE = 64;
  MD5_DATA_SIZE = MD5_BLOCK_SIZE;
  _MD5_DIGEST_LENGTH = 4;
  POLY1305_DIGEST_SIZE = 16;
  POLY1305_BLOCK_SIZE = 16;
  POLY1305_KEY_SIZE = 16;
  POLY1305_AES_KEY_SIZE = 32;
  POLY1305_AES_DIGEST_SIZE = 16;
  POLY1305_AES_NONCE_SIZE = 16;
  RIPEMD160_DIGEST_SIZE = 20;
  RIPEMD160_BLOCK_SIZE = 64;
  RIPEMD160_DATA_SIZE = RIPEMD160_BLOCK_SIZE;
  _RIPEMD160_DIGEST_LENGTH = 5;
  RSA_MINIMUM_N_OCTETS = 12;
  SALSA20_128_KEY_SIZE = 16;
  SALSA20_256_KEY_SIZE = 32;
  SALSA20_BLOCK_SIZE = 64;
  SALSA20_NONCE_SIZE = 8;
  SALSA20_IV_SIZE = SALSA20_NONCE_SIZE;
  SALSA20_MIN_KEY_SIZE = 16;
  SALSA20_MAX_KEY_SIZE = 32;
  SALSA20_KEY_SIZE = 32;
  _SALSA20_INPUT_LENGTH = 16;
  SERPENT_BLOCK_SIZE = 16;
  SERPENT_KEY_SIZE = 32;
  SERPENT_MIN_KEY_SIZE = 16;
  SERPENT_MAX_KEY_SIZE = 32;
  SERPENT128_KEY_SIZE = 16;
  SERPENT192_KEY_SIZE = 24;
  SERPENT256_KEY_SIZE = 32;
  SHA1_DIGEST_SIZE = 20;
  SHA1_BLOCK_SIZE = 64;
  SHA1_DATA_SIZE = SHA1_BLOCK_SIZE;
  _SHA1_DIGEST_LENGTH = 5;
  SHA256_DIGEST_SIZE = 32;
  SHA256_BLOCK_SIZE = 64;
  _SHA256_DIGEST_LENGTH = 8;
  SHA224_DIGEST_SIZE = 28;
  SHA224_BLOCK_SIZE = SHA256_BLOCK_SIZE;
  SHA224_DATA_SIZE = SHA256_BLOCK_SIZE;
  SHA256_DATA_SIZE = SHA256_BLOCK_SIZE;
  SHA512_DIGEST_SIZE = 64;
  SHA512_BLOCK_SIZE = 128;
  _SHA512_DIGEST_LENGTH = 8;
  SHA384_DIGEST_SIZE = 48;
  SHA384_BLOCK_SIZE = SHA512_BLOCK_SIZE;
  SHA512_224_DIGEST_SIZE = 28;
  SHA512_224_BLOCK_SIZE = SHA512_BLOCK_SIZE;
  SHA512_256_DIGEST_SIZE = 32;
  SHA512_256_BLOCK_SIZE = SHA512_BLOCK_SIZE;
  SHA512_DATA_SIZE = SHA512_BLOCK_SIZE;
  SHA384_DATA_SIZE = SHA512_BLOCK_SIZE;
  SHA3_STATE_LENGTH = 25;
  SHA3_224_DIGEST_SIZE = 28;
  SHA3_224_BLOCK_SIZE = 144;
  SHA3_256_DIGEST_SIZE = 32;
  SHA3_256_BLOCK_SIZE = 136;
  SHA3_384_DIGEST_SIZE = 48;
  SHA3_384_BLOCK_SIZE = 104;
  SHA3_512_DIGEST_SIZE = 64;
  SHA3_512_BLOCK_SIZE = 72;
  SHA3_224_DATA_SIZE = SHA3_224_BLOCK_SIZE;
  SHA3_256_DATA_SIZE = SHA3_256_BLOCK_SIZE;
  SHA3_384_DATA_SIZE = SHA3_384_BLOCK_SIZE;
  SHA3_512_DATA_SIZE = SHA3_512_BLOCK_SIZE;
  TWOFISH_BLOCK_SIZE = 16;
  TWOFISH_MIN_KEY_SIZE = 16;
  TWOFISH_MAX_KEY_SIZE = 32;
  TWOFISH_KEY_SIZE = 32;
  TWOFISH128_KEY_SIZE = 16;
  TWOFISH192_KEY_SIZE = 24;
  TWOFISH256_KEY_SIZE = 32;
  UMAC32_DIGEST_SIZE = 4;
  UMAC64_DIGEST_SIZE = 8;
  UMAC96_DIGEST_SIZE = 12;
  UMAC128_DIGEST_SIZE = 16;
  UMAC_BLOCK_SIZE = 1024;
  UMAC_MIN_NONCE_SIZE = 1;
  UMAC_MAX_NONCE_SIZE = AES_BLOCK_SIZE;
  UMAC_POLY64_BLOCKS = 16384;
  UMAC_P64_OFFSET = 59;
  UMAC_P128_OFFSET = 159;
  NETTLE_USE_MINI_GMP = 0;
  YARROW_KEY_EVENT_BUFFER = 16;


{ Pointers to basic pascal types, inserted by h2pas conversion program.}

Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;
  Ppcchar   = ^PAnsiChar;

Type
  TCuint = cuint;
  Tuint8_t = cuint8;
  Tculong = culong;
  Tuint64_t = cuint64;
  Tuint32_t = cuint32;
  Tcunsigned = cunsigned;
  Puint8_t = ^cuint8;
  Tsize_t = csize_t;
  Tuint16_t = cuint16;
  Tcushort = cushort;
  Tcuchar = cuchar;
  Puint32_t = cuint32;
  Tcint = cint;
  Tcchar = cchar;
  Psize_t = ^Tsize_t;
  Ttime_t = int32;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  Pnettle_block16 = ^Tnettle_block16;
  Tnettle_block16 = record
      case longint of
        0 : ( b : array[0..15] of Tuint8_t );
        1 : ( w : array[0..3] of Tculong );
        2 : ( u64 : array[0..1] of Tuint64_t );
      end;

  Pnettle_set_key_func = procedure(ctx: pointer; key: pbyte); cdecl;
  pnettle_random_func = procedure(ctx: pointer; len : tsize_t; dest : pbyte); cdecl;
  pnettle_progress_func = procedure(ctx: pointer; c : cint); cdecl;
  pnettle_realloc_func = function(ctx: pointer; p : pointer; size : tsize_t): pointer; cdecl;
  pnettle_cipher_func = procedure(ctx: pointer; len: tsize_t; dest: pbyte; src: pbyte); cdecl;
  pnettle_crypt_func = procedure(ctx: pointer; len : tsize_t; dest: pbyte; src: pbyte); cdecl;
  pnettle_hash_init_func = procedure(ctx: pointer); cdecl;
  pnettle_hash_update_func = procedure(ctx: pointer; len: tsize_t; src : pbyte); cdecl;
  pnettle_hash_digest_func = procedure(ctx: pointer; len: tsize_t; dest : pbyte); cdecl;
  pnettle_armor_length_func = function(len : tsize_t) : tsize_t; cdecl; 
  pnettle_armor_init_func = procedure(ctx: pointer); cdecl; 
  pnettle_armor_encode_update_func = function(ctx: pointer; dest: pbyte; len: tsize_t; src : pbyte): tsize_t; cdecl;
  pnettle_armor_encode_final_func  = function(ctx: pointer; dest: pbyte) : tsize_t; cdecl;
  pnettle_armor_decode_update_func = function(ctx: pointer; destlen: tsize_t; dest: pbyte; srclen: tsize_t; src : pbyte): cint; cdecl;
  pnettle_armor_decode_final_func = function(ctx: pointer): cint; cdecl;


  Paes128_ctx = ^Taes128_ctx;
  Taes128_ctx = record
      keys : array[0..(4*(10+1))-1] of Tuint32_t;
    end;

  Paes192_ctx = ^Taes192_ctx;
  Taes192_ctx = record
      keys : array[0..(4*(12+1))-1] of Tuint32_t;
    end;

  Paes256_ctx = ^Taes256_ctx;
  Taes256_ctx = record
      keys : array[0..(4*(14+1))-1] of Tuint32_t;
    end;

  Paes_ctx = ^Taes_ctx;
  Taes_ctx = record
      key_size : Tcunsigned;
      u : record
          case longint of
            0 : ( ctx128 : Taes128_ctx );
            1 : ( ctx192 : Taes192_ctx );
            2 : ( ctx256 : Taes256_ctx );
          end;
    end;

var
  nettle_aes128_set_encrypt_key : procedure(ctx:Paes128_ctx; key:Puint8_t);cdecl;
  nettle_aes128_set_decrypt_key : procedure(ctx:Paes128_ctx; key:Puint8_t);cdecl;
  nettle_aes128_invert_key : procedure(dst:Paes128_ctx; src:Paes128_ctx);cdecl;
  nettle_aes128_encrypt : procedure(ctx:Paes128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_aes128_decrypt : procedure(ctx:Paes128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_aes192_set_encrypt_key : procedure(ctx:Paes192_ctx; key:Puint8_t);cdecl;
  nettle_aes192_set_decrypt_key : procedure(ctx:Paes192_ctx; key:Puint8_t);cdecl;
  nettle_aes192_invert_key : procedure(dst:Paes192_ctx; src:Paes192_ctx);cdecl;
  nettle_aes192_encrypt : procedure(ctx:Paes192_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_aes192_decrypt : procedure(ctx:Paes192_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_aes256_set_encrypt_key : procedure(ctx:Paes256_ctx; key:Puint8_t);cdecl;
  nettle_aes256_set_decrypt_key : procedure(ctx:Paes256_ctx; key:Puint8_t);cdecl;
  nettle_aes256_invert_key : procedure(dst:Paes256_ctx; src:Paes256_ctx);cdecl;
  nettle_aes256_encrypt : procedure(ctx:Paes256_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_aes256_decrypt : procedure(ctx:Paes256_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_aes_set_encrypt_key : procedure(ctx:Paes_ctx; length:Tsize_t; key:Puint8_t);cdecl;
  nettle_aes_set_decrypt_key : procedure(ctx:Paes_ctx; length:Tsize_t; key:Puint8_t);cdecl;
  nettle_aes_invert_key : procedure(dst:Paes_ctx; src:Paes_ctx);cdecl;
  nettle_aes_encrypt : procedure(ctx:Paes_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl ;
  nettle_aes_decrypt : procedure(ctx:Paes_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t) deprecated;cdecl ;


Type
  Parcfour_ctx = ^Tarcfour_ctx;
  Tarcfour_ctx = record
      S : array[0..255] of Tuint8_t;
      i : Tuint8_t;
      j : Tuint8_t;
    end;
var
  nettle_arcfour_set_key : procedure(ctx:Parcfour_ctx; length:Tsize_t; key:Puint8_t);cdecl;
  nettle_arcfour128_set_key : procedure(ctx:Parcfour_ctx; key:Puint8_t);cdecl;
  nettle_arcfour_crypt : procedure(ctx:Parcfour_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;

type
  Parctwo_ctx = ^Tarctwo_ctx;
  Tarctwo_ctx = record
      S : array[0..63] of Tuint16_t;
    end;
var
  nettle_arctwo_set_key_ekb : procedure(ctx:Parctwo_ctx; length:Tsize_t; key:Puint8_t; ekb:Tcunsigned);cdecl;
  nettle_arctwo_set_key : procedure(ctx:Parctwo_ctx; length:Tsize_t; key:Puint8_t);cdecl;
  nettle_arctwo40_set_key : procedure(ctx:Parctwo_ctx; key:Puint8_t);cdecl;
  nettle_arctwo64_set_key : procedure(ctx:Parctwo_ctx; key:Puint8_t);cdecl;
  nettle_arctwo128_set_key : procedure(ctx:Parctwo_ctx; key:Puint8_t);cdecl;
  nettle_arctwo_set_key_gutmann : procedure(ctx:Parctwo_ctx; length:Tsize_t; key:Puint8_t);cdecl;
  nettle_arctwo128_set_key_gutmann : procedure(ctx:Parctwo_ctx; key:Puint8_t);cdecl;
  nettle_arctwo_encrypt : procedure(ctx:Parctwo_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_arctwo_decrypt : procedure(ctx:Parctwo_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;

type
  Tasn__class = (
    ASN1_TYPE_CONSTRUCTED := 1 shl 12,
    ASN1_CLASS_UNIVERSAL := 0,
    ASN1_CLASS_APPLICATION := 1 shl 13,
    ASN1_CLASS_CONTEXT_SPECIFIC := 2 shl 13,
    ASN1_CLASS_PRIVATE := 3 shl 13,
    ASN1_CLASS_MASK := 3 shl 13,
    ASN1_CLASS_SHIFT := 13);

  Tasn1_type = (
    ASN1_BOOLEAN := 1,
    ASN1_INTEGER := 2,
    ASN1_BITSTRING := 3,
    ASN1_OCTETSTRING := 4,
    ASN1_NULL := 5,
    ASN1_IDENTIFIER := 6,
    ASN1_REAL := 9,
    ASN1_ENUMERATED := 10,
    ASN1_UTF8STRING := 12,
    ASN1_SEQUENCE := 16 or ord(ASN1_TYPE_CONSTRUCTED),
    ASN1_SET := 17 or ord(ASN1_TYPE_CONSTRUCTED),
    ASN1_PRINTABLESTRING := 19,
    ASN1_TELETEXSTRING := 20,
    ASN1_IA5STRING := 22,
    ASN1_UTC := 23,
    ASN1_UNIVERSALSTRING := 28,
    ASN1_BMPSTRING := 30);

  Tasn1_iterator_result = (ASN1_ITERATOR_ERROR,ASN1_ITERATOR_PRIMITIVE,
    ASN1_ITERATOR_CONSTRUCTED,ASN1_ITERATOR_END
    );

  Pasn1_der_iterator = ^Tasn1_der_iterator;
  Tasn1_der_iterator = record
      buffer_length : Tsize_t;
      buffer : Puint8_t;
      pos : Tsize_t;
      _type : Tasn1_type;
      length : Tsize_t;
      data : Puint8_t;
    end;
var
  nettle_asn1_der_iterator_first : function(iterator:Pasn1_der_iterator; length:Tsize_t; input:Puint8_t):Tasn1_iterator_result;cdecl;
  nettle_asn1_der_iterator_next : function(iterator:Pasn1_der_iterator):Tasn1_iterator_result;cdecl;
  nettle_asn1_der_decode_constructed : function(i:Pasn1_der_iterator; contents:Pasn1_der_iterator):Tasn1_iterator_result;cdecl;
  nettle_asn1_der_decode_constructed_last : function(i:Pasn1_der_iterator):Tasn1_iterator_result;cdecl;
  nettle_asn1_der_decode_bitstring : function(i:Pasn1_der_iterator; contents:Pasn1_der_iterator):Tasn1_iterator_result;cdecl;
  nettle_asn1_der_decode_bitstring_last : function(i:Pasn1_der_iterator):Tasn1_iterator_result;cdecl;
  nettle_asn1_der_get_uint32 : function(i:Pasn1_der_iterator; x:Puint32_t):Tcint;cdecl;
  nettle_base16_encode_single : procedure(dst:pcchar; src:Tuint8_t);cdecl;

nettle_base16_encode_update : procedure(dst:pcchar; length:Tsize_t; src:Puint8_t);cdecl;
type
  Pbase16_decode_ctx = ^Tbase16_decode_ctx;
  Tbase16_decode_ctx = record
      word : Tcuchar;
      bits : Tcuchar;
    end;

var
  nettle_base16_decode_init : procedure(ctx:Pbase16_decode_ctx);cdecl;
  nettle_base16_decode_single : function(ctx:Pbase16_decode_ctx; dst:Puint8_t; src:Tcchar):Tcint;cdecl;
  nettle_base16_decode_update : function(ctx:Pbase16_decode_ctx; dst_length:Psize_t; dst:Puint8_t; src_length:Tsize_t; src:pcchar):Tcint;cdecl;
  nettle_base16_decode_final : function(ctx:Pbase16_decode_ctx):Tcint;cdecl;

type
  Pbase64_encode_ctx = ^Tbase64_encode_ctx;
  Tbase64_encode_ctx = record
      alphabet : pcchar;
      word : Tcushort;
      bits : Tcuchar;
    end;

var
  nettle_base64_encode_init : procedure(ctx:Pbase64_encode_ctx);cdecl;
  nettle_base64url_encode_init : procedure(ctx:Pbase64_encode_ctx);cdecl;
  nettle_base64_encode_single : function(ctx:Pbase64_encode_ctx; dst:pcchar; src:Tuint8_t):Tsize_t;cdecl;
  nettle_base64_encode_update : function(ctx:Pbase64_encode_ctx; dst:pcchar; length:Tsize_t; src:Puint8_t):Tsize_t;cdecl;
  nettle_base64_encode_final : function(ctx:Pbase64_encode_ctx; dst:pcchar):Tsize_t;cdecl;
  nettle_base64_encode_raw : procedure(dst:pcchar; length:Tsize_t; src:Puint8_t);cdecl;
  nettle_base64_encode_group : procedure(dst:pcchar; group:Tuint32_t);cdecl;

type
  Pbase64_decode_ctx = ^Tbase64_decode_ctx;
  Tbase64_decode_ctx = record
      table : pcschar;
      word : Tcushort;
      bits : Tcuchar;
      padding : Tcuchar;
    end;

var
  nettle_base64_decode_init : procedure(ctx:Pbase64_decode_ctx);cdecl;
  nettle_base64url_decode_init : procedure(ctx:Pbase64_decode_ctx);cdecl;
  nettle_base64_decode_single : function(ctx:Pbase64_decode_ctx; dst:Puint8_t; src:Tcchar):Tcint;cdecl;
  nettle_base64_decode_update : function(ctx:Pbase64_decode_ctx; dst_length:Psize_t; dst:Puint8_t; src_length:Tsize_t; src:pcchar):Tcint;cdecl;
  nettle_base64_decode_final : function(ctx:Pbase64_decode_ctx):Tcint;cdecl;

type
  Pnettle_cipher = ^Tnettle_cipher;
  PPnettle_cipher = ^Pnettle_cipher;
  Tnettle_cipher = record
      name : pcchar;
      context_size : Tcunsigned;
      block_size : Tcunsigned;
      key_size : Tcunsigned;
      set_encrypt_key : Pnettle_set_key_func;
      set_decrypt_key : Pnettle_set_key_func;
      encrypt : Pnettle_cipher_func;
      decrypt : Pnettle_cipher_func;
    end;

var
  nettle_get_ciphers : function: PPnettle_cipher;cdecl;
  nettle_aes128 : Pnettle_cipher;
  nettle_aes192 : Pnettle_cipher;
  nettle_aes256 : Pnettle_cipher;
  nettle_camellia128 : Pnettle_cipher;
  nettle_camellia192 : Pnettle_cipher;
  nettle_camellia256 : Pnettle_cipher;
  nettle_cast128 : Pnettle_cipher;
  nettle_serpent128 : Pnettle_cipher;
  nettle_serpent192 : Pnettle_cipher;
  nettle_serpent256 : Pnettle_cipher;
  nettle_twofish128 : Pnettle_cipher;
  nettle_twofish192 : Pnettle_cipher;
  nettle_twofish256 : Pnettle_cipher;
  nettle_arctwo40 : Pnettle_cipher;
  nettle_arctwo64 : Pnettle_cipher;
  nettle_arctwo128 : Pnettle_cipher;
  nettle_arctwo_gutmann128 : Pnettle_cipher;

type
  Pnettle_hash = ^Tnettle_hash;
  PPnettle_hash = ^Pnettle_hash;
  Tnettle_hash = record
      name : pcchar;
      context_size : Tcunsigned;
      digest_size : Tcunsigned;
      block_size : Tcunsigned;
      init : Pnettle_hash_init_func;
      update : Pnettle_hash_update_func;
      digest : Pnettle_hash_digest_func;
    end;

var
  nettle_get_hashes : function: PPnettle_hash;cdecl;
  nettle_lookup_hash : function(name:pcchar):Pnettle_hash;cdecl;

  nettle_md2 : Pnettle_hash;
  nettle_md4 : Pnettle_hash;
  nettle_md5 : Pnettle_hash;
  nettle_gosthash94 : Pnettle_hash;
  nettle_ripemd160 : Pnettle_hash;
  nettle_sha1 : Pnettle_hash;
  nettle_sha224 : Pnettle_hash;
  nettle_sha256 : Pnettle_hash;
  nettle_sha384 : Pnettle_hash;
  nettle_sha512 : Pnettle_hash;
  nettle_sha512_224 : Pnettle_hash;
  nettle_sha512_256 : Pnettle_hash;
  nettle_sha3_224 : Pnettle_hash;
  nettle_sha3_256 : Pnettle_hash;
  nettle_sha3_384 : Pnettle_hash;
  nettle_sha3_512 : Pnettle_hash;

type
  Pnettle_aead = ^Tnettle_aead;
  PPnettle_aead = ^Pnettle_aead;
  Tnettle_aead = record
      name : pcchar;
      context_size : Tcunsigned;
      block_size : Tcunsigned;
      key_size : Tcunsigned;
      nonce_size : Tcunsigned;
      digest_size : Tcunsigned;
      set_encrypt_key : Pnettle_set_key_func;
      set_decrypt_key : Pnettle_set_key_func;
      set_nonce : Pnettle_set_key_func;
      update : Pnettle_hash_update_func;
      encrypt : Pnettle_crypt_func;
      decrypt : Pnettle_crypt_func;
      digest : Pnettle_hash_digest_func;
    end;

var
    nettle_get_aeads : function:PPnettle_aead;cdecl;

    nettle_gcm_aes128 : Pnettle_aead;
    nettle_gcm_aes192 : Pnettle_aead;
    nettle_gcm_aes256 : Pnettle_aead;
    nettle_gcm_camellia128 : Pnettle_aead;
    nettle_gcm_camellia256 : Pnettle_aead;
    nettle_eax_aes128 : Pnettle_aead;
    nettle_chacha_poly1305 : Pnettle_aead;

type
  Pnettle_armor = ^Tnettle_armor;
  PPnettle_armor = ^Pnettle_armor;
  Tnettle_armor = record
      name : pcchar;
      encode_context_size : Tcunsigned;
      decode_context_size : Tcunsigned;
      encode_final_length : Tcunsigned;
      encode_init : Pnettle_armor_init_func;
      encode_length : Pnettle_armor_length_func;
      encode_update : Pnettle_armor_encode_update_func;
      encode_final : Pnettle_armor_encode_final_func;
      decode_init : Pnettle_armor_init_func;
      decode_length : Pnettle_armor_length_func;
      decode_update : Pnettle_armor_decode_update_func;
      decode_final : Pnettle_armor_decode_final_func;
    end;

var
  nettle_get_armors : function: PPnettle_armor;cdecl;

  nettle_base64 : pnettle_armor;
  nettle_base64url : pnettle_armor;
  nettle_base16 : pnettle_armor;

  nettle_version_major : function:Tcint;cdecl;
  nettle_version_minor : function:Tcint;cdecl;

type
  Pblowfish_ctx = ^Tblowfish_ctx;
  Tblowfish_ctx = record
      s : array[0..3] of array[0..255] of Tuint32_t;
      p : array[0..(16+2)-1] of Tuint32_t;
    end;

var
  nettle_blowfish_set_key : function(ctx:Pblowfish_ctx; length:Tsize_t; key:Puint8_t):Tcint;cdecl;
  nettle_blowfish128_set_key : function(ctx:Pblowfish_ctx; key:Puint8_t):Tcint;cdecl;
  nettle_blowfish_encrypt : procedure(ctx:Pblowfish_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_blowfish_decrypt : procedure(ctx:Pblowfish_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
    
  nettle_realloc : pnettle_realloc_func;cvar;public;
  nettle_xrealloc : pnettle_realloc_func;cvar;public;

type
  Pnettle_buffer = ^Tnettle_buffer;
  Tnettle_buffer = record
      contents : Puint8_t;
      alloc : Tsize_t;
      realloc_ctx : pointer;
      realloc : Pnettle_realloc_func;
      size : Tsize_t;
    end;

var
  nettle_buffer_init : procedure(buffer:Pnettle_buffer);cdecl;
  nettle_buffer_init_realloc : procedure(buffer:Pnettle_buffer; realloc_ctx:pointer; realloc:Pnettle_realloc_func);cdecl;
  nettle_buffer_init_size : procedure(buffer:Pnettle_buffer; length:Tsize_t; space:Puint8_t);cdecl;
  nettle_buffer_clear : procedure(buffer:Pnettle_buffer);cdecl;
  nettle_buffer_reset : procedure(buffer:Pnettle_buffer);cdecl;
  nettle_buffer_grow : function(buffer:Pnettle_buffer; length:Tsize_t):Tcint;cdecl;
  nettle_buffer_write : function(buffer:Pnettle_buffer; length:Tsize_t; data:Puint8_t):Tcint;cdecl;
  nettle_buffer_space : function(buffer:Pnettle_buffer; length:Tsize_t):Puint8_t;cdecl;
  nettle_buffer_copy : function(dst:Pnettle_buffer; src:Pnettle_buffer):Tcint;cdecl;

type
  Pcamellia128_ctx = ^Tcamellia128_ctx;
  Tcamellia128_ctx = record
      keys : array[0..23] of Tuint64_t;
    end;
  Pcamellia256_ctx = ^Tcamellia256_ctx;
  Tcamellia256_ctx = record
      keys : array[0..31] of Tuint64_t;
    end;

var
  nettle_camellia128_set_encrypt_key : procedure(ctx:Pcamellia128_ctx; key:Puint8_t);cdecl;
  nettle_camellia_set_decrypt_key : procedure(ctx:Pcamellia128_ctx; key:Puint8_t);cdecl;
  nettle_camellia128_invert_key : procedure(dst:Pcamellia128_ctx; src:Pcamellia128_ctx);cdecl;
  nettle_camellia128_crypt : procedure(ctx:Pcamellia128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_camellia256_set_encrypt_key : procedure(ctx:Pcamellia256_ctx; key:Puint8_t);cdecl;
  nettle_camellia256_set_decrypt_key : procedure(ctx:Pcamellia256_ctx; key:Puint8_t);cdecl;
  nettle_camellia256_invert_key : procedure(dst:Pcamellia256_ctx; src:Pcamellia256_ctx);cdecl;
  nettle_camellia256_crypt : procedure(ctx:Pcamellia256_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_camellia192_set_encrypt_key : procedure(ctx:Pcamellia256_ctx; key:Puint8_t);cdecl;
  nettle_camellia192_set_decrypt_key : procedure(ctx:Pcamellia256_ctx; key:Puint8_t);cdecl;

type
  Pcast128_ctx = ^Tcast128_ctx;
  Tcast128_ctx = record
      rounds : Tcunsigned;
      Kr : array[0..15] of Tcuchar;
      Km : array[0..15] of Tuint32_t;
    end;
var
  nettle_cast5_set_key : procedure(ctx:Pcast128_ctx; length:Tsize_t; key:Puint8_t);cdecl;
  nettle_cast128_set_key : procedure(ctx:Pcast128_ctx; key:Puint8_t);cdecl;
  nettle_cast128_encrypt : procedure(ctx:Pcast128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_cast128_decrypt : procedure(ctx:Pcast128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_cbc_encrypt : procedure(ctx:pointer; f:Pnettle_cipher_func; block_size:Tsize_t; iv:Puint8_t; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_cbc_decrypt : procedure(ctx:pointer; f:Pnettle_cipher_func; block_size:Tsize_t; iv:Puint8_t; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;

type
  Pccm_ctx = ^Tccm_ctx;
  Tccm_ctx = record
      ctr : Tnettle_block16;
      tag : Tnettle_block16;
      blength : Tcuint;
    end;

var
  nettle_ccm_set_nonce : procedure(ctx:Pccm_ctx; cipher:pointer; f:Pnettle_cipher_func; noncelen:Tsize_t; nonce:Puint8_t; authlen:Tsize_t; msglen:Tsize_t; taglen:Tsize_t);cdecl;
  nettle_ccm_update : procedure(ctx:Pccm_ctx; cipher:pointer; f:Pnettle_cipher_func; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_ccm_encrypt : procedure(ctx:Pccm_ctx; cipher:pointer; f:Pnettle_cipher_func; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_decrypt : procedure(ctx:Pccm_ctx; cipher:pointer; f:Pnettle_cipher_func; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_digest : procedure(ctx:Pccm_ctx; cipher:pointer; f:Pnettle_cipher_func; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_ccm_encrypt_message : procedure(cipher:pointer; f:Pnettle_cipher_func; nlength:Tsize_t; nonce:Puint8_t; alength:Tsize_t; adata:Puint8_t; tlength:Tsize_t; clength:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_decrypt_message : function(cipher:pointer; f:Pnettle_cipher_func; nlength:Tsize_t; nonce:Puint8_t; alength:Tsize_t; adata:Puint8_t; tlength:Tsize_t; mlength:Tsize_t; dst:Puint8_t; src:Puint8_t):Tcint;cdecl;

type
  Pccm_aes128_ctx = ^Tccm_aes128_ctx;
  Tccm_aes128_ctx = record
    ccm : Tccm_ctx;
    cipher : Taes128_ctx;
  end;
  Pccm_aes192_ctx = ^Tccm_aes192_ctx;
  Tccm_aes192_ctx = record
    ccm : Tccm_ctx;
    cipher : Taes192_ctx;
  end;
  Pccm_aes256_ctx = ^Tccm_aes256_ctx;
  Tccm_aes256_ctx = record
    ccm : Tccm_ctx;
    cipher : Taes256_ctx;
  end;

var
  nettle_ccm_aes128_set_key : procedure(ctx:Pccm_aes128_ctx; key:Puint8_t);cdecl;
  nettle_ccm_aes128_set_nonce : procedure(ctx:Pccm_aes128_ctx; length:Tsize_t; nonce:Puint8_t; authlen:Tsize_t; msglen:Tsize_t; taglen:Tsize_t);cdecl;
  nettle_ccm_aes128_update : procedure(ctx:Pccm_aes128_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_ccm_aes128_encrypt : procedure(ctx:Pccm_aes128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_aes128_decrypt : procedure(ctx:Pccm_aes128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_aes128_digest : procedure(ctx:Pccm_aes128_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_ccm_aes128_encrypt_message : procedure(ctx:Pccm_aes128_ctx; nlength:Tsize_t; nonce:Puint8_t; alength:Tsize_t; adata:Puint8_t; tlength:Tsize_t; clength:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_aes128_decrypt_message : function(ctx:Pccm_aes128_ctx; nlength:Tsize_t; nonce:Puint8_t; alength:Tsize_t; adata:Puint8_t; tlength:Tsize_t; mlength:Tsize_t; dst:Puint8_t; src:Puint8_t):Tcint;cdecl;
  nettle_ccm_aes192_set_key : procedure(ctx:Pccm_aes192_ctx; key:Puint8_t);cdecl;
  nettle_ccm_aes192_set_nonce : procedure(ctx:Pccm_aes192_ctx; length:Tsize_t; nonce:Puint8_t; authlen:Tsize_t; msglen:Tsize_t; taglen:Tsize_t);cdecl;
  nettle_ccm_aes192_update : procedure(ctx:Pccm_aes192_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_ccm_aes192_encrypt : procedure(ctx:Pccm_aes192_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_aes192_decrypt : procedure(ctx:Pccm_aes192_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_aes192_digest : procedure(ctx:Pccm_aes192_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_ccm_aes192_encrypt_message : procedure(ctx:Pccm_aes192_ctx; nlength:Tsize_t; nonce:Puint8_t; alength:Tsize_t; adata:Puint8_t; tlength:Tsize_t; clength:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_aes192_decrypt_message : function(ctx:Pccm_aes192_ctx; nlength:Tsize_t; nonce:Puint8_t; alength:Tsize_t; adata:Puint8_t; tlength:Tsize_t; mlength:Tsize_t; dst:Puint8_t; src:Puint8_t):Tcint;cdecl;
  nettle_ccm_aes256_set_key : procedure(ctx:Pccm_aes256_ctx; key:Puint8_t);cdecl;
  nettle_ccm_aes256_set_nonce : procedure(ctx:Pccm_aes256_ctx; length:Tsize_t; nonce:Puint8_t; authlen:Tsize_t; msglen:Tsize_t; taglen:Tsize_t);cdecl;
  nettle_ccm_aes256_update : procedure(ctx:Pccm_aes256_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_ccm_aes256_encrypt : procedure(ctx:Pccm_aes256_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_aes256_decrypt : procedure(ctx:Pccm_aes256_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_aes256_digest : procedure(ctx:Pccm_aes256_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_ccm_aes256_encrypt_message : procedure(ctx:Pccm_aes256_ctx; nlength:Tsize_t; nonce:Puint8_t; alength:Tsize_t; adata:Puint8_t; tlength:Tsize_t; clength:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_ccm_aes256_decrypt_message : function(ctx:Pccm_aes256_ctx; nlength:Tsize_t; nonce:Puint8_t; alength:Tsize_t; adata:Puint8_t; tlength:Tsize_t; mlength:Tsize_t; dst:Puint8_t; src:Puint8_t):Tcint;cdecl;
  nettle_cfb_encrypt : procedure(ctx:pointer; f:Pnettle_cipher_func; block_size:Tsize_t; iv:Puint8_t; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_cfb_decrypt : procedure(ctx:pointer; f:Pnettle_cipher_func; block_size:Tsize_t; iv:Puint8_t; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_cfb8_encrypt : procedure(ctx:pointer; f:Pnettle_cipher_func; block_size:Tsize_t; iv:Puint8_t; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_cfb8_decrypt : procedure(ctx:pointer; f:Pnettle_cipher_func; block_size:Tsize_t; iv:Puint8_t; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;

type
  Pchacha_ctx = ^Tchacha_ctx;
  Tchacha_ctx = record
      state : array[0..15] of Tuint32_t;
    end;

var
  nettle_chacha_set_key : procedure(ctx:Pchacha_ctx; key:Puint8_t);cdecl;
  nettle_chacha_set_nonce : procedure(ctx:Pchacha_ctx; nonce:Puint8_t);cdecl;
  nettle_chacha_set_nonce96 : procedure(ctx:Pchacha_ctx; nonce:Puint8_t);cdecl;
  nettle_chacha_crypt : procedure(ctx:Pchacha_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;

type
  Ppoly1305_ctx = ^Tpoly1305_ctx;
  Tpoly1305_ctx = record
    r : record
        case longint of
          0 : ( r32 : array[0..5] of Tuint32_t );
          1 : ( r64 : array[0..2] of Tuint64_t );
        end;
    s32 : array[0..2] of Tuint32_t;
    hh : Tuint32_t;
    h : record
        case longint of
          0 : ( h32 : array[0..3] of Tuint32_t );
          1 : ( h64 : array[0..1] of Tuint64_t );
        end;
    end;

  tnettle_poly1305_set_keytype = array[0..15] of Tuint8_t;
  pnettle_poly1305_set_keytype = ^tnettle_poly1305_set_keytype;

  type
    Ppoly1305_aes_ctx = ^Tpoly1305_aes_ctx;
    Tpoly1305_aes_ctx = record
        pctx : Tpoly1305_ctx;
        block : array[0..15] of Tuint8_t;
        index : Tcunsigned;
        nonce : array[0..15] of Tuint8_t;
        aes : Taes128_ctx;
      end;

var
  nettle_poly1305_set_key : procedure(ctx:Ppoly1305_ctx; key: pnettle_poly1305_set_keytype);cdecl;
  nettle_poly1305_digest : procedure(ctx:Ppoly1305_ctx; s:Pnettle_block16);cdecl;
  _nettle_poly1305_block : procedure(ctx:Ppoly1305_ctx; m:Puint8_t; high:Tcunsigned);cdecl;
  nettle_poly1305_aes_set_key : procedure(ctx:Ppoly1305_aes_ctx; key:Puint8_t);cdecl;
  nettle_poly1305_aes_set_nonce : procedure(ctx:Ppoly1305_aes_ctx; nonce:Puint8_t);cdecl;
  nettle_poly1305_aes_update : procedure(ctx:Ppoly1305_aes_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_poly1305_aes_digest : procedure(ctx:Ppoly1305_aes_ctx; length:Tsize_t; digest:Puint8_t);cdecl;

type
  Pchacha_poly1305_ctx = ^Tchacha_poly1305_ctx;
  Tchacha_poly1305_ctx = record
      chacha : Tchacha_ctx;
      poly1305 : Tpoly1305_ctx;
      s : Tnettle_block16;
      auth_size : Tuint64_t;
      data_size : Tuint64_t;
      block : array[0..15] of Tuint8_t;
      index : Tcunsigned;
    end;
var
  nettle_chacha_poly1305_set_key : procedure(ctx:Pchacha_poly1305_ctx; key:Puint8_t);cdecl;
  nettle_chacha_poly1305_set_nonce : procedure(ctx:Pchacha_poly1305_ctx; nonce:Puint8_t);cdecl;  
  nettle_chacha_poly1305_update : procedure(ctx:Pchacha_poly1305_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_chacha_poly1305_encrypt : procedure(ctx:Pchacha_poly1305_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_chacha_poly1305_decrypt : procedure(ctx:Pchacha_poly1305_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_chacha_poly1305_digest : procedure(ctx:Pchacha_poly1305_ctx; length:Tsize_t; digest:Puint8_t);cdecl;

type
  Pcmac128_key = ^Tcmac128_key;
  Tcmac128_key = record
      K1 : Tnettle_block16;
      K2 : Tnettle_block16;
    end;

  Pcmac128_ctx = ^Tcmac128_ctx;
  Tcmac128_ctx = record
      X : Tnettle_block16;
      block : Tnettle_block16;
      index : Tsize_t;
    end;
var
  nettle_cmac128_set_key : procedure(key:Pcmac128_key; cipher:pointer; encrypt:Pnettle_cipher_func);cdecl;
  nettle_cmac128_init : procedure(ctx:Pcmac128_ctx);cdecl;
  nettle_cmac128_update : procedure(ctx:Pcmac128_ctx; cipher:pointer; encrypt:Pnettle_cipher_func; msg_len:Tsize_t; msg:Puint8_t);cdecl;
  nettle_cmac128_digest : procedure(ctx:Pcmac128_ctx; key:Pcmac128_key; cipher:pointer; encrypt:Pnettle_cipher_func; length:Tcunsigned;  digest:Puint8_t);cdecl;

type
  Pcmac_aes128_ctx = ^Tcmac_aes128_ctx;
  Tcmac_aes128_ctx = record
      key : Tcmac128_key;
      ctx : Tcmac128_ctx;
      cipher : Taes128_ctx;
    end;
var
  nettle_cmac_aes128_set_key : procedure(ctx:Pcmac_aes128_ctx; key:Puint8_t);cdecl;
  nettle_cmac_aes128_update : procedure(ctx:Pcmac_aes128_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_cmac_aes128_digest : procedure(ctx:Pcmac_aes128_ctx; length:Tsize_t; digest:Puint8_t);cdecl;

type
  Pcmac_aes256_ctx = ^Tcmac_aes256_ctx;
  Tcmac_aes256_ctx = record
      key : Tcmac128_key;
      ctx : Tcmac128_ctx;
      cipher : Taes256_ctx;
    end;
var
  nettle_cmac_aes256_set_key : procedure(ctx:Pcmac_aes256_ctx; key:Puint8_t);cdecl;
  nettle_cmac_aes256_update : procedure(ctx:Pcmac_aes256_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_cmac_aes256_digest : procedure(ctx:Pcmac_aes256_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_ctr_crypt : procedure(ctx:pointer; f:Pnettle_cipher_func; block_size:Tsize_t; ctr:Puint8_t; length:Tsize_t;  dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_curve25519_mul_g : procedure(q:Puint8_t; n:Puint8_t);cdecl;
  nettle_curve25519_mul : procedure(q:Puint8_t; n:Puint8_t; p:Puint8_t);cdecl;
type
  Pdes_ctx = ^Tdes_ctx;
  Tdes_ctx = record
      key : array[0..31] of Tuint32_t;
    end;

var
  nettle_des_set_key : function(ctx:Pdes_ctx; key:Puint8_t):Tcint;cdecl;
  nettle_des_encrypt : procedure(ctx:Pdes_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_des_decrypt : procedure(ctx:Pdes_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_des_check_parity : function(length:Tsize_t; key:Puint8_t):Tcint;cdecl;
  nettle_des_fix_parity : procedure(length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;

type
  Pdes3_ctx = ^Tdes3_ctx;
  Tdes3_ctx = record
      des : array[0..2] of Tdes_ctx;
    end;

var
  nettle_des3_set_key : function(ctx:Pdes3_ctx; key:Puint8_t):Tcint;cdecl;
  nettle_des3_encrypt : procedure(ctx:Pdes3_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_des3_decrypt : procedure(ctx:Pdes3_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;

Type
  Pdsa_params = ^Tdsa_params;
  Tdsa_params = record
      p : Tmpz_t;
      q : Tmpz_t;
      g : Tmpz_t;
    end;

  Pdsa_signature = ^Tdsa_signature;
  Tdsa_signature = record
      r : Tmpz_t;
      s : Tmpz_t;
    end;


  Tsexp_type = (SEXP_ATOM,SEXP_LIST,SEXP_END);
  Psexp_iterator = ^Tsexp_iterator;
  Tsexp_iterator = record
      length : Tsize_t;
      buffer : Puint8_t;
      start : Tsize_t;
      pos : Tsize_t;
      level : Tcunsigned;
      _type : Tsexp_type;
      display_length : Tsize_t;
      display : Puint8_t;
      atom_length : Tsize_t;
      atom : Puint8_t;
    end;

var

  nettle_dsa_params_init : procedure(params:Pdsa_params);cdecl;
  nettle_dsa_params_clear : procedure(params:Pdsa_params);cdecl;
  nettle_dsa_signature_init : procedure(signature:Pdsa_signature);cdecl;
  nettle_dsa_signature_clear : procedure(signature:Pdsa_signature);cdecl;
  nettle_dsa_sign : function(params:Pdsa_params; x:Pmpz_t; random_ctx:pointer; random:Pnettle_random_func; digest_size:Tsize_t; 
    digest:Puint8_t; signature:Pdsa_signature):Tcint;cdecl;
  nettle_dsa_verify : function(params:Pdsa_params; y:Pmpz_t; digest_size:Tsize_t; digest:Puint8_t; signature:Pdsa_signature):Tcint;cdecl;
  nettle_dsa_generate_params : function(params:Pdsa_params; random_ctx:pointer; random:Pnettle_random_func; progress_ctx:pointer; progress:Pnettle_progress_func; 
    p_bits:Tcunsigned; q_bits:Tcunsigned):Tcint;cdecl;
  nettle_dsa_generate_keypair : procedure(params:Pdsa_params; pub:Pmpz_t; key:Pmpz_t; random_ctx:pointer; random:Pnettle_random_func);cdecl;
  nettle_dsa_keypair_to_sexp : function(buffer:Pnettle_buffer; algorithm_name:pcchar; params:Pdsa_params; pub:Pmpz_t; priv:Pmpz_t):Tcint;cdecl;
  nettle_dsa_signature_from_sexp : function(rs:Pdsa_signature; i:Psexp_iterator; q_bits:Tcunsigned):Tcint;cdecl;
  nettle_dsa_keypair_from_sexp_alist : function(params:Pdsa_params; pub:Pmpz_t; priv:Pmpz_t; p_max_bits:Tcunsigned; q_bits:Tcunsigned; 
    i:Psexp_iterator):Tcint;cdecl;
  nettle_dsa_sha1_keypair_from_sexp : function(params:Pdsa_params; pub:Pmpz_t; priv:Pmpz_t; p_max_bits:Tcunsigned; length:Tsize_t; 
    expr:Puint8_t):Tcint;cdecl;
  nettle_dsa_sha256_keypair_from_sexp : function(params:Pdsa_params; pub:Pmpz_t; priv:Pmpz_t; p_max_bits:Tcunsigned; length:Tsize_t; 
    expr:Puint8_t):Tcint;cdecl;
  nettle_dsa_params_from_der_iterator : function(params:Pdsa_params; max_bits:Tcunsigned; q_bits:Tcunsigned; i:Pasn1_der_iterator):Tcint;cdecl;
  nettle_dsa_public_key_from_der_iterator : function(params:Pdsa_params; pub:Pmpz_t; i:Pasn1_der_iterator):Tcint;cdecl;
  nettle_dsa_openssl_private_key_from_der_iterator : function(params:Pdsa_params; pub:Pmpz_t; priv:Pmpz_t; p_max_bits:Tcunsigned; i:Pasn1_der_iterator):Tcint;cdecl;
  nettle_openssl_provate_key_from_der : function(params:Pdsa_params; pub:Pmpz_t; priv:Pmpz_t; p_max_bits:Tcunsigned; length:Tsize_t; 
    data:Puint8_t):Tcint;cdecl;

type
  Psha1_ctx = ^Tsha1_ctx;
  Tsha1_ctx = record
      state : array[0..4] of Tuint32_t;
      count : Tuint64_t;
      index : Tcuint;
      block : array[0..63] of Tuint8_t;
    end;

  Psha256_ctx = ^Tsha256_ctx;
  Tsha256_ctx = record
      state : array[0..7] of Tuint32_t;
      count : Tuint64_t;
      index : Tcuint;
      block : array[0..63] of Tuint8_t;
    end;
  Psha512_ctx = ^Tsha512_ctx;
  Tsha512_ctx = record
      state : array[0..7] of Tuint64_t;
      count_low : Tuint64_t;
      count_high : Tuint64_t;
      index : Tcuint;
      block : array[0..127] of Tuint8_t;
    end;

var
  nettle_sha1_init : procedure(ctx:Psha1_ctx);cdecl;
  nettle_sha1_update : procedure(ctx:Psha1_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_sha1_digest : procedure(ctx:Psha1_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_sha1_compress : procedure(state:Puint32_t; data:Puint8_t);cdecl;
  nettle_sha256_init : procedure(ctx:Psha256_ctx);cdecl;
  nettle_sha256_update : procedure(ctx:Psha256_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_sha256_digest : procedure(ctx:Psha256_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_sha224_init : procedure(ctx:Psha256_ctx);cdecl;
  nettle_sha224_digest : procedure(ctx:Psha256_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_sha512_init : procedure(ctx:Psha512_ctx);cdecl;
  nettle_sha512_update : procedure(ctx:Psha512_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_sha512_digest : procedure(ctx:Psha512_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_sha384_init : procedure(ctx:Psha512_ctx);cdecl;
  nettle_sha384_digest : procedure(ctx:Psha512_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_sha512_224_init : procedure(ctx:Psha512_ctx);cdecl;
  nettle_sha512_224_digest : procedure(ctx:Psha512_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_sha512_256_init : procedure(ctx:Psha512_ctx);cdecl;
  nettle_sha512_256_digest : procedure(ctx:Psha512_ctx; length:Tsize_t; digest:Puint8_t);cdecl;

type
  Pdsa_public_key = ^Tdsa_public_key;
  Tdsa_public_key = record
      p : Tmpz_t;
      q : Tmpz_t;
      g : Tmpz_t;
      y : Tmpz_t;
    end;

  Pdsa_private_key = ^Tdsa_private_key;
  Tdsa_private_key = record
      x : Tmpz_t;
    end;

var
  nettle_dsa_public_key_init : procedure(key:Pdsa_public_key);cdecl;
  nettle_dsa_public_key_clear : procedure(key:Pdsa_public_key);cdecl;
  nettle_dsa_private_key_init : procedure(key:Pdsa_private_key);cdecl;
  nettle_dsa_private_key_clear : procedure(key:Pdsa_private_key);cdecl;
  nettle_dsa_sha1_sign : function(pub:Pdsa_public_key; key:Pdsa_private_key; random_ctx:pointer; random:Pnettle_random_func; hash:Psha1_ctx; 
      signature:Pdsa_signature):Tcint;cdecl;
  nettle_dsa_sha256_sign : function(pub:Pdsa_public_key; key:Pdsa_private_key; random_ctx:pointer; random:Pnettle_random_func; hash:Psha256_ctx; 
      signature:Pdsa_signature):Tcint;cdecl;
  nettle_dsa_sha1_verify : function(key:Pdsa_public_key; hash:Psha1_ctx; signature:Pdsa_signature):Tcint;cdecl;
  nettle_dsa_sha256_verify : function(key:Pdsa_public_key; hash:Psha256_ctx; signature:Pdsa_signature):Tcint;cdecl;
  nettle_dsa_sha1_sign_digest : function(pub:Pdsa_public_key; key:Pdsa_private_key; random_ctx:pointer; random:Pnettle_random_func; digest:Puint8_t; 
      signature:Pdsa_signature):Tcint;cdecl;
  nettle_dsa_sha256_sign_digest : function(pub:Pdsa_public_key; key:Pdsa_private_key; random_ctx:pointer; random:Pnettle_random_func; digest:Puint8_t; 
      signature:Pdsa_signature):Tcint;cdecl;
  nettle_dsa_sha1_verify_digest : function(key:Pdsa_public_key; digest:Puint8_t; signature:Pdsa_signature):Tcint;cdecl;
  nettle_dsa_sha256_verify_digest : function(key:Pdsa_public_key; digest:Puint8_t; signature:Pdsa_signature):Tcint;cdecl;
  nettle_dsa_compat_generate_keypair : function(pub:Pdsa_public_key; key:Pdsa_private_key; random_ctx:pointer; random:Pnettle_random_func; progress_ctx:pointer; 
      progress:Pnettle_progress_func; p_bits:Tcunsigned; q_bits:Tcunsigned):Tcint;cdecl;

type
  Peax_key = ^Teax_key;
  Teax_key = record
      pad_block : Tnettle_block16;
      pad_partial : Tnettle_block16;
    end;

  Peax_ctx = ^Teax_ctx;
  Teax_ctx = record
      omac_nonce : Tnettle_block16;
      omac_data : Tnettle_block16;
      omac_message : Tnettle_block16;
      ctr : Tnettle_block16;
    end;

  Peax_aes128_ctx = ^Teax_aes128_ctx;
  Teax_aes128_ctx = record
      key : Teax_key;
      eax : Teax_ctx;
      cipher : Taes128_ctx;
    end;

var
  nettle_eax_set_key : procedure(key:Peax_key; cipher:pointer; f:Pnettle_cipher_func);cdecl;
  nettle_eax_set_nonce : procedure(eax:Peax_ctx; key:Peax_key; cipher:pointer; f:Pnettle_cipher_func; nonce_length:Tsize_t; nonce:Puint8_t);cdecl;
  nettle_eax_update : procedure(eax:Peax_ctx; key:Peax_key; cipher:pointer; f:Pnettle_cipher_func; data_length:Tsize_t; data:Puint8_t);cdecl;
  nettle_eax_encrypt : procedure(eax:Peax_ctx; key:Peax_key; cipher:pointer; f:Pnettle_cipher_func; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_eax_decrypt : procedure(eax:Peax_ctx; key:Peax_key; cipher:pointer; f:Pnettle_cipher_func; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_eax_digest : procedure(eax:Peax_ctx; key:Peax_key; cipher:pointer; f:Pnettle_cipher_func; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_eax_aes128_set_key : procedure(ctx:Peax_aes128_ctx; key:Puint8_t);cdecl;
  nettle_eax_aes128_set_nonce : procedure(ctx:Peax_aes128_ctx; length:Tsize_t; iv:Puint8_t);cdecl;
  nettle_eax_aes128_update : procedure(ctx:Peax_aes128_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_eax_aes128_encrypt : procedure(ctx:Peax_aes128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_eax_aes128_decrypt : procedure(ctx:Peax_aes128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_eax_aes128_digest : procedure(ctx:Peax_aes128_ctx; length:Tsize_t; digest:Puint8_t);cdecl;

type
  Pecc_curve = ^Tecc_curve;
  Tecc_curve = record
      {undefined structure}
    end;
  Pecc_point = ^Tecc_point;
  Tecc_point = record
      ecc : Pecc_curve;
      p : Pmp_limb_t;
    end;
  Pecc_scalar = ^Tecc_scalar;
  Tecc_scalar = record
      ecc : Pecc_curve;
      p : Pmp_limb_t;
    end;

var
  nettle_get_secp_192r1 : function:Pecc_curve;cdecl;
  nettle_get_secp_224r1 : function:Pecc_curve;cdecl; 
  nettle_get_secp_256r1 : function:Pecc_curve;cdecl;
  nettle_get_secp_384r1 : function:Pecc_curve;cdecl;
  nettle_get_secp_521r1 : function:Pecc_curve;cdecl;
  nettle_ecc_point_init : procedure(p:Pecc_point; ecc:Pecc_curve);cdecl;
  nettle_ecc_point_clear : procedure(p:Pecc_point);cdecl;
  nettle_ecc_point_set : function(p:Pecc_point; x:Pmpz_t; y:Pmpz_t):Tcint;cdecl;
  nettle_ecc_point_get : procedure(p:Pecc_point; x:Pmpz_t; y:Pmpz_t);cdecl;
  nettle_ecc_scalar_init : procedure(s:Pecc_scalar; ecc:Pecc_curve);cdecl;
  nettle_ecc_scalar_clear : procedure(s:Pecc_scalar);cdecl;
  nettle_ecc_scalar_set : function(s:Pecc_scalar; z:Pmpz_t):Tcint;cdecl;
  nettle_ecc_scalar_get : procedure(s:Pecc_scalar; z:Pmpz_t);cdecl;
  nettle_ecc_scalar_random : procedure(s:Pecc_scalar; random_ctx:pointer; random:Pnettle_random_func);cdecl;
  nettle_ecc_point_mul : procedure(r:Pecc_point; n:Pecc_scalar; p:Pecc_point);cdecl;
  nettle_ecc_point_mul_g : procedure(r:Pecc_point; n:Pecc_scalar);cdecl;
  nettle_ecc_bit_size : function(ecc:Pecc_curve):Tcunsigned;cdecl;
  nettle_ecc_size : function(ecc:Pecc_curve):Tmp_size_t;cdecl;
  nettle_ecc_size_a : function(ecc:Pecc_curve):Tmp_size_t;cdecl;
  nettle_ecc_size_j : function(ecc:Pecc_curve):Tmp_size_t;cdecl;
  nettle_ecdsa_sign : procedure(key:Pecc_scalar; random_ctx:pointer; random:Pnettle_random_func; digest_length:Tsize_t; digest:Puint8_t; signature:Pdsa_signature);cdecl;
  nettle_ecdsa_verify : function(pub:Pecc_point; length:Tsize_t; digest:Puint8_t; signature:Pdsa_signature):Tcint;cdecl;
  nettle_ecdsa_generate_keypair : procedure(pub:Pecc_point; key:Pecc_scalar; random_ctx:pointer; random:Pnettle_random_func);cdecl;
  nettle_ecc_ecdsa_sign_itch : function(ecc:Pecc_curve):Tmp_size_t;cdecl;
  nettle_ecc_ecdsa_sign : procedure(ecc:Pecc_curve; zp:Pmp_limb_t; kp:Pmp_limb_t; length:Tsize_t; digest:Puint8_t; rp:Pmp_limb_t; sp:Pmp_limb_t; scratch:Pmp_limb_t);cdecl;
  nettle_ecc_ecdsa_verify_itch : function(ecc:Pecc_curve):Tmp_size_t;cdecl;
  nettle_ecc_ecdsa_verify : function(ecc:Pecc_curve; pp:Pmp_limb_t; length:Tsize_t; digest:Puint8_t; rp:Pmp_limb_t;  sp:Pmp_limb_t; scratch:Pmp_limb_t):Tcint;cdecl;
  nettle_ed25519_sha512_public_key : procedure(pub:Puint8_t; priv:Puint8_t);cdecl;
  nettle_ed25519_sha512_sign : procedure(pub:Puint8_t; priv:Puint8_t; length:Tsize_t; msg:Puint8_t; signature:Puint8_t);cdecl;
  nettle_ed25519_sha512_verify : function(pub:Puint8_t; length:Tsize_t; msg:Puint8_t; signature:Puint8_t):Tcint;cdecl;

type
  Pgcm_key = ^Tgcm_key;
  Tgcm_key = record
      h : array[0..(1 shl 8)-1] of Tnettle_block16;
    end;

  Pgcm_ctx = ^Tgcm_ctx;
  Tgcm_ctx = record
      iv : Tnettle_block16;
      ctr : Tnettle_block16;
      x : Tnettle_block16;
      auth_size : Tuint64_t;
      data_size : Tuint64_t;
    end;

  Pgcm_aes128_ctx = ^Tgcm_aes128_ctx;
  Tgcm_aes128_ctx = record
      key : Tgcm_key;
      gcm : Tgcm_ctx;
      cipher : Taes128_ctx;
    end;

  Pgcm_aes192_ctx = ^Tgcm_aes192_ctx;
  Tgcm_aes192_ctx = record
      key : Tgcm_key;
      gcm : Tgcm_ctx;
      cipher : Taes192_ctx;
    end;

  Pgcm_aes256_ctx = ^Tgcm_aes256_ctx;
  Tgcm_aes256_ctx = record
      key : Tgcm_key;
      gcm : Tgcm_ctx;
      cipher : Taes256_ctx;
    end;

  Pgcm_aes_ctx = ^Tgcm_aes_ctx;
  Tgcm_aes_ctx = record
      key : Tgcm_key;
      gcm : Tgcm_ctx;
      cipher : Taes_ctx;
    end;

  Pgcm_camellia128_ctx = ^Tgcm_camellia128_ctx;
  Tgcm_camellia128_ctx = record
      key : Tgcm_key;
      gcm : Tgcm_ctx;
      cipher : Tcamellia128_ctx;
    end;
  Pgcm_camellia256_ctx = ^Tgcm_camellia256_ctx;
  Tgcm_camellia256_ctx = record
      key : Tgcm_key;
      gcm : Tgcm_ctx;
      cipher : Tcamellia256_ctx;
    end;

var
  nettle_gcm_set_key : procedure(key:Pgcm_key; cipher:pointer; f:Pnettle_cipher_func);cdecl;
  nettle_gcm_set_iv : procedure(ctx:Pgcm_ctx; key:Pgcm_key; length:Tsize_t; iv:Puint8_t);cdecl;
  nettle_gcm_update : procedure(ctx:Pgcm_ctx; key:Pgcm_key; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_gcm_encrypt : procedure(ctx:Pgcm_ctx; key:Pgcm_key; cipher:pointer; f:Pnettle_cipher_func; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_decrypt : procedure(ctx:Pgcm_ctx; key:Pgcm_key; cipher:pointer; f:Pnettle_cipher_func; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_digest : procedure(ctx:Pgcm_ctx; key:Pgcm_key; cipher:pointer; f:Pnettle_cipher_func; length:Tsize_t;  digest:Puint8_t);cdecl;
  nettle_gcm_aes128_set_key : procedure(ctx:Pgcm_aes128_ctx; key:Puint8_t);cdecl;
  nettle_gcm_aes128_update : procedure(ctx:Pgcm_aes128_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_gcm_aes128_set_iv : procedure(ctx:Pgcm_aes128_ctx; length:Tsize_t; iv:Puint8_t);cdecl;
  nettle_gcm_aes128_encrypt : procedure(ctx:Pgcm_aes128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_aes128_decrypt : procedure(ctx:Pgcm_aes128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_aes128_digest : procedure(ctx:Pgcm_aes128_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_gcm_aes192_set_key : procedure(ctx:Pgcm_aes192_ctx; key:Puint8_t);cdecl;
  nettle_gcm_aes192_update : procedure(ctx:Pgcm_aes192_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_gcm_aes192_set_iv : procedure(ctx:Pgcm_aes192_ctx; length:Tsize_t; iv:Puint8_t);cdecl;
  nettle_gcm_aes192_encrypt : procedure(ctx:Pgcm_aes192_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_aes192_decrypt : procedure(ctx:Pgcm_aes192_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_aes192_digest : procedure(ctx:Pgcm_aes192_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_gcm_aes256_set_key : procedure(ctx:Pgcm_aes256_ctx; key:Puint8_t);cdecl;
  nettle_gcm_aes256_update : procedure(ctx:Pgcm_aes256_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_gcm_aes256_set_iv : procedure(ctx:Pgcm_aes256_ctx; length:Tsize_t; iv:Puint8_t);cdecl;
  nettle_gcm_aes256_encrypt : procedure(ctx:Pgcm_aes256_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_aes256_decrypt : procedure(ctx:Pgcm_aes256_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_aes256_digest : procedure(ctx:Pgcm_aes256_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_gcm_aes_set_key : procedure(ctx:Pgcm_aes_ctx; length:Tsize_t; key:Puint8_t) deprecated;cdecl;
  nettle_gcm_aes_set_iv : procedure(ctx:Pgcm_aes_ctx; length:Tsize_t; iv:Puint8_t) deprecated; cdecl;
  nettle_gcm_aes_update : procedure(ctx:Pgcm_aes_ctx; length:Tsize_t; data:Puint8_t) deprecated; cdecl;
  nettle_gcm_aes_encrypt : procedure(ctx:Pgcm_aes_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t) deprecated;cdecl;
  nettle_gcm_aes_decrypt : procedure(ctx:Pgcm_aes_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t) deprecated;cdecl;
  nettle_gcm_aes_digest : procedure(ctx:Pgcm_aes_ctx; length:Tsize_t; digest:Puint8_t) deprecated;cdecl;
  nettle_gcm_camellia128_set_key : procedure(ctx:Pgcm_camellia128_ctx; key:Puint8_t);cdecl;
  nettle_gcm_camellia128_set_iv : procedure(ctx:Pgcm_camellia128_ctx; length:Tsize_t; iv:Puint8_t);cdecl;
  nettle_gcm_camellia128_update : procedure(ctx:Pgcm_camellia128_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_gcm_camellia128_encrypt : procedure(ctx:Pgcm_camellia128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_camellia128_decrypt : procedure(ctx:Pgcm_camellia128_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_camellia128_digest : procedure(ctx:Pgcm_camellia128_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_gcm_camellia256_set_key : procedure(ctx:Pgcm_camellia256_ctx; key:Puint8_t);cdecl;
  nettle_gcm_camellia256_set_iv : procedure(ctx:Pgcm_camellia256_ctx; length:Tsize_t; iv:Puint8_t);cdecl;
  nettle_gcm_camellia256_update : procedure(ctx:Pgcm_camellia256_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_gcm_camellia256_encrypt : procedure(ctx:Pgcm_camellia256_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_camellia256_decrypt : procedure(ctx:Pgcm_camellia256_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_gcm_camellia256_digest : procedure(ctx:Pgcm_camellia256_ctx; length:Tsize_t; digest:Puint8_t);cdecl;

type
  Pgosthash94_ctx = ^Tgosthash94_ctx;
  Tgosthash94_ctx = record
      hash : array[0..7] of Tuint32_t;
      sum : array[0..7] of Tuint32_t;
      length : Tuint64_t;
      message : array[0..31] of Tuint8_t;
    end;

var
  nettle_gosthash94_init : procedure(ctx:Pgosthash94_ctx);cdecl;
  nettle_gosthash94_update : procedure(ctx:Pgosthash94_ctx; length:Tsize_t; msg:Puint8_t);cdecl;
  nettle_gosthash94_digest : procedure(ctx:Pgosthash94_ctx; length:Tsize_t; result:Puint8_t);cdecl;
  nettle_hkdf_extract : procedure(mac_ctx:pointer; update:Pnettle_hash_update_func; digest:Pnettle_hash_digest_func; digest_size:Tsize_t; secret_size:Tsize_t;  secret:Puint8_t; dst:Puint8_t);cdecl;
  nettle_hkdf_expand : procedure(mac_ctx:pointer; update:Pnettle_hash_update_func; digest:Pnettle_hash_digest_func; digest_size:Tsize_t; info_size:Tsize_t; info:Puint8_t; length:Tsize_t; dst:Puint8_t);cdecl;

type
  Pmd5_ctx = ^Tmd5_ctx;
  Tmd5_ctx = record
      state : array[0..3] of Tuint32_t;
      count : Tuint64_t;
      index : Tcunsigned;
      block : array[0..63] of Tuint8_t;
    end;

var
  nettle_md5_init : procedure(ctx:Pmd5_ctx);cdecl;
  nettle_md5_update : procedure(ctx:Pmd5_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_md5_digest : procedure(ctx:Pmd5_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_md5_compress : procedure(state:Puint32_t; data:Puint8_t);cdecl;

type
  Pripemd160_ctx = ^Tripemd160_ctx;
  Tripemd160_ctx = record
      state : array[0..4] of Tuint32_t;
      count : Tuint64_t;
      index : Tcuint;
      block : array[0..63] of Tuint8_t;
    end;

var
  nettle_ripemd160_init : procedure(ctx:Pripemd160_ctx);cdecl;
  nettle_ripemd160_update : procedure(ctx:Pripemd160_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_ripemd160_digest : procedure(ctx:Pripemd160_ctx; length:Tsize_t; digest:Puint8_t);cdecl;

type
  Phmac_md5_ctx = ^Thmac_md5_ctx;
  Thmac_md5_ctx = record
      outer : Tmd5_ctx;
      inner : Tmd5_ctx;
      state : Tmd5_ctx;
    end;
  Phmac_ripemd160_ctx = ^Thmac_ripemd160_ctx;
  Thmac_ripemd160_ctx = record
      outer : Tripemd160_ctx;
      inner : Tripemd160_ctx;
      state : Tripemd160_ctx;
    end;
  Phmac_sha1_ctx = ^Thmac_sha1_ctx;
  Thmac_sha1_ctx = record
      outer : Tsha1_ctx;
      inner : Tsha1_ctx;
      state : Tsha1_ctx;
    end;
  Phmac_sha256_ctx = ^Thmac_sha256_ctx;
  Thmac_sha256_ctx = record
      outer : Tsha256_ctx;
      inner : Tsha256_ctx;
      state : Tsha256_ctx;
    end;
  Phmac_sha512_ctx = ^Thmac_sha512_ctx;
  Thmac_sha512_ctx = record
      outer : Tsha512_ctx;
      inner : Tsha512_ctx;
      state : Tsha512_ctx;
    end;

var
  nettle_hmac_set_key : procedure(outer:pointer; inner:pointer; state:pointer; hash:Pnettle_hash; length:Tsize_t; key:Puint8_t);cdecl;
  nettle_hmac_update : procedure(state:pointer; hash:Pnettle_hash; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_hmac_digest : procedure(outer:pointer; inner:pointer; state:pointer; hash:Pnettle_hash; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_hmac_md5_set_key : procedure(ctx:Phmac_md5_ctx; key_length:Tsize_t; key:Puint8_t);cdecl;
  nettle_hmac_md5_update : procedure(ctx:Phmac_md5_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_hmac_md5_digest : procedure(ctx:Phmac_md5_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_hmac_ripemd160_set_key : procedure(ctx:Phmac_ripemd160_ctx; key_length:Tsize_t; key:Puint8_t);cdecl;
  nettle_hmac_ripemd160_update : procedure(ctx:Phmac_ripemd160_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_hmac_ripemd160_digest : procedure(ctx:Phmac_ripemd160_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_hmac_sha1_set_key : procedure(ctx:Phmac_sha1_ctx; key_length:Tsize_t; key:Puint8_t);cdecl;
  nettle_hmac_sha1_update : procedure(ctx:Phmac_sha1_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_hmac_sha1_digest : procedure(ctx:Phmac_sha1_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_hmac_sha256_set_key : procedure(ctx:Phmac_sha256_ctx; key_length:Tsize_t; key:Puint8_t);cdecl;
  nettle_hmac_sha256_update : procedure(ctx:Phmac_sha256_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_hmac_sha256_digest : procedure(ctx:Phmac_sha256_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_hmac_sha224_set_key : procedure(ctx:Phmac_sha256_ctx; key_length:Tsize_t; key:Puint8_t);cdecl;
  nettle_hmac_sha224_digest : procedure(ctx:Phmac_sha256_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_hmac_sha512_set_key : procedure(ctx:Phmac_sha512_ctx; key_length:Tsize_t; key:Puint8_t);cdecl;
  nettle_hmac_sha512_update : procedure(ctx:Phmac_sha512_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_hmac_sha512_digest : procedure(ctx:Phmac_sha512_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_hmac_sha384_set_key : procedure(ctx:Phmac_sha512_ctx; key_length:Tsize_t; key:Puint8_t);cdecl;
  nettle_hmac_sha384_digest : procedure(ctx:Phmac_sha512_ctx; length:Tsize_t; digest:Puint8_t);cdecl;

type
  Pknuth_lfib_ctx = ^Tknuth_lfib_ctx;
  Tknuth_lfib_ctx = record
      x : array[0..99] of Tuint32_t;
      index : Tcunsigned;
    end;

var
  nettle_knuth_lfib_init : procedure(ctx:Pknuth_lfib_ctx; seed:Tuint32_t);cdecl;
  nettle_knuth_lfib_get : function(ctx:Pknuth_lfib_ctx):Tuint32_t;cdecl;
  nettle_knuth_lfib_get_array : procedure(ctx:Pknuth_lfib_ctx; n:Tsize_t; a:Puint32_t);cdecl;
  nettle_knuth_lfib_random : procedure(ctx:Pknuth_lfib_ctx; n:Tsize_t; dst:Puint8_t);cdecl;

type
  Pmd2_ctx = ^Tmd2_ctx;
  Tmd2_ctx = record
      C : array[0..15] of Tuint8_t;
      X : array[0..(3*16)-1] of Tuint8_t;
      index : Tcunsigned;
      block : array[0..15] of Tuint8_t;
    end;

  Pmd4_ctx = ^Tmd4_ctx;
  Tmd4_ctx = record
      state : array[0..3] of Tuint32_t;
      count : Tuint64_t;
      index : Tcunsigned;
      block : array[0..63] of Tuint8_t;
    end;

var
  nettle_md2_init : procedure(ctx:Pmd2_ctx);cdecl;
  nettle_md2_update : procedure(ctx:Pmd2_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_md2_digest : procedure(ctx:Pmd2_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_md4_init : procedure(ctx:Pmd4_ctx);cdecl;
  nettle_md4_update : procedure(ctx:Pmd4_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_md4_digest : procedure(ctx:Pmd4_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_MD5Init : procedure(ctx:PMD5_CTX);cdecl;
  nettle_MD5Update : procedure(ctx:PMD5_CTX; data:pcuchar; length:Tcuint);cdecl;
  nettle_MD5Final : procedure(_out:pcuchar; ctx:PMD5_CTX);cdecl;

var
  nettle_memxor : function(dst:pointer; src:pointer; n:Tsize_t):pointer;cdecl;
  nettle_memxor3 : function(dst:pointer; a:pointer; b:pointer; n:Tsize_t):pointer;cdecl;
  nettle_memeql_sec : function(a:pointer; b:pointer; n:Tsize_t):Tcint;cdecl;
  nettle_cnd_memcpy : procedure(cnd:Tcint; dst:pointer; src:pointer; n:Tsize_t);cdecl;
  nettle_pbkdf2 : procedure(mac_ctx:pointer; update:Pnettle_hash_update_func; digest:Pnettle_hash_digest_func; digest_size:Tsize_t; iterations:Tcunsigned;  salt_length:Tsize_t; salt:Puint8_t; length:Tsize_t; dst:Puint8_t);cdecl;
  nettle_pbkdf2_hmac_sha1 : procedure(key_length:Tsize_t; key:Puint8_t; iterations:Tcunsigned; salt_length:Tsize_t; salt:Puint8_t;  length:Tsize_t; dst:Puint8_t);cdecl;
  nettle_pbkdf2_hmac_sha256 : procedure(key_length:Tsize_t; key:Puint8_t; iterations:Tcunsigned; salt_length:Tsize_t; salt:Puint8_t; length:Tsize_t; dst:Puint8_t);cdecl;
type
  Prsa_public_key = ^Trsa_public_key;
  Trsa_public_key = record
      size : Tsize_t;
      n : Tmpz_t;
      e : Tmpz_t;
    end;

  Prsa_private_key = ^Trsa_private_key;
  Trsa_private_key = record
      size : Tsize_t;
      d : Tmpz_t;
      p : Tmpz_t;
      q : Tmpz_t;
      a : Tmpz_t;
      b : Tmpz_t;
      c : Tmpz_t;
    end;

var
  nettle_pgp_put_uint32 : function(buffer:Pnettle_buffer; i:Tuint32_t):Tcint;cdecl;
  nettle_pgp_put_uint16 : function(buffer:Pnettle_buffer; i:Tcunsigned):Tcint;cdecl;
  nettle_pgp_put_mpi : function(buffer:Pnettle_buffer; x:Pmpz_t):Tcint;cdecl;
  nettle_pgp_put_string : function(buffer:Pnettle_buffer; length:Tcunsigned; s:Puint8_t):Tcint;cdecl;
  nettle_pgp_put_length : function(buffer:Pnettle_buffer; length:Tcunsigned):Tcint;cdecl;
  nettle_pgp_put_header : function(buffer:Pnettle_buffer; tag:Tcunsigned; length:Tcunsigned):Tcint;cdecl;
  nettle_pgp_put_header_length : procedure(buffer:Pnettle_buffer; start:Tcunsigned; field_size:Tcunsigned);cdecl;
  nettle_pgp_sub_packet_start : function(buffer:Pnettle_buffer):Tcunsigned;cdecl;
  nettle_pgp_put_sub_packet : function(buffer:Pnettle_buffer; _type:Tcunsigned; length:Tcunsigned; data:Puint8_t):Tcint;cdecl;
  nettle_pgp_sub_packet_end : procedure(buffer:Pnettle_buffer; start:Tcunsigned);cdecl;
  nettle_pgp_put_public_rsa_key : function(para1:Pnettle_buffer; key:Prsa_public_key; timestamp:Ttime_t):Tcint;cdecl;
  nettle_pgp_put_rsa_sha1_signature : function(buffer:Pnettle_buffer; key:Prsa_private_key; keyid:Puint8_t; _type:Tcunsigned; hash:Psha1_ctx):Tcint;cdecl;
  nettle_pgp_put_userid : function(buffer:Pnettle_buffer; length:Tcunsigned; name:Puint8_t):Tcint;cdecl;
  nettle_pgp_crc24 : function(length:Tcunsigned; data:Puint8_t):Tuint32_t;cdecl;
  nettle_pgp_armor : function(buffer:Pnettle_buffer; tag:pcchar; length:Tcunsigned; data:Puint8_t):Tcint;cdecl;
type
  Tpgp_lengths = (PGP_LENGTH_ONE_OCTET := 0,PGP_LENGTH_TWO_OCTETS := 192,
    PGP_LENGTH_FOUR_OCTETS := 8384);

  Tpgp_public_key_algorithm = (PGP_RSA := 1,PGP_RSA_ENCRYPT := 2,PGP_RSA_SIGN := 3,
    PGP_EL_GAMAL_ENCRYPT := 16,PGP_DSA := 17,
    PGP_EL_GAMAL := 20);

  Tpgp_symmetric_algorithm = (PGP_PLAINTEXT := 0,PGP_IDEA := 1,PGP_3DES := 2,
    PGP_CAST5 := 3,PGP_BLOWFISH := 4,PGP_SAFER_SK := 5,
    PGP_AES128 := 7,PGP_AES192 := 8,PGP_AES256 := 9
    );

  Tpgp_compression_algorithm = (PGP_UNCOMPRESSED := 0,PGP_ZIP := 1,PGP_ZLIB := 2
    );

  Tpgp_hash_algorithm = (PGP_MD5 := 1,PGP_SHA1 := 2,PGP_RIPEMD := 3,
    PGP_MD2 := 5,PGP_TIGER192 := 6,PGP_HAVAL := 7
    );

  Tpgp_tag = (PGP_TAG_PUBLIC_SESSION_KEY := 1,PGP_TAG_SIGNATURE := 2,
    PGP_TAG_SYMMETRIC_SESSION_KEY := 3,PGP_TAG_ONE_PASS_SIGNATURE := 4,
    PGP_TAG_SECRET_KEY := 5,PGP_TAG_PUBLIC_KEY := 6,
    PGP_TAG_SECRET_SUBKEY := 7,PGP_TAG_COMPRESSED := 8,
    PGP_TAG_ENCRYPTED := 9,PGP_TAG_MARKER := 10,
    PGP_TAG_LITERAL := 11,PGP_TAG_TRUST := 12,
    PGP_TAG_USERID := 13,PGP_TAG_PUBLIC_SUBKEY := 14
    );

  Tpgp_signature_type = (PGP_SIGN_BINARY := 0,PGP_SIGN_TEXT := 1,
    PGP_SIGN_STANDALONE := 2,PGP_SIGN_CERTIFICATION := $10,
    PGP_SIGN_CERTIFICATION_PERSONA := $11,PGP_SIGN_CERTIFICATION_CASUAL := $12,
    PGP_SIGN_CERTIFICATION_POSITIVE := $13,
    PGP_SIGN_SUBKEY := $18,PGP_SIGN_KEY := $1f,
    PGP_SIGN_REVOCATION := $20,PGP_SIGN_REVOCATION_SUBKEY := $28,
    PGP_SIGN_REVOCATION_CERTIFICATE := $30,
    PGP_SIGN_TIMESTAMP := $40);

  Tpgp_subpacket_tag = (PGP_SUBPACKET_CREATION_TIME := 2,PGP_SUBPACKET_SIGNATURE_EXPIRATION_TIME := 3,
    PGP_SUBPACKET_EXPORTABLE_CERTIFICATION := 4,
    PGP_SUBPACKET_TRUST_SIGNATURE := 5,PGP_SUBPACKET_REGULAR_EXPRESSION := 6,
    PGP_SUBPACKET_REVOCABLE := 7,PGP_SUBPACKET_KEY_EXPIRATION_TIME := 9,
    PGP_SUBPACKET_PLACEHOLDER := 10,PGP_SUBPACKET_PREFERRED_SYMMETRIC_ALGORITHMS := 11,
    PGP_SUBPACKET_REVOCATION_KEY := 12,PGP_SUBPACKET_ISSUER_KEY_ID := 16,
    PGP_SUBPACKET_NOTATION_DATA := 20,PGP_SUBPACKET_PREFERRED_HASH_ALGORITHMS := 21,
    PGP_SUBPACKET_PREFERRED_COMPRESSION_ALGORITHMS := 22,
    PGP_SUBPACKET_KEY_SERVER_PREFERENCES := 23,
    PGP_SUBPACKET_PREFERRED_KEY_SERVER := 24,
    PGP_SUBPACKET_PRIMARY_USER_ID := 25,PGP_SUBPACKET_POLICY_URL := 26,
    PGP_SUBPACKET_KEY_FLAGS := 27,PGP_SUBPACKET_SIGNERS_USER_ID := 28,
    PGP_SUBPACKET_REASON_FOR_REVOCATION := 29
    );

var
  nettle_pkcs1_encrypt : function(key_size:Tsize_t; random_ctx:pointer; random:Pnettle_random_func; length:Tsize_t; message:Puint8_t; m:Pmpz_t):Tcint;cdecl;
  nettle_pkcs1_decrypt : function(key_size:Tsize_t; m:Pmpz_t; length:Psize_t; message:Puint8_t):Tcint;cdecl;
  nettle_pkcs1_rsa_digest_encode : function(m:Pmpz_t; key_size:Tsize_t; di_length:Tsize_t; digest_info:Puint8_t):Tcint;cdecl;
  nettle_pkcs1_rsa_md5_encode : function(m:Pmpz_t; length:Tsize_t; hash:Pmd5_ctx):Tcint;cdecl;
  nettle_pkcs1_rsa_md5_encode_digest : function(m:Pmpz_t; length:Tsize_t; digest:Puint8_t):Tcint;cdecl;
  nettle_pkcs1_rsa_sha1_encode : function(m:Pmpz_t; length:Tsize_t; hash:Psha1_ctx):Tcint;cdecl;
  nettle_pkcs1_rsa_sha1_encode_digest : function(m:Pmpz_t; length:Tsize_t; digest:Puint8_t):Tcint;cdecl;
  nettle_pkcs1_rsa_sha256_encode : function(m:Pmpz_t; length:Tsize_t; hash:Psha256_ctx):Tcint;cdecl;
  nettle_pkcs1_rsa_sha256_encode_digest : function(m:Pmpz_t; length:Tsize_t; digest:Puint8_t):Tcint;cdecl;
  nettle_pkcs1_rsa_sha512_encode : function(m:Pmpz_t; length:Tsize_t; hash:Psha512_ctx):Tcint;cdecl;
  nettle_pkcs1_rsa_sha512_encode_digest : function(m:Pmpz_t; length:Tsize_t; digest:Puint8_t):Tcint;cdecl;
  nettle_pss_encode_mgf1 : function(m:Pmpz_t; bits:Tsize_t; hash:Pnettle_hash; salt_length:Tsize_t; salt:Puint8_t;  digest:Puint8_t):Tcint;cdecl;
  nettle_pss_verify_mgf1 : function(m:Pmpz_t; bits:Tsize_t; hash:Pnettle_hash; salt_length:Tsize_t; digest:Puint8_t):Tcint;cdecl;
  nettle_pss_mgf1 : procedure(seed:pointer; hash:Pnettle_hash; length:Tsize_t; mask:Puint8_t);cdecl;

var
  nettle_rsa_public_key_init : procedure(key:Prsa_public_key);cdecl;
  nettle_rsa_public_key_clear : procedure(key:Prsa_public_key);cdecl;
  nettle_rsa_public_key_prepare : function(key:Prsa_public_key):Tcint;cdecl;
  nettle_rsa_private_key_init : procedure(key:Prsa_private_key);cdecl;
  nettle_rsa_private_key_clear : procedure(key:Prsa_private_key);cdecl;
  nettle_rsa_private_key_prepare : function(key:Prsa_private_key):Tcint;cdecl;
  nettle_rsa_pkcs1_sign : function(key:Prsa_private_key; length:Tsize_t; digest_info:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_pkcs1_sign_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; length:Tsize_t;  digest_info:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_pkcs1_verify : function(key:Prsa_public_key; length:Tsize_t; digest_info:Puint8_t; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_md5_sign : function(key:Prsa_private_key; hash:Pmd5_ctx; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_md5_sign_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; hash:Pmd5_ctx; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_md5_verify : function(key:Prsa_public_key; hash:Pmd5_ctx; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha1_sign : function(key:Prsa_private_key; hash:Psha1_ctx; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha1_sign_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; hash:Psha1_ctx; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha1_verify : function(key:Prsa_public_key; hash:Psha1_ctx; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha256_sign : function(key:Prsa_private_key; hash:Psha256_ctx; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha256_sign_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; hash:Psha256_ctx; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha256_verify : function(key:Prsa_public_key; hash:Psha256_ctx; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha512_sign : function(key:Prsa_private_key; hash:Psha512_ctx; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha512_sign_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; hash:Psha512_ctx; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha512_verify : function(key:Prsa_public_key; hash:Psha512_ctx; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_md5_sign_digest : function(key:Prsa_private_key; digest:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_md5_sign_digest_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; digest:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_md5_verify_digest : function(key:Prsa_public_key; digest:Puint8_t; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha1_sign_digest : function(key:Prsa_private_key; digest:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha1_sign_digest_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; digest:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha1_verify_digest : function(key:Prsa_public_key; digest:Puint8_t; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha256_sign_digest : function(key:Prsa_private_key; digest:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha256_sign_digest_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; digest:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha256_verify_digest : function(key:Prsa_public_key; digest:Puint8_t; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha512_sign_digest : function(key:Prsa_private_key; digest:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha512_sign_digest_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; digest:Puint8_t;  s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sha512_verify_digest : function(key:Prsa_public_key; digest:Puint8_t; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_pss_sha256_sign_digest_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; salt_length:Tsize_t; salt:Puint8_t; digest:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_pss_sha256_verify_digest : function(key:Prsa_public_key; salt_length:Tsize_t; digest:Puint8_t; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_pss_sha384_sign_digest_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; salt_length:Tsize_t; salt:Puint8_t; digest:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_pss_sha384_verify_digest : function(key:Prsa_public_key; salt_length:Tsize_t; digest:Puint8_t; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_pss_sha512_sign_digest_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; salt_length:Tsize_t; salt:Puint8_t; digest:Puint8_t; s:Pmpz_t):Tcint;cdecl;
  nettle_rsa_pss_sha512_verify_digest : function(key:Prsa_public_key; salt_length:Tsize_t; digest:Puint8_t; signature:Pmpz_t):Tcint;cdecl;
  nettle_rsa_encrypt : function(key:Prsa_public_key; random_ctx:pointer; random:Pnettle_random_func; length:Tsize_t; cleartext:Puint8_t;  cipher:Pmpz_t):Tcint;cdecl;
  nettle_rsa_decrypt : function(key:Prsa_private_key; length:Psize_t; cleartext:Puint8_t; ciphertext:Pmpz_t):Tcint;cdecl;
  nettle_rsa_decrypt_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; length:Psize_t;  message:Puint8_t; gibberish:Pmpz_t):Tcint;cdecl;
  nettle_rsa_sec_decrypt : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; length:Tsize_t;  message:Puint8_t; gibberish:Pmpz_t):Tcint;cdecl;
  nettle_rsa_compute_root : procedure(key:Prsa_private_key; x:Pmpz_t; m:Pmpz_t);cdecl;
  nettle_rsa_compute_root_tr : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; x:Pmpz_t;   m:Pmpz_t):Tcint;cdecl;
  nettle_rsa_generate_keypair : function(pub:Prsa_public_key; key:Prsa_private_key; random_ctx:pointer; random:Pnettle_random_func; progress_ctx:pointer;  progress:Pnettle_progress_func; n_size:Tcunsigned; e_size:Tcunsigned):Tcint;cdecl;
  nettle_rsa_keypair_to_sexp : function(buffer:Pnettle_buffer; algorithm_name:pcchar; pub:Prsa_public_key; priv:Prsa_private_key):Tcint;cdecl;
  nettle_rsa_keypair_from_sexp_alist : function(pub:Prsa_public_key; priv:Prsa_private_key; limit:Tcunsigned; i:Psexp_iterator):Tcint;cdecl;
  nettle_rsa_keypair_from_sexp : function(pub:Prsa_public_key; priv:Prsa_private_key; limit:Tcunsigned; length:Tsize_t; expr:Puint8_t):Tcint;cdecl;
  nettle_rsa_public_key_from_der_iterator : function(pub:Prsa_public_key; limit:Tcunsigned; i:Pasn1_der_iterator):Tcint;cdecl;
  nettle_rsa_private_key_from_der_iterator : function(pub:Prsa_public_key; priv:Prsa_private_key; limit:Tcunsigned; i:Pasn1_der_iterator):Tcint;cdecl;
  nettle_rsa_keypair_from_der : function(pub:Prsa_public_key; priv:Prsa_private_key; limit:Tcunsigned; length:Tsize_t; data:Puint8_t):Tcint;cdecl;
  nettle_rsa_keypair_to_openpgp : function(buffer:Pnettle_buffer; pub:Prsa_public_key; priv:Prsa_private_key; userid:pcchar):Tcint;cdecl;

type
  Psalsa20_ctx = ^Tsalsa20_ctx;
  Tsalsa20_ctx = record
      input : array[0..15] of Tuint32_t;
    end;

var
  nettle_salsa20_128_set_key : procedure(ctx:Psalsa20_ctx; key:Puint8_t);cdecl;
  nettle_salsa20_256_set_key : procedure(ctx:Psalsa20_ctx; key:Puint8_t);cdecl;
  nettle_salsa20_set_key : procedure(ctx:Psalsa20_ctx; length:Tsize_t; key:Puint8_t);cdecl;
  nettle_salsa20_set_nonce : procedure(ctx:Psalsa20_ctx; nonce:Puint8_t);cdecl;
  nettle_salsa20_crypt : procedure(ctx:Psalsa20_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_salsa20r12_crypt : procedure(ctx:Psalsa20_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;


type
  Pserpent_ctx = ^Tserpent_ctx;
  Tserpent_ctx = record
      keys : array[0..32] of array[0..3] of Tuint32_t;
    end;

var
  nettle_serpent_set_key : procedure(ctx:Pserpent_ctx; length:Tsize_t; key:Puint8_t);cdecl;
  nettle_serpent128_set_key : procedure(ctx:Pserpent_ctx; key:Puint8_t);cdecl;
  nettle_serpent192_set_key : procedure(ctx:Pserpent_ctx; key:Puint8_t);cdecl;
  nettle_serpent256_set_key : procedure(ctx:Pserpent_ctx; key:Puint8_t);cdecl;
  nettle_serpent_encrypt : procedure(ctx:Pserpent_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_serpent_decrypt : procedure(ctx:Pserpent_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;


var
  nettle_sexp_iterator_first : function(iterator:Psexp_iterator; length:Tsize_t; input:Puint8_t):Tcint;cdecl;
  nettle_sexp_transport_iterator_first : function(iterator:Psexp_iterator; length:Tsize_t; input:Puint8_t):Tcint;cdecl;
  nettle_sexp_iterator_next : function(iterator:Psexp_iterator):Tcint;cdecl;
  nettle_sexp_iterator_enter_list : function(iterator:Psexp_iterator):Tcint;cdecl;
  nettle_sexp_iterator_exit_list : function(iterator:Psexp_iterator):Tcint;cdecl;

  nettle_sexp_iterator_subexpr : function(iterator:Psexp_iterator; length:Psize_t):Puint8_t;cdecl;
  nettle_sexp_iterator_get_uint32 : function(iterator:Psexp_iterator; x:Puint32_t):Tcint;cdecl;

  nettle_sexp_iterator_check_type : function(iterator:Psexp_iterator; _type:pcchar):Tcint;cdecl;
  nettle_sexp_iterator_check_types : function(iterator:Psexp_iterator; ntypes:Tcunsigned; types:Ppcchar):pcchar;cdecl;
  nettle_sexp_iterator_assoc : function(iterator:Psexp_iterator; nkeys:Tcunsigned; keys:Ppcchar; values:Psexp_iterator):Tcint;cdecl;
  nettle_sexp_format : function(buffer:Pnettle_buffer; format:pcchar):Tsize_t;varargs; cdecl;
  nettle_sexp_transport_format : function(buffer:Pnettle_buffer; format:pcchar):Tsize_t;varargs; cdecl;

type
  Psha3_state = ^Tsha3_state;
  Tsha3_state = record
      a : array[0..24] of Tuint64_t;
    end;
  Psha3_224_ctx = ^Tsha3_224_ctx;
  Tsha3_224_ctx = record
      state : Tsha3_state;
      index : Tcunsigned;
      block : array[0..143] of Tuint8_t;
    end;
  Psha3_256_ctx = ^Tsha3_256_ctx;
  Tsha3_256_ctx = record
      state : Tsha3_state;
      index : Tcunsigned;
      block : array[0..135] of Tuint8_t;
    end;
  Psha3_384_ctx = ^Tsha3_384_ctx;
  Tsha3_384_ctx = record
      state : Tsha3_state;
      index : Tcunsigned;
      block : array[0..103] of Tuint8_t;
    end;
  Psha3_512_ctx = ^Tsha3_512_ctx;
  Tsha3_512_ctx = record
      state : Tsha3_state;
      index : Tcunsigned;
      block : array[0..71] of Tuint8_t;
    end;

var
  nettle_sha3_permute : procedure(state:Psha3_state);cdecl;
  nettle_sha3_224_init : procedure(ctx:Psha3_224_ctx);cdecl;
  nettle_sha3_224_update : procedure(ctx:Psha3_224_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_sha3_224_digest : procedure(ctx:Psha3_224_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_sha3_256_init : procedure(ctx:Psha3_256_ctx);cdecl;
  nettle_sha3_256_update : procedure(ctx:Psha3_256_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_sha3_256_digest : procedure(ctx:Psha3_256_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_sha3_384_init : procedure(ctx:Psha3_384_ctx);cdecl;
  nettle_sha3_384_update : procedure(ctx:Psha3_384_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_sha3_384_digest : procedure(ctx:Psha3_384_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_sha3_512_init : procedure(ctx:Psha3_512_ctx);cdecl;
  nettle_sha3_512_update : procedure(ctx:Psha3_512_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_sha3_512_digest : procedure(ctx:Psha3_512_ctx; length:Tsize_t; digest:Puint8_t);cdecl;

type
  Ptwofish_ctx = ^Ttwofish_ctx;
  Ttwofish_ctx = record
      keys : array[0..39] of Tuint32_t;
      s_box : array[0..3] of array[0..255] of Tuint32_t;
    end;

var
  nettle_twofish_set_key : procedure(ctx:Ptwofish_ctx; length:Tsize_t; key:Puint8_t);cdecl;
  nettle_twofish128_set_key : procedure(context:Ptwofish_ctx; key:Puint8_t);cdecl;
  nettle_twofish192_set_key : procedure(context:Ptwofish_ctx; key:Puint8_t);cdecl;
  nettle_twofish256_set_key : procedure(context:Ptwofish_ctx; key:Puint8_t);cdecl;
  nettle_twofish_encrypt : procedure(ctx:Ptwofish_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_twofish_decrypt : procedure(ctx:Ptwofish_ctx; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;

Const
  Tumac32_ctx_l1_key_size =  ((1024 div 4)+(4*(1-1)));

type
  Pumac32_ctx = ^Tumac32_ctx;
  Tumac32_ctx = record
      l1_key : array[0..Tumac32_ctx_l1_key_size-1] of Tuint32_t;
      l2_key : array[0..(6*1)-1] of Tuint32_t;
      l3_key1 : array[0..(8*1)-1] of Tuint64_t;
      l3_key2 : array[0..0] of Tuint32_t;
      pdf_key : Taes128_ctx;
      l2_state : array[0..(3*1)-1] of Tuint64_t;
      nonce : array[0..15] of Tuint8_t;
      nonce_length : Tcushort;
      nonce_low : Tcushort;
      pad_cache : array[0..(16 div 4)-1] of Tuint32_t;
      index : Tcunsigned;
      count : Tuint64_t;
      block : array[0..1023] of Tuint8_t;
    end;

  Pumac64_ctx = ^Tumac64_ctx;
  Tumac64_ctx = record
      l1_key : array[0..((1024 div 4)+(4*(2-1)))-1] of Tuint32_t;
      l2_key : array[0..(6*2)-1] of Tuint32_t;
      l3_key1 : array[0..(8*2)-1] of Tuint64_t;
      l3_key2 : array[0..1] of Tuint32_t;
      pdf_key : Taes128_ctx;
      l2_state : array[0..(3*2)-1] of Tuint64_t;
      nonce : array[0..15] of Tuint8_t;
      nonce_length : Tcushort;
      nonce_low : Tcushort;
      pad_cache : array[0..(16 div 4)-1] of Tuint32_t;
      index : Tcunsigned;
      count : Tuint64_t;
      block : array[0..1023] of Tuint8_t;
    end;

  Pumac96_ctx = ^Tumac96_ctx;
  Tumac96_ctx = record
      l1_key : array[0..((1024 div 4)+(4*(3-1)))-1] of Tuint32_t;
      l2_key : array[0..(6*3)-1] of Tuint32_t;
      l3_key1 : array[0..(8*3)-1] of Tuint64_t;
      l3_key2 : array[0..2] of Tuint32_t;
      pdf_key : Taes128_ctx;
      l2_state : array[0..(3*3)-1] of Tuint64_t;
      nonce : array[0..15] of Tuint8_t;
      nonce_length : Tcushort;
      index : Tcunsigned;
      count : Tuint64_t;
      block : array[0..1023] of Tuint8_t;
    end;

  Pumac128_ctx = ^Tumac128_ctx;
  Tumac128_ctx = record
      l1_key : array[0..((1024 div 4)+(4*(4-1)))-1] of Tuint32_t;
      l2_key : array[0..(6*4)-1] of Tuint32_t;
      l3_key1 : array[0..(8*4)-1] of Tuint64_t;
      l3_key2 : array[0..3] of Tuint32_t;
      pdf_key : Taes128_ctx;
      l2_state : array[0..(3*4)-1] of Tuint64_t;
      nonce : array[0..15] of Tuint8_t;
      nonce_length : Tcushort;
      index : Tcunsigned;
      count : Tuint64_t;
      block : array[0..1023] of Tuint8_t;
    end;
var
  nettle_umac32_set_key : procedure(ctx:Pumac32_ctx; key:Puint8_t);cdecl;
  nettle_umac64_set_key : procedure(ctx:Pumac64_ctx; key:Puint8_t);cdecl;
  nettle_umac96_set_key : procedure(ctx:Pumac96_ctx; key:Puint8_t);cdecl;
  nettle_umac128_set_key : procedure(ctx:Pumac128_ctx; key:Puint8_t);cdecl;
  nettle_umac32_set_nonce : procedure(ctx:Pumac32_ctx; nonce_length:Tsize_t; nonce:Puint8_t);cdecl;
  nettle_umac64_set_nonce : procedure(ctx:Pumac64_ctx; nonce_length:Tsize_t; nonce:Puint8_t);cdecl;
  nettle_umac96_set_nonce : procedure(ctx:Pumac96_ctx; nonce_length:Tsize_t; nonce:Puint8_t);cdecl;
  nettle_umac128_set_nonce : procedure(ctx:Pumac128_ctx; nonce_length:Tsize_t; nonce:Puint8_t);cdecl;
  nettle_umac32_update : procedure(ctx:Pumac32_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_umac64_update : procedure(ctx:Pumac64_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_umac96_update : procedure(ctx:Pumac96_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_umac128_update : procedure(ctx:Pumac128_ctx; length:Tsize_t; data:Puint8_t);cdecl;
  nettle_umac32_digest : procedure(ctx:Pumac32_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_umac64_digest : procedure(ctx:Pumac64_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_umac96_digest : procedure(ctx:Pumac96_ctx; length:Tsize_t; digest:Puint8_t);cdecl;
  nettle_umac128_digest : procedure(ctx:Pumac128_ctx; length:Tsize_t; digest:Puint8_t);cdecl;

Type
  Pxts_aes128_key = ^Txts_aes128_key;
  Txts_aes128_key = record
      cipher : Taes128_ctx;
      tweak_cipher : Taes128_ctx;
  end;

  Pxts_aes256_key = ^Txts_aes256_key;
  Txts_aes256_key = record
      cipher : Taes256_ctx;
      tweak_cipher : Taes256_ctx;
  end;


var
  nettle_xts_encrypt_message : procedure(enc_ctx:pointer; twk_ctx:pointer; encf:Pnettle_cipher_func; tweak:Puint8_t; length:Tsize_t;  dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_xts_decrypt_message : procedure(dec_ctx:pointer; twk_ctx:pointer; decf:Pnettle_cipher_func; encf:Pnettle_cipher_func; tweak:Puint8_t;length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;

  nettle_xts_aes128_set_encrypt_key : procedure(xts_key:Pxts_aes128_key; key:Puint8_t);cdecl;
  nettle_xts_aes128_set_decrypt_key : procedure(xts_key:Pxts_aes128_key; key:Puint8_t);cdecl;
  nettle_xts_aes128_encrypt_message : procedure(xtskey:Pxts_aes128_key; tweak:Puint8_t; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_xts_aes128_decrypt_message : procedure(xts_key:Pxts_aes128_key; tweak:Puint8_t; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_xts_aes256_set_encrypt_key : procedure(xts_key:Pxts_aes256_key; key:Puint8_t);cdecl;
  nettle_xts_aes256_set_decrypt_key : procedure(xts_key:Pxts_aes256_key; key:Puint8_t);cdecl;
  nettle_xts_aes256_encrypt_message : procedure(xts_key:Pxts_aes256_key; tweak:Puint8_t; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;
  nettle_xts_aes256_decrypt_message : procedure(xts_key:Pxts_aes256_key; tweak:Puint8_t; length:Tsize_t; dst:Puint8_t; src:Puint8_t);cdecl;

type
  Tyarrow_pool_id = (YARROW_FAST := 0,YARROW_SLOW := 1);

  Pyarrow_source = ^Tyarrow_source;
  Tyarrow_source = record
      estimate : array[0..1] of Tuint32_t;
      next : Tyarrow_pool_id;
    end;

  Pyarrow256_ctx = ^Tyarrow256_ctx;
  Tyarrow256_ctx = record
      pools : array[0..1] of Tsha256_ctx;
      seeded : Tcint;
      key : Taes256_ctx;
      counter : array[0..15] of Tuint8_t;
      nsources : Tcunsigned;
      sources : Pyarrow_source;
    end;

  Pyarrow_key_event_ctx = ^Tyarrow_key_event_ctx;
  Tyarrow_key_event_ctx = record
      index : Tcunsigned;
      chars : array[0..15] of Tcunsigned;
      previous : Tcunsigned;
    end;
var
  nettle_yarrow256_init : procedure(ctx:Pyarrow256_ctx; nsources:Tcunsigned; sources:Pyarrow_source);cdecl;
  nettle_yarrow256_seed : procedure(ctx:Pyarrow256_ctx; length:Tsize_t; seed_file:Puint8_t);cdecl;
  nettle_yarrow256_update : function(ctx:Pyarrow256_ctx; source:Tcunsigned; entropy:Tcunsigned; length:Tsize_t; data:Puint8_t):Tcint;cdecl;
  nettle_yarrow256_random : procedure(ctx:Pyarrow256_ctx; length:Tsize_t; dst:Puint8_t);cdecl;
  nettle_yarrow256_is_seeded : function(ctx:Pyarrow256_ctx):Tcint;cdecl;
  nettle_yarrow256_needed_sources : function(ctx:Pyarrow256_ctx):Tcunsigned;cdecl;
  nettle_yarrow256_fast_reseed : procedure(ctx:Pyarrow256_ctx);cdecl;
  nettle_yarrow256_slow_reseed : procedure(ctx:Pyarrow256_ctx);cdecl;
  nettle_yarrow_key_event_init : procedure(ctx:Pyarrow_key_event_ctx);cdecl;
  nettle_yarrow_key_event_estimate : function(ctx:Pyarrow_key_event_ctx; key:Tcunsigned; time:Tcunsigned):Tcunsigned;cdecl;

// Load/Unload libnettle
function LibNettleLoaded : boolean;
procedure Loadlibnettle(const lib : string);
// Uses the default name
procedure Loadlibnettle;
procedure Freelibnettle;

// Load/Unload libhogweed (hogweed contains cryptographic routines);
function LibHogweedLoaded : boolean;
procedure LoadlibHogweed(const lib : string);
// Uses the default name
procedure LoadlibHogweed;
procedure FreelibHogweed;

// Load/Unload both in 1 call
Function NettleInitialized : Boolean;
Procedure InitializeNettle(aNettleLib : String = ''; aHogweedLib : String = '');
Procedure FinalizeNettle;

Type
  TUnknownSymbolCallback = Procedure(const aLibrary, aSymbolName : string);

Var
  OnUnknownNettleSymbol : TUnknownSymbolCallback;

implementation

uses sysUtils, dynlibs;

var
  hlibnettle,hlibhogweed : tlibhandle;

function libnettleloaded : boolean;
begin
  Result:=hlibnettle<>nilhandle;
end;

function libhogweedloaded : boolean;
begin
  Result:=hlibhogweed<>nilhandle;
end;

procedure InitializeNettle(aNettleLib: String = ''; aHogweedLib: String = '');
begin
  if aNettleLib<>'' then
    LoadLibNettle(aNettleLib)
  else
    LoadLibNettle;
  if aHogweedLib<>'' then
    LoadLibHogWeed(aHogweedLib)
  else
    LoadLibHogWeed;
end;

function NettleInitialized: Boolean;
begin
  Result:=libnettleloaded and libhogweedloaded;
end;

procedure FinalizeNettle;
begin
  if libhogweedloaded then
    FreeLibHogweed;
  if libnettleloaded then
    FreeLibNettle;
end;


procedure FreeLibNettle;

begin
  FreeLibrary(hlibnettle);
  hlibnettle:=NilHandle;
  // Hashes
  nettle_gosthash94:=Nil;
  nettle_md2:=Nil;
  nettle_md4:=Nil;
  nettle_md5:=Nil;
  nettle_ripemd160:=Nil;
  nettle_sha1:=Nil;
  nettle_sha224:=Nil;
  nettle_sha256:=Nil;
  nettle_sha384:=Nil;
  nettle_sha3_224:=Nil;
  nettle_sha3_256:=Nil;
  nettle_sha3_384:=Nil;
  nettle_sha3_512:=Nil;
  nettle_sha512:=Nil;
  nettle_sha512_224:=Nil;
  nettle_sha512_256:=Nil;
  // Cyphers
  nettle_aes128:=Nil;
  nettle_aes192:=Nil;
  nettle_aes256:=Nil;
  nettle_camellia128:=Nil;
  nettle_camellia192:=Nil;
  nettle_camellia256:=Nil;
  nettle_cast128:=Nil;
  nettle_serpent128:=Nil;
  nettle_serpent192:=Nil;
  nettle_serpent256:=Nil;
  nettle_twofish128:=Nil;
  nettle_twofish192:=Nil;
  nettle_twofish256:=Nil;
  nettle_arctwo40:=Nil;
  nettle_arctwo64:=Nil;
  nettle_arctwo128:=Nil;
  nettle_arctwo_gutmann128:=Nil;

  // AEAD
  nettle_gcm_aes128:=Nil;
  nettle_gcm_aes192:=Nil;
  nettle_gcm_aes256:=Nil;
  nettle_gcm_camellia128:=Nil;
  nettle_gcm_camellia256:=Nil;
  nettle_eax_aes128:=Nil;
  nettle_chacha_poly1305:=Nil;

  // armor
  nettle_base64:=Nil;
  nettle_base64url:=Nil;
  nettle_base16:=Nil;

  nettle_aes128_decrypt:=Nil;
  nettle_aes128_encrypt:=Nil;
  nettle_aes128_invert_key:=Nil;
  nettle_aes128_set_decrypt_key:=Nil;
  nettle_aes128_set_encrypt_key:=Nil;
  nettle_aes192_decrypt:=Nil;
  nettle_aes192_encrypt:=Nil;
  nettle_aes192_invert_key:=Nil;
  nettle_aes192_set_decrypt_key:=Nil;
  nettle_aes192_set_encrypt_key:=Nil;
  nettle_aes256_decrypt:=Nil;
  nettle_aes256_encrypt:=Nil;
  nettle_aes256_invert_key:=Nil;
  nettle_aes256_set_decrypt_key:=Nil;
  nettle_aes256_set_encrypt_key:=Nil;
  nettle_aes_decrypt:=Nil;
  nettle_aes_encrypt:=Nil;
  nettle_aes_invert_key:=Nil;
  nettle_aes_set_decrypt_key:=Nil;
  nettle_aes_set_encrypt_key:=Nil;
  nettle_arcfour128_set_key:=Nil;
  nettle_arcfour_crypt:=Nil;
  nettle_arcfour_set_key:=Nil;
  nettle_arctwo128_set_key:=Nil;
  nettle_arctwo128_set_key_gutmann:=Nil;
  nettle_arctwo40_set_key:=Nil;
  nettle_arctwo64_set_key:=Nil;
  nettle_arctwo_decrypt:=Nil;
  nettle_arctwo_encrypt:=Nil;
  nettle_arctwo_set_key:=Nil;
  nettle_arctwo_set_key_ekb:=Nil;
  nettle_arctwo_set_key_gutmann:=Nil;
  nettle_base16_decode_final:=Nil;
  nettle_base16_decode_init:=Nil;
  nettle_base16_decode_single:=Nil;
  nettle_base16_decode_update:=Nil;
  nettle_base16_encode_single:=Nil;
  nettle_base16_encode_update:=Nil;
  nettle_base64url_decode_init:=Nil;
  nettle_base64url_encode_init:=Nil;
  nettle_base64_decode_final:=Nil;
  nettle_base64_decode_init:=Nil;
  nettle_base64_decode_single:=Nil;
  nettle_base64_decode_update:=Nil;
  nettle_base64_encode_final:=Nil;
  nettle_base64_encode_group:=Nil;
  nettle_base64_encode_init:=Nil;
  nettle_base64_encode_raw:=Nil;
  nettle_base64_encode_single:=Nil;
  nettle_base64_encode_update:=Nil;
  nettle_blowfish128_set_key:=Nil;
  nettle_blowfish_decrypt:=Nil;
  nettle_blowfish_encrypt:=Nil;
  nettle_blowfish_set_key:=Nil;
  nettle_buffer_clear:=Nil;
  nettle_buffer_copy:=Nil;
  nettle_buffer_grow:=Nil;
  nettle_buffer_init:=Nil;
  nettle_buffer_init_realloc:=Nil;
  nettle_buffer_init_size:=Nil;
  nettle_buffer_reset:=Nil;
  nettle_buffer_space:=Nil;
  nettle_buffer_write:=Nil;
  nettle_camellia128_crypt:=Nil;
  nettle_camellia128_invert_key:=Nil;
  nettle_camellia128_set_encrypt_key:=Nil;
  nettle_camellia192_set_decrypt_key:=Nil;
  nettle_camellia192_set_encrypt_key:=Nil;
  nettle_camellia256_crypt:=Nil;
  nettle_camellia256_invert_key:=Nil;
  nettle_camellia256_set_decrypt_key:=Nil;
  nettle_camellia256_set_encrypt_key:=Nil;
  nettle_camellia_set_decrypt_key:=Nil;
  nettle_cast128_decrypt:=Nil;
  nettle_cast128_encrypt:=Nil;
  nettle_cast128_set_key:=Nil;
  nettle_cast5_set_key:=Nil;
  nettle_cbc_decrypt:=Nil;
  nettle_cbc_encrypt:=Nil;
  nettle_ccm_aes128_decrypt:=Nil;
  nettle_ccm_aes128_decrypt_message:=Nil;
  nettle_ccm_aes128_digest:=Nil;
  nettle_ccm_aes128_encrypt:=Nil;
  nettle_ccm_aes128_encrypt_message:=Nil;
  nettle_ccm_aes128_set_key:=Nil;
  nettle_ccm_aes128_set_nonce:=Nil;
  nettle_ccm_aes128_update:=Nil;
  nettle_ccm_aes192_decrypt:=Nil;
  nettle_ccm_aes192_decrypt_message:=Nil;
  nettle_ccm_aes192_digest:=Nil;
  nettle_ccm_aes192_encrypt:=Nil;
  nettle_ccm_aes192_encrypt_message:=Nil;
  nettle_ccm_aes192_set_key:=Nil;
  nettle_ccm_aes192_set_nonce:=Nil;
  nettle_ccm_aes192_update:=Nil;
  nettle_ccm_aes256_decrypt:=Nil;
  nettle_ccm_aes256_decrypt_message:=Nil;
  nettle_ccm_aes256_digest:=Nil;
  nettle_ccm_aes256_encrypt:=Nil;
  nettle_ccm_aes256_encrypt_message:=Nil;
  nettle_ccm_aes256_set_key:=Nil;
  nettle_ccm_aes256_set_nonce:=Nil;
  nettle_ccm_aes256_update:=Nil;
  nettle_ccm_decrypt:=Nil;
  nettle_ccm_decrypt_message:=Nil;
  nettle_ccm_digest:=Nil;
  nettle_ccm_encrypt:=Nil;
  nettle_ccm_encrypt_message:=Nil;
  nettle_ccm_set_nonce:=Nil;
  nettle_ccm_update:=Nil;
  nettle_cfb8_decrypt:=Nil;
  nettle_cfb8_encrypt:=Nil;
  nettle_cfb_decrypt:=Nil;
  nettle_cfb_encrypt:=Nil;
  nettle_chacha_crypt:=Nil;
  nettle_chacha_poly1305_decrypt:=Nil;
  nettle_chacha_poly1305_digest:=Nil;
  nettle_chacha_poly1305_encrypt:=Nil;
  nettle_chacha_poly1305_set_key:=Nil;
  nettle_chacha_poly1305_set_nonce:=Nil;
  nettle_chacha_poly1305_update:=Nil;
  nettle_chacha_set_key:=Nil;
  nettle_chacha_set_nonce:=Nil;
  nettle_chacha_set_nonce96:=Nil;
  nettle_cmac128_digest:=Nil;
  nettle_cmac128_init:=Nil;
  nettle_cmac128_set_key:=Nil;
  nettle_cmac128_update:=Nil;
  nettle_cmac_aes128_digest:=Nil;
  nettle_cmac_aes128_set_key:=Nil;
  nettle_cmac_aes128_update:=Nil;
  nettle_cmac_aes256_digest:=Nil;
  nettle_cmac_aes256_set_key:=Nil;
  nettle_cmac_aes256_update:=Nil;
  nettle_cnd_memcpy:=Nil;
  nettle_ctr_crypt:=Nil;
  nettle_des3_decrypt:=Nil;
  nettle_des3_encrypt:=Nil;
  nettle_des3_set_key:=Nil;
  nettle_des_check_parity:=Nil;
  nettle_des_decrypt:=Nil;
  nettle_des_encrypt:=Nil;
  nettle_des_fix_parity:=Nil;
  nettle_des_set_key:=Nil;
  nettle_eax_aes128_decrypt:=Nil;
  nettle_eax_aes128_digest:=Nil;
  nettle_eax_aes128_encrypt:=Nil;
  nettle_eax_aes128_set_key:=Nil;
  nettle_eax_aes128_set_nonce:=Nil;
  nettle_eax_aes128_update:=Nil;
  nettle_eax_decrypt:=Nil;
  nettle_eax_digest:=Nil;
  nettle_eax_encrypt:=Nil;
  nettle_eax_set_key:=Nil;
  nettle_eax_set_nonce:=Nil;
  nettle_eax_update:=Nil;
  nettle_gcm_aes128_decrypt:=Nil;
  nettle_gcm_aes128_digest:=Nil;
  nettle_gcm_aes128_encrypt:=Nil;
  nettle_gcm_aes128_set_iv:=Nil;
  nettle_gcm_aes128_set_key:=Nil;
  nettle_gcm_aes128_update:=Nil;
  nettle_gcm_aes192_decrypt:=Nil;
  nettle_gcm_aes192_digest:=Nil;
  nettle_gcm_aes192_encrypt:=Nil;
  nettle_gcm_aes192_set_iv:=Nil;
  nettle_gcm_aes192_set_key:=Nil;
  nettle_gcm_aes192_update:=Nil;
  nettle_gcm_aes256_decrypt:=Nil;
  nettle_gcm_aes256_digest:=Nil;
  nettle_gcm_aes256_encrypt:=Nil;
  nettle_gcm_aes256_set_iv:=Nil;
  nettle_gcm_aes256_set_key:=Nil;
  nettle_gcm_aes256_update:=Nil;
  nettle_gcm_aes_decrypt:=Nil;
  nettle_gcm_aes_digest:=Nil;
  nettle_gcm_aes_encrypt:=Nil;
  nettle_gcm_aes_set_iv:=Nil;
  nettle_gcm_aes_set_key:=Nil;
  nettle_gcm_aes_update:=Nil;
  nettle_gcm_camellia128_decrypt:=Nil;
  nettle_gcm_camellia128_digest:=Nil;
  nettle_gcm_camellia128_encrypt:=Nil;
  nettle_gcm_camellia128_set_iv:=Nil;
  nettle_gcm_camellia128_set_key:=Nil;
  nettle_gcm_camellia128_update:=Nil;
  nettle_gcm_camellia256_decrypt:=Nil;
  nettle_gcm_camellia256_digest:=Nil;
  nettle_gcm_camellia256_encrypt:=Nil;
  nettle_gcm_camellia256_set_iv:=Nil;
  nettle_gcm_camellia256_set_key:=Nil;
  nettle_gcm_camellia256_update:=Nil;
  nettle_gcm_decrypt:=Nil;
  nettle_gcm_digest:=Nil;
  nettle_gcm_encrypt:=Nil;
  nettle_gcm_set_iv:=Nil;
  nettle_gcm_set_key:=Nil;
  nettle_gcm_update:=Nil;
  nettle_get_aeads:=Nil;
  nettle_get_armors:=Nil;
  nettle_get_ciphers:=Nil;
  nettle_get_hashes:=Nil;
  nettle_gosthash94_digest:=Nil;
  nettle_gosthash94_init:=Nil;
  nettle_gosthash94_update:=Nil;
  nettle_hkdf_expand:=Nil;
  nettle_hkdf_extract:=Nil;
  nettle_hmac_digest:=Nil;
  nettle_hmac_md5_digest:=Nil;
  nettle_hmac_md5_set_key:=Nil;
  nettle_hmac_md5_update:=Nil;
  nettle_hmac_ripemd160_digest:=Nil;
  nettle_hmac_ripemd160_set_key:=Nil;
  nettle_hmac_ripemd160_update:=Nil;
  nettle_hmac_set_key:=Nil;
  nettle_hmac_sha1_digest:=Nil;
  nettle_hmac_sha1_set_key:=Nil;
  nettle_hmac_sha1_update:=Nil;
  nettle_hmac_sha224_digest:=Nil;
  nettle_hmac_sha224_set_key:=Nil;
  nettle_hmac_sha256_digest:=Nil;
  nettle_hmac_sha256_set_key:=Nil;
  nettle_hmac_sha256_update:=Nil;
  nettle_hmac_sha384_digest:=Nil;
  nettle_hmac_sha384_set_key:=Nil;
  nettle_hmac_sha512_digest:=Nil;
  nettle_hmac_sha512_set_key:=Nil;
  nettle_hmac_sha512_update:=Nil;
  nettle_hmac_update:=Nil;
  nettle_knuth_lfib_get:=Nil;
  nettle_knuth_lfib_get_array:=Nil;
  nettle_knuth_lfib_init:=Nil;
  nettle_knuth_lfib_random:=Nil;
  nettle_lookup_hash:=Nil;
  nettle_md2_digest:=Nil;
  nettle_md2_init:=Nil;
  nettle_md2_update:=Nil;
  nettle_md4_digest:=Nil;
  nettle_md4_init:=Nil;
  nettle_md4_update:=Nil;
  nettle_MD5Final:=Nil;
  nettle_MD5Init:=Nil;
  nettle_MD5Update:=Nil;
  nettle_md5_compress:=Nil;
  nettle_md5_digest:=Nil;
  nettle_md5_init:=Nil;
  nettle_md5_update:=Nil;
  nettle_memeql_sec:=Nil;
  nettle_memxor:=Nil;
  nettle_memxor3:=Nil;
  nettle_pbkdf2:=Nil;
  nettle_pbkdf2_hmac_sha1:=Nil;
  nettle_pbkdf2_hmac_sha256:=Nil;
  nettle_poly1305_aes_digest:=Nil;
  nettle_poly1305_aes_set_key:=Nil;
  nettle_poly1305_aes_set_nonce:=Nil;
  nettle_poly1305_aes_update:=Nil;
  nettle_poly1305_digest:=Nil;
  nettle_poly1305_set_key:=Nil;
  nettle_ripemd160_digest:=Nil;
  nettle_ripemd160_init:=Nil;
  nettle_ripemd160_update:=Nil;
  nettle_salsa20r12_crypt:=Nil;
  nettle_salsa20_128_set_key:=Nil;
  nettle_salsa20_256_set_key:=Nil;
  nettle_salsa20_crypt:=Nil;
  nettle_salsa20_set_key:=Nil;
  nettle_salsa20_set_nonce:=Nil;
  nettle_serpent128_set_key:=Nil;
  nettle_serpent192_set_key:=Nil;
  nettle_serpent256_set_key:=Nil;
  nettle_serpent_decrypt:=Nil;
  nettle_serpent_encrypt:=Nil;
  nettle_serpent_set_key:=Nil;
  nettle_sha1_compress:=Nil;
  nettle_sha1_digest:=Nil;
  nettle_sha1_init:=Nil;
  nettle_sha1_update:=Nil;
  nettle_sha224_digest:=Nil;
  nettle_sha224_init:=Nil;
  nettle_sha256_digest:=Nil;
  nettle_sha256_init:=Nil;
  nettle_sha256_update:=Nil;
  nettle_sha384_digest:=Nil;
  nettle_sha384_init:=Nil;
  nettle_sha3_224_digest:=Nil;
  nettle_sha3_224_init:=Nil;
  nettle_sha3_224_update:=Nil;
  nettle_sha3_256_digest:=Nil;
  nettle_sha3_256_init:=Nil;
  nettle_sha3_256_update:=Nil;
  nettle_sha3_384_digest:=Nil;
  nettle_sha3_384_init:=Nil;
  nettle_sha3_384_update:=Nil;
  nettle_sha3_512_digest:=Nil;
  nettle_sha3_512_init:=Nil;
  nettle_sha3_512_update:=Nil;
  nettle_sha3_permute:=Nil;
  nettle_sha512_224_digest:=Nil;
  nettle_sha512_224_init:=Nil;
  nettle_sha512_256_digest:=Nil;
  nettle_sha512_256_init:=Nil;
  nettle_sha512_digest:=Nil;
  nettle_sha512_init:=Nil;
  nettle_sha512_update:=Nil;
  nettle_twofish128_set_key:=Nil;
  nettle_twofish192_set_key:=Nil;
  nettle_twofish256_set_key:=Nil;
  nettle_twofish_decrypt:=Nil;
  nettle_twofish_encrypt:=Nil;
  nettle_twofish_set_key:=Nil;
  nettle_umac128_digest:=Nil;
  nettle_umac128_set_key:=Nil;
  nettle_umac128_set_nonce:=Nil;
  nettle_umac128_update:=Nil;
  nettle_umac32_digest:=Nil;
  nettle_umac32_set_key:=Nil;
  nettle_umac32_set_nonce:=Nil;
  nettle_umac32_update:=Nil;
  nettle_umac64_digest:=Nil;
  nettle_umac64_set_key:=Nil;
  nettle_umac64_set_nonce:=Nil;
  nettle_umac64_update:=Nil;
  nettle_umac96_digest:=Nil;
  nettle_umac96_set_key:=Nil;
  nettle_umac96_set_nonce:=Nil;
  nettle_umac96_update:=Nil;
  nettle_version_major:=Nil;
  nettle_version_minor:=Nil;
  nettle_xts_aes128_decrypt_message:=Nil;
  nettle_xts_aes128_encrypt_message:=Nil;
  nettle_xts_aes128_set_decrypt_key:=Nil;
  nettle_xts_aes128_set_encrypt_key:=Nil;
  nettle_xts_aes256_decrypt_message:=Nil;
  nettle_xts_aes256_encrypt_message:=Nil;
  nettle_xts_aes256_set_decrypt_key:=Nil;
  nettle_xts_aes256_set_encrypt_key:=Nil;
  nettle_xts_decrypt_message:=Nil;
  nettle_xts_encrypt_message:=Nil;
  nettle_yarrow256_fast_reseed:=Nil;
  nettle_yarrow256_init:=Nil;
  nettle_yarrow256_is_seeded:=Nil;
  nettle_yarrow256_needed_sources:=Nil;
  nettle_yarrow256_random:=Nil;
  nettle_yarrow256_seed:=Nil;
  nettle_yarrow256_slow_reseed:=Nil;
  nettle_yarrow256_update:=Nil;
  nettle_yarrow_key_event_estimate:=Nil;
  nettle_yarrow_key_event_init:=Nil;
  _nettle_poly1305_block:=Nil;
end;

procedure Freelibhogweed;

begin
  nettle_asn1_der_decode_bitstring:=Nil;
  nettle_asn1_der_decode_bitstring_last:=Nil;
  nettle_asn1_der_decode_constructed:=Nil;
  nettle_asn1_der_decode_constructed_last:=Nil;
  nettle_asn1_der_get_uint32:=Nil;
  nettle_asn1_der_iterator_first:=Nil;
  nettle_asn1_der_iterator_next:=Nil;
  nettle_curve25519_mul:=Nil;
  nettle_curve25519_mul_g:=Nil;
  nettle_dsa_compat_generate_keypair:=Nil;
  nettle_dsa_generate_keypair:=Nil;
  nettle_dsa_generate_params:=Nil;
  nettle_dsa_keypair_from_sexp_alist:=Nil;
  nettle_dsa_keypair_to_sexp:=Nil;
  nettle_dsa_openssl_private_key_from_der_iterator:=Nil;
  nettle_dsa_params_clear:=Nil;
  nettle_dsa_params_from_der_iterator:=Nil;
  nettle_dsa_params_init:=Nil;
  nettle_dsa_private_key_clear:=Nil;
  nettle_dsa_private_key_init:=Nil;
  nettle_dsa_public_key_clear:=Nil;
  nettle_dsa_public_key_from_der_iterator:=Nil;
  nettle_dsa_public_key_init:=Nil;
  nettle_dsa_sha1_keypair_from_sexp:=Nil;
  nettle_dsa_sha1_sign:=Nil;
  nettle_dsa_sha1_sign_digest:=Nil;
  nettle_dsa_sha1_verify:=Nil;
  nettle_dsa_sha1_verify_digest:=Nil;
  nettle_dsa_sha256_keypair_from_sexp:=Nil;
  nettle_dsa_sha256_sign:=Nil;
  nettle_dsa_sha256_sign_digest:=Nil;
  nettle_dsa_sha256_verify:=Nil;
  nettle_dsa_sha256_verify_digest:=Nil;
  nettle_dsa_sign:=Nil;
  nettle_dsa_signature_clear:=Nil;
  nettle_dsa_signature_from_sexp:=Nil;
  nettle_dsa_signature_init:=Nil;
  nettle_dsa_verify:=Nil;
  nettle_ecc_bit_size:=Nil;
  nettle_ecc_ecdsa_sign:=Nil;
  nettle_ecc_ecdsa_sign_itch:=Nil;
  nettle_ecc_ecdsa_verify:=Nil;
  nettle_ecc_ecdsa_verify_itch:=Nil;
  nettle_ecc_point_clear:=Nil;
  nettle_ecc_point_get:=Nil;
  nettle_ecc_point_init:=Nil;
  nettle_ecc_point_mul:=Nil;
  nettle_ecc_point_mul_g:=Nil;
  nettle_ecc_point_set:=Nil;
  nettle_ecc_scalar_clear:=Nil;
  nettle_ecc_scalar_get:=Nil;
  nettle_ecc_scalar_init:=Nil;
  nettle_ecc_scalar_random:=Nil;
  nettle_ecc_scalar_set:=Nil;
  nettle_ecc_size:=Nil;
  nettle_ecc_size_a:=Nil;
  nettle_ecc_size_j:=Nil;
  nettle_ecdsa_generate_keypair:=Nil;
  nettle_ecdsa_sign:=Nil;
  nettle_ecdsa_verify:=Nil;
  nettle_ed25519_sha512_public_key:=Nil;
  nettle_ed25519_sha512_sign:=Nil;
  nettle_ed25519_sha512_verify:=Nil;
  nettle_get_secp_192r1:=Nil;
  nettle_get_secp_224r1:=Nil;
  nettle_get_secp_256r1:=Nil;
  nettle_get_secp_384r1:=Nil;
  nettle_get_secp_521r1:=Nil;
  nettle_openssl_provate_key_from_der:=Nil;
  nettle_pgp_armor:=Nil;
  nettle_pgp_crc24:=Nil;
  nettle_pgp_put_header:=Nil;
  nettle_pgp_put_header_length:=Nil;
  nettle_pgp_put_length:=Nil;
  nettle_pgp_put_mpi:=Nil;
  nettle_pgp_put_public_rsa_key:=Nil;
  nettle_pgp_put_rsa_sha1_signature:=Nil;
  nettle_pgp_put_string:=Nil;
  nettle_pgp_put_sub_packet:=Nil;
  nettle_pgp_put_uint16:=Nil;
  nettle_pgp_put_uint32:=Nil;
  nettle_pgp_put_userid:=Nil;
  nettle_pgp_sub_packet_end:=Nil;
  nettle_pgp_sub_packet_start:=Nil;
  nettle_pkcs1_decrypt:=Nil;
  nettle_pkcs1_encrypt:=Nil;
  nettle_pkcs1_rsa_digest_encode:=Nil;
  nettle_pkcs1_rsa_md5_encode:=Nil;
  nettle_pkcs1_rsa_md5_encode_digest:=Nil;
  nettle_pkcs1_rsa_sha1_encode:=Nil;
  nettle_pkcs1_rsa_sha1_encode_digest:=Nil;
  nettle_pkcs1_rsa_sha256_encode:=Nil;
  nettle_pkcs1_rsa_sha256_encode_digest:=Nil;
  nettle_pkcs1_rsa_sha512_encode:=Nil;
  nettle_pkcs1_rsa_sha512_encode_digest:=Nil;
  nettle_pss_encode_mgf1:=Nil;
  nettle_pss_mgf1:=Nil;
  nettle_pss_verify_mgf1:=Nil;
  nettle_rsa_compute_root:=Nil;
  nettle_rsa_compute_root_tr:=Nil;
  nettle_rsa_decrypt:=Nil;
  nettle_rsa_decrypt_tr:=Nil;
  nettle_rsa_encrypt:=Nil;
  nettle_rsa_generate_keypair:=Nil;
  nettle_rsa_keypair_from_der:=Nil;
  nettle_rsa_keypair_from_sexp:=Nil;
  nettle_rsa_keypair_from_sexp_alist:=Nil;
  nettle_rsa_keypair_to_openpgp:=Nil;
  nettle_rsa_keypair_to_sexp:=Nil;
  nettle_rsa_md5_sign:=Nil;
  nettle_rsa_md5_sign_digest:=Nil;
  nettle_rsa_md5_sign_digest_tr:=Nil;
  nettle_rsa_md5_sign_tr:=Nil;
  nettle_rsa_md5_verify:=Nil;
  nettle_rsa_md5_verify_digest:=Nil;
  nettle_rsa_pkcs1_sign:=Nil;
  nettle_rsa_pkcs1_sign_tr:=Nil;
  nettle_rsa_pkcs1_verify:=Nil;
  nettle_rsa_private_key_clear:=Nil;
  nettle_rsa_private_key_from_der_iterator:=Nil;
  nettle_rsa_private_key_init:=Nil;
  nettle_rsa_private_key_prepare:=Nil;
  nettle_rsa_pss_sha256_sign_digest_tr:=Nil;
  nettle_rsa_pss_sha256_verify_digest:=Nil;
  nettle_rsa_pss_sha384_sign_digest_tr:=Nil;
  nettle_rsa_pss_sha384_verify_digest:=Nil;
  nettle_rsa_pss_sha512_sign_digest_tr:=Nil;
  nettle_rsa_pss_sha512_verify_digest:=Nil;
  nettle_rsa_public_key_clear:=Nil;
  nettle_rsa_public_key_from_der_iterator:=Nil;
  nettle_rsa_public_key_init:=Nil;
  nettle_rsa_public_key_prepare:=Nil;
  nettle_rsa_sec_decrypt:=Nil;
  nettle_rsa_sha1_sign:=Nil;
  nettle_rsa_sha1_sign_digest:=Nil;
  nettle_rsa_sha1_sign_digest_tr:=Nil;
  nettle_rsa_sha1_sign_tr:=Nil;
  nettle_rsa_sha1_verify:=Nil;
  nettle_rsa_sha1_verify_digest:=Nil;
  nettle_rsa_sha256_sign:=Nil;
  nettle_rsa_sha256_sign_digest:=Nil;
  nettle_rsa_sha256_sign_digest_tr:=Nil;
  nettle_rsa_sha256_sign_tr:=Nil;
  nettle_rsa_sha256_verify:=Nil;
  nettle_rsa_sha256_verify_digest:=Nil;
  nettle_rsa_sha512_sign:=Nil;
  nettle_rsa_sha512_sign_digest:=Nil;
  nettle_rsa_sha512_sign_digest_tr:=Nil;
  nettle_rsa_sha512_sign_tr:=Nil;
  nettle_rsa_sha512_verify:=Nil;
  nettle_rsa_sha512_verify_digest:=Nil;
  nettle_sexp_format:=Nil;
  nettle_sexp_iterator_assoc:=Nil;
  nettle_sexp_iterator_check_type:=Nil;
  nettle_sexp_iterator_check_types:=Nil;
  nettle_sexp_iterator_enter_list:=Nil;
  nettle_sexp_iterator_exit_list:=Nil;
  nettle_sexp_iterator_first:=Nil;
  nettle_sexp_iterator_get_uint32:=Nil;
  nettle_sexp_iterator_next:=Nil;
  nettle_sexp_iterator_subexpr:=Nil;
  nettle_sexp_transport_format:=Nil;
  nettle_sexp_transport_iterator_first:=Nil;
end;

procedure LoadlibHogweed;

begin
  LoadlibHogweed(HogweedLibraryFileName);
end;


procedure LoadlibHogweed(const lib : string);

  Function ProcAddr(aName : string) : Pointer;

  begin
    Result:=GetProcAddress(hlibhogweed,aName);
    If (Result=Nil) and assigned(OnUnknownNettleSymbol) then
      OnUnknownNettleSymbol(lib,aName);
  end;

begin
  if libHogweedLoaded then
    FreelibHogweed;
  hlibHogweed:=LoadLibrary(lib);
  if hlibHogweed=0 then
    raise Exception.Create(format('Could not load library: %s',[lib]));

  pointer(nettle_asn1_der_decode_bitstring):=ProcAddr('nettle_asn1_der_decode_bitstring');
  pointer(nettle_asn1_der_decode_bitstring_last):=ProcAddr('nettle_asn1_der_decode_bitstring_last');
  pointer(nettle_asn1_der_decode_constructed):=ProcAddr('nettle_asn1_der_decode_constructed');
  pointer(nettle_asn1_der_decode_constructed_last):=ProcAddr('nettle_asn1_der_decode_constructed_last');
  pointer(nettle_asn1_der_get_uint32):=ProcAddr('nettle_asn1_der_get_uint32');
  pointer(nettle_asn1_der_iterator_first):=ProcAddr('nettle_asn1_der_iterator_first');
  pointer(nettle_asn1_der_iterator_next):=ProcAddr('nettle_asn1_der_iterator_next');
  pointer(nettle_curve25519_mul):=ProcAddr('nettle_curve25519_mul');
  pointer(nettle_curve25519_mul_g):=ProcAddr('nettle_curve25519_mul_g');
  pointer(nettle_dsa_compat_generate_keypair):=ProcAddr('nettle_dsa_compat_generate_keypair');
  pointer(nettle_dsa_generate_keypair):=ProcAddr('nettle_dsa_generate_keypair');
  pointer(nettle_dsa_generate_params):=ProcAddr('nettle_dsa_generate_params');
  pointer(nettle_dsa_keypair_from_sexp_alist):=ProcAddr('nettle_dsa_keypair_from_sexp_alist');
  pointer(nettle_dsa_keypair_to_sexp):=ProcAddr('nettle_dsa_keypair_to_sexp');
  pointer(nettle_dsa_openssl_private_key_from_der_iterator):=ProcAddr('nettle_dsa_openssl_private_key_from_der_iterator');
  pointer(nettle_dsa_params_clear):=ProcAddr('nettle_dsa_params_clear');
  pointer(nettle_dsa_params_from_der_iterator):=ProcAddr('nettle_dsa_params_from_der_iterator');
  pointer(nettle_dsa_params_init):=ProcAddr('nettle_dsa_params_init');
  pointer(nettle_dsa_private_key_clear):=ProcAddr('nettle_dsa_private_key_clear');
  pointer(nettle_dsa_private_key_init):=ProcAddr('nettle_dsa_private_key_init');
  pointer(nettle_dsa_public_key_clear):=ProcAddr('nettle_dsa_public_key_clear');
  pointer(nettle_dsa_public_key_from_der_iterator):=ProcAddr('nettle_dsa_public_key_from_der_iterator');
  pointer(nettle_dsa_public_key_init):=ProcAddr('nettle_dsa_public_key_init');
  pointer(nettle_dsa_sha1_keypair_from_sexp):=ProcAddr('nettle_dsa_sha1_keypair_from_sexp');
  pointer(nettle_dsa_sha1_sign):=ProcAddr('nettle_dsa_sha1_sign');
  pointer(nettle_dsa_sha1_sign_digest):=ProcAddr('nettle_dsa_sha1_sign_digest');
  pointer(nettle_dsa_sha1_verify):=ProcAddr('nettle_dsa_sha1_verify');
  pointer(nettle_dsa_sha1_verify_digest):=ProcAddr('nettle_dsa_sha1_verify_digest');
  pointer(nettle_dsa_sha256_keypair_from_sexp):=ProcAddr('nettle_dsa_sha256_keypair_from_sexp');
  pointer(nettle_dsa_sha256_sign):=ProcAddr('nettle_dsa_sha256_sign');
  pointer(nettle_dsa_sha256_sign_digest):=ProcAddr('nettle_dsa_sha256_sign_digest');
  pointer(nettle_dsa_sha256_verify):=ProcAddr('nettle_dsa_sha256_verify');
  pointer(nettle_dsa_sha256_verify_digest):=ProcAddr('nettle_dsa_sha256_verify_digest');
  pointer(nettle_dsa_sign):=ProcAddr('nettle_dsa_sign');
  pointer(nettle_dsa_signature_clear):=ProcAddr('nettle_dsa_signature_clear');
  pointer(nettle_dsa_signature_from_sexp):=ProcAddr('nettle_dsa_signature_from_sexp');
  pointer(nettle_dsa_signature_init):=ProcAddr('nettle_dsa_signature_init');
  pointer(nettle_dsa_verify):=ProcAddr('nettle_dsa_verify');
  pointer(nettle_ecc_bit_size):=ProcAddr('nettle_ecc_bit_size');
  pointer(nettle_ecc_ecdsa_sign):=ProcAddr('nettle_ecc_ecdsa_sign');
  pointer(nettle_ecc_ecdsa_sign_itch):=ProcAddr('nettle_ecc_ecdsa_sign_itch');
  pointer(nettle_ecc_ecdsa_verify):=ProcAddr('nettle_ecc_ecdsa_verify');
  pointer(nettle_ecc_ecdsa_verify_itch):=ProcAddr('nettle_ecc_ecdsa_verify_itch');
  pointer(nettle_ecc_point_clear):=ProcAddr('nettle_ecc_point_clear');
  pointer(nettle_ecc_point_get):=ProcAddr('nettle_ecc_point_get');
  pointer(nettle_ecc_point_init):=ProcAddr('nettle_ecc_point_init');
  pointer(nettle_ecc_point_mul):=ProcAddr('nettle_ecc_point_mul');
  pointer(nettle_ecc_point_mul_g):=ProcAddr('nettle_ecc_point_mul_g');
  pointer(nettle_ecc_point_set):=ProcAddr('nettle_ecc_point_set');
  pointer(nettle_ecc_scalar_clear):=ProcAddr('nettle_ecc_scalar_clear');
  pointer(nettle_ecc_scalar_get):=ProcAddr('nettle_ecc_scalar_get');
  pointer(nettle_ecc_scalar_init):=ProcAddr('nettle_ecc_scalar_init');
  pointer(nettle_ecc_scalar_random):=ProcAddr('nettle_ecc_scalar_random');
  pointer(nettle_ecc_scalar_set):=ProcAddr('nettle_ecc_scalar_set');
  pointer(nettle_ecc_size):=ProcAddr('nettle_ecc_size');
  pointer(nettle_ecc_size_a):=ProcAddr('nettle_ecc_size_a');
  pointer(nettle_ecc_size_j):=ProcAddr('nettle_ecc_size_j');
  pointer(nettle_ecdsa_generate_keypair):=ProcAddr('nettle_ecdsa_generate_keypair');
  pointer(nettle_ecdsa_sign):=ProcAddr('nettle_ecdsa_sign');
  pointer(nettle_ecdsa_verify):=ProcAddr('nettle_ecdsa_verify');
  pointer(nettle_ed25519_sha512_public_key):=ProcAddr('nettle_ed25519_sha512_public_key');
  pointer(nettle_ed25519_sha512_sign):=ProcAddr('nettle_ed25519_sha512_sign');
  pointer(nettle_ed25519_sha512_verify):=ProcAddr('nettle_ed25519_sha512_verify');
  pointer(nettle_get_secp_192r1):=ProcAddr('nettle_get_secp_192r1');
  pointer(nettle_get_secp_224r1):=ProcAddr('nettle_get_secp_224r1');
  pointer(nettle_get_secp_256r1):=ProcAddr('nettle_get_secp_256r1');
  pointer(nettle_get_secp_384r1):=ProcAddr('nettle_get_secp_384r1');
  pointer(nettle_get_secp_521r1):=ProcAddr('nettle_get_secp_521r1');
  pointer(nettle_openssl_provate_key_from_der):=ProcAddr('nettle_openssl_provate_key_from_der');
  pointer(nettle_pgp_armor):=ProcAddr('nettle_pgp_armor');
  pointer(nettle_pgp_crc24):=ProcAddr('nettle_pgp_crc24');
  pointer(nettle_pgp_put_header):=ProcAddr('nettle_pgp_put_header');
  pointer(nettle_pgp_put_header_length):=ProcAddr('nettle_pgp_put_header_length');
  pointer(nettle_pgp_put_length):=ProcAddr('nettle_pgp_put_length');
  pointer(nettle_pgp_put_mpi):=ProcAddr('nettle_pgp_put_mpi');
  pointer(nettle_pgp_put_public_rsa_key):=ProcAddr('nettle_pgp_put_public_rsa_key');
  pointer(nettle_pgp_put_rsa_sha1_signature):=ProcAddr('nettle_pgp_put_rsa_sha1_signature');
  pointer(nettle_pgp_put_string):=ProcAddr('nettle_pgp_put_string');
  pointer(nettle_pgp_put_sub_packet):=ProcAddr('nettle_pgp_put_sub_packet');
  pointer(nettle_pgp_put_uint16):=ProcAddr('nettle_pgp_put_uint16');
  pointer(nettle_pgp_put_uint32):=ProcAddr('nettle_pgp_put_uint32');
  pointer(nettle_pgp_put_userid):=ProcAddr('nettle_pgp_put_userid');
  pointer(nettle_pgp_sub_packet_end):=ProcAddr('nettle_pgp_sub_packet_end');
  pointer(nettle_pgp_sub_packet_start):=ProcAddr('nettle_pgp_sub_packet_start');
  pointer(nettle_pkcs1_decrypt):=ProcAddr('nettle_pkcs1_decrypt');
  pointer(nettle_pkcs1_encrypt):=ProcAddr('nettle_pkcs1_encrypt');
  pointer(nettle_pkcs1_rsa_digest_encode):=ProcAddr('nettle_pkcs1_rsa_digest_encode');
  pointer(nettle_pkcs1_rsa_md5_encode):=ProcAddr('nettle_pkcs1_rsa_md5_encode');
  pointer(nettle_pkcs1_rsa_md5_encode_digest):=ProcAddr('nettle_pkcs1_rsa_md5_encode_digest');
  pointer(nettle_pkcs1_rsa_sha1_encode):=ProcAddr('nettle_pkcs1_rsa_sha1_encode');
  pointer(nettle_pkcs1_rsa_sha1_encode_digest):=ProcAddr('nettle_pkcs1_rsa_sha1_encode_digest');
  pointer(nettle_pkcs1_rsa_sha256_encode):=ProcAddr('nettle_pkcs1_rsa_sha256_encode');
  pointer(nettle_pkcs1_rsa_sha256_encode_digest):=ProcAddr('nettle_pkcs1_rsa_sha256_encode_digest');
  pointer(nettle_pkcs1_rsa_sha512_encode):=ProcAddr('nettle_pkcs1_rsa_sha512_encode');
  pointer(nettle_pkcs1_rsa_sha512_encode_digest):=ProcAddr('nettle_pkcs1_rsa_sha512_encode_digest');
  pointer(nettle_pss_encode_mgf1):=ProcAddr('nettle_pss_encode_mgf1');
  pointer(nettle_pss_mgf1):=ProcAddr('nettle_pss_mgf1');
  pointer(nettle_pss_verify_mgf1):=ProcAddr('nettle_pss_verify_mgf1');
  pointer(nettle_rsa_compute_root):=ProcAddr('nettle_rsa_compute_root');
  pointer(nettle_rsa_compute_root_tr):=ProcAddr('nettle_rsa_compute_root_tr');
  pointer(nettle_rsa_decrypt):=ProcAddr('nettle_rsa_decrypt');
  pointer(nettle_rsa_decrypt_tr):=ProcAddr('nettle_rsa_decrypt_tr');
  pointer(nettle_rsa_encrypt):=ProcAddr('nettle_rsa_encrypt');
  pointer(nettle_rsa_generate_keypair):=ProcAddr('nettle_rsa_generate_keypair');
  pointer(nettle_rsa_keypair_from_der):=ProcAddr('nettle_rsa_keypair_from_der');
  pointer(nettle_rsa_keypair_from_sexp):=ProcAddr('nettle_rsa_keypair_from_sexp');
  pointer(nettle_rsa_keypair_from_sexp_alist):=ProcAddr('nettle_rsa_keypair_from_sexp_alist');
  pointer(nettle_rsa_keypair_to_openpgp):=ProcAddr('nettle_rsa_keypair_to_openpgp');
  pointer(nettle_rsa_keypair_to_sexp):=ProcAddr('nettle_rsa_keypair_to_sexp');
  pointer(nettle_rsa_md5_sign):=ProcAddr('nettle_rsa_md5_sign');
  pointer(nettle_rsa_md5_sign_digest):=ProcAddr('nettle_rsa_md5_sign_digest');
  pointer(nettle_rsa_md5_sign_digest_tr):=ProcAddr('nettle_rsa_md5_sign_digest_tr');
  pointer(nettle_rsa_md5_sign_tr):=ProcAddr('nettle_rsa_md5_sign_tr');
  pointer(nettle_rsa_md5_verify):=ProcAddr('nettle_rsa_md5_verify');
  pointer(nettle_rsa_md5_verify_digest):=ProcAddr('nettle_rsa_md5_verify_digest');
  pointer(nettle_rsa_pkcs1_sign):=ProcAddr('nettle_rsa_pkcs1_sign');
  pointer(nettle_rsa_pkcs1_sign_tr):=ProcAddr('nettle_rsa_pkcs1_sign_tr');
  pointer(nettle_rsa_pkcs1_verify):=ProcAddr('nettle_rsa_pkcs1_verify');
  pointer(nettle_rsa_private_key_clear):=ProcAddr('nettle_rsa_private_key_clear');
  pointer(nettle_rsa_private_key_from_der_iterator):=ProcAddr('nettle_rsa_private_key_from_der_iterator');
  pointer(nettle_rsa_private_key_init):=ProcAddr('nettle_rsa_private_key_init');
  pointer(nettle_rsa_private_key_prepare):=ProcAddr('nettle_rsa_private_key_prepare');
  pointer(nettle_rsa_pss_sha256_sign_digest_tr):=ProcAddr('nettle_rsa_pss_sha256_sign_digest_tr');
  pointer(nettle_rsa_pss_sha256_verify_digest):=ProcAddr('nettle_rsa_pss_sha256_verify_digest');
  pointer(nettle_rsa_pss_sha384_sign_digest_tr):=ProcAddr('nettle_rsa_pss_sha384_sign_digest_tr');
  pointer(nettle_rsa_pss_sha384_verify_digest):=ProcAddr('nettle_rsa_pss_sha384_verify_digest');
  pointer(nettle_rsa_pss_sha512_sign_digest_tr):=ProcAddr('nettle_rsa_pss_sha512_sign_digest_tr');
  pointer(nettle_rsa_pss_sha512_verify_digest):=ProcAddr('nettle_rsa_pss_sha512_verify_digest');
  pointer(nettle_rsa_public_key_clear):=ProcAddr('nettle_rsa_public_key_clear');
  pointer(nettle_rsa_public_key_from_der_iterator):=ProcAddr('nettle_rsa_public_key_from_der_iterator');
  pointer(nettle_rsa_public_key_init):=ProcAddr('nettle_rsa_public_key_init');
  pointer(nettle_rsa_public_key_prepare):=ProcAddr('nettle_rsa_public_key_prepare');
  pointer(nettle_rsa_sec_decrypt):=ProcAddr('nettle_rsa_sec_decrypt');
  pointer(nettle_rsa_sha1_sign):=ProcAddr('nettle_rsa_sha1_sign');
  pointer(nettle_rsa_sha1_sign_digest):=ProcAddr('nettle_rsa_sha1_sign_digest');
  pointer(nettle_rsa_sha1_sign_digest_tr):=ProcAddr('nettle_rsa_sha1_sign_digest_tr');
  pointer(nettle_rsa_sha1_sign_tr):=ProcAddr('nettle_rsa_sha1_sign_tr');
  pointer(nettle_rsa_sha1_verify):=ProcAddr('nettle_rsa_sha1_verify');
  pointer(nettle_rsa_sha1_verify_digest):=ProcAddr('nettle_rsa_sha1_verify_digest');
  pointer(nettle_rsa_sha256_sign):=ProcAddr('nettle_rsa_sha256_sign');
  pointer(nettle_rsa_sha256_sign_digest):=ProcAddr('nettle_rsa_sha256_sign_digest');
  pointer(nettle_rsa_sha256_sign_digest_tr):=ProcAddr('nettle_rsa_sha256_sign_digest_tr');
  pointer(nettle_rsa_sha256_sign_tr):=ProcAddr('nettle_rsa_sha256_sign_tr');
  pointer(nettle_rsa_sha256_verify):=ProcAddr('nettle_rsa_sha256_verify');
  pointer(nettle_rsa_sha256_verify_digest):=ProcAddr('nettle_rsa_sha256_verify_digest');
  pointer(nettle_rsa_sha512_sign):=ProcAddr('nettle_rsa_sha512_sign');
  pointer(nettle_rsa_sha512_sign_digest):=ProcAddr('nettle_rsa_sha512_sign_digest');
  pointer(nettle_rsa_sha512_sign_digest_tr):=ProcAddr('nettle_rsa_sha512_sign_digest_tr');
  pointer(nettle_rsa_sha512_sign_tr):=ProcAddr('nettle_rsa_sha512_sign_tr');
  pointer(nettle_rsa_sha512_verify):=ProcAddr('nettle_rsa_sha512_verify');
  pointer(nettle_rsa_sha512_verify_digest):=ProcAddr('nettle_rsa_sha512_verify_digest');
  pointer(nettle_sexp_format):=ProcAddr('nettle_sexp_format');
  pointer(nettle_sexp_iterator_assoc):=ProcAddr('nettle_sexp_iterator_assoc');
  pointer(nettle_sexp_iterator_check_type):=ProcAddr('nettle_sexp_iterator_check_type');
  pointer(nettle_sexp_iterator_check_types):=ProcAddr('nettle_sexp_iterator_check_types');
  pointer(nettle_sexp_iterator_enter_list):=ProcAddr('nettle_sexp_iterator_enter_list');
  pointer(nettle_sexp_iterator_exit_list):=ProcAddr('nettle_sexp_iterator_exit_list');
  pointer(nettle_sexp_iterator_first):=ProcAddr('nettle_sexp_iterator_first');
  pointer(nettle_sexp_iterator_get_uint32):=ProcAddr('nettle_sexp_iterator_get_uint32');
  pointer(nettle_sexp_iterator_next):=ProcAddr('nettle_sexp_iterator_next');
  pointer(nettle_sexp_iterator_subexpr):=ProcAddr('nettle_sexp_iterator_subexpr');
  pointer(nettle_sexp_transport_format):=ProcAddr('nettle_sexp_transport_format');
end;

procedure Loadlibnettle;

begin
  LoadLibNettle(NettleLibraryFileName);
end;


procedure Loadlibnettle(const lib : string);

  Function ProcAddr(aName : string) : Pointer;

  begin
    Result:=GetProcAddress(hlibnettle,aName);
    If (Result=Nil) and assigned(OnUnknownNettleSymbol) then
      OnUnknownNettleSymbol(lib,aName);
  end;

begin
  if libNettleLoaded then
    Freelibnettle;
  hlibnettle:=LoadLibrary(lib);
  if hlibnettle=0 then
    raise Exception.Create(format('Could not load library: %s',[lib]));

  // Hashes
  pointer(nettle_gosthash94):=ProcAddr('nettle_gosthash94');
  pointer(nettle_md2):=ProcAddr('nettle_md2');
  pointer(nettle_md4):=ProcAddr('nettle_md4');
  pointer(nettle_md5):=ProcAddr('nettle_md5');
  pointer(nettle_ripemd160):=ProcAddr('nettle_ripemd160');
  pointer(nettle_sha1):=ProcAddr('nettle_sha1');
  pointer(nettle_sha224):=ProcAddr('nettle_sha224');
  pointer(nettle_sha256):=ProcAddr('nettle_sha256');
  pointer(nettle_sha384):=ProcAddr('nettle_sha384');
  pointer(nettle_sha3_224):=ProcAddr('nettle_sha3_224');
  pointer(nettle_sha3_256):=ProcAddr('nettle_sha3_256');
  pointer(nettle_sha3_384):=ProcAddr('nettle_sha3_384');
  pointer(nettle_sha3_512):=ProcAddr('nettle_sha3_512');
  pointer(nettle_sha512):=ProcAddr('nettle_sha512');
  pointer(nettle_sha512_224):=ProcAddr('nettle_sha512_224');
  pointer(nettle_sha512_256):=ProcAddr('nettle_sha512_256');
  // Ciphers
  pointer(nettle_aes128):=ProcAddr('nettle_aes128');
  pointer(nettle_aes192):=ProcAddr('nettle_aes192');
  pointer(nettle_aes256):=ProcAddr('nettle_aes256');
  pointer(nettle_camellia128):=ProcAddr('nettle_camellia128');
  pointer(nettle_camellia192):=ProcAddr('nettle_camellia192');
  pointer(nettle_camellia256):=ProcAddr('nettle_camellia256');
  pointer(nettle_cast128):=ProcAddr('nettle_cast128');
  pointer(nettle_serpent128):=ProcAddr('nettle_serpent128');
  pointer(nettle_serpent192):=ProcAddr('nettle_serpent192');
  pointer(nettle_serpent256):=ProcAddr('nettle_serpent256');
  pointer(nettle_twofish128):=ProcAddr('nettle_twofish128');
  pointer(nettle_twofish192):=ProcAddr('nettle_twofish192');
  pointer(nettle_twofish256):=ProcAddr('nettle_twofish256');
  pointer(nettle_arctwo40):=ProcAddr('nettle_arctwo40');
  pointer(nettle_arctwo64):=ProcAddr('nettle_arctwo64');
  pointer(nettle_arctwo128):=ProcAddr('nettle_arctwo128');
  pointer(nettle_arctwo_gutmann128):=ProcAddr('nettle_arctwo_gutmann128');

  // AEAD
  pointer(nettle_gcm_aes128):=ProcAddr('nettle_gcm_aes128');
  pointer(nettle_gcm_aes192):=ProcAddr('nettle_gcm_aes192');
  pointer(nettle_gcm_aes256):=ProcAddr('nettle_gcm_aes256');
  pointer(nettle_gcm_camellia128):=ProcAddr('nettle_gcm_camellia128');
  pointer(nettle_gcm_camellia256):=ProcAddr('nettle_gcm_camellia256');
  pointer(nettle_eax_aes128):=ProcAddr('nettle_eax_aes128');
  pointer(nettle_chacha_poly1305):=ProcAddr('nettle_chacha_poly1305');
  // armor
  pointer(nettle_base64):=ProcAddr('nettle_base64');
  pointer(nettle_base64url):=ProcAddr('nettle_base64url');
  pointer(nettle_base16):=ProcAddr('nettle_base16');


  pointer(nettle_aes128_decrypt):=ProcAddr('nettle_aes128_decrypt');
  pointer(nettle_aes128_encrypt):=ProcAddr('nettle_aes128_encrypt');
  pointer(nettle_aes128_invert_key):=ProcAddr('nettle_aes128_invert_key');
  pointer(nettle_aes128_set_decrypt_key):=ProcAddr('nettle_aes128_set_decrypt_key');
  pointer(nettle_aes128_set_encrypt_key):=ProcAddr('nettle_aes128_set_encrypt_key');
  pointer(nettle_aes192_decrypt):=ProcAddr('nettle_aes192_decrypt');
  pointer(nettle_aes192_encrypt):=ProcAddr('nettle_aes192_encrypt');
  pointer(nettle_aes192_invert_key):=ProcAddr('nettle_aes192_invert_key');
  pointer(nettle_aes192_set_decrypt_key):=ProcAddr('nettle_aes192_set_decrypt_key');
  pointer(nettle_aes192_set_encrypt_key):=ProcAddr('nettle_aes192_set_encrypt_key');
  pointer(nettle_aes256_decrypt):=ProcAddr('nettle_aes256_decrypt');
  pointer(nettle_aes256_encrypt):=ProcAddr('nettle_aes256_encrypt');
  pointer(nettle_aes256_invert_key):=ProcAddr('nettle_aes256_invert_key');
  pointer(nettle_aes256_set_decrypt_key):=ProcAddr('nettle_aes256_set_decrypt_key');
  pointer(nettle_aes256_set_encrypt_key):=ProcAddr('nettle_aes256_set_encrypt_key');
  pointer(nettle_aes_decrypt):=ProcAddr('nettle_aes_decrypt');
  pointer(nettle_aes_encrypt):=ProcAddr('nettle_aes_encrypt');
  pointer(nettle_aes_invert_key):=ProcAddr('nettle_aes_invert_key');
  pointer(nettle_aes_set_decrypt_key):=ProcAddr('nettle_aes_set_decrypt_key');
  pointer(nettle_aes_set_encrypt_key):=ProcAddr('nettle_aes_set_encrypt_key');
  pointer(nettle_arcfour128_set_key):=ProcAddr('nettle_arcfour128_set_key');
  pointer(nettle_arcfour_crypt):=ProcAddr('nettle_arcfour_crypt');
  pointer(nettle_arcfour_set_key):=ProcAddr('nettle_arcfour_set_key');
  pointer(nettle_arctwo128_set_key):=ProcAddr('nettle_arctwo128_set_key');
  pointer(nettle_arctwo128_set_key_gutmann):=ProcAddr('nettle_arctwo128_set_key_gutmann');
  pointer(nettle_arctwo40_set_key):=ProcAddr('nettle_arctwo40_set_key');
  pointer(nettle_arctwo64_set_key):=ProcAddr('nettle_arctwo64_set_key');
  pointer(nettle_arctwo_decrypt):=ProcAddr('nettle_arctwo_decrypt');
  pointer(nettle_arctwo_encrypt):=ProcAddr('nettle_arctwo_encrypt');
  pointer(nettle_arctwo_set_key):=ProcAddr('nettle_arctwo_set_key');
  pointer(nettle_arctwo_set_key_ekb):=ProcAddr('nettle_arctwo_set_key_ekb');
  pointer(nettle_arctwo_set_key_gutmann):=ProcAddr('nettle_arctwo_set_key_gutmann');
  pointer(nettle_base16_decode_final):=ProcAddr('nettle_base16_decode_final');
  pointer(nettle_base16_decode_init):=ProcAddr('nettle_base16_decode_init');
  pointer(nettle_base16_decode_single):=ProcAddr('nettle_base16_decode_single');
  pointer(nettle_base16_decode_update):=ProcAddr('nettle_base16_decode_update');
  pointer(nettle_base16_encode_single):=ProcAddr('nettle_base16_encode_single');
  pointer(nettle_base16_encode_update):=ProcAddr('nettle_base16_encode_update');
  pointer(nettle_base64url_decode_init):=ProcAddr('nettle_base64url_decode_init');
  pointer(nettle_base64url_encode_init):=ProcAddr('nettle_base64url_encode_init');
  pointer(nettle_base64_decode_final):=ProcAddr('nettle_base64_decode_final');
  pointer(nettle_base64_decode_init):=ProcAddr('nettle_base64_decode_init');
  pointer(nettle_base64_decode_single):=ProcAddr('nettle_base64_decode_single');
  pointer(nettle_base64_decode_update):=ProcAddr('nettle_base64_decode_update');
  pointer(nettle_base64_encode_final):=ProcAddr('nettle_base64_encode_final');
  pointer(nettle_base64_encode_group):=ProcAddr('nettle_base64_encode_group');
  pointer(nettle_base64_encode_init):=ProcAddr('nettle_base64_encode_init');
  pointer(nettle_base64_encode_raw):=ProcAddr('nettle_base64_encode_raw');
  pointer(nettle_base64_encode_single):=ProcAddr('nettle_base64_encode_single');
  pointer(nettle_base64_encode_update):=ProcAddr('nettle_base64_encode_update');
  pointer(nettle_blowfish128_set_key):=ProcAddr('nettle_blowfish128_set_key');
  pointer(nettle_blowfish_decrypt):=ProcAddr('nettle_blowfish_decrypt');
  pointer(nettle_blowfish_encrypt):=ProcAddr('nettle_blowfish_encrypt');
  pointer(nettle_blowfish_set_key):=ProcAddr('nettle_blowfish_set_key');
  pointer(nettle_buffer_clear):=ProcAddr('nettle_buffer_clear');
  pointer(nettle_buffer_copy):=ProcAddr('nettle_buffer_copy');
  pointer(nettle_buffer_grow):=ProcAddr('nettle_buffer_grow');
  pointer(nettle_buffer_init):=ProcAddr('nettle_buffer_init');
  pointer(nettle_buffer_init_realloc):=ProcAddr('nettle_buffer_init_realloc');
  pointer(nettle_buffer_init_size):=ProcAddr('nettle_buffer_init_size');
  pointer(nettle_buffer_reset):=ProcAddr('nettle_buffer_reset');
  pointer(nettle_buffer_space):=ProcAddr('nettle_buffer_space');
  pointer(nettle_buffer_write):=ProcAddr('nettle_buffer_write');
  pointer(nettle_camellia128_crypt):=ProcAddr('nettle_camellia128_crypt');
  pointer(nettle_camellia128_invert_key):=ProcAddr('nettle_camellia128_invert_key');
  pointer(nettle_camellia128_set_encrypt_key):=ProcAddr('nettle_camellia128_set_encrypt_key');
  pointer(nettle_camellia192_set_decrypt_key):=ProcAddr('nettle_camellia192_set_decrypt_key');
  pointer(nettle_camellia192_set_encrypt_key):=ProcAddr('nettle_camellia192_set_encrypt_key');
  pointer(nettle_camellia256_crypt):=ProcAddr('nettle_camellia256_crypt');
  pointer(nettle_camellia256_invert_key):=ProcAddr('nettle_camellia256_invert_key');
  pointer(nettle_camellia256_set_decrypt_key):=ProcAddr('nettle_camellia256_set_decrypt_key');
  pointer(nettle_camellia256_set_encrypt_key):=ProcAddr('nettle_camellia256_set_encrypt_key');
  pointer(nettle_camellia_set_decrypt_key):=ProcAddr('nettle_camellia_set_decrypt_key');
  pointer(nettle_cast128_decrypt):=ProcAddr('nettle_cast128_decrypt');
  pointer(nettle_cast128_encrypt):=ProcAddr('nettle_cast128_encrypt');
  pointer(nettle_cast128_set_key):=ProcAddr('nettle_cast128_set_key');
  pointer(nettle_cast5_set_key):=ProcAddr('nettle_cast5_set_key');
  pointer(nettle_cbc_decrypt):=ProcAddr('nettle_cbc_decrypt');
  pointer(nettle_cbc_encrypt):=ProcAddr('nettle_cbc_encrypt');
  pointer(nettle_ccm_aes128_decrypt):=ProcAddr('nettle_ccm_aes128_decrypt');
  pointer(nettle_ccm_aes128_decrypt_message):=ProcAddr('nettle_ccm_aes128_decrypt_message');
  pointer(nettle_ccm_aes128_digest):=ProcAddr('nettle_ccm_aes128_digest');
  pointer(nettle_ccm_aes128_encrypt):=ProcAddr('nettle_ccm_aes128_encrypt');
  pointer(nettle_ccm_aes128_encrypt_message):=ProcAddr('nettle_ccm_aes128_encrypt_message');
  pointer(nettle_ccm_aes128_set_key):=ProcAddr('nettle_ccm_aes128_set_key');
  pointer(nettle_ccm_aes128_set_nonce):=ProcAddr('nettle_ccm_aes128_set_nonce');
  pointer(nettle_ccm_aes128_update):=ProcAddr('nettle_ccm_aes128_update');
  pointer(nettle_ccm_aes192_decrypt):=ProcAddr('nettle_ccm_aes192_decrypt');
  pointer(nettle_ccm_aes192_decrypt_message):=ProcAddr('nettle_ccm_aes192_decrypt_message');
  pointer(nettle_ccm_aes192_digest):=ProcAddr('nettle_ccm_aes192_digest');
  pointer(nettle_ccm_aes192_encrypt):=ProcAddr('nettle_ccm_aes192_encrypt');
  pointer(nettle_ccm_aes192_encrypt_message):=ProcAddr('nettle_ccm_aes192_encrypt_message');
  pointer(nettle_ccm_aes192_set_key):=ProcAddr('nettle_ccm_aes192_set_key');
  pointer(nettle_ccm_aes192_set_nonce):=ProcAddr('nettle_ccm_aes192_set_nonce');
  pointer(nettle_ccm_aes192_update):=ProcAddr('nettle_ccm_aes192_update');
  pointer(nettle_ccm_aes256_decrypt):=ProcAddr('nettle_ccm_aes256_decrypt');
  pointer(nettle_ccm_aes256_decrypt_message):=ProcAddr('nettle_ccm_aes256_decrypt_message');
  pointer(nettle_ccm_aes256_digest):=ProcAddr('nettle_ccm_aes256_digest');
  pointer(nettle_ccm_aes256_encrypt):=ProcAddr('nettle_ccm_aes256_encrypt');
  pointer(nettle_ccm_aes256_encrypt_message):=ProcAddr('nettle_ccm_aes256_encrypt_message');
  pointer(nettle_ccm_aes256_set_key):=ProcAddr('nettle_ccm_aes256_set_key');
  pointer(nettle_ccm_aes256_set_nonce):=ProcAddr('nettle_ccm_aes256_set_nonce');
  pointer(nettle_ccm_aes256_update):=ProcAddr('nettle_ccm_aes256_update');
  pointer(nettle_ccm_decrypt):=ProcAddr('nettle_ccm_decrypt');
  pointer(nettle_ccm_decrypt_message):=ProcAddr('nettle_ccm_decrypt_message');
  pointer(nettle_ccm_digest):=ProcAddr('nettle_ccm_digest');
  pointer(nettle_ccm_encrypt):=ProcAddr('nettle_ccm_encrypt');
  pointer(nettle_ccm_encrypt_message):=ProcAddr('nettle_ccm_encrypt_message');
  pointer(nettle_ccm_set_nonce):=ProcAddr('nettle_ccm_set_nonce');
  pointer(nettle_ccm_update):=ProcAddr('nettle_ccm_update');
  pointer(nettle_cfb8_decrypt):=ProcAddr('nettle_cfb8_decrypt');
  pointer(nettle_cfb8_encrypt):=ProcAddr('nettle_cfb8_encrypt');
  pointer(nettle_cfb_decrypt):=ProcAddr('nettle_cfb_decrypt');
  pointer(nettle_cfb_encrypt):=ProcAddr('nettle_cfb_encrypt');
  pointer(nettle_chacha_crypt):=ProcAddr('nettle_chacha_crypt');
  pointer(nettle_chacha_poly1305_decrypt):=ProcAddr('nettle_chacha_poly1305_decrypt');
  pointer(nettle_chacha_poly1305_digest):=ProcAddr('nettle_chacha_poly1305_digest');
  pointer(nettle_chacha_poly1305_encrypt):=ProcAddr('nettle_chacha_poly1305_encrypt');
  pointer(nettle_chacha_poly1305_set_key):=ProcAddr('nettle_chacha_poly1305_set_key');
  pointer(nettle_chacha_poly1305_set_nonce):=ProcAddr('nettle_chacha_poly1305_set_nonce');
  pointer(nettle_chacha_poly1305_update):=ProcAddr('nettle_chacha_poly1305_update');
  pointer(nettle_chacha_set_key):=ProcAddr('nettle_chacha_set_key');
  pointer(nettle_chacha_set_nonce):=ProcAddr('nettle_chacha_set_nonce');
  pointer(nettle_chacha_set_nonce96):=ProcAddr('nettle_chacha_set_nonce96');
  pointer(nettle_cmac128_digest):=ProcAddr('nettle_cmac128_digest');
  pointer(nettle_cmac128_init):=ProcAddr('nettle_cmac128_init');
  pointer(nettle_cmac128_set_key):=ProcAddr('nettle_cmac128_set_key');
  pointer(nettle_cmac128_update):=ProcAddr('nettle_cmac128_update');
  pointer(nettle_cmac_aes128_digest):=ProcAddr('nettle_cmac_aes128_digest');
  pointer(nettle_cmac_aes128_set_key):=ProcAddr('nettle_cmac_aes128_set_key');
  pointer(nettle_cmac_aes128_update):=ProcAddr('nettle_cmac_aes128_update');
  pointer(nettle_cmac_aes256_digest):=ProcAddr('nettle_cmac_aes256_digest');
  pointer(nettle_cmac_aes256_set_key):=ProcAddr('nettle_cmac_aes256_set_key');
  pointer(nettle_cmac_aes256_update):=ProcAddr('nettle_cmac_aes256_update');
  pointer(nettle_cnd_memcpy):=ProcAddr('nettle_cnd_memcpy');
  pointer(nettle_ctr_crypt):=ProcAddr('nettle_ctr_crypt');
  pointer(nettle_des3_decrypt):=ProcAddr('nettle_des3_decrypt');
  pointer(nettle_des3_encrypt):=ProcAddr('nettle_des3_encrypt');
  pointer(nettle_des3_set_key):=ProcAddr('nettle_des3_set_key');
  pointer(nettle_des_check_parity):=ProcAddr('nettle_des_check_parity');
  pointer(nettle_des_decrypt):=ProcAddr('nettle_des_decrypt');
  pointer(nettle_des_encrypt):=ProcAddr('nettle_des_encrypt');
  pointer(nettle_des_fix_parity):=ProcAddr('nettle_des_fix_parity');
  pointer(nettle_des_set_key):=ProcAddr('nettle_des_set_key');
  pointer(nettle_eax_aes128_decrypt):=ProcAddr('nettle_eax_aes128_decrypt');
  pointer(nettle_eax_aes128_digest):=ProcAddr('nettle_eax_aes128_digest');
  pointer(nettle_eax_aes128_encrypt):=ProcAddr('nettle_eax_aes128_encrypt');
  pointer(nettle_eax_aes128_set_key):=ProcAddr('nettle_eax_aes128_set_key');
  pointer(nettle_eax_aes128_set_nonce):=ProcAddr('nettle_eax_aes128_set_nonce');
  pointer(nettle_eax_aes128_update):=ProcAddr('nettle_eax_aes128_update');
  pointer(nettle_eax_decrypt):=ProcAddr('nettle_eax_decrypt');
  pointer(nettle_eax_digest):=ProcAddr('nettle_eax_digest');
  pointer(nettle_eax_encrypt):=ProcAddr('nettle_eax_encrypt');
  pointer(nettle_eax_set_key):=ProcAddr('nettle_eax_set_key');
  pointer(nettle_eax_set_nonce):=ProcAddr('nettle_eax_set_nonce');
  pointer(nettle_eax_update):=ProcAddr('nettle_eax_update');
  pointer(nettle_gcm_aes128_decrypt):=ProcAddr('nettle_gcm_aes128_decrypt');
  pointer(nettle_gcm_aes128_digest):=ProcAddr('nettle_gcm_aes128_digest');
  pointer(nettle_gcm_aes128_encrypt):=ProcAddr('nettle_gcm_aes128_encrypt');
  pointer(nettle_gcm_aes128_set_iv):=ProcAddr('nettle_gcm_aes128_set_iv');
  pointer(nettle_gcm_aes128_set_key):=ProcAddr('nettle_gcm_aes128_set_key');
  pointer(nettle_gcm_aes128_update):=ProcAddr('nettle_gcm_aes128_update');
  pointer(nettle_gcm_aes192_decrypt):=ProcAddr('nettle_gcm_aes192_decrypt');
  pointer(nettle_gcm_aes192_digest):=ProcAddr('nettle_gcm_aes192_digest');
  pointer(nettle_gcm_aes192_encrypt):=ProcAddr('nettle_gcm_aes192_encrypt');
  pointer(nettle_gcm_aes192_set_iv):=ProcAddr('nettle_gcm_aes192_set_iv');
  pointer(nettle_gcm_aes192_set_key):=ProcAddr('nettle_gcm_aes192_set_key');
  pointer(nettle_gcm_aes192_update):=ProcAddr('nettle_gcm_aes192_update');
  pointer(nettle_gcm_aes256_decrypt):=ProcAddr('nettle_gcm_aes256_decrypt');
  pointer(nettle_gcm_aes256_digest):=ProcAddr('nettle_gcm_aes256_digest');
  pointer(nettle_gcm_aes256_encrypt):=ProcAddr('nettle_gcm_aes256_encrypt');
  pointer(nettle_gcm_aes256_set_iv):=ProcAddr('nettle_gcm_aes256_set_iv');
  pointer(nettle_gcm_aes256_set_key):=ProcAddr('nettle_gcm_aes256_set_key');
  pointer(nettle_gcm_aes256_update):=ProcAddr('nettle_gcm_aes256_update');
  pointer(nettle_gcm_aes_decrypt):=ProcAddr('nettle_gcm_aes_decrypt');
  pointer(nettle_gcm_aes_digest):=ProcAddr('nettle_gcm_aes_digest');
  pointer(nettle_gcm_aes_encrypt):=ProcAddr('nettle_gcm_aes_encrypt');
  pointer(nettle_gcm_aes_set_iv):=ProcAddr('nettle_gcm_aes_set_iv');
  pointer(nettle_gcm_aes_set_key):=ProcAddr('nettle_gcm_aes_set_key');
  pointer(nettle_gcm_aes_update):=ProcAddr('nettle_gcm_aes_update');
  pointer(nettle_gcm_camellia128_decrypt):=ProcAddr('nettle_gcm_camellia128_decrypt');
  pointer(nettle_gcm_camellia128_digest):=ProcAddr('nettle_gcm_camellia128_digest');
  pointer(nettle_gcm_camellia128_encrypt):=ProcAddr('nettle_gcm_camellia128_encrypt');
  pointer(nettle_gcm_camellia128_set_iv):=ProcAddr('nettle_gcm_camellia128_set_iv');
  pointer(nettle_gcm_camellia128_set_key):=ProcAddr('nettle_gcm_camellia128_set_key');
  pointer(nettle_gcm_camellia128_update):=ProcAddr('nettle_gcm_camellia128_update');
  pointer(nettle_gcm_camellia256_decrypt):=ProcAddr('nettle_gcm_camellia256_decrypt');
  pointer(nettle_gcm_camellia256_digest):=ProcAddr('nettle_gcm_camellia256_digest');
  pointer(nettle_gcm_camellia256_encrypt):=ProcAddr('nettle_gcm_camellia256_encrypt');
  pointer(nettle_gcm_camellia256_set_iv):=ProcAddr('nettle_gcm_camellia256_set_iv');
  pointer(nettle_gcm_camellia256_set_key):=ProcAddr('nettle_gcm_camellia256_set_key');
  pointer(nettle_gcm_camellia256_update):=ProcAddr('nettle_gcm_camellia256_update');
  pointer(nettle_gcm_decrypt):=ProcAddr('nettle_gcm_decrypt');
  pointer(nettle_gcm_digest):=ProcAddr('nettle_gcm_digest');
  pointer(nettle_gcm_encrypt):=ProcAddr('nettle_gcm_encrypt');
  pointer(nettle_gcm_set_iv):=ProcAddr('nettle_gcm_set_iv');
  pointer(nettle_gcm_set_key):=ProcAddr('nettle_gcm_set_key');
  pointer(nettle_gcm_update):=ProcAddr('nettle_gcm_update');
  pointer(nettle_get_aeads):=ProcAddr('nettle_get_aeads');
  pointer(nettle_get_armors):=ProcAddr('nettle_get_armors');
  pointer(nettle_get_ciphers):=ProcAddr('nettle_get_ciphers');
  pointer(nettle_get_hashes):=ProcAddr('nettle_get_hashes');
  pointer(nettle_gosthash94_digest):=ProcAddr('nettle_gosthash94_digest');
  pointer(nettle_gosthash94_init):=ProcAddr('nettle_gosthash94_init');
  pointer(nettle_gosthash94_update):=ProcAddr('nettle_gosthash94_update');
  pointer(nettle_hkdf_expand):=ProcAddr('nettle_hkdf_expand');
  pointer(nettle_hkdf_extract):=ProcAddr('nettle_hkdf_extract');
  pointer(nettle_hmac_digest):=ProcAddr('nettle_hmac_digest');
  pointer(nettle_hmac_md5_digest):=ProcAddr('nettle_hmac_md5_digest');
  pointer(nettle_hmac_md5_set_key):=ProcAddr('nettle_hmac_md5_set_key');
  pointer(nettle_hmac_md5_update):=ProcAddr('nettle_hmac_md5_update');
  pointer(nettle_hmac_ripemd160_digest):=ProcAddr('nettle_hmac_ripemd160_digest');
  pointer(nettle_hmac_ripemd160_set_key):=ProcAddr('nettle_hmac_ripemd160_set_key');
  pointer(nettle_hmac_ripemd160_update):=ProcAddr('nettle_hmac_ripemd160_update');
  pointer(nettle_hmac_set_key):=ProcAddr('nettle_hmac_set_key');
  pointer(nettle_hmac_sha1_digest):=ProcAddr('nettle_hmac_sha1_digest');
  pointer(nettle_hmac_sha1_set_key):=ProcAddr('nettle_hmac_sha1_set_key');
  pointer(nettle_hmac_sha1_update):=ProcAddr('nettle_hmac_sha1_update');
  pointer(nettle_hmac_sha224_digest):=ProcAddr('nettle_hmac_sha224_digest');
  pointer(nettle_hmac_sha224_set_key):=ProcAddr('nettle_hmac_sha224_set_key');
  pointer(nettle_hmac_sha256_digest):=ProcAddr('nettle_hmac_sha256_digest');
  pointer(nettle_hmac_sha256_set_key):=ProcAddr('nettle_hmac_sha256_set_key');
  pointer(nettle_hmac_sha256_update):=ProcAddr('nettle_hmac_sha256_update');
  pointer(nettle_hmac_sha384_digest):=ProcAddr('nettle_hmac_sha384_digest');
  pointer(nettle_hmac_sha384_set_key):=ProcAddr('nettle_hmac_sha384_set_key');
  pointer(nettle_hmac_sha512_digest):=ProcAddr('nettle_hmac_sha512_digest');
  pointer(nettle_hmac_sha512_set_key):=ProcAddr('nettle_hmac_sha512_set_key');
  pointer(nettle_hmac_sha512_update):=ProcAddr('nettle_hmac_sha512_update');
  pointer(nettle_hmac_update):=ProcAddr('nettle_hmac_update');
  pointer(nettle_knuth_lfib_get):=ProcAddr('nettle_knuth_lfib_get');
  pointer(nettle_knuth_lfib_get_array):=ProcAddr('nettle_knuth_lfib_get_array');
  pointer(nettle_knuth_lfib_init):=ProcAddr('nettle_knuth_lfib_init');
  pointer(nettle_knuth_lfib_random):=ProcAddr('nettle_knuth_lfib_random');
  pointer(nettle_lookup_hash):=ProcAddr('nettle_lookup_hash');
  pointer(nettle_md2_digest):=ProcAddr('nettle_md2_digest');
  pointer(nettle_md2_init):=ProcAddr('nettle_md2_init');
  pointer(nettle_md2_update):=ProcAddr('nettle_md2_update');
  pointer(nettle_md4_digest):=ProcAddr('nettle_md4_digest');
  pointer(nettle_md4_init):=ProcAddr('nettle_md4_init');
  pointer(nettle_md4_update):=ProcAddr('nettle_md4_update');
  pointer(nettle_MD5Final):=ProcAddr('nettle_MD5Final');
  pointer(nettle_MD5Init):=ProcAddr('nettle_MD5Init');
  pointer(nettle_MD5Update):=ProcAddr('nettle_MD5Update');
  pointer(nettle_md5_compress):=ProcAddr('nettle_md5_compress');
  pointer(nettle_md5_digest):=ProcAddr('nettle_md5_digest');
  pointer(nettle_md5_init):=ProcAddr('nettle_md5_init');
  pointer(nettle_md5_update):=ProcAddr('nettle_md5_update');
  pointer(nettle_memeql_sec):=ProcAddr('nettle_memeql_sec');
  pointer(nettle_memxor):=ProcAddr('nettle_memxor');
  pointer(nettle_memxor3):=ProcAddr('nettle_memxor3');
  pointer(nettle_pbkdf2):=ProcAddr('nettle_pbkdf2');
  pointer(nettle_pbkdf2_hmac_sha1):=ProcAddr('nettle_pbkdf2_hmac_sha1');
  pointer(nettle_pbkdf2_hmac_sha256):=ProcAddr('nettle_pbkdf2_hmac_sha256');
  pointer(nettle_poly1305_aes_digest):=ProcAddr('nettle_poly1305_aes_digest');
  pointer(nettle_poly1305_aes_set_key):=ProcAddr('nettle_poly1305_aes_set_key');
  pointer(nettle_poly1305_aes_set_nonce):=ProcAddr('nettle_poly1305_aes_set_nonce');
  pointer(nettle_poly1305_aes_update):=ProcAddr('nettle_poly1305_aes_update');
  pointer(nettle_poly1305_digest):=ProcAddr('nettle_poly1305_digest');
  pointer(nettle_poly1305_set_key):=ProcAddr('nettle_poly1305_set_key');
  pointer(nettle_ripemd160_digest):=ProcAddr('nettle_ripemd160_digest');
  pointer(nettle_ripemd160_init):=ProcAddr('nettle_ripemd160_init');
  pointer(nettle_ripemd160_update):=ProcAddr('nettle_ripemd160_update');
  pointer(nettle_salsa20r12_crypt):=ProcAddr('nettle_salsa20r12_crypt');
  pointer(nettle_salsa20_128_set_key):=ProcAddr('nettle_salsa20_128_set_key');
  pointer(nettle_salsa20_256_set_key):=ProcAddr('nettle_salsa20_256_set_key');
  pointer(nettle_salsa20_crypt):=ProcAddr('nettle_salsa20_crypt');
  pointer(nettle_salsa20_set_key):=ProcAddr('nettle_salsa20_set_key');
  pointer(nettle_salsa20_set_nonce):=ProcAddr('nettle_salsa20_set_nonce');
  pointer(nettle_serpent128_set_key):=ProcAddr('nettle_serpent128_set_key');
  pointer(nettle_serpent192_set_key):=ProcAddr('nettle_serpent192_set_key');
  pointer(nettle_serpent256_set_key):=ProcAddr('nettle_serpent256_set_key');
  pointer(nettle_serpent_decrypt):=ProcAddr('nettle_serpent_decrypt');
  pointer(nettle_serpent_encrypt):=ProcAddr('nettle_serpent_encrypt');
  pointer(nettle_serpent_set_key):=ProcAddr('nettle_serpent_set_key');
  pointer(nettle_sha1_compress):=ProcAddr('nettle_sha1_compress');
  pointer(nettle_sha1_digest):=ProcAddr('nettle_sha1_digest');
  pointer(nettle_sha1_init):=ProcAddr('nettle_sha1_init');
  pointer(nettle_sha1_update):=ProcAddr('nettle_sha1_update');
  pointer(nettle_sha224_digest):=ProcAddr('nettle_sha224_digest');
  pointer(nettle_sha224_init):=ProcAddr('nettle_sha224_init');
  pointer(nettle_sha256_digest):=ProcAddr('nettle_sha256_digest');
  pointer(nettle_sha256_init):=ProcAddr('nettle_sha256_init');
  pointer(nettle_sha256_update):=ProcAddr('nettle_sha256_update');
  pointer(nettle_sha384_digest):=ProcAddr('nettle_sha384_digest');
  pointer(nettle_sha384_init):=ProcAddr('nettle_sha384_init');
  pointer(nettle_sha3_224_digest):=ProcAddr('nettle_sha3_224_digest');
  pointer(nettle_sha3_224_init):=ProcAddr('nettle_sha3_224_init');
  pointer(nettle_sha3_224_update):=ProcAddr('nettle_sha3_224_update');
  pointer(nettle_sha3_256_digest):=ProcAddr('nettle_sha3_256_digest');
  pointer(nettle_sha3_256_init):=ProcAddr('nettle_sha3_256_init');
  pointer(nettle_sha3_256_update):=ProcAddr('nettle_sha3_256_update');
  pointer(nettle_sha3_384_digest):=ProcAddr('nettle_sha3_384_digest');
  pointer(nettle_sha3_384_init):=ProcAddr('nettle_sha3_384_init');
  pointer(nettle_sha3_384_update):=ProcAddr('nettle_sha3_384_update');
  pointer(nettle_sha3_512_digest):=ProcAddr('nettle_sha3_512_digest');
  pointer(nettle_sha3_512_init):=ProcAddr('nettle_sha3_512_init');
  pointer(nettle_sha3_512_update):=ProcAddr('nettle_sha3_512_update');
  pointer(nettle_sha3_permute):=ProcAddr('nettle_sha3_permute');
  pointer(nettle_sha512_224_digest):=ProcAddr('nettle_sha512_224_digest');
  pointer(nettle_sha512_224_init):=ProcAddr('nettle_sha512_224_init');
  pointer(nettle_sha512_256_digest):=ProcAddr('nettle_sha512_256_digest');
  pointer(nettle_sha512_256_init):=ProcAddr('nettle_sha512_256_init');
  pointer(nettle_sha512_digest):=ProcAddr('nettle_sha512_digest');
  pointer(nettle_sha512_init):=ProcAddr('nettle_sha512_init');
  pointer(nettle_sha512_update):=ProcAddr('nettle_sha512_update');
  pointer(nettle_twofish128_set_key):=ProcAddr('nettle_twofish128_set_key');
  pointer(nettle_twofish192_set_key):=ProcAddr('nettle_twofish192_set_key');
  pointer(nettle_twofish256_set_key):=ProcAddr('nettle_twofish256_set_key');
  pointer(nettle_twofish_decrypt):=ProcAddr('nettle_twofish_decrypt');
  pointer(nettle_twofish_encrypt):=ProcAddr('nettle_twofish_encrypt');
  pointer(nettle_twofish_set_key):=ProcAddr('nettle_twofish_set_key');
  pointer(nettle_umac128_digest):=ProcAddr('nettle_umac128_digest');
  pointer(nettle_umac128_set_key):=ProcAddr('nettle_umac128_set_key');
  pointer(nettle_umac128_set_nonce):=ProcAddr('nettle_umac128_set_nonce');
  pointer(nettle_umac128_update):=ProcAddr('nettle_umac128_update');
  pointer(nettle_umac32_digest):=ProcAddr('nettle_umac32_digest');
  pointer(nettle_umac32_set_key):=ProcAddr('nettle_umac32_set_key');
  pointer(nettle_umac32_set_nonce):=ProcAddr('nettle_umac32_set_nonce');
  pointer(nettle_umac32_update):=ProcAddr('nettle_umac32_update');
  pointer(nettle_umac64_digest):=ProcAddr('nettle_umac64_digest');
  pointer(nettle_umac64_set_key):=ProcAddr('nettle_umac64_set_key');
  pointer(nettle_umac64_set_nonce):=ProcAddr('nettle_umac64_set_nonce');
  pointer(nettle_umac64_update):=ProcAddr('nettle_umac64_update');
  pointer(nettle_umac96_digest):=ProcAddr('nettle_umac96_digest');
  pointer(nettle_umac96_set_key):=ProcAddr('nettle_umac96_set_key');
  pointer(nettle_umac96_set_nonce):=ProcAddr('nettle_umac96_set_nonce');
  pointer(nettle_umac96_update):=ProcAddr('nettle_umac96_update');
  pointer(nettle_version_major):=ProcAddr('nettle_version_major');
  pointer(nettle_version_minor):=ProcAddr('nettle_version_minor');
  pointer(nettle_xts_aes128_decrypt_message):=ProcAddr('nettle_xts_aes128_decrypt_message');
  pointer(nettle_xts_aes128_encrypt_message):=ProcAddr('nettle_xts_aes128_encrypt_message');
  pointer(nettle_xts_aes128_set_decrypt_key):=ProcAddr('nettle_xts_aes128_set_decrypt_key');
  pointer(nettle_xts_aes128_set_encrypt_key):=ProcAddr('nettle_xts_aes128_set_encrypt_key');
  pointer(nettle_xts_aes256_decrypt_message):=ProcAddr('nettle_xts_aes256_decrypt_message');
  pointer(nettle_xts_aes256_encrypt_message):=ProcAddr('nettle_xts_aes256_encrypt_message');
  pointer(nettle_xts_aes256_set_decrypt_key):=ProcAddr('nettle_xts_aes256_set_decrypt_key');
  pointer(nettle_xts_aes256_set_encrypt_key):=ProcAddr('nettle_xts_aes256_set_encrypt_key');
  pointer(nettle_xts_decrypt_message):=ProcAddr('nettle_xts_decrypt_message');
  pointer(nettle_xts_encrypt_message):=ProcAddr('nettle_xts_encrypt_message');
  pointer(nettle_yarrow256_fast_reseed):=ProcAddr('nettle_yarrow256_fast_reseed');
  pointer(nettle_yarrow256_init):=ProcAddr('nettle_yarrow256_init');
  pointer(nettle_yarrow256_is_seeded):=ProcAddr('nettle_yarrow256_is_seeded');
  pointer(nettle_yarrow256_needed_sources):=ProcAddr('nettle_yarrow256_needed_sources');
  pointer(nettle_yarrow256_random):=ProcAddr('nettle_yarrow256_random');
  pointer(nettle_yarrow256_seed):=ProcAddr('nettle_yarrow256_seed');
  pointer(nettle_yarrow256_slow_reseed):=ProcAddr('nettle_yarrow256_slow_reseed');
  pointer(nettle_yarrow256_update):=ProcAddr('nettle_yarrow256_update');
  pointer(nettle_yarrow_key_event_estimate):=ProcAddr('nettle_yarrow_key_event_estimate');
  pointer(nettle_yarrow_key_event_init):=ProcAddr('nettle_yarrow_key_event_init');
  pointer(_nettle_poly1305_block):=ProcAddr('_nettle_poly1305_block');
end;

end.
