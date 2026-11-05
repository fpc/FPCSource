{ Tests the bcrypt unit: record layouts against bcrypt.h and calls into bcrypt.dll with known answers. }
program tbcrypt;

{$mode objfpc}{$H+}

uses
  SysUtils, Windows, BCrypt;

var
  Errors: Integer = 0;

// Report a failure when aOK is False
procedure Check(aOK: Boolean; const aMsg: string);

begin
  if aOK then
    Writeln('OK:   ', aMsg)
  else
    begin
    Writeln('FAIL: ', aMsg);
    Inc(Errors);
    end;
end;


// Report a failure when aActual differs from aExpected, quietly otherwise
procedure CheckLayout(const aMsg: string; aActual, aExpected: PtrUInt);

begin
  if aActual<>aExpected then
    begin
    Writeln('FAIL: ', aMsg, ' is ', aActual, ', bcrypt.h has ', aExpected);
    Inc(Errors);
    end;
end;


// Report a failure when aStatus is not STATUS_SUCCESS; returns True on success
function CheckStatus(aStatus: NTSTATUS; const aMsg: string): Boolean;

begin
  Result:=aStatus=STATUS_SUCCESS;
  if not Result then
    begin
    Writeln('FAIL: ', aMsg, ' returned $', IntToHex(LongWord(aStatus), 8));
    Inc(Errors);
    end;
end;


// Return True when the program runs under wine
function RunningOnWine: Boolean;

begin
  Result:=GetProcAddress(GetModuleHandle('ntdll.dll'), 'wine_get_version')<>Nil;
end;


// Return the bytes of aHex
function HexBytes(const aHex: string): TBytes;

var
  I: Integer;

begin
  Result:=Nil;
  SetLength(Result, Length(aHex) div 2);
  for I:=0 to Length(Result)-1 do
    Result[I]:=StrToInt('$'+Copy(aHex, 2*I+1, 2));
end;


// Return aBytes as lowercase hexadecimal
function BytesHex(const aBytes: TBytes): string;

var
  I: Integer;

begin
  Result:='';
  for I:=0 to Length(aBytes)-1 do
    Result:=Result+LowerCase(IntToHex(aBytes[I], 2));
end;


// Return the bytes of string aText
function TextBytes(const aText: AnsiString): TBytes;

begin
  Result:=Nil;
  SetLength(Result, Length(aText));
  if Length(aText)>0 then
    Move(aText[1], Result[0], Length(aText));
end;


// Return the ULONG property aName of aObject
function GetULongProperty(aObject: BCRYPT_HANDLE; aName: LPCWSTR): ULONG;

var
  lSize: ULONG;

begin
  Result:=0;
  lSize:=0;
  CheckStatus(BCryptGetProperty(aObject, aName, @Result, SizeOf(Result), @lSize, 0), 'BCryptGetProperty');
end;


// Set the chaining mode of AES provider aAlg to aMode
function SetChainingMode(aAlg: BCRYPT_ALG_HANDLE; aMode: PWideChar): Boolean;

begin
  Result:=CheckStatus(BCryptSetProperty(aAlg, BCRYPT_CHAINING_MODE, PUCHAR(aMode),
                                        (StrLen(aMode)+1)*SizeOf(WideChar), 0), 'BCryptSetProperty(ChainingMode)');
end;


// Return the hash of aData with algorithm aAlgId, keyed with aKey if it is not empty
function Hash(aAlgId: PWideChar; const aKey, aData: TBytes): TBytes;

var
  lAlg: BCRYPT_ALG_HANDLE;
  lHash: BCRYPT_HASH_HANDLE;
  lFlags: ULONG;
  lObject: TBytes;

begin
  Result:=Nil;
  lFlags:=0;
  if Length(aKey)>0 then
    lFlags:=BCRYPT_ALG_HANDLE_HMAC_FLAG;
  lAlg:=Nil;
  if not CheckStatus(BCryptOpenAlgorithmProvider(@lAlg, aAlgId, Nil, lFlags), 'BCryptOpenAlgorithmProvider') then
    exit;
  try
    lObject:=Nil;
    SetLength(lObject, GetULongProperty(lAlg, BCRYPT_OBJECT_LENGTH));
    SetLength(Result, GetULongProperty(lAlg, BCRYPT_HASH_LENGTH));
    lHash:=Nil;
    if not CheckStatus(BCryptCreateHash(lAlg, @lHash, PUCHAR(lObject), Length(lObject),
                                        PUCHAR(aKey), Length(aKey), 0), 'BCryptCreateHash') then
      exit(Nil);
    try
      CheckStatus(BCryptHashData(lHash, PUCHAR(aData), Length(aData), 0), 'BCryptHashData');
      CheckStatus(BCryptFinishHash(lHash, PUCHAR(Result), Length(Result), 0), 'BCryptFinishHash');
    finally
      BCryptDestroyHash(lHash);
    end;
  finally
    BCryptCloseAlgorithmProvider(lAlg, 0);
  end;
end;


// Check the record layouts against bcrypt.h
procedure TestLayout;

var
  lErrors: Integer;

begin
  lErrors:=Errors;
  {$i tbcryptlayout.inc}
  Check(Errors=lErrors, 'record sizes and field offsets match bcrypt.h');
end;


// Check random number generation
procedure TestRandom;

var
  lAlg: BCRYPT_ALG_HANDLE;
  lA, lB: array[0..31] of Byte;

begin
  lAlg:=Nil;
  if not CheckStatus(BCryptOpenAlgorithmProvider(@lAlg, BCRYPT_RNG_ALGORITHM, Nil, 0), 'open RNG provider') then
    exit;
  try
    FillChar(lA, SizeOf(lA), 0);
    FillChar(lB, SizeOf(lB), 0);
    CheckStatus(BCryptGenRandom(lAlg, @lA[0], SizeOf(lA), 0), 'BCryptGenRandom');
    CheckStatus(BCryptGenRandom(lAlg, @lB[0], SizeOf(lB), 0), 'BCryptGenRandom');
    Check(not CompareMem(@lA[0], @lB[0], SizeOf(lA)), 'two random buffers differ');
  finally
    BCryptCloseAlgorithmProvider(lAlg, 0);
  end;
  FillChar(lA, SizeOf(lA), 0);
  Check(BCRYPT_SUCCESS(BCryptGenRandom(Nil, @lA[0], SizeOf(lA), BCRYPT_USE_SYSTEM_PREFERRED_RNG)),
        'BCryptGenRandom with the system preferred RNG');
  Check(not CompareMem(@lA[0], @lB[0], SizeOf(lA)), 'system preferred RNG gives other bytes');
end;


// Check SHA-256 and HMAC-SHA256 with the FIPS 180-2 and RFC 4231 vectors
procedure TestHash;

begin
  Check(BytesHex(Hash(BCRYPT_SHA256_ALGORITHM, Nil, TextBytes('abc')))
        ='ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad', 'SHA-256 of abc');
  Check(BytesHex(Hash(BCRYPT_SHA256_ALGORITHM, TextBytes('Jefe'), TextBytes('what do ya want for nothing?')))
        ='5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843', 'HMAC-SHA256 RFC 4231 test case 2');
end;


// Check AES-128-CBC with the NIST SP 800-38A F.2.1 vector
procedure TestAESCBC;

var
  lAlg: BCRYPT_ALG_HANDLE;
  lKey: BCRYPT_KEY_HANDLE;
  lKeyObject, lSecret, lIV, lPlain, lCipher, lBack: TBytes;
  lSize: ULONG;

begin
  lAlg:=Nil;
  if not CheckStatus(BCryptOpenAlgorithmProvider(@lAlg, BCRYPT_AES_ALGORITHM, Nil, 0), 'open AES provider') then
    exit;
  try
    if not SetChainingMode(lAlg, BCRYPT_CHAIN_MODE_CBC) then
      exit;
    Check(GetULongProperty(lAlg, BCRYPT_BLOCK_LENGTH)=16, 'AES block length is 16');
    lKeyObject:=Nil;
    SetLength(lKeyObject, GetULongProperty(lAlg, BCRYPT_OBJECT_LENGTH));
    lSecret:=HexBytes('2b7e151628aed2a6abf7158809cf4f3c');
    lKey:=Nil;
    if not CheckStatus(BCryptGenerateSymmetricKey(lAlg, @lKey, PUCHAR(lKeyObject), Length(lKeyObject),
                                                  PUCHAR(lSecret), Length(lSecret), 0), 'BCryptGenerateSymmetricKey') then
      exit;
    try
      lPlain:=HexBytes('6bc1bee22e409f96e93d7e117393172a');
      lIV:=HexBytes('000102030405060708090a0b0c0d0e0f');
      lCipher:=Nil;
      SetLength(lCipher, 16);
      lSize:=0;
      CheckStatus(BCryptEncrypt(lKey, PUCHAR(lPlain), Length(lPlain), Nil, PUCHAR(lIV), Length(lIV),
                                PUCHAR(lCipher), Length(lCipher), @lSize, 0), 'BCryptEncrypt CBC');
      Check(BytesHex(lCipher)='7649abac8119b246cee98e9b12e9197d', 'AES-128-CBC ciphertext');
      lIV:=HexBytes('000102030405060708090a0b0c0d0e0f');
      lBack:=Nil;
      SetLength(lBack, 16);
      CheckStatus(BCryptDecrypt(lKey, PUCHAR(lCipher), Length(lCipher), Nil, PUCHAR(lIV), Length(lIV),
                                PUCHAR(lBack), Length(lBack), @lSize, 0), 'BCryptDecrypt CBC');
      Check(BytesHex(lBack)=BytesHex(lPlain), 'AES-128-CBC decrypts to the plaintext');
    finally
      BCryptDestroyKey(lKey);
    end;
  finally
    BCryptCloseAlgorithmProvider(lAlg, 0);
  end;
end;


// Check AES-128-GCM with test case 2 of the GCM specification
procedure TestAESGCM;

var
  lAlg: BCRYPT_ALG_HANDLE;
  lKey: BCRYPT_KEY_HANDLE;
  lInfo: BCRYPT_AUTHENTICATED_CIPHER_MODE_INFO;
  lSecret, lNonce, lTag, lPlain, lCipher, lBack: TBytes;
  lSize: ULONG;
  lStatus: NTSTATUS;

begin
  lAlg:=Nil;
  if not CheckStatus(BCryptOpenAlgorithmProvider(@lAlg, BCRYPT_AES_ALGORITHM, Nil, 0), 'open AES provider') then
    exit;
  try
    if not SetChainingMode(lAlg, BCRYPT_CHAIN_MODE_GCM) then
      exit;
    lSecret:=HexBytes('00000000000000000000000000000000');
    lKey:=Nil;
    if not CheckStatus(BCryptGenerateSymmetricKey(lAlg, @lKey, Nil, 0, PUCHAR(lSecret), Length(lSecret), 0),
                       'BCryptGenerateSymmetricKey GCM') then
      exit;
    try
      lNonce:=HexBytes('000000000000000000000000');
      lPlain:=HexBytes('00000000000000000000000000000000');
      lTag:=Nil;
      SetLength(lTag, 16);
      lCipher:=Nil;
      SetLength(lCipher, 16);
      BCRYPT_INIT_AUTH_MODE_INFO(lInfo);
      Check(lInfo.cbSize=SizeOf(BCRYPT_AUTHENTICATED_CIPHER_MODE_INFO), 'BCRYPT_INIT_AUTH_MODE_INFO sets cbSize');
      lInfo.pbNonce:=PUCHAR(lNonce);
      lInfo.cbNonce:=Length(lNonce);
      lInfo.pbTag:=PUCHAR(lTag);
      lInfo.cbTag:=Length(lTag);
      lSize:=0;
      CheckStatus(BCryptEncrypt(lKey, PUCHAR(lPlain), Length(lPlain), @lInfo, Nil, 0,
                                PUCHAR(lCipher), Length(lCipher), @lSize, 0), 'BCryptEncrypt GCM');
      Check(BytesHex(lCipher)='0388dace60b6a392f328c2b971b2fe78', 'AES-128-GCM ciphertext');
      Check(BytesHex(lTag)='ab6e47d42cec13bdf53a67b21257bddf', 'AES-128-GCM tag');
      lTag[0]:=lTag[0] xor 1;
      lBack:=Nil;
      SetLength(lBack, 16);
      lStatus:=BCryptDecrypt(lKey, PUCHAR(lCipher), Length(lCipher), @lInfo, Nil, 0,
                             PUCHAR(lBack), Length(lBack), @lSize, 0);
      Check(lStatus=STATUS_AUTH_TAG_MISMATCH, 'changed GCM tag gives STATUS_AUTH_TAG_MISMATCH');
    finally
      BCryptDestroyKey(lKey);
    end;
  finally
    BCryptCloseAlgorithmProvider(lAlg, 0);
  end;
end;


// Check PBKDF2-HMAC-SHA256 with password, salt and 1 iteration
procedure TestPBKDF2;

var
  lAlg: BCRYPT_ALG_HANDLE;
  lPassword, lSalt, lKey: TBytes;

begin
  lAlg:=Nil;
  if not CheckStatus(BCryptOpenAlgorithmProvider(@lAlg, BCRYPT_SHA256_ALGORITHM, Nil, BCRYPT_ALG_HANDLE_HMAC_FLAG),
                     'open HMAC-SHA256 provider') then
    exit;
  try
    lPassword:=TextBytes('password');
    lSalt:=TextBytes('salt');
    lKey:=Nil;
    SetLength(lKey, 32);
    CheckStatus(BCryptDeriveKeyPBKDF2(lAlg, PUCHAR(lPassword), Length(lPassword), PUCHAR(lSalt), Length(lSalt), 1,
                                      PUCHAR(lKey), Length(lKey), 0), 'BCryptDeriveKeyPBKDF2');
    Check(BytesHex(lKey)='120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b', 'PBKDF2-HMAC-SHA256 key');
  finally
    BCryptCloseAlgorithmProvider(lAlg, 0);
  end;
end;


// Check ECDSA P-256 key generation, export, signing and verification
procedure TestECDSA;

var
  lAlg: BCRYPT_ALG_HANDLE;
  lKey: BCRYPT_KEY_HANDLE;
  lHash, lSig, lBlob: TBytes;
  lSize: ULONG;

begin
  lAlg:=Nil;
  if not CheckStatus(BCryptOpenAlgorithmProvider(@lAlg, BCRYPT_ECDSA_P256_ALGORITHM, Nil, 0), 'open ECDSA provider') then
    exit;
  try
    lKey:=Nil;
    if not CheckStatus(BCryptGenerateKeyPair(lAlg, @lKey, 256, 0), 'BCryptGenerateKeyPair') then
      exit;
    try
      CheckStatus(BCryptFinalizeKeyPair(lKey, 0), 'BCryptFinalizeKeyPair');
      lSize:=0;
      CheckStatus(BCryptExportKey(lKey, Nil, BCRYPT_ECCPUBLIC_BLOB, Nil, 0, @lSize, 0), 'BCryptExportKey size');
      Check(lSize=SizeOf(BCRYPT_ECCKEY_BLOB)+64, 'ECC public blob size');
      lBlob:=Nil;
      SetLength(lBlob, lSize);
      CheckStatus(BCryptExportKey(lKey, Nil, BCRYPT_ECCPUBLIC_BLOB, PUCHAR(lBlob), Length(lBlob), @lSize, 0), 'BCryptExportKey');
      Check(PBCRYPT_ECCKEY_BLOB(lBlob)^.dwMagic=BCRYPT_ECDSA_PUBLIC_P256_MAGIC, 'ECC public blob magic');
      Check(PBCRYPT_ECCKEY_BLOB(lBlob)^.cbKey=32, 'ECC public blob key size');
      lHash:=Hash(BCRYPT_SHA256_ALGORITHM, Nil, TextBytes('abc'));
      lSig:=Nil;
      SetLength(lSig, 64);
      CheckStatus(BCryptSignHash(lKey, Nil, PUCHAR(lHash), Length(lHash), PUCHAR(lSig), Length(lSig), @lSize, 0), 'BCryptSignHash');
      Check(lSize=64, 'ECDSA P-256 signature size');
      Check(BCryptVerifySignature(lKey, Nil, PUCHAR(lHash), Length(lHash), PUCHAR(lSig), Length(lSig), 0)=STATUS_SUCCESS,
            'signature verifies');
      lSig[10]:=lSig[10] xor $FF;
      Check(BCryptVerifySignature(lKey, Nil, PUCHAR(lHash), Length(lHash), PUCHAR(lSig), Length(lSig), 0)=STATUS_INVALID_SIGNATURE,
            'changed signature gives STATUS_INVALID_SIGNATURE');
    finally
      BCryptDestroyKey(lKey);
    end;
  finally
    BCryptCloseAlgorithmProvider(lAlg, 0);
  end;
end;


// Check algorithm enumeration, buffer release and the error and FIPS status functions
procedure TestEnumAndStatus;

var
  lCount, I: ULONG;
  lList: PBCRYPT_ALGORITHM_IDENTIFIER;
  lFound: Boolean;
  lAlg: BCRYPT_ALG_HANDLE;
  lStatus: NTSTATUS;
  lFIPS: ByteBool;

begin
  lCount:=0;
  lList:=Nil;
  if CheckStatus(BCryptEnumAlgorithms(BCRYPT_HASH_OPERATION, @lCount, @lList, 0), 'BCryptEnumAlgorithms') then
    begin
    lFound:=False;
    for I:=0 to lCount-1 do
      if WideString(lList[I].pszName)='SHA256' then
        lFound:=True;
    Check(lFound, 'SHA256 is among the hash algorithms');
    BCryptFreeBuffer(lList);
    end;
  lAlg:=Nil;
  lStatus:=BCryptOpenAlgorithmProvider(@lAlg, 'NoSuchAlgorithm', Nil, 0);
  if RunningOnWine and (lStatus<>STATUS_NOT_FOUND) then
    Writeln('WINE: unknown algorithm gives $', IntToHex(LongWord(lStatus), 8), ', Windows gives STATUS_NOT_FOUND')
  else
    Check(lStatus=STATUS_NOT_FOUND, 'unknown algorithm gives STATUS_NOT_FOUND, got $'+IntToHex(LongWord(lStatus), 8));
  Check(not BCRYPT_SUCCESS(lStatus), 'BCRYPT_SUCCESS is False for STATUS_NOT_FOUND');
  lFIPS:=False;
  CheckStatus(BCryptGetFipsAlgorithmMode(@lFIPS), 'BCryptGetFipsAlgorithmMode');
end;


begin
  {$IFDEF CPU64}
  Writeln('bcrypt test, 64 bit');
  {$ELSE}
  Writeln('bcrypt test, 32 bit');
  {$ENDIF}
  TestLayout;
  TestRandom;
  TestHash;
  TestAESCBC;
  TestAESGCM;
  TestPBKDF2;
  TestECDSA;
  TestEnumAndStatus;
  if Errors>0 then
    begin
    Writeln(Errors, ' check(s) failed');
    Halt(1);
    end;
  Writeln('All checks passed');
end.
