unit fprsa;

{$mode ObjFPC}
{$H+}
{$ModeSwitch advancedrecords}

interface

{off $DEFINE CRYPTO_DEBUG}

uses
  sysutils, Classes, sha1, fpsha256, fpTLSBigInt, fphashutils, fpasn, basenenc;

const
  RSAPublicKeyOID = '1.2.840.113549.1.1.1';

  RSADigestInfoSHA256 = 1;
  RSADigestInfoSHA384 = 2;
  RSADigestInfoSHA512 = 3;
  RSADigestInfoSHA224 = 4;
  RSADigestInfoSHA512_224 = 5;
  RSADigestInfoSHA512_256 = 6;
  RSADigestInfoSHA3_224 = 7;
  RSADigestInfoSHA3_256 = 8;
  RSADigestInfoSHA3_384 = 9;
  RSADigestInfoSHA3_512 = 10;

type
  TRSA = record
    M: PBigInt;             // Modulus
    E: PBigInt;             // Public exponent
    D: PBigInt;             // Private exponent
    P: PBigInt;             // p in m = pq
    Q: PBigInt;             // q in m = pq
    DP: PBigInt;            // d mod (p-1)
    DQ: PBigInt;            // d mod (q-1)
    QInv: PBigInt;          // q^-1 mod p
    ModulusLen: Integer;
    Context: TBigIntContext;
  end;

  { TX509RSAPrivateKey }

  TX509RSAPrivateKey = record
    Version: integer;
    Modulus,             // m or n
    PublicExponent,      // e
    PrivateExponent,     // d
    Prime1,              // p
    Prime2,              // q
    Exponent1,           // dp
    Exponent2,           // dq
    Coefficient: TBytes; // qi
    procedure InitWithHexStrings(const n, e, d, p, q, dp, dq, qi: string);
    procedure InitWithBase64UrlEncoded(const n, e, d, p, q, dp, dq, qi: string);
    procedure WriteASN(ms: TMemoryStream);
    function AsDER: TBytes;
  end;

  { TX509RSAPublicKey }

  TX509RSAPublicKey = record
    Modulus: TBytes; // m or n
    Exponent: TBytes; // e
    procedure InitWithHexStrings(const n, e: string);
    procedure InitWithBase64UrlEncoded(const n, e: string);
    procedure WriteASN(ms: TMemoryStream);
    function AsDER: TBytes;
  end;

procedure RSACreate(out RSA: TRSA);
procedure RSAFree(var RSA: TRSA);

procedure RsaPublicKeyToHexa(const Modulus, Exponent: String; var PublicKeyHexa: String);
procedure RsaPublicKeyFromHexa(const PublicKeyHexa: String; out Modulus, Exponent: String);
procedure RsaInitFromPublicKey(var RSA: TRSA; const Modulus, Exponent: String); overload;
procedure RSAInitFromPublicKey(var RSA: TRSA; const RSAPublicKey: TX509RSAPublicKey); overload;
procedure RSAInitFromPublicKeyDER(var RSA: TRSA; const PublicKeyDER: TBytes);
procedure X509RsaPublicKeyInitFromDER(out RSA: TX509RSAPublicKey; const PublicKeyDER: TBytes);

procedure RSAInitFromX509PrivateKey(var RSA: TRSA; const RSAPrivateKey: TX509RSAPrivateKey);
procedure RSAInitFromPrivateKeyDER(var RSA: TRSA; const PrivateKeyDER: TBytes);
procedure X509RsaPrivateKeyInitFromDER(out RSA: TX509RSAPrivateKey; const PrivateKeyDER: TBytes);

{ Perform PKCS1.5 Encryption or Signing
  Context: The RSA context containing Private and/or Public keys
  Input: The data to be encrypted
  Len: The size of the input data in bytes (Must be <= Modulus length - 11 to
       make the padding at least 8 bytes as recommended by RFC2313)
  Output: The buffer for the encrypted result (Must always be Modulus length)
  Sign: If true then sign instead of encrypting
  Return: The number of bytes encrypted or -1 on error }
function RSAEncryptSign(var RSA: TRSA; const Input: PByte; Len: Integer; Output: PByte; Sign: Boolean): Integer;

{ Perform PKCS1.5 Decryption or Verification
  Context: The RSA context containing Private and/or Public keys
  Input: The data to be decrypted (Must always be Modulus length)
  Output: The buffer for the decrypted result
  Len: The size of the output buffer in bytes
  Verify: If true then verify instead of decrypting
  Return: The number of bytes decrypted or -1 on error }
function RSADecryptVerify(var RSA: TRSA; const Input: PByte; Output: PByte; Len: Integer; Verify: Boolean): Integer;

function RS256VerifyFromPublicKeyHexa(const PublicKeyHexa, SignatureBaseHash, Signature: String): Boolean;
function TestRS256Verify: Boolean;

function EncodeDigestInfoSHA(SHAType, len: byte): TBytes;

// integer <-> octetstring
function I2OSP(c: DWord; Len: integer): string;
function OSP2I(const Octet: string): DWord;

// MGF1 (Mask Generating Function 1) of PKCS1 (Public Key Cryptography Standard #1)
type
  THashFunction = function(const s: string): string; // string to hash digest
function MGF1(const InputStr: string; Len: integer; HashFunc: THashFunction): string;
function MGF1SHA1(const InputStr: string; Len: integer): string;
function MGF1SHA256(const InputStr: string; Len: integer): string;

implementation

const
  RSA_MODULUS_BYTES_MAX = 512; // 4096 bit maximum

procedure RSACreate(out RSA: TRSA);
begin
  RSA:=Default(TRSA);
  BIInitialize(RSA.Context);
end;

procedure RSAFree(var RSA: TRSA);
begin
  if RSA.M = nil then
    Exit;
  BITerminate(RSA.Context);
end;

procedure RsaPublicKeyToHexa(const Modulus, Exponent: String;
  var PublicKeyHexa: String);
begin
  PublicKeyHexa:=PublicKeyHexa+BytesToHexStr(Exponent)+BytesToHexStr(Modulus);
end;

procedure RsaPublicKeyFromHexa(const PublicKeyHexa: String; out Modulus,
  Exponent: String);
var
  aBytes: TBytes;
begin
  HexStrToBytes(PublicKeyHexa,aBytes);
  if length(aBytes)<4 then
    raise Exception.Create('20220426235757');
  SetLength(Exponent{%H-},3);
  Move(aBytes[0],Exponent[1],3);
  SetLength(Modulus{%H-},length(aBytes)-3);
  Move(aBytes[3],Modulus[1],length(Modulus));
end;

procedure RsaInitFromPublicKey(var RSA: TRSA; const Modulus, Exponent: String);
begin
  RSA.ModulusLen := length(Modulus);
  RSA.M := BIImport(RSA.Context, Modulus);
  BISetMod(RSA.Context, RSA.M, BIGINT_M_OFFSET);
  RSA.E := BIImport(RSA.Context, Exponent);
  BIPermanent(RSA.E);
end;

procedure RSAInitFromPublicKey(var RSA: TRSA;
  const RSAPublicKey: TX509RSAPublicKey);
begin
  if RSAPublicKey.Modulus = nil then
    Exit;
  if RSAPublicKey.Exponent = nil then
    Exit;
  RSA.ModulusLen := length(RSAPublicKey.Modulus);
  RSA.M := BIImport(RSA.Context, RSAPublicKey.Modulus);
  BISetMod(RSA.Context, RSA.M, BIGINT_M_OFFSET);
  RSA.E := BIImport(RSA.Context, RSAPublicKey.Exponent);
  BIPermanent(RSA.E);
end;

procedure RSAInitFromPublicKeyDER(var RSA: TRSA; const PublicKeyDER: TBytes);
var
  X508PublicKey: TX509RSAPublicKey;
begin
  X509RsaPublicKeyInitFromDER(X508PublicKey,PublicKeyDER);
  RSAInitFromPublicKey(RSA,X508PublicKey);
end;

procedure X509RsaPublicKeyInitFromDER(out RSA: TX509RSAPublicKey;
  const PublicKeyDER: TBytes);
var
  ASNType, ASNSize: integer;
  List: TStringList;
begin
  RSA:=Default(TX509RSAPublicKey);
  List:=TStringList.Create;
  try
    ASNParse(PublicKeyDER,List);

    {$IFDEF TLS_DEBUG}
    ASNDebugList('X509RsaPublicKeyInitFromDER',List);
    {$ENDIF}

    if List.Count<7 then
      raise Exception.Create('20220428180055');

    // check sequence
    ASNParse_GetItem(List,0,ASNType,ASNSize);
    if ASNType<>ASN1_SEQ then
      raise Exception.Create('20220428180058');

    // check sequence
    ASNParse_GetItem(List,1,ASNType,ASNSize);
    if ASNType<>ASN1_SEQ then
      raise Exception.Create('20220428183025');

    // check algorithm OID
    ASNParse_GetItem(List,2,ASNType,ASNSize);
    if ASNType<>ASN1_OBJID then
      raise Exception.Create('20220428180512');
    if List[2]<>RSAPublicKeyOID then
      raise Exception.Create('20220428181542');

    // check null
    ASNParse_GetItem(List,3,ASNType,ASNSize);
    if ASNType<>ASN1_NULL then
      raise Exception.Create('20220428181659');

    // check optional algorithm params
    ASNParse_GetItem(List,4,ASNType,ASNSize);
    if ASNType<>ASN1_BITSTR then
      raise Exception.Create('20220428181913');

    // check sequence
    ASNParse_GetItem(List,5,ASNType,ASNSize);
    if ASNType<>ASN1_SEQ then
      raise Exception.Create('20220428181933');

    // public key
    RSA.Modulus:=ASNParse_GetIntBytes(List,6,20220428182235);
    RSA.Exponent:=ASNParse_GetIntBytes(List,7,20220428182241);

    {$IFDEF TLS_DEBUG}
    writeln('X509RsaPublicKeyInitFromDER: ');
    writeln('  Modulus=$',BytesToHexStr(RSA.Modulus));
    writeln('  Exponent=$',BytesToHexStr(RSA.Exponent));
    {$ENDIF}
  finally
    List.Free;
  end;
end;

procedure RSAInitFromX509PrivateKey(var RSA: TRSA;
  const RSAPrivateKey: TX509RSAPrivateKey);
begin
  if RSAPrivateKey.PrivateExponent = nil then
    Exit;
  if RSAPrivateKey.Prime1 = nil then
    Exit;
  if RSAPrivateKey.Prime2 = nil then
    Exit;
  if RSAPrivateKey.Exponent1 = nil then
    Exit;
  if RSAPrivateKey.Exponent2 = nil then
    Exit;
  if RSAPrivateKey.Coefficient = nil then
    Exit;
  if RSAPrivateKey.Modulus = nil then
    Exit;
  if RSAPrivateKey.PublicExponent = nil then
    Exit;
  RSA.ModulusLen := length(RSAPrivateKey.Modulus);
  RSA.M := BIImport(RSA.Context, RSAPrivateKey.Modulus);
  BISetMod(RSA.Context, RSA.M, BIGINT_M_OFFSET);
  RSA.E := BIImport(RSA.Context, RSAPrivateKey.PublicExponent);
  BIPermanent(RSA.E);
  RSA.D := BIImport(RSA.Context, RSAPrivateKey.PrivateExponent);
  BIPermanent(RSA.D);
  RSA.P := BIImport(RSA.Context, RSAPrivateKey.Prime1);
  RSA.Q := BIImport(RSA.Context, RSAPrivateKey.Prime2);
  RSA.DP := BIImport(RSA.Context, RSAPrivateKey.Exponent1);
  RSA.DQ := BIImport(RSA.Context, RSAPrivateKey.Exponent2);
  RSA.QInv := BIImport(RSA.Context, RSAPrivateKey.Coefficient);
  BIPermanent(RSA.DP);
  BIPermanent(RSA.DQ);
  BIPermanent(RSA.QInv);
  BISetMod(RSA.Context, RSA.P, BIGINT_P_OFFSET);
  BISetMod(RSA.Context, RSA.Q, BIGINT_Q_OFFSET);
end;

procedure RSAInitFromPrivateKeyDER(var RSA: TRSA; const PrivateKeyDER: TBytes);
var
  X509RSA: TX509RSAPrivateKey;
begin
  X509RsaPrivateKeyInitFromDER(X509RSA,PrivateKeyDER);
  RSAInitFromX509PrivateKey(RSA,X509RSA);
end;

procedure X509RsaPrivateKeyInitFromDER(out RSA: TX509RSAPrivateKey; const PrivateKeyDER: TBytes);
var
  List: TStringList;
  ASNType, ASNSize: integer;
begin
  RSA:=Default(TX509RSAPrivateKey);
  List:=TStringList.Create;
  try
    ASNParse(PrivateKeyDER,List);
    if List.Count<10 then
      raise Exception.Create('20220428161533');

    // check sequence
    ASNParse_GetItem(List,0,ASNType,ASNSize);
    if ASNType<>ASN1_SEQ then
      raise Exception.Create('20220428161631');

    // version
    ASNParse_GetItem(List,1,ASNType,ASNSize);
    if ASNType<>ASN1_INT then
      raise Exception.Create('20220428161716');
    RSA.Version:=StrToIntDef(List[1],0);

    RSA.Modulus:=ASNParse_GetIntBytes(List,2,20220428173827);
    RSA.PublicExponent:=ASNParse_GetIntBytes(List,3,20220428173840);
    RSA.PrivateExponent:=ASNParse_GetIntBytes(List,4,20220428173852);
    RSA.Prime1:=ASNParse_GetIntBytes(List,5,20220428173906);
    RSA.Prime2:=ASNParse_GetIntBytes(List,6,20220428173915);
    RSA.Exponent1:=ASNParse_GetIntBytes(List,7,20220428173923);
    RSA.Exponent2:=ASNParse_GetIntBytes(List,8,20220428173930);
    RSA.Coefficient:=ASNParse_GetIntBytes(List,9,20220428173939);

    {$IFDEF TLS_DEBUG}
    with RSA do begin
      writeln('RsaInitFromPrivateKey ');
      writeln('   Modulus=',BytesToHexStr(Modulus));
      writeln('   PublicExponent=',BytesToHexStr(PublicExponent));
      writeln('   PrivateExponent=',BytesToHexStr(PrivateExponent));
      writeln('   Prime1=',BytesToHexStr(Prime1));
      writeln('   Prime2=',BytesToHexStr(Prime2));
      writeln('   Exponent1=',BytesToHexStr(Exponent1));
      writeln('   Exponent2=',BytesToHexStr(Exponent2));
      writeln('   Coefficient=',BytesToHexStr(Coefficient));
    end;
    {$ENDIF}
  finally
    List.Free;
  end;
end;

function RSAEncryptSign(var RSA: TRSA; const Input: PByte; Len: Integer;
  Output: PByte; Sign: Boolean): Integer;
var
  Size: Integer;
  Padding: Integer;
  Imported: PByte;
  Decrypted: PBigInt;
  Encrypted: PBigInt;
  Block: array[0..RSA_MODULUS_BYTES_MAX-1] of Byte;
  {$IFDEF CRYPTO_DEBUG}
  i: integer;
  {$ENDIF}
begin
  Result := -1;
  if Input = nil then
    Exit;
  if Output = nil then
    Exit;
  Size := RSA.ModulusLen;
  Padding := Size-Len-3;
  if Len > Size-8-3 then
    Exit;
  {$IFDEF CRYPTO_DEBUG}
  writeln('RSAEncryptSign - Len = ' + IntToStr(Len) + ' Size = ' + IntToStr(Size) + ' Padding = ' + IntToStr(Padding)); //To Do
  {$ENDIF}
  if Size > RSA_MODULUS_BYTES_MAX then
    Imported := GetMem(Size)
  else
    Imported := @Block[0];
  try
    // Leading zero to ensure encryption block is less than modulus (when converted to an integer)
    Imported[0]:=0;

    // Check Sign
    if Sign then
    begin
      // Block Type 1
      Imported[1]:=1;

      // Pad with 0xff bytes
      FillByte(Imported[2],Padding,$FF);
    end else
    begin
      // Block Type 2
      Imported[1]:=2;

      // Pad with random non-zero bytes
      if not CryptoGetRandomBytes(@Imported[2], Padding, false) then
        Exit;
      {$IFDEF CRYPTO_DEBUG}
      for i:=0 to Padding-1 do
        if Imported[2+i]=0 then
          raise Exception.Create('20220429000653');
      {$ENDIF}
    end;

    // Trailing zero after padding bytes
    Imported[2 + Padding]:=0;

    // Copy Input to Block
    System.Move(Input^,Imported[3 + Padding],Len);

    {$IFDEF CRYPTO_DEBUG}
    writeln('RSAEncryptSign - Imported Size = ' + IntToStr(Size) + ' Len = ',Len);
    {$ENDIF}

    // Encrypt the Block
    // Note: the RSA.M was set in RSAInitFromPublicKey
    Decrypted:=BIImport(RSA.Context,Imported,Size);
    if Sign then
    begin
      // Sign with Private Key
      Encrypted:=BICRT(RSA.Context,Decrypted,RSA.DP,RSA.DQ,RSA.P,RSA.Q,RSA.QInv); // this releases Decrypted
    end else
    begin
      // Encrypt with Public Key
      RSA.Context.ModOffset:=BIGINT_M_OFFSET;
      Encrypted:=BIModPower(RSA.Context,Decrypted,RSA.E); // this releases Decrypted
    end;
    BIExport(RSA.Context,Encrypted,Output,Size); // this releases Encrypted

    {$IFDEF CRYPTO_DEBUG}
    writeln('RSAEncryptSign - Output Size = ' + IntToStr(Size) + ' Len = ',Len);
    {$ENDIF}

    // Return Result
    Result:=Size;
  finally
    if Size > RSA_MODULUS_BYTES_MAX then
      FreeMem(Imported);
  end;
end;

function RSADecryptVerify(var RSA: TRSA; const Input: PByte; Output: PByte;
  Len: Integer; Verify: Boolean): Integer;
var
  Size: Integer;
  Count: Integer;
  Padding: Integer;
  Exported: PByte;
  Encrypted: PBigInt;
  Decrypted: PBigInt;
  Block: array[0..RSA_MODULUS_BYTES_MAX-1] of Byte;
begin
  Result := -1;
  if Input = nil then
    Exit;
  if Output = nil then
    Exit;
  Size := RSA.ModulusLen;
  Count := 0;
  Padding := 0;
  {$IFDEF CRYPTO_DEBUG}
  writeln('RSA', 'RSADecryptVerify Len: ', Len, ' Size: ', Size, ' Padding: ', Padding);
  {$ENDIF}
  Encrypted := BIImport(RSA.Context, Input, Size);
  if Verify then
  begin
    // Verify with Public Key
    RSA.Context.ModOffset := BIGINT_M_OFFSET;
    Decrypted := BIModPower(RSA.Context, Encrypted, RSA.E);
  end else
  begin
    // Decrypt with Private Key
    Decrypted := BICRT(RSA.Context,Encrypted,RSA.DP,RSA.DQ,RSA.P,RSA.Q,RSA.QInv);
  end;
  Exported := @Block[0];
  if Size > RSA_MODULUS_BYTES_MAX then
  begin
    Exported := GetMem(Size);
    if Exported = nil then
    begin
      {$IFDEF CRYPTO_DEBUG}
      writeln('RSADecryptVerify GetMem failed');
      {$ENDIF}
      Exit;
    end;
  end;
  try
    BIExport(RSA.Context, Decrypted, Exported, Size);
    if Exported[Count] <> 0 then
    begin
      {$IFDEF CRYPTO_DEBUG}
      writeln('RSADecryptVerify leading zero missing');
      {$ENDIF}
      Exit; // Check Leading Zero
    end;
    Inc(Count);
    if Verify then
    begin
      // Check Block Type 1
      if Exported[Count] <> 1 then
      begin
        {$IFDEF CRYPTO_DEBUG}
        writeln('RSADecryptVerify Verify Blockt Type<>1');
        {$ENDIF}
        Exit;
      end;
      Inc(Count);
      while (Exported[Count] = $FF) and (Count < Size) do
      begin
        // Padded with 0xff bytes
        Inc(Count);
        Inc(Padding);
      end;
    end else
    begin
      // Check Block Type 2
      if Exported[Count] <> 2 then
      begin
        {$IFDEF CRYPTO_DEBUG}
        writeln('RSADecryptVerify Decrypt Blockt Type<>2');
        {$ENDIF}
        Exit;
      end;
      Inc(Count);
      while (Exported[Count] <> 0) and (Count < Size) do
      begin
        // Padded with random non-zero bytes
        Inc(Count);
        Inc(Padding);
      end;
    end;
    // Check trailing zero byte and padding size
    if (Count = Size) or (Padding < 8) then
    begin
      {$IFDEF CRYPTO_DEBUG}
      writeln('RSADecryptVerify invalid padding');
      {$ENDIF}
      Exit;
    end;
    if Exported[Count] <> 0 then
    begin
      {$IFDEF CRYPTO_DEBUG}
      writeln('RSADecryptVerify after padding zero missing');
      {$ENDIF}
      Exit;
    end;
    Inc(Count);
    Result := Size-Count;
    if Len < Result then
    begin
      {$IFDEF CRYPTO_DEBUG}
      writeln('RSADecryptVerify Output too small');
      {$ENDIF}
      Result := -1;
      Exit;
    end;
    if Len > Result then
      FillByte(Output[Result], Len-Result, 0);
    System.Move(Exported[Count], Output^, Result);
    {$IFDEF CRYPTO_DEBUG}
      writeln('RSADecryptVerify - Output Size: ', Result);
      //writeln('Output: ', Output);
    {$ENDIF}
  finally
    if Size > RSA_MODULUS_BYTES_MAX then
      FreeMem(Exported);
  end;
end;

function RsaVerify(const Modulus, Exponent, Hash, Signature: String): Boolean;
var
  ASNType, ASNSize: Int32;
  Data: array[0..4095] of byte;
  Digest: String;
  DataP, DataEnd: PByte;
  OID: String;
  RSA: TRSA;
  Size: Integer;
begin
  Result := False;
  {$IFDEF TLS_DEBUG}
    writeln('RsaVerify - Modulus: ', Modulus,' Exponent: ', Exponent,
       ' Hash: ', Hash, ' Signature: ', Signature,
       ' Signature Length: ', Signature.Length,
       ' Modulus Length: ', Modulus.Length);
  {$ENDIF}
  if length(Modulus) <> length(Signature) then
    Exit;
  if length(Signature)>length(Data) then
    Exit;
  RsaCreate(RSA);
  try
    RsaInitFromPublicKey(RSA, Modulus, Exponent);
    DataP:=PByte(@Data[0]);
    DataEnd:=DataP+length(Data);
    Size := RSADecryptVerify(RSA, PByte(Signature), DataP, length(Signature), True); // Decrypt the signature
    if Size = -1 then
      Exit;
    if not ASNFetch(DataP, DataEnd, ASNType, ASNSize) then  // Sequence: DigestInfo
      Exit;
    if ASNType <> ASN1_SEQ then
      Exit;
    if not ASNFetch(DataP, DataEnd, ASNType, ASNSize) then  // Sequence: AlgorithmIdentifier
      Exit;
    if ASNType <> ASN1_SEQ then
      Exit;
    if not ASNFetchOID(DataP, DataEnd, OID) then  // OID: Algorithm
      Exit;
    if not ASNFetch(DataP, DataEnd, ASNType, ASNSize) then  // ASN1_NULL
      Exit;
    if ASNType = ASN1_NULL then
    begin
      if not ASNFetch(DataP, DataEnd, ASNType, ASNSize) then  // OctetString: Digest
        Exit;
    end;
    if ASNType <> ASN1_OCTSTR then
    begin
      {$IFDEF TLS_DEBUG}
      writeln('RsaVerify - OCTETSTRING Digest not found in decoded Signature');
      {$ENDIF}
      Exit;
    end;
    if ASNSize <> length(Hash) then
    begin
      {$IFDEF TLS_DEBUG}
      writeln('RsaVerify - Mismatch digest size: ',ASNSize,' != Hash.Length: ', length(Hash));
      {$ENDIF}
      Exit;
    end;
    if DataEnd-DataP<ASNSize then
      Exit;
    SetLength(Digest{%H-},ASNSize);
    System.Move(DataP^,Digest[1],ASNSize);
    {$IFDEF TLS_DEBUG}
      writeln('RsaVerify - Compare Digest: ', Digest);
      writeln('RsaVerify - with Hash: ', Hash);
    {$ENDIF}
    Result := Digest=Hash;
    {$IFDEF TLS_DEBUG}
    if Result then
      writeln('RsaVerify - Success')
    else
      writeln('RsaVerify - Failed');
    {$ENDIF}
  finally
    RSAFree(RSA);
  end;
end;

function RS256VerifyFromPublicKeyHexa(const PublicKeyHexa, SignatureBaseHash,
  Signature: String): Boolean;
var
  Modulus, Exponent: String;
begin
  RsaPublicKeyFromHexa(PublicKeyHexa, Modulus, Exponent);
  Result := RsaVerify(Modulus, Exponent, SignatureBaseHash, Signature);
end;

function TestRS256Verify: Boolean;
const
  _Modulus = 'BB32B4D0D89E9A9E8C79294C2BA8EF5C43D4933B9478FF3054C71BC8E52F1B99CD108D67C8540C075AE4F4067FC7D684B42CCD2D4E4426011AEA37BEEFF4C715'
            +'07C3164C6B261909D2FF5910445B8A8981941DFEE25F9A5F8A36D8B0E91F6F802254ACAC29435552D815BE92687B94565118D0A7D5C35A47A8D83CC61D72DC04'
            +'369DACCF152C2E87D7F0FD497755AEEC4AA9DB8B291E3567FE9D9520DD798D600A7873DC2875A586DF31FB130936A6C3E02D46DC252B76F6ADF4C77DF868C23B'
            +'B3335E542ADF9BAEBFDC1019408D04EF6BCAEEB5853D2BD38D825FA91B6BBB06FE75E83C26372F31CFDC0E8D378EA5E87433D37F7B0ABC7206D1F3B2C56B18B5';
  _Hash = 'A135E3608E956E91743421E0677C03FBE2C7CE0890FF06423B66335E3428EF9A';
  _Exponent = '010001';
  _Signature =  '75BDCF54B21FD4F2891EEC91D1E9F6D82ADEB63BBB1DB4E03A389B525E8F5B97669FEB2E9C87EF4E785124F5499918771E03E4FF83E31CE0CF4A8276809C35AA'
               +'FBF9B45B7918F5D891D863CA441D5803DFD1C4190640A73ADA10DC05C2EF480C449FDD157AB4CD1ADE0B067930E07607134ED425BE5A0A1F78AFD6045BA638E7'
               +'18BFB311B8377C0FACDED4CD2B1E2692E480BE260BE355F050EBABF89E24F2833F56F0A74C185225DB3B47B63612FB9BDEE1E1B8707807093E1551F24527A763'
               +'1947D033ED7052C439E50B8A46E4D0C06DBC38AF1D64B49766A5CF9A82644650FFD733B61942DB0BD8D47C8EF24A02DC9FD2EF557B12DED804519F2B2B6C284D';
var
  Exponent, Modulus, Hash, Signature: string;
begin
  Exponent:=HexStrToString(_Exponent);
  Modulus:=HexStrToString(_Modulus);
  Hash:=HexStrToString(_Hash);
  Signature:=HexStrToString(_Signature);
  Result := RsaVerify(Modulus, Exponent, Hash, Signature);
end;

function EncodeDigestInfoSHA(SHAType, len: byte): TBytes;
begin
  Result:= [
    ASN1_SEQ, 17 + len,
      ASN1_SEQ, 13,
        ASN1_OBJID, 9, 2*40 + 16, $86, $48, 1, 101, 3, 4, 2, SHAType,
        ASN1_NULL, 0,
      ASN1_OCTSTR, len
    ];
end;

function I2OSP(c: DWord; Len: integer): string;
var
  i: DWord;
begin
  if Len>4 then
    raise Exception.Create('20220501190110');
  SetLength(Result{%H-},Len);
  for i:=Len downto 1 do
  begin
    Result[i]:=chr(c and $ff);
    c:=c shr 8;
  end;
  if c>0 then
    raise Exception.Create('20220501190124');
end;

function OSP2I(const Octet: string): DWord;
var
  i: Integer;
begin
  Result:=0;
  if length(Octet)>4 then
    raise Exception.Create('20220501190308');
  for i:=1 to length(Octet) do
    Result:=Result shl 8 + ord(Octet[i]);
end;

function MGF1(const InputStr: string; Len: integer; HashFunc: THashFunction
  ): string;
var
  Counter: DWord;
begin
  Counter:=0;
  Result:='';
  while length(Result)<Len do
  begin
    Result:=Result+HashFunc(InputStr+I2OSP(Counter,4));
    inc(Counter);
  end;
  SetLength(Result,Len);
end;

function SHA1StrToDigest(const InputStr: string): string;
var
  Digest: TSHA1Digest;
begin
  Digest:=SHA1String(InputStr);
  SetLength(Result{%H-},length(Digest));
  System.Move(Digest[0],Result[1],length(Digest));
  if Digest[0]=0 then ;
end;

function MGF1SHA1(const InputStr: string; Len: integer): string;
begin
  Result:=MGF1(InputStr,Len,@SHA1StrToDigest);
end;

function SHA256StrToDigest(const InputStr: string): string;
var
  SHA256: TSHA256;
begin
  SHA256.Init;
  SHA256.Update(@InputStr[1],length(InputStr));
  SHA256.Final;

  SetLength(Result{%H-},length(SHA256.Digest));
  System.Move(SHA256.Digest[0],Result[1],length(SHA256.Digest));
end;

function MGF1SHA256(const InputStr: string; Len: integer): string;
begin
  Result:=MGF1(InputStr,Len,@SHA256StrToDigest);
end;

{ TX509RSAPrivateKey }

procedure TX509RSAPrivateKey.InitWithHexStrings(const n, e, d, p, q, dp, dq, qi: string
  );
begin
  Version:=0;
  Modulus:=HexStrToBytes(n);
  PublicExponent:=HexStrToBytes(e);
  PrivateExponent:=HexStrToBytes(d);
  Prime1:=HexStrToBytes(p);
  Prime2:=HexStrToBytes(q);
  Exponent1:=HexStrToBytes(dp);
  Exponent2:=HexStrToBytes(dq);
  Coefficient:=HexStrToBytes(qi);
end;

procedure TX509RSAPrivateKey.InitWithBase64UrlEncoded(const n, e, d, p, q, dp,
  dq, qi: string);
begin
  Version:=0;
  Modulus:=Base64URL.Decode(n,false);
  PublicExponent:=Base64URL.Decode(e,false);
  PrivateExponent:=Base64URL.Decode(d,false);
  Prime1:=Base64URL.Decode(p,false);
  Prime2:=Base64URL.Decode(q,false);
  Exponent1:=Base64URL.Decode(dp,false);
  Exponent2:=Base64URL.Decode(dq,false);
  Coefficient:=Base64URL.Decode(qi,false);
end;

procedure TX509RSAPrivateKey.WriteASN(ms: TMemoryStream);
var
  SeqBegin: Int64;
begin
  SeqBegin:=ASNWriteSequenceBegin(ms);
  ASNWriteInt(Version,ms);
  ASNWriteBigInt(Modulus,ms);
  ASNWriteBigInt(PublicExponent,ms);
  ASNWriteBigInt(PrivateExponent,ms);
  ASNWriteBigInt(Prime1,ms);
  ASNWriteBigInt(Prime2,ms);
  ASNWriteBigInt(Exponent1,ms);
  ASNWriteBigInt(Exponent2,ms);
  ASNWriteBigInt(Coefficient,ms);
  ASNWriteSequenceEnd(SeqBegin,ms);
end;

function TX509RSAPrivateKey.AsDER: TBytes;
var
  ms: TMemoryStream;
begin
  Result:=[];
  ms:=TMemoryStream.Create;
  try
    WriteASN(ms);
    SetLength(Result,ms.Size);
    Move(ms.Memory^,Result[0],ms.Size);
  finally
    ms.Free;
  end;
end;

{ TX509RSAPublicKey }

procedure TX509RSAPublicKey.InitWithHexStrings(const n, e: string);
begin
  Modulus:=HexStrToBytes(n);
  Exponent:=HexStrToBytes(e);
end;

procedure TX509RSAPublicKey.InitWithBase64UrlEncoded(const n, e: string);
begin
  Modulus:=Base64URL.Decode(n,false);
  Exponent:=Base64URL.Decode(e,false);
end;

procedure TX509RSAPublicKey.WriteASN(ms: TMemoryStream);
var
  SeqBegin, AlgoSeqBegin, RSASeqBegin, BitStrBegin: Int64;
begin
  SeqBegin:=ASNWriteSequenceBegin(ms);
  AlgoSeqBegin:=ASNWriteSequenceBegin(ms);
  ASNWriteObjID(RSAPublicKeyOID,ms);
  ASNWriteNull(ms);
  ASNWriteSequenceEnd(AlgoSeqBegin,ms);

  BitStrBegin:=ASNWriteBitStrBegin(ms);
  RSASeqBegin:=ASNWriteSequenceBegin(ms);
  ASNWriteBigInt(Modulus,ms);
  ASNWriteBigInt(Exponent,ms);
  ASNWriteSequenceEnd(RSASeqBegin,ms);
  ASNWriteBitStrEnd(BitStrBegin,ms);

  ASNWriteSequenceEnd(SeqBegin,ms);
end;

function TX509RSAPublicKey.AsDER: TBytes;
var
  ms: TMemoryStream;
begin
  Result:=[];
  ms:=TMemoryStream.Create;
  try
    WriteASN(ms);
    SetLength(Result,ms.Size);
    Move(ms.Memory^,Result[0],ms.Size);
  finally
    ms.Free;
  end;
end;

end.

