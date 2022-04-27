unit fprsa;

{$mode ObjFPC}
{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  sysutils, fpTLSBigInt, fphashutils, fpasn;

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

procedure RSACreate(var RSA: TRSA);
procedure RSAFree(var RSA: TRSA);
procedure RsaPublicKeyToHexa(const Modulus, Exponent: String; var PublicKeyHexa: String);
procedure RsaPublicKeyFromHexa(const PublicKeyHexa: String; out Modulus, Exponent: String);
{$IFDEF TLS}
procedure RSAInitFromPrivateKey(var RSA: TRSA; const RSAPrivateKey: TX509RSAPrivateKey);
procedure RSAInitFromPublicKey(var RSA: TRSA; const RSAPublicKey: TX509RSAPublicKey); overload;
{$ENDIF}
procedure RsaInitFromPublicKey(var RSA: TRSA; const Modulus, Exponent: String); overload;
function RSAEncryptSign(var RSA: TRSA; const Input: PByte; Len: Integer; Output: PByte; Sign: Boolean): Integer;
function RSADecryptVerify(var RSA: TRSA; const Input: PByte; Output: PByte; Len: Integer; Verify: Boolean): Integer;
function RS256VerifyFromPublicKeyHexa(const PublicKeyHexa, SignatureBaseHash, Signature: String): Boolean;
function TestRS256Verify: Boolean;

implementation

const
  RSA_MODULUS_BYTES_MAX = 512; // 4096 bit maximum

procedure RSACreate(var RSA: TRSA);
begin
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

function RSAEncryptSign(var RSA: TRSA; const Input: PByte; Len: Integer;
  Output: PByte; Sign: Boolean): Integer;
{ Perform PKCS1.5 Encryption or Signing
  Context: The RSA context containing Private and/or Public keys
  Input: The data to be encrypted
  Len: The size of the input data in bytes (Must be <= Modulus length - 11 to
       make the padding at least 8 bytes as recommended by RFC2313)
  Output: The buffer for the encrypted result (Must always be Modulus length)
  Sign: If true then sign instead of encrypting
  Return: The number of bytes encrypted or -1 on error
}
var
  Size: Integer;
  Padding: Integer;
  Imported: PByte;
  Decrypted: PBigInt;
  Encrypted: PBigInt;
  Block: array[0..RSA_MODULUS_BYTES_MAX-1] of Byte;
begin
  Result := -1;
  if Input = nil then
    Exit;
  if Output = nil then
    Exit;
  Size := RSA.ModulusLen;
  Padding := Size-Len-3;
  if Len > Size-11 then
    Exit;
  {$IFDEF CRYPTO_DEBUG}
    writeln('RSAEncryptSign - Len = ' + IntToStr(Len) + ' Size = ' + IntToStr(Size) + ' Padding = ' + IntToStr(Padding)); //To Do
  {$ENDIF}
  if Size > RSA_MODULUS_BYTES_MAX then
    Imported := GetMem(Size)
  else
    Imported := @Block;
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
      if not CryptoGetRandomBytes(@Imported[2], Padding) then
        Exit;
    end;

    // Trailing zero after padding bytes
    Imported[2 + Padding]:=0;

    // Copy Input to Block
    System.Move(Input^,Imported[3 + Padding],Len);

    {$IFDEF CRYPTO_DEBUG}
    writeln('RSAEncryptSign - Imported Size = ' + IntToStr(Size) + ' Imported = ',Imported,Size);
    {$ENDIF}

    // Encrypt the Block
    Decrypted:=BIImport(RSA.Context,Imported,Size);
    if Sign then
    begin
      // Sign with Private Key
      Encrypted:=BICRT(RSA.Context,Decrypted,RSA.DP,RSA.DQ,RSA.P,RSA.Q,RSA.QInv);
    end else
    begin
      // Encrypt with Public Key
      RSA.Context.ModOffset:=BIGINT_M_OFFSET;
      Encrypted:=BIModPower(RSA.Context,Decrypted,RSA.E);
    end;
    BIExport(RSA.Context,Encrypted,Output,Size);

    {$IFDEF CRYPTO_DEBUG}
    writeln('RSAEncryptSign - Output Size = ' + IntToStr(Size) + ' Output = ',Output,Size);
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
// Perform PKCS1.5 Decryption or Verification
// Context: The RSA context containing Private and/or Public keys
// @Input: The data to be decrypted (Must always be Modulus length)
// @Output: The buffer for the decrypted result
// Len: The size of the output buffer in bytes
// Verify: If true then verify instead of decrypting
// Return: The number of bytes decrypted or -1 on error
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
  Exported := @Block;
  if Size > RSA_MODULUS_BYTES_MAX then
  begin
    Exported := GetMem(Size);
    if Exported = nil then
      Exit;
  end;
  try
    BIExport(RSA.Context, Decrypted, Exported, Size);
    if Exported[Count] <> 0 then
      Exit; // Check Leading Zero
    Inc(Count);
    if Verify then
    begin
      // Check Block Type 1
      if Exported[Count] <> 1 then
        Exit;
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
        Exit;
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
      Exit;
    if Exported[Count] <> 0 then
      Exit;
    Inc(Count);
    Result := Size-Count;
    if Len < Result then
    begin
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
    if not ASNFetch(DataP, DataEnd, ASNType, ASNSize) then  // ASN1_NULL OctetString: Digest
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

end.

