program tprivatekeys;

{ TSSLContext.UsePrivateKey loads RSA, EC and Ed25519 keys as PEM file, DER file and DER bytes
  (issue #42087). usage: tprivatekeys }

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, base64, sslbase, openssl, fpopenssl;

const
  KeyRsa1 =
    '-----BEGIN RSA PRIVATE KEY-----' + LineEnding +
    'MIIEowIBAAKCAQEAxjPlWLK69ez+4zfyfzhLJVxSApBWv55JoByus7eKYh3oyAxP' + LineEnding +
    'qAtqP4GnyayhXNZsLhclik83pZ76BbTwjQkJeTfJcV0Hg8FIHKy+hwfOJThyaYhQ' + LineEnding +
    'E+JYyHXry0XT516gqjpsxlYzJ5WcPX8R611SgK34+mnUyClP7GKr/qjlRssnLogi' + LineEnding +
    '9pZRrZEZ3vYlFcDCmQVudAVKR611YyA8aHQR0ysM46khuIrWfTfKwXgEDwnPEdet' + LineEnding +
    'MSMt2eXvbTBJcuIl4DGRABR2+UOFtI6EUbfDOYbZsdOtPM/sAceGpv7V8RQyK9ge' + LineEnding +
    'IdWASahTHBBv9Y/dmIAffpRNX22u+Zjf71mpQQIDAQABAoIBAAnCE9RRsQk7Er3S' + LineEnding +
    'lb6O4bTpmO214q4lR6LZo/3xTHTyBobU2QDLwRPL2RwsLYycekUsUBBBarAIMJni' + LineEnding +
    'fju0r8dMnb8WUVtQAzIgN3LT9ExaUq2175qvAz0pVcN+AQW237T0fUGoNvVdCmJv' + LineEnding +
    'tLeQXRJnSPxhL1VEeBd0sIWa60uDI6SwDLy35ogwYy5HwLRX3yPbfBcTT04ZLie8' + LineEnding +
    'H2HdU1wKiPaqEp7Tit2Q0fb7BN7OOjTvg/qVm1evisMPy1c83UXNnRVeEGtDwaHR' + LineEnding +
    'X1R8lqRxQp00i1fA/EH3Ql2+krmIttBWbMzLQzueEFL2L1VqTVWQn6rSTgeX+dbm' + LineEnding +
    '8c/NeF0CgYEA5qvZItJ5uk75+bBF3jp84LF4qnZ7t5O83nGHDkrzztYp/6rVBls5' + LineEnding +
    '/FVlvYhsLZ1pc8KWu33ttXJ1YQ6rhZyz6HWp3zuMPhDFkg2CRd+S5mn1KhQpEXtt' + LineEnding +
    'du4ijUiXLWfLLoo9YDwASnLRB642ldc74FRn1J5yOsoVJ9VFvymWBlUCgYEA2/db' + LineEnding +
    '4IkWp2cIhwPDiboFm5cdYAkx+hrF+7hTQ9Dk5PkqGNKv/1a3tYLlTyoQuOxRv6tj' + LineEnding +
    '93o1eDIa9UQgbtVkCe/iedqM/IlmuD16UVO0EbjdkID3LNijdNlkxjGP8jWBjoAL' + LineEnding +
    'TWh4Inuii1D7wOFPjzsRDCQcUad7O0uJR/KCiz0CgYBaGPlg7pD5QmvFKSLBIFsy' + LineEnding +
    'cF9PRaQhGiWFRRaZcJE2UtRotlMqfmtxsfFhTG3kiz1rJ7A0X4V/f8t1M4OvVnu2' + LineEnding +
    '6m6FcP7gJzDkUT53CFKaiPxniSRZXbpt36nDQX07EPV9oiVC92ytp5cZxuQvNrh+' + LineEnding +
    'cLl1hMXdTJ1ix5zMg/PDWQKBgQDSmT9z+Iyh9fmqGqZqBJo93y6WtfcNd/i6Cj5e' + LineEnding +
    'HXAo8Y2meoFavJHizNHzO1kLU+bo1ZU+adkrnP1GedV7h/7hrLRu6ho15u9b0jB8' + LineEnding +
    'EOHEXfRz16XPYBMPWtwrDWS+BxBleJ5ZnH34tdpn/D8s6gscHPczYpdibjGEEcv8' + LineEnding +
    'AGmQGQKBgCVBw6OmCd40CDKxUmSxR8BisqvUEXL9Zy5ggvchFFT14RI953c0VgGj' + LineEnding +
    'KBF5Y59OfNfWnaPtU8Jwcr9I3YwfqYY/Iin/Xm/Jbpbuv8p68VDWFUaofFUOj2jl' + LineEnding +
    'gRABc69UuthdXHppHSxQtngATT2QhYmIKk9B2Io6PMTIuUkuMpM/' + LineEnding +
    '-----END RSA PRIVATE KEY-----';
  KeyRsa8 =
    '-----BEGIN PRIVATE KEY-----' + LineEnding +
    'MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDGM+VYsrr17P7j' + LineEnding +
    'N/J/OEslXFICkFa/nkmgHK6zt4piHejIDE+oC2o/gafJrKFc1mwuFyWKTzelnvoF' + LineEnding +
    'tPCNCQl5N8lxXQeDwUgcrL6HB84lOHJpiFAT4ljIdevLRdPnXqCqOmzGVjMnlZw9' + LineEnding +
    'fxHrXVKArfj6adTIKU/sYqv+qOVGyycuiCL2llGtkRne9iUVwMKZBW50BUpHrXVj' + LineEnding +
    'IDxodBHTKwzjqSG4itZ9N8rBeAQPCc8R160xIy3Z5e9tMEly4iXgMZEAFHb5Q4W0' + LineEnding +
    'joRRt8M5htmx0608z+wBx4am/tXxFDIr2B4h1YBJqFMcEG/1j92YgB9+lE1fba75' + LineEnding +
    'mN/vWalBAgMBAAECggEACcIT1FGxCTsSvdKVvo7htOmY7bXiriVHotmj/fFMdPIG' + LineEnding +
    'htTZAMvBE8vZHCwtjJx6RSxQEEFqsAgwmeJ+O7Svx0ydvxZRW1ADMiA3ctP0TFpS' + LineEnding +
    'rbXvmq8DPSlVw34BBbbftPR9Qag29V0KYm+0t5BdEmdI/GEvVUR4F3SwhZrrS4Mj' + LineEnding +
    'pLAMvLfmiDBjLkfAtFffI9t8FxNPThkuJ7wfYd1TXAqI9qoSntOK3ZDR9vsE3s46' + LineEnding +
    'NO+D+pWbV6+Kww/LVzzdRc2dFV4Qa0PBodFfVHyWpHFCnTSLV8D8QfdCXb6SuYi2' + LineEnding +
    '0FZszMtDO54QUvYvVWpNVZCfqtJOB5f51ubxz814XQKBgQDmq9ki0nm6Tvn5sEXe' + LineEnding +
    'OnzgsXiqdnu3k7zecYcOSvPO1in/qtUGWzn8VWW9iGwtnWlzwpa7fe21cnVhDquF' + LineEnding +
    'nLPodanfO4w+EMWSDYJF35LmafUqFCkRe2127iKNSJctZ8suij1gPABKctEHrjaV' + LineEnding +
    '1zvgVGfUnnI6yhUn1UW/KZYGVQKBgQDb91vgiRanZwiHA8OJugWblx1gCTH6GsX7' + LineEnding +
    'uFND0OTk+SoY0q//Vre1guVPKhC47FG/q2P3ejV4Mhr1RCBu1WQJ7+J52oz8iWa4' + LineEnding +
    'PXpRU7QRuN2QgPcs2KN02WTGMY/yNYGOgAtNaHgie6KLUPvA4U+POxEMJBxRp3s7' + LineEnding +
    'S4lH8oKLPQKBgFoY+WDukPlCa8UpIsEgWzJwX09FpCEaJYVFFplwkTZS1Gi2Uyp+' + LineEnding +
    'a3Gx8WFMbeSLPWsnsDRfhX9/y3Uzg69We7bqboVw/uAnMORRPncIUpqI/GeJJFld' + LineEnding +
    'um3fqcNBfTsQ9X2iJUL3bK2nlxnG5C82uH5wuXWExd1MnWLHnMyD88NZAoGBANKZ' + LineEnding +
    'P3P4jKH1+aoapmoEmj3fLpa19w13+LoKPl4dcCjxjaZ6gVq8keLM0fM7WQtT5ujV' + LineEnding +
    'lT5p2Suc/UZ51XuH/uGstG7qGjXm71vSMHwQ4cRd9HPXpc9gEw9a3CsNZL4HEGV4' + LineEnding +
    'nlmcffi12mf8PyzqCxwc9zNil2JuMYQRy/wAaZAZAoGAJUHDo6YJ3jQIMrFSZLFH' + LineEnding +
    'wGKyq9QRcv1nLmCC9yEUVPXhEj3ndzRWAaMoEXljn05819ado+1TwnByv0jdjB+p' + LineEnding +
    'hj8iKf9eb8lulu6/ynrxUNYVRqh8VQ6PaOWBEAFzr1S62F1cemkdLFC2eABNPZCF' + LineEnding +
    'iYgqT0HYijo8xMi5SS4ykz8=' + LineEnding +
    '-----END PRIVATE KEY-----';
  KeyEc1 =
    '-----BEGIN EC PRIVATE KEY-----' + LineEnding +
    'MHcCAQEEIO1b5/MJkvbznq8KC351ZVuqBmYukhdvBslQDTq+3VdBoAoGCCqGSM49' + LineEnding +
    'AwEHoUQDQgAEEzpd5tG264UVcU+eneA1HyYnM7CmZxxl0eCp3yyXYWnHlOP3qmkf' + LineEnding +
    'v2teCppMUvvKhjDQA2vxK0EVvh/KO2mtWg==' + LineEnding +
    '-----END EC PRIVATE KEY-----';
  KeyEc8 =
    '-----BEGIN PRIVATE KEY-----' + LineEnding +
    'MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQg7Vvn8wmS9vOerwoL' + LineEnding +
    'fnVlW6oGZi6SF28GyVANOr7dV0GhRANCAAQTOl3m0bbrhRVxT56d4DUfJiczsKZn' + LineEnding +
    'HGXR4KnfLJdhaceU4/eqaR+/a14KmkxS+8qGMNADa/ErQRW+H8o7aa1a' + LineEnding +
    '-----END PRIVATE KEY-----';
  KeyEd8 =
    '-----BEGIN PRIVATE KEY-----' + LineEnding +
    'MC4CAQAwBQYDK2VwBCIEIGWgx54Z5Xq7S8aPzLJNudhHnTdONOd8+dRTuyNnwu2p' + LineEnding +
    '-----END PRIVATE KEY-----';

  KeyNames : Array[0..4] of String = (
    'RSA PKCS#1',
    'RSA PKCS#8',
    'EC SEC1',
    'EC PKCS#8',
    'Ed25519 PKCS#8');

var
  Errors : Integer = 0;
  TempDir : String;

// Reports a failed check
procedure Check(aCondition: Boolean; const aMsg: String);

begin
  if aCondition then
    Writeln('ok    ',aMsg)
  else
    begin
    Writeln('FAIL  ',aMsg);
    Inc(Errors);
    end;
end;


// The DER bytes of a PEM text: the base64 body between the BEGIN and END lines
function PemToDer(const aPem: String): TBytes;

var
  lLines : TStringList;
  lBody : String;
  i : Integer;

begin
  lLines:=TStringList.Create;
  try
    lLines.Text:=aPem;
    lBody:='';
    for i:=0 to lLines.Count-1 do
      if Pos('-----',lLines[i])<>1 then
        lBody:=lBody+Trim(lLines[i]);
  finally
    lLines.Free;
  end;
  Result:=BytesOf(DecodeStringBase64(lBody));
end;


// Writes aData to file aName in the temporary directory and returns its path
function WriteTempFile(const aName: String; const aData: TBytes): String;

var
  lStream : TFileStream;

begin
  Result:=IncludeTrailingPathDelimiter(TempDir)+aName;
  lStream:=TFileStream.Create(Result,fmCreate);
  try
    if Length(aData)>0 then
      lStream.WriteBuffer(aData[0],Length(aData));
  finally
    lStream.Free;
  end;
end;


// The result of TSSLContext.UsePrivateKey for aData on a new context
function LoadKey(aData: TSSLData): Integer;

var
  lContext : TSSLContext;

begin
  lContext:=TSSLContext.Create(stAny);
  try
    Result:=lContext.UsePrivateKey(aData);
  finally
    lContext.Free;
  end;
end;


// Checks the three forms of one key
procedure CheckKey(const aName, aPem: String; aIndex: Integer);

var
  lData : TSSLData;

begin
  lData:=TSSLData.Create;
  try
    lData.FileName:=WriteTempFile(Format('key%d.pem',[aIndex]),BytesOf(aPem));
    ErrClearError;
    Check(LoadKey(lData)=1,aName+' PEM file loads');
    Check(ErrGetError=0,aName+' PEM file leaves no error');
    lData.FileName:=WriteTempFile(Format('key%d.der',[aIndex]),PemToDer(aPem));
    ErrClearError;
    Check(LoadKey(lData)=1,aName+' DER file loads');
    Check(ErrGetError=0,aName+' DER file leaves no error');
    lData.FileName:='';
    lData.Value:=PemToDer(aPem);
    ErrClearError;
    Check(LoadKey(lData)=1,aName+' DER bytes load');
    Check(ErrGetError=0,aName+' DER bytes leave no error');
  finally
    lData.Free;
  end;
end;


// Checks that data that is not a key is refused
procedure CheckGarbage;

var
  lData : TSSLData;

begin
  lData:=TSSLData.Create;
  try
    lData.FileName:=WriteTempFile('garbage.key',BytesOf('This is not a private key'));
    Check(LoadKey(lData)<>1,'A file that is not a key is refused');
    lData.FileName:='';
    lData.Value:=BytesOf('This is not a private key');
    Check(LoadKey(lData)<>1,'Bytes that are not a key are refused');
  finally
    lData.Free;
  end;
  ErrClearError;
end;


var
  i : Integer;
  lKeys : Array[0..4] of String;

begin
  if not InitSSLInterface then
    begin
    Writeln('OpenSSL library not found');
    Halt(2);
    end;
  TempDir:=IncludeTrailingPathDelimiter(GetTempDir(False))+'tprivatekeys'+IntToStr(GetProcessID);
  ForceDirectories(TempDir);
  try
    lKeys[0]:=KeyRsa1;
    lKeys[1]:=KeyRsa8;
    lKeys[2]:=KeyEc1;
    lKeys[3]:=KeyEc8;
    lKeys[4]:=KeyEd8;
    for i:=Low(KeyNames) to High(KeyNames) do
      CheckKey(KeyNames[i],lKeys[i],i);
    CheckGarbage;
  finally
    for i:=Low(KeyNames) to High(KeyNames) do
      begin
      DeleteFile(IncludeTrailingPathDelimiter(TempDir)+Format('key%d.pem',[i]));
      DeleteFile(IncludeTrailingPathDelimiter(TempDir)+Format('key%d.der',[i]));
      end;
    DeleteFile(IncludeTrailingPathDelimiter(TempDir)+'garbage.key');
    RemoveDir(TempDir);
  end;
  if Errors>0 then
    begin
    Writeln(Errors,' checks failed');
    Halt(1);
    end;
  Writeln('All checks passed');
end.
