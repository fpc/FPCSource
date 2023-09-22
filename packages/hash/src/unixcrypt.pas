{$IFNDEF FPC_DOTTEDUNITS}
unit unixcrypt;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}
{$linklib crypt}
{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes;
{$ENDIF FPC_DOTTEDUNITS}

function crypt(const key: PAnsiChar; const salt: PAnsiChar): PAnsiChar; cdecl; external;

// salt helper functions
function gen_des_salt: RawByteString;
function gen_md5_salt: RawByteString;

// crypt helper functions
function crypt_password(const key: RawByteString; const UseMD5: boolean): RawByteString;
function validate_password(const key: RawByteString; const hash: RawByteString): boolean;

implementation

const
  salt_chars: array[0..63] of AnsiChar = (
    'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
    'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
    '0','1','2','3','4','5','6','7','8','9','.','/');

function gen_des_salt: RawByteString;
begin
  Result := salt_chars[Random(64)] + salt_chars[Random(64)];
end;

function gen_md5_salt: RawByteString;
var
  i: integer;
begin
  Result := '$1$';
  for i := 0 to 7 do
    Result := Result + salt_chars[Random(64)];
end;

function crypt_password(const key: RawByteString; const UseMD5: boolean): RawByteString;
begin
  if UseMD5 then
    Result := crypt(PAnsiChar(key), PAnsiChar(gen_md5_salt)) else
    Result := crypt(PAnsiChar(key), PAnsiChar(gen_des_salt));
end;

function validate_password(const key: RawByteString; const hash: RawByteString): boolean;
begin
  Result :=
  // MD5 compare
    ((Length(hash) = 34) and (hash[1] = '$') and (hash[2] = '1') and (hash[3] = '$') and (hash[12] = '$') and (crypt(PAnsiChar(key), PAnsiChar(copy(hash, 1, 11))) = hash)) or
  // DES compare
    ((Length(hash) = 13) and (crypt(PAnsiChar(key), PAnsiChar(copy(hash, 1, 2))) = hash));
end;

end.
