

{ defined earlier in unistdh.inc...
function crypt(__key:PAnsiChar; __salt:PAnsiChar):PAnsiChar;cdecl;external cryptlib name 'crypt';
procedure setkey(__key:PAnsiChar);cdecl;external cryptlib name 'setkey';
procedure encrypt(__block:PAnsiChar; __edflag:longint);cdecl;external cryptlib name 'encrypt';
}
type
   Pcrypt_data = ^crypt_data;
   crypt_data = record
        keysched : array[0..(16 * 8)-1] of AnsiChar;
        sb0 : array[0..32767] of AnsiChar;
        sb1 : array[0..32767] of AnsiChar;
        sb2 : array[0..32767] of AnsiChar;
        sb3 : array[0..32767] of AnsiChar;
        crypt_3_buf : array[0..13] of AnsiChar;
        current_salt : array[0..1] of AnsiChar;
        current_saltbits : longint;
        direction : longint;
        initialized : longint;
     end;

function crypt_r(__key:PAnsiChar; __salt:PAnsiChar; __data:Pcrypt_data):PAnsiChar;cdecl;external cryptlib name 'crypt_r';
procedure setkey_r(__key:PAnsiChar; __data:Pcrypt_data);cdecl;external cryptlib name 'setkey_r';
procedure encrypt_r(__block:PAnsiChar; __edflag:longint; __data:Pcrypt_data);cdecl;external cryptlib name 'encrypt_r';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TCryptData = crypt_data;
  PCryptData = ^TCryptData;


