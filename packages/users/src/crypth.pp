unit crypth;

interface

uses BaseUnix,initc;

{$ifdef Linux}{$linklib crypt}{$endif}

Const libname ={$ifdef Linux}'crypt'{$else}clib{$endif};

function crypt(key,salt:pchar):pchar;cdecl; external libname name 'crypt';

implementation
end.