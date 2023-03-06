{$IFNDEF FPC_DOTTEDUNITS}
unit crypth;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses UnixApi.Base,System.InitC;
{$ELSE FPC_DOTTEDUNITS}
uses BaseUnix,initc;
{$ENDIF FPC_DOTTEDUNITS}

{$ifdef Linux}{$linklib crypt}{$endif}

Const libname ={$ifdef Linux}'crypt'{$else}clib{$endif};

function crypt(key,salt:PAnsiChar):PAnsiChar;cdecl; external libname name 'crypt';

implementation
end.
