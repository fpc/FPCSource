unit tb0497a;

{$inline on}

interface

function compress(dest:Pchar;var destLen:cardinal; source : Pchar; sourceLen:cardinal):longint;inline;

implementation

uses tb0497b;

function compress(dest:Pchar;var destLen:cardinal; source : Pchar; sourceLen:cardinal):longint;inline;

type Pbytearray=^Tbytearray;
     Tbytearray=array[0..0] of byte;

begin
  compress:=tb0497b.compress(Pbyte(dest),destlen,Pbytearray(source)^,sourcelen);
end;

end.
