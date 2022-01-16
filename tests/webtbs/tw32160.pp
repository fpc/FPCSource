{ %norun }

unit tw32160;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,Classes,Math;

type PPpvInt32=^PpvInt32;
     PpvInt32=^TpvInt32;
     TpvInt32={$ifdef fpc}Int32{$else}longint{$endif};

     PPpvUInt32=^PpvUInt32;
     PpvUInt32=^TpvUInt32;
     TpvUInt32={$ifdef fpc}UInt32{$else}longword{$endif};

     PPpvInt64=^PpvInt64;
     PpvInt64=^TpvInt64;
     TpvInt64=Int64;

     PPpvUInt64=^PpvUInt64;
     PpvUInt64=^TpvUInt64;
     TpvUInt64=UInt64;

     PPpvUInt128=^PpvUInt128;
     PpvUInt128=^TpvUInt128;
     TpvUInt128=packed record
      public
       class operator IntDivide(const a:TpvUInt128;const b:TpvUInt64):TpvUInt128;
{$ifdef BIG_ENDIAN}
       case byte of
        0:(
         Hi,Lo:TpvUInt64;
        );
        1:(
         Q3,Q2,Q1,Q0:TpvUInt32;
        );
{$else}
       case byte of
        0:(
         Lo,Hi:TpvUInt64;
        );
        1:(
         Q0,Q1,Q2,Q3:TpvUInt32;
        );
 {$endif}
     end;

implementation

class operator TpvUInt128.IntDivide(const a:TpvUInt128;const b:TpvUInt64):TpvUInt128;
var Quotient:TpvUInt128;
    Remainder:TpvUInt64;
    Bit:TpvInt32;
    Dividend:TpvUInt128 absolute a;
    Divisor:TpvUInt64 absolute b;
begin
 Quotient:=Dividend;
 Remainder:=0;
 for Bit:=1 to 128 do begin
  Remainder:=(Remainder shl 1) or (ord((Quotient.Hi and $8000000000000000)<>0) and 1);
  Quotient.Hi:=(Quotient.Hi shl 1) or (Quotient.Lo shr 63);
  Quotient.Lo:=Quotient.Lo shl 1;
  if (TpvUInt32(Remainder shr 32)>TpvUInt32(Divisor shr 32)) or
     ((TpvUInt32(Remainder shr 32)=TpvUInt32(Divisor shr 32)) and (TpvUInt32(Remainder and $ffffffff)>=TpvUInt32(Divisor and $ffffffff))) then begin
   dec(Remainder,Divisor);
   Quotient.Lo:=Quotient.Lo or 1;
  end;
 end;
 result:=Quotient;
end;

end.
