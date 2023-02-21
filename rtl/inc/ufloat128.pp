{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by the FPC development time

    Implements overloaded operators and misc. functions to
    provide a float128 type

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$inline on}
{$IFNDEF FPC_DOTTEDUNITS}
unit ufloat128;
{$ENDIF FPC_DOTTEDUNITS}

  interface

{$IFDEF FPC_DOTTEDUNITS}
    uses
      System.SoftFpu128;
{$ELSE FPC_DOTTEDUNITS}
    uses
      sfpu128;
{$ENDIF FPC_DOTTEDUNITS}

    type
      float128 = {$IFDEF FPC_DOTTEDUNITS}System.SoftFpu128{$ELSE}sfpu128{$ENDIF}.float128;

    operator+ (const f1,f2 : float128) result : float128;inline;
    operator* (const f1,f2 : float128) result : float128;inline;
    operator- (const f1,f2 : float128) result : float128;inline;
    operator/ (const f1,f2 : float128) result : float128;inline;

    operator :=(const source : double) dest : float128;inline;

    operator :=(const source : float128) dest : double;inline;

    procedure DumpFloat128(const f : float128);

  implementation

    procedure DumpFloat128(const f : float128);
      type
        ta = packed array[0..SizeOf(float128)-1] of byte;
      var
        i : longint;
      begin
        for i:=SizeOf(float128)-1 downto 0 do
          begin
            write(hexstr(ta(f)[i],2));
            if i<15 then
              write(' ');
          end;
      end;


    operator+ (const f1,f2 : float128) result : float128;inline;
      begin
        result:=float128_add(f1,f2);
      end;


    operator* (const f1,f2 : float128) result : float128;inline;
      begin
        result:=float128_mul(f1,f2);
      end;


    operator- (const f1,f2 : float128) result : float128;inline;
      begin
        result:=float128_sub(f1,f2);
      end;


    operator/ (const f1,f2 : float128) result : float128;inline;
      begin
        result:=float128_div(f1,f2);
      end;


    operator :=(const source : double) dest : float128;inline;
      begin
        dest:=float64_to_float128(float64(source));
      end;


    operator :=(const source : float128) dest : double;inline;
      begin
        dest:=double(float128_to_float64(source));
      end;


end.
