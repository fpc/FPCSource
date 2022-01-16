{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by the FPC development time

    Implements overloaded operators and misc. functions to
    provide a floatx80 type

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$inline on}
unit ufloatx80;

  interface

    uses
      sfpux80;

    type
      floatx80 = sfpux80.floatx80;

    operator+ (const f1,f2 : floatx80) result : floatx80;inline;
    operator* (const f1,f2 : floatx80) result : floatx80;inline;
    operator- (const f1,f2 : floatx80) result : floatx80;inline;
    operator/ (const f1,f2 : floatx80) result : floatx80;inline;

    operator :=(const source : double) dest : floatx80;inline;

    operator :=(const source : floatx80) dest : double;inline;

    procedure DumpFloatx80(const f : floatx80);

  implementation

    procedure DumpFloatx80(const f : floatx80);
      type
        ta = packed array[0..SizeOf(floatx80)-1] of byte;
      var
        i : longint;
      begin
        for i:=SizeOf(floatx80)-1 downto 0 do
          begin
            write(hexstr(ta(f)[i],2));
            if i<15 then
              write(' ');
          end;
      end;


    operator+ (const f1,f2 : floatx80) result : floatx80;inline;
      begin
        result:=floatx80_add(f1,f2);
      end;


    operator* (const f1,f2 : floatx80) result : floatx80;inline;
      begin
        result:=floatx80_mul(f1,f2);
      end;


    operator- (const f1,f2 : floatx80) result : floatx80;inline;
      begin
        result:=floatx80_sub(f1,f2);
      end;


    operator/ (const f1,f2 : floatx80) result : floatx80;inline;
      begin
        result:=floatx80_div(f1,f2);
      end;


    operator :=(const source : double) dest : floatx80;inline;
      begin
        dest:=float64_to_floatx80(float64(source));
      end;


    operator :=(const source : floatx80) dest : double;inline;
      begin
        dest:=double(floatx80_to_float64(source));
      end;


end.
