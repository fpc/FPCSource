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
unit float128;

  interface

    uses
      softfpu;

    operator+ (const f1,f2 : float128) result : float128;inline;
    operator* (const f1,f2 : float128) result : float128;inline;
    operator- (const f1,f2 : float128) result : float128;inline;
    operator/ (const f1,f2 : float128) result : float128;inline;

    operator :=(const source : double) dest : float128;inline;

    operator :=(const source : float128) dest : double;inline;


  implementation

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
        dest:=float64_to_float128(source);
      end;


    operator :=(const source : float128) dest : double;inline;
      begin
        dest:=float128_to_float64(source);
      end;

end.
