{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2007 by Several contributors

    Generic mathematical routines (on type currency)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$ifdef FPC_CURRENCY_IS_INT64}

    function trunc(c : currency) : int64;
      type
        tmyrec = record
          i: int64;
        end;
      begin
        result := int64(tmyrec(c)) div 10000
      end;


    function trunc(c : comp) : int64;
      begin
        result := c
      end;


    function round(c : currency) : int64;
      type
        tmyrec = record
          i: int64;
        end;
      var
        rem, absrem: longint;
      begin
        { (int64(tmyrec(c))(+/-)5000) div 10000 can overflow }
        result := int64(tmyrec(c)) div 10000;
        rem := int64(tmyrec(c)) - result * 10000;
        absrem := abs(rem);
        if (absrem > 5000) or
           ((absrem = 5000) and
            (rem > 0)) then
          if (rem > 0) then
            inc(result)
          else
            dec(result);
      end;


    function round(c : comp) : int64;
      begin
        result := c
      end;

{$endif FPC_CURRENCY_IS_INT64}




