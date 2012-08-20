{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifdef FPC_CURRENCY_IS_INT64}
    function trunc(c : currency) : int64;
{$ifndef FPUNONE}
    function round(c : currency) : int64;
{$endif FPUNONE}
{$ifndef cpujvm}
    function trunc(c : comp) : int64;
    function round(c : comp) : int64;
{$else not cpujvm}
    function trunc_comp(c: comp) : int64;
    function round_comp(c : comp) : int64;
{$endif not cpujvm}
{$endif FPC_CURRENCY_IS_INT64}


