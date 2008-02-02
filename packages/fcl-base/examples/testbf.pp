{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ short test for blowfish unit }

{$mode objfpc}
{$h+}

program testbf;

uses blowfish;

Var
  i : integer;
  L,R : TBFBlock;
  K : TBlowFishKey;

begin
  L[0]:=$DEAD;
  L[1]:=$BEEF;
  R:=L;
  For I:=0 to 55 do
    K[i]:=Random(255)+1;
  With TBlowfish.Create(K,56) do
    Try
    Encrypt(L);
    Decrypt(L);
    If (L[0]<>R[0]) or (L[1]<>R[1]) then
      Writeln('Error');
    finally
      Free;
    end;
end.