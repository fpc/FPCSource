{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2010 by Florian Klaempfl

    This unit contain procedures specific for iso pascal mode.
    It should be platform independant.

    See the file COPYING.FPC, included in this distribution,
    For details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit iso7185;

  interface

    const
       MaxInt  = MaxLongint;

    type
      Integer = Longint;

    Procedure Rewrite(var t : Text);
    Procedure Reset(var t : Text);
    Procedure Reset(var f : TypedFile);   [INTERNPROC: fpc_in_Reset_TypedFile];
    Procedure Rewrite(var f : TypedFile); [INTERNPROC: fpc_in_Rewrite_TypedFile];

  implementation

  {$i textrec.inc}

{$i-}
    procedure DoAssign(var t : Text);
      begin
        Assign(t,'fpc_'+HexStr(random(1000000000),8)+'.tmp');
      end;


    Procedure Rewrite(var t : Text);[IOCheck];
      Begin
        { create file name? }
        if Textrec(t).mode=0 then
          DoAssign(t);

        System.Rewrite(t);
      End;


    Procedure Reset(var t : Text);[IOCheck];
      Begin
        { create file name? }
        if Textrec(t).mode=0 then
          DoAssign(t);

        System.Reset(t);
      End;

begin
  { we shouldn't do this because it might confuse user programs, but for now it
    is good enough to get pretty unique tmp file names }
  Randomize;
end.




