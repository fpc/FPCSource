{
    Copyright (c) 2022 by Free Pascal development team

    GEM interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
    Shared types between aes/vdi/gem.
    Only type declarations should go here.
}

{$MODE FPC}
{$MODESWITCH OUT+}
{$PACKRECORDS 2}

{$IFNDEF FPC_DOTTEDUNITS}
unit gemcmmn;
{$ENDIF FPC_DOTTEDUNITS}

interface

type
    ARRAY_8     = ARRAY[0..7] of smallint;

{ AES/VDI mouse form structure }
type
  PMFORM = ^TMFORM;
  TMFORM = record
    mf_xhot: smallint;       {* X-position hot-spot *}
    mf_yhot: smallint;       {* Y-position hot-spot *}
    mf_nplanes: smallint;    {* Number of planes    *}
    mf_fg: smallint;         {* Mask colour         *}
    mf_bg: smallint;         {* Pointer colour      *}
    mf_mask: array[0..15] of smallint;   {* Mask form           *}
    mf_data: array[0..15] of smallint;   {* Pointer form        *}
  end;

implementation

end.
