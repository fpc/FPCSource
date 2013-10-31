{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2012 Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

function X11ConvertKeySymToUnicode(sym: TKeySym): Integer;
begin
  if (sym >= 0) and (sym <= $FF) then
    exit(sym);  { ascii or latin-1 }

  if (sym >= $1000100) and (sym <= $110FFFF) then
    exit(sym - $1000000);

  case sym of
    XK_BackSpace: exit(8);
    XK_Tab: exit(9);
    XK_Return: exit(13);
    XK_KP_Enter: exit(13);
    XK_KP_Multiply: exit($2A);
    XK_KP_Divide: exit($2F);
    XK_KP_Add: exit($2B);
    XK_KP_Subtract: exit($2D);
    XK_KP_Decimal: exit($2E);
    XK_KP_0: exit($30);
    XK_KP_1: exit($31);
    XK_KP_2: exit($32);
    XK_KP_3: exit($33);
    XK_KP_4: exit($34);
    XK_KP_5: exit($35);
    XK_KP_6: exit($36);
    XK_KP_7: exit($37);
    XK_KP_8: exit($38);
    XK_KP_9: exit($39);
    XK_Escape: exit(27);
  End;
  case sym of
    XKc_Cyrillic_GHE_bar     : exit($492);
    XK_Cyrillic_ghe_bar      : exit($493);
    XKc_Cyrillic_ZHE_descender: exit($496);
    XK_Cyrillic_zhe_descender: exit($497);
    XKc_Cyrillic_KA_descender: exit($49A);
    XK_Cyrillic_ka_descender : exit($49B);
    XKc_Cyrillic_KA_vertstroke: exit($49C);
    XK_Cyrillic_ka_vertstroke: exit($49D);
    XKc_Cyrillic_EN_descender: exit($4A2);
    XK_Cyrillic_en_descender : exit($4A3);
    XKc_Cyrillic_U_straight  : exit($4AE);
    XK_Cyrillic_u_straight   : exit($4AF);
    XKc_Cyrillic_U_straight_bar: exit($4B0);
    XK_Cyrillic_u_straight_bar: exit($4B1);
    XKc_Cyrillic_HA_descender: exit($4B2);
    XK_Cyrillic_ha_descender : exit($4B3);
    XKc_Cyrillic_CHE_descender: exit($4B6);
    XK_Cyrillic_che_descender: exit($4B7);
    XKc_Cyrillic_CHE_vertstroke: exit($4B8);
    XK_Cyrillic_che_vertstroke: exit($4B9);
    XKc_Cyrillic_SHHA        : exit($4BA);
    XK_Cyrillic_shha         : exit($4BB);

    XKc_Cyrillic_SCHWA       : exit($4D8);
    XK_Cyrillic_schwa        : exit($4D9);
    XKc_Cyrillic_I_macron    : exit($4E2);
    XK_Cyrillic_i_macron     : exit($4E3);
    XKc_Cyrillic_O_bar       : exit($4E8);
    XK_Cyrillic_o_bar        : exit($4E9);
    XKc_Cyrillic_U_macron    : exit($4EE);
    XK_Cyrillic_u_macron     : exit($4EF);

    XK_Serbian_dje           : exit($452);
    XK_Macedonia_gje         : exit($453);
    XK_Cyrillic_io           : exit($451);
    XK_Ukrainian_ie          : exit($454);
    XK_Macedonia_dse         : exit($455);
    XK_Ukrainian_i           : exit($456);
    XK_Ukrainian_yi          : exit($457);
    XK_Cyrillic_je           : exit($458);
    XK_Cyrillic_lje          : exit($459);
    XK_Cyrillic_nje          : exit($45A);
    XK_Serbian_tshe          : exit($45B);
    XK_Macedonia_kje         : exit($45C);
    XK_Ukrainian_ghe_with_upturn: exit($491);
    XK_Byelorussian_shortu   : exit($45E);
    XK_Cyrillic_dzhe         : exit($45F);
    XK_numerosign            : exit($2116);
    XKc_Serbian_DJE          : exit($402);
    XKc_Macedonia_GJE        : exit($403);
    XKc_Cyrillic_IO          : exit($401);
    XKc_Ukrainian_IE         : exit($404);
    XKc_Macedonia_DSE        : exit($405);
    XKc_Ukrainian_I          : exit($406);
    XKc_Ukrainian_YI         : exit($407);
    XKc_Cyrillic_JE          : exit($408);
    XKc_Cyrillic_LJE         : exit($409);
    XKc_Cyrillic_NJE         : exit($40A);
    XKc_Serbian_TSHE         : exit($40B);
    XKc_Macedonia_KJE        : exit($40C);
    XKc_Ukrainian_GHE_WITH_UPTURN: exit($490);
    XKc_Byelorussian_SHORTU  : exit($40E);
    XKc_Cyrillic_DZHE        : exit($40F);
    XK_Cyrillic_yu           : exit($44E);
    XK_Cyrillic_a            : exit($430);
    XK_Cyrillic_be           : exit($431);
    XK_Cyrillic_tse          : exit($446);
    XK_Cyrillic_de           : exit($434);
    XK_Cyrillic_ie           : exit($435);
    XK_Cyrillic_ef           : exit($444);
    XK_Cyrillic_ghe          : exit($433);
    XK_Cyrillic_ha           : exit($445);
    XK_Cyrillic_i            : exit($438);
    XK_Cyrillic_shorti       : exit($439);
    XK_Cyrillic_ka           : exit($43A);
    XK_Cyrillic_el           : exit($43B);
    XK_Cyrillic_em           : exit($43C);
    XK_Cyrillic_en           : exit($43D);
    XK_Cyrillic_o            : exit($43E);
    XK_Cyrillic_pe           : exit($43F);
    XK_Cyrillic_ya           : exit($44F);
    XK_Cyrillic_er           : exit($440);
    XK_Cyrillic_es           : exit($441);
    XK_Cyrillic_te           : exit($442);
    XK_Cyrillic_u            : exit($443);
    XK_Cyrillic_zhe          : exit($436);
    XK_Cyrillic_ve           : exit($432);
    XK_Cyrillic_softsign     : exit($44C);
    XK_Cyrillic_yeru         : exit($44B);
    XK_Cyrillic_ze           : exit($437);
    XK_Cyrillic_sha          : exit($448);
    XK_Cyrillic_e            : exit($44D);
    XK_Cyrillic_shcha        : exit($449);
    XK_Cyrillic_che          : exit($447);
    XK_Cyrillic_hardsign     : exit($44A);
    XKc_Cyrillic_YU          : exit($42E);
    XKc_Cyrillic_A           : exit($410);
    XKc_Cyrillic_BE          : exit($411);
    XKc_Cyrillic_TSE         : exit($426);
    XKc_Cyrillic_DE          : exit($414);
    XKc_Cyrillic_IE          : exit($415);
    XKc_Cyrillic_EF          : exit($424);
    XKc_Cyrillic_GHE         : exit($413);
    XKc_Cyrillic_HA          : exit($425);
    XKc_Cyrillic_I           : exit($418);
    XKc_Cyrillic_SHORTI      : exit($419);
    XKc_Cyrillic_KA          : exit($41A);
    XKc_Cyrillic_EL          : exit($41B);
    XKc_Cyrillic_EM          : exit($41C);
    XKc_Cyrillic_EN          : exit($41D);
    XKc_Cyrillic_O           : exit($41E);
    XKc_Cyrillic_PE          : exit($41F);
    XKc_Cyrillic_YA          : exit($42F);
    XKc_Cyrillic_ER          : exit($420);
    XKc_Cyrillic_ES          : exit($421);
    XKc_Cyrillic_TE          : exit($422);
    XKc_Cyrillic_U           : exit($423);
    XKc_Cyrillic_ZHE         : exit($416);
    XKc_Cyrillic_VE          : exit($412);
    XKc_Cyrillic_SOFTSIGN    : exit($42C);
    XKc_Cyrillic_YERU        : exit($42B);
    XKc_Cyrillic_ZE          : exit($417);
    XKc_Cyrillic_SHA         : exit($428);
    XKc_Cyrillic_E           : exit($42D);
    XKc_Cyrillic_SHCHA       : exit($429);
    XKc_Cyrillic_CHE         : exit($427);
    XKc_Cyrillic_HARDSIGN    : exit($42A);

{    XKc_Greek_ALPHAaccent    : exit($);
    XKc_Greek_EPSILONaccent  : exit($);
    XKc_Greek_ETAaccent      : exit($);
    XKc_Greek_IOTAaccent     : exit($);
    XKc_Greek_IOTAdieresis   : exit($);
    XKc_Greek_OMICRONaccent  : exit($);
    XKc_Greek_UPSILONaccent  : exit($);
    XKc_Greek_UPSILONdieresis: exit($);
    XKc_Greek_OMEGAaccent    : exit($);
    XK_Greek_accentdieresis  : exit($);
    XK_Greek_horizbar        : exit($);
    XK_Greek_alphaaccent     : exit($);
    XK_Greek_epsilonaccent   : exit($);
    XK_Greek_etaaccent       : exit($);
    XK_Greek_iotaaccent      : exit($);
    XK_Greek_iotadieresis    : exit($);
    XK_Greek_iotaaccentdieresis: exit($);
    XK_Greek_omicronaccent   : exit($);
    XK_Greek_upsilonaccent   : exit($);
    XK_Greek_upsilondieresis : exit($);
    XK_Greek_upsilonaccentdieresis: exit($);
    XK_Greek_omegaaccent     : exit($);}
    XKc_Greek_ALPHA          : exit($391);
    XKc_Greek_BETA           : exit($392);
    XKc_Greek_GAMMA          : exit($393);
    XKc_Greek_DELTA          : exit($394);
    XKc_Greek_EPSILON        : exit($395);
    XKc_Greek_ZETA           : exit($396);
    XKc_Greek_ETA            : exit($397);
    XKc_Greek_THETA          : exit($398);
    XKc_Greek_IOTA           : exit($399);
    XKc_Greek_KAPPA          : exit($39A);
    XKc_Greek_LAMDA          : exit($39B);
    XKc_Greek_MU             : exit($39C);
    XKc_Greek_NU             : exit($39D);
    XKc_Greek_XI             : exit($39E);
    XKc_Greek_OMICRON        : exit($39F);
    XKc_Greek_PI             : exit($3A0);
    XKc_Greek_RHO            : exit($3A1);
    XKc_Greek_SIGMA          : exit($3A3);
    XKc_Greek_TAU            : exit($3A4);
    XKc_Greek_UPSILON        : exit($3A5);
    XKc_Greek_PHI            : exit($3A6);
    XKc_Greek_CHI            : exit($3A7);
    XKc_Greek_PSI            : exit($3A8);
    XKc_Greek_OMEGA          : exit($3A9);
    XK_Greek_alpha           : exit($3B1);
    XK_Greek_beta            : exit($3B2);
    XK_Greek_gamma           : exit($3B3);
    XK_Greek_delta           : exit($3B4);
    XK_Greek_epsilon         : exit($3B5);
    XK_Greek_zeta            : exit($3B6);
    XK_Greek_eta             : exit($3B7);
    XK_Greek_theta           : exit($3B8);
    XK_Greek_iota            : exit($3B9);
    XK_Greek_kappa           : exit($3BA);
    XK_Greek_lamda           : exit($3BB);
    XK_Greek_mu              : exit($3BC);
    XK_Greek_nu              : exit($3BD);
    XK_Greek_xi              : exit($3BE);
    XK_Greek_omicron         : exit($3BF);
    XK_Greek_pi              : exit($3C0);
    XK_Greek_rho             : exit($3C1);
    XK_Greek_sigma           : exit($3C2);
    XK_Greek_finalsmallsigma : exit($3C3);
    XK_Greek_tau             : exit($3C4);
    XK_Greek_upsilon         : exit($3C5);
    XK_Greek_phi             : exit($3C6);
    XK_Greek_chi             : exit($3C7);
    XK_Greek_psi             : exit($3C8);
    XK_Greek_omega           : exit($3C9);

  end;
  X11ConvertKeySymToUnicode := -1;
end;
