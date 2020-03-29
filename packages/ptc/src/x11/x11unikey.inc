{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2012,2017 Nikolay Nikolov (nickysn@users.sourceforge.net)
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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

function X11IsDeadKey(sym: TKeySym): Boolean;
begin
  case sym of
    XK_dead_grave, XK_dead_acute, XK_dead_circumflex,
    XK_dead_tilde, XK_dead_macron, XK_dead_breve, XK_dead_abovedot,
    XK_dead_diaeresis, XK_dead_abovering, XK_dead_doubleacute, XK_dead_caron,
    XK_dead_cedilla, XK_dead_ogonek, XK_dead_iota, XK_dead_voiced_sound,
    XK_dead_semivoiced_sound, XK_dead_belowdot, XK_dead_hook, XK_dead_horn,
    XK_dead_stroke, XK_dead_abovecomma, XK_dead_abovereversedcomma,
    XK_dead_doublegrave, XK_dead_belowring, XK_dead_belowmacron,
    XK_dead_belowcircumflex, XK_dead_belowtilde, XK_dead_belowbreve,
    XK_dead_belowdiaeresis, XK_dead_invertedbreve, XK_dead_belowcomma,
    XK_dead_currency:
      Result := True;
    else
      Result := False;
  end;
end;

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
    { Latin 2 }
    XKc_Aogonek      : exit($0104);  { U+0104 LATIN CAPITAL LETTER A WITH OGONEK }
    XK_breve         : exit($02D8);  { U+02D8 BREVE }
    XKc_Lstroke      : exit($0141);  { U+0141 LATIN CAPITAL LETTER L WITH STROKE }
    XKc_Lcaron       : exit($013D);  { U+013D LATIN CAPITAL LETTER L WITH CARON }
    XKc_Sacute       : exit($015A);  { U+015A LATIN CAPITAL LETTER S WITH ACUTE }
    XKc_Scaron       : exit($0160);  { U+0160 LATIN CAPITAL LETTER S WITH CARON }
    XKc_Scedilla     : exit($015E);  { U+015E LATIN CAPITAL LETTER S WITH CEDILLA }
    XKc_Tcaron       : exit($0164);  { U+0164 LATIN CAPITAL LETTER T WITH CARON }
    XKc_Zacute       : exit($0179);  { U+0179 LATIN CAPITAL LETTER Z WITH ACUTE }
    XKc_Zcaron       : exit($017D);  { U+017D LATIN CAPITAL LETTER Z WITH CARON }
    XKc_Zabovedot    : exit($017B);  { U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE }
    XK_aogonek       : exit($0105);  { U+0105 LATIN SMALL LETTER A WITH OGONEK }
    XK_ogonek        : exit($02DB);  { U+02DB OGONEK }
    XK_lstroke       : exit($0142);  { U+0142 LATIN SMALL LETTER L WITH STROKE }
    XK_lcaron        : exit($013E);  { U+013E LATIN SMALL LETTER L WITH CARON }
    XK_sacute        : exit($015B);  { U+015B LATIN SMALL LETTER S WITH ACUTE }
    XK_caron         : exit($02C7);  { U+02C7 CARON }
    XK_scaron        : exit($0161);  { U+0161 LATIN SMALL LETTER S WITH CARON }
    XK_scedilla      : exit($015F);  { U+015F LATIN SMALL LETTER S WITH CEDILLA }
    XK_tcaron        : exit($0165);  { U+0165 LATIN SMALL LETTER T WITH CARON }
    XK_zacute        : exit($017A);  { U+017A LATIN SMALL LETTER Z WITH ACUTE }
    XK_doubleacute   : exit($02DD);  { U+02DD DOUBLE ACUTE ACCENT }
    XK_zcaron        : exit($017E);  { U+017E LATIN SMALL LETTER Z WITH CARON }
    XK_zabovedot     : exit($017C);  { U+017C LATIN SMALL LETTER Z WITH DOT ABOVE }
    XKc_Racute       : exit($0154);  { U+0154 LATIN CAPITAL LETTER R WITH ACUTE }
    XKc_Abreve       : exit($0102);  { U+0102 LATIN CAPITAL LETTER A WITH BREVE }
    XKc_Lacute       : exit($0139);  { U+0139 LATIN CAPITAL LETTER L WITH ACUTE }
    XKc_Cacute       : exit($0106);  { U+0106 LATIN CAPITAL LETTER C WITH ACUTE }
    XKc_Ccaron       : exit($010C);  { U+010C LATIN CAPITAL LETTER C WITH CARON }
    XKc_Eogonek      : exit($0118);  { U+0118 LATIN CAPITAL LETTER E WITH OGONEK }
    XKc_Ecaron       : exit($011A);  { U+011A LATIN CAPITAL LETTER E WITH CARON }
    XKc_Dcaron       : exit($010E);  { U+010E LATIN CAPITAL LETTER D WITH CARON }
    XKc_Dstroke      : exit($0110);  { U+0110 LATIN CAPITAL LETTER D WITH STROKE }
    XKc_Nacute       : exit($0143);  { U+0143 LATIN CAPITAL LETTER N WITH ACUTE }
    XKc_Ncaron       : exit($0147);  { U+0147 LATIN CAPITAL LETTER N WITH CARON }
    XKc_Odoubleacute : exit($0150);  { U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE }
    XKc_Rcaron       : exit($0158);  { U+0158 LATIN CAPITAL LETTER R WITH CARON }
    XKc_Uring        : exit($016E);  { U+016E LATIN CAPITAL LETTER U WITH RING ABOVE }
    XKc_Udoubleacute : exit($0170);  { U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE }
    XKc_Tcedilla     : exit($0162);  { U+0162 LATIN CAPITAL LETTER T WITH CEDILLA }
    XK_racute        : exit($0155);  { U+0155 LATIN SMALL LETTER R WITH ACUTE }
    XK_abreve        : exit($0103);  { U+0103 LATIN SMALL LETTER A WITH BREVE }
    XK_lacute        : exit($013A);  { U+013A LATIN SMALL LETTER L WITH ACUTE }
    XK_cacute        : exit($0107);  { U+0107 LATIN SMALL LETTER C WITH ACUTE }
    XK_ccaron        : exit($010D);  { U+010D LATIN SMALL LETTER C WITH CARON }
    XK_eogonek       : exit($0119);  { U+0119 LATIN SMALL LETTER E WITH OGONEK }
    XK_ecaron        : exit($011B);  { U+011B LATIN SMALL LETTER E WITH CARON }
    XK_dcaron        : exit($010F);  { U+010F LATIN SMALL LETTER D WITH CARON }
    XK_dstroke       : exit($0111);  { U+0111 LATIN SMALL LETTER D WITH STROKE }
    XK_nacute        : exit($0144);  { U+0144 LATIN SMALL LETTER N WITH ACUTE }
    XK_ncaron        : exit($0148);  { U+0148 LATIN SMALL LETTER N WITH CARON }
    XK_odoubleacute  : exit($0151);  { U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE }
    XK_rcaron        : exit($0159);  { U+0159 LATIN SMALL LETTER R WITH CARON }
    XK_uring         : exit($016F);  { U+016F LATIN SMALL LETTER U WITH RING ABOVE }
    XK_udoubleacute  : exit($0171);  { U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE }
    XK_tcedilla      : exit($0163);  { U+0163 LATIN SMALL LETTER T WITH CEDILLA }
    XK_abovedot      : exit($02D9);  { U+02D9 DOT ABOVE }

    { Latin 3 }
    XKc_Hstroke     : exit($0126);  { U+0126 LATIN CAPITAL LETTER H WITH STROKE }
    XKc_Hcircumflex : exit($0124);  { U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX }
    XKc_Iabovedot   : exit($0130);  { U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE }
    XKc_Gbreve      : exit($011E);  { U+011E LATIN CAPITAL LETTER G WITH BREVE }
    XKc_Jcircumflex : exit($0134);  { U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX }
    XK_hstroke      : exit($0127);  { U+0127 LATIN SMALL LETTER H WITH STROKE }
    XK_hcircumflex  : exit($0125);  { U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX }
    XK_idotless     : exit($0131);  { U+0131 LATIN SMALL LETTER DOTLESS I }
    XK_gbreve       : exit($011F);  { U+011F LATIN SMALL LETTER G WITH BREVE }
    XK_jcircumflex  : exit($0135);  { U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX }
    XKc_Cabovedot   : exit($010A);  { U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE }
    XKc_Ccircumflex : exit($0108);  { U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX }
    XKc_Gabovedot   : exit($0120);  { U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE }
    XKc_Gcircumflex : exit($011C);  { U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX }
    XKc_Ubreve      : exit($016C);  { U+016C LATIN CAPITAL LETTER U WITH BREVE }
    XKc_Scircumflex : exit($015C);  { U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX }
    XK_cabovedot    : exit($010B);  { U+010B LATIN SMALL LETTER C WITH DOT ABOVE }
    XK_ccircumflex  : exit($0109);  { U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX }
    XK_gabovedot    : exit($0121);  { U+0121 LATIN SMALL LETTER G WITH DOT ABOVE }
    XK_gcircumflex  : exit($011D);  { U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX }
    XK_ubreve       : exit($016D);  { U+016D LATIN SMALL LETTER U WITH BREVE }
    XK_scircumflex  : exit($015D);  { U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX }

    { Latin 4 }
    XK_kra        : exit($0138);  { U+0138 LATIN SMALL LETTER KRA }
    XKc_Rcedilla  : exit($0156);  { U+0156 LATIN CAPITAL LETTER R WITH CEDILLA }
    XKc_Itilde    : exit($0128);  { U+0128 LATIN CAPITAL LETTER I WITH TILDE }
    XKc_Lcedilla  : exit($013B);  { U+013B LATIN CAPITAL LETTER L WITH CEDILLA }
    XKc_Emacron   : exit($0112);  { U+0112 LATIN CAPITAL LETTER E WITH MACRON }
    XKc_Gcedilla  : exit($0122);  { U+0122 LATIN CAPITAL LETTER G WITH CEDILLA }
    XKc_Tslash    : exit($0166);  { U+0166 LATIN CAPITAL LETTER T WITH STROKE }
    XK_rcedilla   : exit($0157);  { U+0157 LATIN SMALL LETTER R WITH CEDILLA }
    XK_itilde     : exit($0129);  { U+0129 LATIN SMALL LETTER I WITH TILDE }
    XK_lcedilla   : exit($013C);  { U+013C LATIN SMALL LETTER L WITH CEDILLA }
    XK_emacron    : exit($0113);  { U+0113 LATIN SMALL LETTER E WITH MACRON }
    XK_gcedilla   : exit($0123);  { U+0123 LATIN SMALL LETTER G WITH CEDILLA }
    XK_tslash     : exit($0167);  { U+0167 LATIN SMALL LETTER T WITH STROKE }
    XKc_ENG       : exit($014A);  { U+014A LATIN CAPITAL LETTER ENG }
    XK_eng        : exit($014B);  { U+014B LATIN SMALL LETTER ENG }
    XKc_Amacron   : exit($0100);  { U+0100 LATIN CAPITAL LETTER A WITH MACRON }
    XKc_Iogonek   : exit($012E);  { U+012E LATIN CAPITAL LETTER I WITH OGONEK }
    XKc_Eabovedot : exit($0116);  { U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE }
    XKc_Imacron   : exit($012A);  { U+012A LATIN CAPITAL LETTER I WITH MACRON }
    XKc_Ncedilla  : exit($0145);  { U+0145 LATIN CAPITAL LETTER N WITH CEDILLA }
    XKc_Omacron   : exit($014C);  { U+014C LATIN CAPITAL LETTER O WITH MACRON }
    XKc_Kcedilla  : exit($0136);  { U+0136 LATIN CAPITAL LETTER K WITH CEDILLA }
    XKc_Uogonek   : exit($0172);  { U+0172 LATIN CAPITAL LETTER U WITH OGONEK }
    XKc_Utilde    : exit($0168);  { U+0168 LATIN CAPITAL LETTER U WITH TILDE }
    XKc_Umacron   : exit($016A);  { U+016A LATIN CAPITAL LETTER U WITH MACRON }
    XK_amacron    : exit($0101);  { U+0101 LATIN SMALL LETTER A WITH MACRON }
    XK_iogonek    : exit($012F);  { U+012F LATIN SMALL LETTER I WITH OGONEK }
    XK_eabovedot  : exit($0117);  { U+0117 LATIN SMALL LETTER E WITH DOT ABOVE }
    XK_imacron    : exit($012B);  { U+012B LATIN SMALL LETTER I WITH MACRON }
    XK_ncedilla   : exit($0146);  { U+0146 LATIN SMALL LETTER N WITH CEDILLA }
    XK_omacron    : exit($014D);  { U+014D LATIN SMALL LETTER O WITH MACRON }
    XK_kcedilla   : exit($0137);  { U+0137 LATIN SMALL LETTER K WITH CEDILLA }
    XK_uogonek    : exit($0173);  { U+0173 LATIN SMALL LETTER U WITH OGONEK }
    XK_utilde     : exit($0169);  { U+0169 LATIN SMALL LETTER U WITH TILDE }
    XK_umacron    : exit($016B);  { U+016B LATIN SMALL LETTER U WITH MACRON }

    { Latin 9 }
    XKc_OE         : exit($0152);  { U+0152 LATIN CAPITAL LIGATURE OE }
    XK_oe          : exit($0153);  { U+0153 LATIN SMALL LIGATURE OE }
    XKc_Ydiaeresis : exit($0178);  { U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS }

    { Katakana }
    XK_overline            : exit($203E);  { U+203E OVERLINE }
    XK_kana_fullstop       : exit($3002);  { U+3002 IDEOGRAPHIC FULL STOP }
    XK_kana_openingbracket : exit($300C);  { U+300C LEFT CORNER BRACKET }
    XK_kana_closingbracket : exit($300D);  { U+300D RIGHT CORNER BRACKET }
    XK_kana_comma          : exit($3001);  { U+3001 IDEOGRAPHIC COMMA }
    XK_kana_conjunctive    : exit($30FB);  { U+30FB KATAKANA MIDDLE DOT }
    XKc_kana_WO            : exit($30F2);  { U+30F2 KATAKANA LETTER WO }
    XK_kana_a              : exit($30A1);  { U+30A1 KATAKANA LETTER SMALL A }
    XK_kana_i              : exit($30A3);  { U+30A3 KATAKANA LETTER SMALL I }
    XK_kana_u              : exit($30A5);  { U+30A5 KATAKANA LETTER SMALL U }
    XK_kana_e              : exit($30A7);  { U+30A7 KATAKANA LETTER SMALL E }
    XK_kana_o              : exit($30A9);  { U+30A9 KATAKANA LETTER SMALL O }
    XK_kana_ya             : exit($30E3);  { U+30E3 KATAKANA LETTER SMALL YA }
    XK_kana_yu             : exit($30E5);  { U+30E5 KATAKANA LETTER SMALL YU }
    XK_kana_yo             : exit($30E7);  { U+30E7 KATAKANA LETTER SMALL YO }
    XK_kana_tsu            : exit($30C3);  { U+30C3 KATAKANA LETTER SMALL TU }
    XK_prolongedsound      : exit($30FC);  { U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK }
    XKc_kana_A             : exit($30A2);  { U+30A2 KATAKANA LETTER A }
    XKc_kana_I             : exit($30A4);  { U+30A4 KATAKANA LETTER I }
    XKc_kana_U             : exit($30A6);  { U+30A6 KATAKANA LETTER U }
    XKc_kana_E             : exit($30A8);  { U+30A8 KATAKANA LETTER E }
    XKc_kana_O             : exit($30AA);  { U+30AA KATAKANA LETTER O }
    XKc_kana_KA            : exit($30AB);  { U+30AB KATAKANA LETTER KA }
    XKc_kana_KI            : exit($30AD);  { U+30AD KATAKANA LETTER KI }
    XKc_kana_KU            : exit($30AF);  { U+30AF KATAKANA LETTER KU }
    XKc_kana_KE            : exit($30B1);  { U+30B1 KATAKANA LETTER KE }
    XKc_kana_KO            : exit($30B3);  { U+30B3 KATAKANA LETTER KO }
    XKc_kana_SA            : exit($30B5);  { U+30B5 KATAKANA LETTER SA }
    XKc_kana_SHI           : exit($30B7);  { U+30B7 KATAKANA LETTER SI }
    XKc_kana_SU            : exit($30B9);  { U+30B9 KATAKANA LETTER SU }
    XKc_kana_SE            : exit($30BB);  { U+30BB KATAKANA LETTER SE }
    XKc_kana_SO            : exit($30BD);  { U+30BD KATAKANA LETTER SO }
    XKc_kana_TA            : exit($30BF);  { U+30BF KATAKANA LETTER TA }
    XKc_kana_CHI           : exit($30C1);  { U+30C1 KATAKANA LETTER TI }
    XKc_kana_TSU           : exit($30C4);  { U+30C4 KATAKANA LETTER TU }
    XKc_kana_TE            : exit($30C6);  { U+30C6 KATAKANA LETTER TE }
    XKc_kana_TO            : exit($30C8);  { U+30C8 KATAKANA LETTER TO }
    XKc_kana_NA            : exit($30CA);  { U+30CA KATAKANA LETTER NA }
    XKc_kana_NI            : exit($30CB);  { U+30CB KATAKANA LETTER NI }
    XKc_kana_NU            : exit($30CC);  { U+30CC KATAKANA LETTER NU }
    XKc_kana_NE            : exit($30CD);  { U+30CD KATAKANA LETTER NE }
    XKc_kana_NO            : exit($30CE);  { U+30CE KATAKANA LETTER NO }
    XKc_kana_HA            : exit($30CF);  { U+30CF KATAKANA LETTER HA }
    XKc_kana_HI            : exit($30D2);  { U+30D2 KATAKANA LETTER HI }
    XKc_kana_FU            : exit($30D5);  { U+30D5 KATAKANA LETTER HU }
    XKc_kana_HE            : exit($30D8);  { U+30D8 KATAKANA LETTER HE }
    XKc_kana_HO            : exit($30DB);  { U+30DB KATAKANA LETTER HO }
    XKc_kana_MA            : exit($30DE);  { U+30DE KATAKANA LETTER MA }
    XKc_kana_MI            : exit($30DF);  { U+30DF KATAKANA LETTER MI }
    XKc_kana_MU            : exit($30E0);  { U+30E0 KATAKANA LETTER MU }
    XKc_kana_ME            : exit($30E1);  { U+30E1 KATAKANA LETTER ME }
    XKc_kana_MO            : exit($30E2);  { U+30E2 KATAKANA LETTER MO }
    XKc_kana_YA            : exit($30E4);  { U+30E4 KATAKANA LETTER YA }
    XKc_kana_YU            : exit($30E6);  { U+30E6 KATAKANA LETTER YU }
    XKc_kana_YO            : exit($30E8);  { U+30E8 KATAKANA LETTER YO }
    XKc_kana_RA            : exit($30E9);  { U+30E9 KATAKANA LETTER RA }
    XKc_kana_RI            : exit($30EA);  { U+30EA KATAKANA LETTER RI }
    XKc_kana_RU            : exit($30EB);  { U+30EB KATAKANA LETTER RU }
    XKc_kana_RE            : exit($30EC);  { U+30EC KATAKANA LETTER RE }
    XKc_kana_RO            : exit($30ED);  { U+30ED KATAKANA LETTER RO }
    XKc_kana_WA            : exit($30EF);  { U+30EF KATAKANA LETTER WA }
    XKc_kana_N             : exit($30F3);  { U+30F3 KATAKANA LETTER N }
    XK_voicedsound         : exit($309B);  { U+309B KATAKANA-HIRAGANA VOICED SOUND MARK }
    XK_semivoicedsound     : exit($309C);  { U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK }

    { Arabic }
    XK_Arabic_comma          : exit($060C);     { U+060C ARABIC COMMA }
    XK_Arabic_semicolon      : exit($061B);     { U+061B ARABIC SEMICOLON }
    XK_Arabic_question_mark  : exit($061F);     { U+061F ARABIC QUESTION MARK }
    XK_Arabic_hamza          : exit($0621);     { U+0621 ARABIC LETTER HAMZA }
    XK_Arabic_maddaonalef    : exit($0622);     { U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE }
    XK_Arabic_hamzaonalef    : exit($0623);     { U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE }
    XK_Arabic_hamzaonwaw     : exit($0624);     { U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE }
    XK_Arabic_hamzaunderalef : exit($0625);     { U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW }
    XK_Arabic_hamzaonyeh     : exit($0626);     { U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE }
    XK_Arabic_alef           : exit($0627);     { U+0627 ARABIC LETTER ALEF }
    XK_Arabic_beh            : exit($0628);     { U+0628 ARABIC LETTER BEH }
    XK_Arabic_tehmarbuta     : exit($0629);     { U+0629 ARABIC LETTER TEH MARBUTA }
    XK_Arabic_teh            : exit($062A);     { U+062A ARABIC LETTER TEH }
    XK_Arabic_theh           : exit($062B);     { U+062B ARABIC LETTER THEH }
    XK_Arabic_jeem           : exit($062C);     { U+062C ARABIC LETTER JEEM }
    XK_Arabic_hah            : exit($062D);     { U+062D ARABIC LETTER HAH }
    XK_Arabic_khah           : exit($062E);     { U+062E ARABIC LETTER KHAH }
    XK_Arabic_dal            : exit($062F);     { U+062F ARABIC LETTER DAL }
    XK_Arabic_thal           : exit($0630);     { U+0630 ARABIC LETTER THAL }
    XK_Arabic_ra             : exit($0631);     { U+0631 ARABIC LETTER REH }
    XK_Arabic_zain           : exit($0632);     { U+0632 ARABIC LETTER ZAIN }
    XK_Arabic_seen           : exit($0633);     { U+0633 ARABIC LETTER SEEN }
    XK_Arabic_sheen          : exit($0634);     { U+0634 ARABIC LETTER SHEEN }
    XK_Arabic_sad            : exit($0635);     { U+0635 ARABIC LETTER SAD }
    XK_Arabic_dad            : exit($0636);     { U+0636 ARABIC LETTER DAD }
    XK_Arabic_tah            : exit($0637);     { U+0637 ARABIC LETTER TAH }
    XK_Arabic_zah            : exit($0638);     { U+0638 ARABIC LETTER ZAH }
    XK_Arabic_ain            : exit($0639);     { U+0639 ARABIC LETTER AIN }
    XK_Arabic_ghain          : exit($063A);     { U+063A ARABIC LETTER GHAIN }
    XK_Arabic_tatweel        : exit($0640);     { U+0640 ARABIC TATWEEL }
    XK_Arabic_feh            : exit($0641);     { U+0641 ARABIC LETTER FEH }
    XK_Arabic_qaf            : exit($0642);     { U+0642 ARABIC LETTER QAF }
    XK_Arabic_kaf            : exit($0643);     { U+0643 ARABIC LETTER KAF }
    XK_Arabic_lam            : exit($0644);     { U+0644 ARABIC LETTER LAM }
    XK_Arabic_meem           : exit($0645);     { U+0645 ARABIC LETTER MEEM }
    XK_Arabic_noon           : exit($0646);     { U+0646 ARABIC LETTER NOON }
    XK_Arabic_ha             : exit($0647);     { U+0647 ARABIC LETTER HEH }
    XK_Arabic_waw            : exit($0648);     { U+0648 ARABIC LETTER WAW }
    XK_Arabic_alefmaksura    : exit($0649);     { U+0649 ARABIC LETTER ALEF MAKSURA }
    XK_Arabic_yeh            : exit($064A);     { U+064A ARABIC LETTER YEH }
    XK_Arabic_fathatan       : exit($064B);     { U+064B ARABIC FATHATAN }
    XK_Arabic_dammatan       : exit($064C);     { U+064C ARABIC DAMMATAN }
    XK_Arabic_kasratan       : exit($064D);     { U+064D ARABIC KASRATAN }
    XK_Arabic_fatha          : exit($064E);     { U+064E ARABIC FATHA }
    XK_Arabic_damma          : exit($064F);     { U+064F ARABIC DAMMA }
    XK_Arabic_kasra          : exit($0650);     { U+0650 ARABIC KASRA }
    XK_Arabic_shadda         : exit($0651);     { U+0651 ARABIC SHADDA }
    XK_Arabic_sukun          : exit($0652);     { U+0652 ARABIC SUKUN }

    { Cyrillic }
    XK_Serbian_dje                : exit($0452);  { U+0452 CYRILLIC SMALL LETTER DJE }
    XK_Macedonia_gje              : exit($0453);  { U+0453 CYRILLIC SMALL LETTER GJE }
    XK_Cyrillic_io                : exit($0451);  { U+0451 CYRILLIC SMALL LETTER IO }
    XK_Ukrainian_ie               : exit($0454);  { U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE }
    XK_Macedonia_dse              : exit($0455);  { U+0455 CYRILLIC SMALL LETTER DZE }
    XK_Ukrainian_i                : exit($0456);  { U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I }
    XK_Ukrainian_yi               : exit($0457);  { U+0457 CYRILLIC SMALL LETTER YI }
    XK_Cyrillic_je                : exit($0458);  { U+0458 CYRILLIC SMALL LETTER JE }
    XK_Cyrillic_lje               : exit($0459);  { U+0459 CYRILLIC SMALL LETTER LJE }
    XK_Cyrillic_nje               : exit($045A);  { U+045A CYRILLIC SMALL LETTER NJE }
    XK_Serbian_tshe               : exit($045B);  { U+045B CYRILLIC SMALL LETTER TSHE }
    XK_Macedonia_kje              : exit($045C);  { U+045C CYRILLIC SMALL LETTER KJE }
    XK_Ukrainian_ghe_with_upturn  : exit($0491);  { U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN }
    XK_Byelorussian_shortu        : exit($045E);  { U+045E CYRILLIC SMALL LETTER SHORT U }
    XK_Cyrillic_dzhe              : exit($045F);  { U+045F CYRILLIC SMALL LETTER DZHE }
    XK_numerosign                 : exit($2116);  { U+2116 NUMERO SIGN }
    XKc_Serbian_DJE               : exit($0402);  { U+0402 CYRILLIC CAPITAL LETTER DJE }
    XKc_Macedonia_GJE             : exit($0403);  { U+0403 CYRILLIC CAPITAL LETTER GJE }
    XKc_Cyrillic_IO               : exit($0401);  { U+0401 CYRILLIC CAPITAL LETTER IO }
    XKc_Ukrainian_IE              : exit($0404);  { U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE }
    XKc_Macedonia_DSE             : exit($0405);  { U+0405 CYRILLIC CAPITAL LETTER DZE }
    XKc_Ukrainian_I               : exit($0406);  { U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I }
    XKc_Ukrainian_YI              : exit($0407);  { U+0407 CYRILLIC CAPITAL LETTER YI }
    XKc_Cyrillic_JE               : exit($0408);  { U+0408 CYRILLIC CAPITAL LETTER JE }
    XKc_Cyrillic_LJE              : exit($0409);  { U+0409 CYRILLIC CAPITAL LETTER LJE }
    XKc_Cyrillic_NJE              : exit($040A);  { U+040A CYRILLIC CAPITAL LETTER NJE }
    XKc_Serbian_TSHE              : exit($040B);  { U+040B CYRILLIC CAPITAL LETTER TSHE }
    XKc_Macedonia_KJE             : exit($040C);  { U+040C CYRILLIC CAPITAL LETTER KJE }
    XKc_Ukrainian_GHE_WITH_UPTURN : exit($0490);  { U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN }
    XKc_Byelorussian_SHORTU       : exit($040E);  { U+040E CYRILLIC CAPITAL LETTER SHORT U }
    XKc_Cyrillic_DZHE             : exit($040F);  { U+040F CYRILLIC CAPITAL LETTER DZHE }
    XK_Cyrillic_yu                : exit($044E);  { U+044E CYRILLIC SMALL LETTER YU }
    XK_Cyrillic_a                 : exit($0430);  { U+0430 CYRILLIC SMALL LETTER A }
    XK_Cyrillic_be                : exit($0431);  { U+0431 CYRILLIC SMALL LETTER BE }
    XK_Cyrillic_tse               : exit($0446);  { U+0446 CYRILLIC SMALL LETTER TSE }
    XK_Cyrillic_de                : exit($0434);  { U+0434 CYRILLIC SMALL LETTER DE }
    XK_Cyrillic_ie                : exit($0435);  { U+0435 CYRILLIC SMALL LETTER IE }
    XK_Cyrillic_ef                : exit($0444);  { U+0444 CYRILLIC SMALL LETTER EF }
    XK_Cyrillic_ghe               : exit($0433);  { U+0433 CYRILLIC SMALL LETTER GHE }
    XK_Cyrillic_ha                : exit($0445);  { U+0445 CYRILLIC SMALL LETTER HA }
    XK_Cyrillic_i                 : exit($0438);  { U+0438 CYRILLIC SMALL LETTER I }
    XK_Cyrillic_shorti            : exit($0439);  { U+0439 CYRILLIC SMALL LETTER SHORT I }
    XK_Cyrillic_ka                : exit($043A);  { U+043A CYRILLIC SMALL LETTER KA }
    XK_Cyrillic_el                : exit($043B);  { U+043B CYRILLIC SMALL LETTER EL }
    XK_Cyrillic_em                : exit($043C);  { U+043C CYRILLIC SMALL LETTER EM }
    XK_Cyrillic_en                : exit($043D);  { U+043D CYRILLIC SMALL LETTER EN }
    XK_Cyrillic_o                 : exit($043E);  { U+043E CYRILLIC SMALL LETTER O }
    XK_Cyrillic_pe                : exit($043F);  { U+043F CYRILLIC SMALL LETTER PE }
    XK_Cyrillic_ya                : exit($044F);  { U+044F CYRILLIC SMALL LETTER YA }
    XK_Cyrillic_er                : exit($0440);  { U+0440 CYRILLIC SMALL LETTER ER }
    XK_Cyrillic_es                : exit($0441);  { U+0441 CYRILLIC SMALL LETTER ES }
    XK_Cyrillic_te                : exit($0442);  { U+0442 CYRILLIC SMALL LETTER TE }
    XK_Cyrillic_u                 : exit($0443);  { U+0443 CYRILLIC SMALL LETTER U }
    XK_Cyrillic_zhe               : exit($0436);  { U+0436 CYRILLIC SMALL LETTER ZHE }
    XK_Cyrillic_ve                : exit($0432);  { U+0432 CYRILLIC SMALL LETTER VE }
    XK_Cyrillic_softsign          : exit($044C);  { U+044C CYRILLIC SMALL LETTER SOFT SIGN }
    XK_Cyrillic_yeru              : exit($044B);  { U+044B CYRILLIC SMALL LETTER YERU }
    XK_Cyrillic_ze                : exit($0437);  { U+0437 CYRILLIC SMALL LETTER ZE }
    XK_Cyrillic_sha               : exit($0448);  { U+0448 CYRILLIC SMALL LETTER SHA }
    XK_Cyrillic_e                 : exit($044D);  { U+044D CYRILLIC SMALL LETTER E }
    XK_Cyrillic_shcha             : exit($0449);  { U+0449 CYRILLIC SMALL LETTER SHCHA }
    XK_Cyrillic_che               : exit($0447);  { U+0447 CYRILLIC SMALL LETTER CHE }
    XK_Cyrillic_hardsign          : exit($044A);  { U+044A CYRILLIC SMALL LETTER HARD SIGN }
    XKc_Cyrillic_YU               : exit($042E);  { U+042E CYRILLIC CAPITAL LETTER YU }
    XKc_Cyrillic_A                : exit($0410);  { U+0410 CYRILLIC CAPITAL LETTER A }
    XKc_Cyrillic_BE               : exit($0411);  { U+0411 CYRILLIC CAPITAL LETTER BE }
    XKc_Cyrillic_TSE              : exit($0426);  { U+0426 CYRILLIC CAPITAL LETTER TSE }
    XKc_Cyrillic_DE               : exit($0414);  { U+0414 CYRILLIC CAPITAL LETTER DE }
    XKc_Cyrillic_IE               : exit($0415);  { U+0415 CYRILLIC CAPITAL LETTER IE }
    XKc_Cyrillic_EF               : exit($0424);  { U+0424 CYRILLIC CAPITAL LETTER EF }
    XKc_Cyrillic_GHE              : exit($0413);  { U+0413 CYRILLIC CAPITAL LETTER GHE }
    XKc_Cyrillic_HA               : exit($0425);  { U+0425 CYRILLIC CAPITAL LETTER HA }
    XKc_Cyrillic_I                : exit($0418);  { U+0418 CYRILLIC CAPITAL LETTER I }
    XKc_Cyrillic_SHORTI           : exit($0419);  { U+0419 CYRILLIC CAPITAL LETTER SHORT I }
    XKc_Cyrillic_KA               : exit($041A);  { U+041A CYRILLIC CAPITAL LETTER KA }
    XKc_Cyrillic_EL               : exit($041B);  { U+041B CYRILLIC CAPITAL LETTER EL }
    XKc_Cyrillic_EM               : exit($041C);  { U+041C CYRILLIC CAPITAL LETTER EM }
    XKc_Cyrillic_EN               : exit($041D);  { U+041D CYRILLIC CAPITAL LETTER EN }
    XKc_Cyrillic_O                : exit($041E);  { U+041E CYRILLIC CAPITAL LETTER O }
    XKc_Cyrillic_PE               : exit($041F);  { U+041F CYRILLIC CAPITAL LETTER PE }
    XKc_Cyrillic_YA               : exit($042F);  { U+042F CYRILLIC CAPITAL LETTER YA }
    XKc_Cyrillic_ER               : exit($0420);  { U+0420 CYRILLIC CAPITAL LETTER ER }
    XKc_Cyrillic_ES               : exit($0421);  { U+0421 CYRILLIC CAPITAL LETTER ES }
    XKc_Cyrillic_TE               : exit($0422);  { U+0422 CYRILLIC CAPITAL LETTER TE }
    XKc_Cyrillic_U                : exit($0423);  { U+0423 CYRILLIC CAPITAL LETTER U }
    XKc_Cyrillic_ZHE              : exit($0416);  { U+0416 CYRILLIC CAPITAL LETTER ZHE }
    XKc_Cyrillic_VE               : exit($0412);  { U+0412 CYRILLIC CAPITAL LETTER VE }
    XKc_Cyrillic_SOFTSIGN         : exit($042C);  { U+042C CYRILLIC CAPITAL LETTER SOFT SIGN }
    XKc_Cyrillic_YERU             : exit($042B);  { U+042B CYRILLIC CAPITAL LETTER YERU }
    XKc_Cyrillic_ZE               : exit($0417);  { U+0417 CYRILLIC CAPITAL LETTER ZE }
    XKc_Cyrillic_SHA              : exit($0428);  { U+0428 CYRILLIC CAPITAL LETTER SHA }
    XKc_Cyrillic_E                : exit($042D);  { U+042D CYRILLIC CAPITAL LETTER E }
    XKc_Cyrillic_SHCHA            : exit($0429);  { U+0429 CYRILLIC CAPITAL LETTER SHCHA }
    XKc_Cyrillic_CHE              : exit($0427);  { U+0427 CYRILLIC CAPITAL LETTER CHE }
    XKc_Cyrillic_HARDSIGN         : exit($042A);  { U+042A CYRILLIC CAPITAL LETTER HARD SIGN }

    { Greek }
    XKc_Greek_ALPHAaccent          : exit($0386);  { U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS }
    XKc_Greek_EPSILONaccent        : exit($0388);  { U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS }
    XKc_Greek_ETAaccent            : exit($0389);  { U+0389 GREEK CAPITAL LETTER ETA WITH TONOS }
    XKc_Greek_IOTAaccent           : exit($038A);  { U+038A GREEK CAPITAL LETTER IOTA WITH TONOS }
    XKc_Greek_IOTAdieresis         : exit($03AA);  { U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA }
    XKc_Greek_OMICRONaccent        : exit($038C);  { U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS }
    XKc_Greek_UPSILONaccent        : exit($038E);  { U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS }
    XKc_Greek_UPSILONdieresis      : exit($03AB);  { U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA }
    XKc_Greek_OMEGAaccent          : exit($038F);  { U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS }
    XK_Greek_accentdieresis        : exit($0385);  { U+0385 GREEK DIALYTIKA TONOS }
    XK_Greek_horizbar              : exit($2015);  { U+2015 HORIZONTAL BAR }
    XK_Greek_alphaaccent           : exit($03AC);  { U+03AC GREEK SMALL LETTER ALPHA WITH TONOS }
    XK_Greek_epsilonaccent         : exit($03AD);  { U+03AD GREEK SMALL LETTER EPSILON WITH TONOS }
    XK_Greek_etaaccent             : exit($03AE);  { U+03AE GREEK SMALL LETTER ETA WITH TONOS }
    XK_Greek_iotaaccent            : exit($03AF);  { U+03AF GREEK SMALL LETTER IOTA WITH TONOS }
    XK_Greek_iotadieresis          : exit($03CA);  { U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA }
    XK_Greek_iotaaccentdieresis    : exit($0390);  { U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS }
    XK_Greek_omicronaccent         : exit($03CC);  { U+03CC GREEK SMALL LETTER OMICRON WITH TONOS }
    XK_Greek_upsilonaccent         : exit($03CD);  { U+03CD GREEK SMALL LETTER UPSILON WITH TONOS }
    XK_Greek_upsilondieresis       : exit($03CB);  { U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA }
    XK_Greek_upsilonaccentdieresis : exit($03B0);  { U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS }
    XK_Greek_omegaaccent           : exit($03CE);  { U+03CE GREEK SMALL LETTER OMEGA WITH TONOS }
    XKc_Greek_ALPHA                : exit($0391);  { U+0391 GREEK CAPITAL LETTER ALPHA }
    XKc_Greek_BETA                 : exit($0392);  { U+0392 GREEK CAPITAL LETTER BETA }
    XKc_Greek_GAMMA                : exit($0393);  { U+0393 GREEK CAPITAL LETTER GAMMA }
    XKc_Greek_DELTA                : exit($0394);  { U+0394 GREEK CAPITAL LETTER DELTA }
    XKc_Greek_EPSILON              : exit($0395);  { U+0395 GREEK CAPITAL LETTER EPSILON }
    XKc_Greek_ZETA                 : exit($0396);  { U+0396 GREEK CAPITAL LETTER ZETA }
    XKc_Greek_ETA                  : exit($0397);  { U+0397 GREEK CAPITAL LETTER ETA }
    XKc_Greek_THETA                : exit($0398);  { U+0398 GREEK CAPITAL LETTER THETA }
    XKc_Greek_IOTA                 : exit($0399);  { U+0399 GREEK CAPITAL LETTER IOTA }
    XKc_Greek_KAPPA                : exit($039A);  { U+039A GREEK CAPITAL LETTER KAPPA }
    XKc_Greek_LAMDA                : exit($039B);  { U+039B GREEK CAPITAL LETTER LAMDA }
    XKc_Greek_MU                   : exit($039C);  { U+039C GREEK CAPITAL LETTER MU }
    XKc_Greek_NU                   : exit($039D);  { U+039D GREEK CAPITAL LETTER NU }
    XKc_Greek_XI                   : exit($039E);  { U+039E GREEK CAPITAL LETTER XI }
    XKc_Greek_OMICRON              : exit($039F);  { U+039F GREEK CAPITAL LETTER OMICRON }
    XKc_Greek_PI                   : exit($03A0);  { U+03A0 GREEK CAPITAL LETTER PI }
    XKc_Greek_RHO                  : exit($03A1);  { U+03A1 GREEK CAPITAL LETTER RHO }
    XKc_Greek_SIGMA                : exit($03A3);  { U+03A3 GREEK CAPITAL LETTER SIGMA }
    XKc_Greek_TAU                  : exit($03A4);  { U+03A4 GREEK CAPITAL LETTER TAU }
    XKc_Greek_UPSILON              : exit($03A5);  { U+03A5 GREEK CAPITAL LETTER UPSILON }
    XKc_Greek_PHI                  : exit($03A6);  { U+03A6 GREEK CAPITAL LETTER PHI }
    XKc_Greek_CHI                  : exit($03A7);  { U+03A7 GREEK CAPITAL LETTER CHI }
    XKc_Greek_PSI                  : exit($03A8);  { U+03A8 GREEK CAPITAL LETTER PSI }
    XKc_Greek_OMEGA                : exit($03A9);  { U+03A9 GREEK CAPITAL LETTER OMEGA }
    XK_Greek_alpha                 : exit($03B1);  { U+03B1 GREEK SMALL LETTER ALPHA }
    XK_Greek_beta                  : exit($03B2);  { U+03B2 GREEK SMALL LETTER BETA }
    XK_Greek_gamma                 : exit($03B3);  { U+03B3 GREEK SMALL LETTER GAMMA }
    XK_Greek_delta                 : exit($03B4);  { U+03B4 GREEK SMALL LETTER DELTA }
    XK_Greek_epsilon               : exit($03B5);  { U+03B5 GREEK SMALL LETTER EPSILON }
    XK_Greek_zeta                  : exit($03B6);  { U+03B6 GREEK SMALL LETTER ZETA }
    XK_Greek_eta                   : exit($03B7);  { U+03B7 GREEK SMALL LETTER ETA }
    XK_Greek_theta                 : exit($03B8);  { U+03B8 GREEK SMALL LETTER THETA }
    XK_Greek_iota                  : exit($03B9);  { U+03B9 GREEK SMALL LETTER IOTA }
    XK_Greek_kappa                 : exit($03BA);  { U+03BA GREEK SMALL LETTER KAPPA }
    XK_Greek_lamda                 : exit($03BB);  { U+03BB GREEK SMALL LETTER LAMDA }
    XK_Greek_mu                    : exit($03BC);  { U+03BC GREEK SMALL LETTER MU }
    XK_Greek_nu                    : exit($03BD);  { U+03BD GREEK SMALL LETTER NU }
    XK_Greek_xi                    : exit($03BE);  { U+03BE GREEK SMALL LETTER XI }
    XK_Greek_omicron               : exit($03BF);  { U+03BF GREEK SMALL LETTER OMICRON }
    XK_Greek_pi                    : exit($03C0);  { U+03C0 GREEK SMALL LETTER PI }
    XK_Greek_rho                   : exit($03C1);  { U+03C1 GREEK SMALL LETTER RHO }
    XK_Greek_sigma                 : exit($03C2);  { U+03C3 GREEK SMALL LETTER SIGMA }
    XK_Greek_finalsmallsigma       : exit($03C3);  { U+03C2 GREEK SMALL LETTER FINAL SIGMA }
    XK_Greek_tau                   : exit($03C4);  { U+03C4 GREEK SMALL LETTER TAU }
    XK_Greek_upsilon               : exit($03C5);  { U+03C5 GREEK SMALL LETTER UPSILON }
    XK_Greek_phi                   : exit($03C6);  { U+03C6 GREEK SMALL LETTER PHI }
    XK_Greek_chi                   : exit($03C7);  { U+03C7 GREEK SMALL LETTER CHI }
    XK_Greek_psi                   : exit($03C8);  { U+03C8 GREEK SMALL LETTER PSI }
    XK_Greek_omega                 : exit($03C9);  { U+03C9 GREEK SMALL LETTER OMEGA }

    { Technical }
    XK_leftradical               : exit($23B7);  { U+23B7 RADICAL SYMBOL BOTTOM }
    XK_topleftradical            : exit($250C);  {(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)}
    XK_horizconnector            : exit($2500);  {(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)}
    XK_topintegral               : exit($2320);  { U+2320 TOP HALF INTEGRAL }
    XK_botintegral               : exit($2321);  { U+2321 BOTTOM HALF INTEGRAL }
    XK_vertconnector             : exit($2502);  {(U+2502 BOX DRAWINGS LIGHT VERTICAL)}
    XK_topleftsqbracket          : exit($23A1);  { U+23A1 LEFT SQUARE BRACKET UPPER CORNER }
    XK_botleftsqbracket          : exit($23A3);  { U+23A3 LEFT SQUARE BRACKET LOWER CORNER }
    XK_toprightsqbracket         : exit($23A4);  { U+23A4 RIGHT SQUARE BRACKET UPPER CORNER }
    XK_botrightsqbracket         : exit($23A6);  { U+23A6 RIGHT SQUARE BRACKET LOWER CORNER }
    XK_topleftparens             : exit($239B);  { U+239B LEFT PARENTHESIS UPPER HOOK }
    XK_botleftparens             : exit($239D);  { U+239D LEFT PARENTHESIS LOWER HOOK }
    XK_toprightparens            : exit($239E);  { U+239E RIGHT PARENTHESIS UPPER HOOK }
    XK_botrightparens            : exit($23A0);  { U+23A0 RIGHT PARENTHESIS LOWER HOOK }
    XK_leftmiddlecurlybrace      : exit($23A8);  { U+23A8 LEFT CURLY BRACKET MIDDLE PIECE }
    XK_rightmiddlecurlybrace     : exit($23AC);  { U+23AC RIGHT CURLY BRACKET MIDDLE PIECE }
{    XK_topleftsummation          : exit($);
    XK_botleftsummation          : exit($);
    XK_topvertsummationconnector : exit($);
    XK_botvertsummationconnector : exit($);
    XK_toprightsummation         : exit($);
    XK_botrightsummation         : exit($);
    XK_rightmiddlesummation      : exit($);}
    XK_lessthanequal             : exit($2264);  { U+2264 LESS-THAN OR EQUAL TO }
    XK_notequal                  : exit($2260);  { U+2260 NOT EQUAL TO }
    XK_greaterthanequal          : exit($2265);  { U+2265 GREATER-THAN OR EQUAL TO }
    XK_integral                  : exit($222B);  { U+222B INTEGRAL }
    XK_therefore                 : exit($2234);  { U+2234 THEREFORE }
    XK_variation                 : exit($221D);  { U+221D PROPORTIONAL TO }
    XK_infinity                  : exit($221E);  { U+221E INFINITY }
    XK_nabla                     : exit($2207);  { U+2207 NABLA }
    XK_approximate               : exit($223C);  { U+223C TILDE OPERATOR }
    XK_similarequal              : exit($2243);  { U+2243 ASYMPTOTICALLY EQUAL TO }
    XK_ifonlyif                  : exit($21D4);  { U+21D4 LEFT RIGHT DOUBLE ARROW }
    XK_implies                   : exit($21D2);  { U+21D2 RIGHTWARDS DOUBLE ARROW }
    XK_identical                 : exit($2261);  { U+2261 IDENTICAL TO }
    XK_radical                   : exit($221A);  { U+221A SQUARE ROOT }
    XK_includedin                : exit($2282);  { U+2282 SUBSET OF }
    XK_includes                  : exit($2283);  { U+2283 SUPERSET OF }
    XK_intersection              : exit($2229);  { U+2229 INTERSECTION }
    XK_union                     : exit($222A);  { U+222A UNION }
    XK_logicaland                : exit($2227);  { U+2227 LOGICAL AND }
    XK_logicalor                 : exit($2228);  { U+2228 LOGICAL OR }
    XK_partialderivative         : exit($2202);  { U+2202 PARTIAL DIFFERENTIAL }
    XK_function                  : exit($0192);  { U+0192 LATIN SMALL LETTER F WITH HOOK }
    XK_leftarrow                 : exit($2190);  { U+2190 LEFTWARDS ARROW }
    XK_uparrow                   : exit($2191);  { U+2191 UPWARDS ARROW }
    XK_rightarrow                : exit($2192);  { U+2192 RIGHTWARDS ARROW }
    XK_downarrow                 : exit($2193);  { U+2193 DOWNWARDS ARROW }

    { Special }
{    XK_blank          : exit($);}
    XK_soliddiamond   : exit($25C6);  { U+25C6 BLACK DIAMOND }
    XK_checkerboard   : exit($2592);  { U+2592 MEDIUM SHADE }
    XK_ht             : exit($2409);  { U+2409 SYMBOL FOR HORIZONTAL TABULATION }
    XK_ff             : exit($240C);  { U+240C SYMBOL FOR FORM FEED }
    XK_cr             : exit($240D);  { U+240D SYMBOL FOR CARRIAGE RETURN }
    XK_lf             : exit($240A);  { U+240A SYMBOL FOR LINE FEED }
    XK_nl             : exit($2424);  { U+2424 SYMBOL FOR NEWLINE }
    XK_vt             : exit($240B);  { U+240B SYMBOL FOR VERTICAL TABULATION }
    XK_lowrightcorner : exit($2518);  { U+2518 BOX DRAWINGS LIGHT UP AND LEFT }
    XK_uprightcorner  : exit($2510);  { U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT }
    XK_upleftcorner   : exit($250C);  { U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT }
    XK_lowleftcorner  : exit($2514);  { U+2514 BOX DRAWINGS LIGHT UP AND RIGHT }
    XK_crossinglines  : exit($253C);  { U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL }
    XK_horizlinescan1 : exit($23BA);  { U+23BA HORIZONTAL SCAN LINE-1 }
    XK_horizlinescan3 : exit($23BB);  { U+23BB HORIZONTAL SCAN LINE-3 }
    XK_horizlinescan5 : exit($2500);  { U+2500 BOX DRAWINGS LIGHT HORIZONTAL }
    XK_horizlinescan7 : exit($23BC);  { U+23BC HORIZONTAL SCAN LINE-7 }
    XK_horizlinescan9 : exit($23BD);  { U+23BD HORIZONTAL SCAN LINE-9 }
    XK_leftt          : exit($251C);  { U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT }
    XK_rightt         : exit($2524);  { U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT }
    XK_bott           : exit($2534);  { U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL }
    XK_topt           : exit($252C);  { U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL }
    XK_vertbar        : exit($2502);  { U+2502 BOX DRAWINGS LIGHT VERTICAL }

    { Publishing }
    XK_emspace              : exit($2003);  { U+2003 EM SPACE }
    XK_enspace              : exit($2002);  { U+2002 EN SPACE }
    XK_em3space             : exit($2004);  { U+2004 THREE-PER-EM SPACE }
    XK_em4space             : exit($2005);  { U+2005 FOUR-PER-EM SPACE }
    XK_digitspace           : exit($2007);  { U+2007 FIGURE SPACE }
    XK_punctspace           : exit($2008);  { U+2008 PUNCTUATION SPACE }
    XK_thinspace            : exit($2009);  { U+2009 THIN SPACE }
    XK_hairspace            : exit($200A);  { U+200A HAIR SPACE }
    XK_emdash               : exit($2014);  { U+2014 EM DASH }
    XK_endash               : exit($2013);  { U+2013 EN DASH }
    XK_signifblank          : exit($2423);  {(U+2423 OPEN BOX)}
    XK_ellipsis             : exit($2026);  { U+2026 HORIZONTAL ELLIPSIS }
    XK_doubbaselinedot      : exit($2025);  { U+2025 TWO DOT LEADER }
    XK_onethird             : exit($2153);  { U+2153 VULGAR FRACTION ONE THIRD }
    XK_twothirds            : exit($2154);  { U+2154 VULGAR FRACTION TWO THIRDS }
    XK_onefifth             : exit($2155);  { U+2155 VULGAR FRACTION ONE FIFTH }
    XK_twofifths            : exit($2156);  { U+2156 VULGAR FRACTION TWO FIFTHS }
    XK_threefifths          : exit($2157);  { U+2157 VULGAR FRACTION THREE FIFTHS }
    XK_fourfifths           : exit($2158);  { U+2158 VULGAR FRACTION FOUR FIFTHS }
    XK_onesixth             : exit($2159);  { U+2159 VULGAR FRACTION ONE SIXTH }
    XK_fivesixths           : exit($215A);  { U+215A VULGAR FRACTION FIVE SIXTHS }
    XK_careof               : exit($2105);  { U+2105 CARE OF }
    XK_figdash              : exit($2012);  { U+2012 FIGURE DASH }
    XK_leftanglebracket     : exit($27E8);  {(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)}
    XK_decimalpoint         : exit($002E);  {(U+002E FULL STOP)}
    XK_rightanglebracket    : exit($27E9);  {(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)}
{    XK_marker               : exit($);}
    XK_oneeighth            : exit($215B);  { U+215B VULGAR FRACTION ONE EIGHTH }
    XK_threeeighths         : exit($215C);  { U+215C VULGAR FRACTION THREE EIGHTHS }
    XK_fiveeighths          : exit($215D);  { U+215D VULGAR FRACTION FIVE EIGHTHS }
    XK_seveneighths         : exit($215E);  { U+215E VULGAR FRACTION SEVEN EIGHTHS }
    XK_trademark            : exit($2122);  { U+2122 TRADE MARK SIGN }
    XK_signaturemark        : exit($2613);  {(U+2613 SALTIRE)}
{    XK_trademarkincircle    : exit($);}
    XK_leftopentriangle     : exit($25C1);  {(U+25C1 WHITE LEFT-POINTING TRIANGLE)}
    XK_rightopentriangle    : exit($25B7);  {(U+25B7 WHITE RIGHT-POINTING TRIANGLE)}
    XK_emopencircle         : exit($25CB);  {(U+25CB WHITE CIRCLE)}
    XK_emopenrectangle      : exit($25AF);  {(U+25AF WHITE VERTICAL RECTANGLE)}
    XK_leftsinglequotemark  : exit($2018);  { U+2018 LEFT SINGLE QUOTATION MARK }
    XK_rightsinglequotemark : exit($2019);  { U+2019 RIGHT SINGLE QUOTATION MARK }
    XK_leftdoublequotemark  : exit($201C);  { U+201C LEFT DOUBLE QUOTATION MARK }
    XK_rightdoublequotemark : exit($201D);  { U+201D RIGHT DOUBLE QUOTATION MARK }
    XK_prescription         : exit($211E);  { U+211E PRESCRIPTION TAKE }
    XK_permille             : exit($2030);  { U+2030 PER MILLE SIGN }
    XK_minutes              : exit($2032);  { U+2032 PRIME }
    XK_seconds              : exit($2033);  { U+2033 DOUBLE PRIME }
    XK_latincross           : exit($271D);  { U+271D LATIN CROSS }
{    XK_hexagram             : exit($);}
    XK_filledrectbullet     : exit($25AC);  {(U+25AC BLACK RECTANGLE)}
    XK_filledlefttribullet  : exit($25C0);  {(U+25C0 BLACK LEFT-POINTING TRIANGLE)}
    XK_filledrighttribullet : exit($25B6);  {(U+25B6 BLACK RIGHT-POINTING TRIANGLE)}
    XK_emfilledcircle       : exit($25CF);  {(U+25CF BLACK CIRCLE)}
    XK_emfilledrect         : exit($25AE);  {(U+25AE BLACK VERTICAL RECTANGLE)}
    XK_enopencircbullet     : exit($25E6);  {(U+25E6 WHITE BULLET)}
    XK_enopensquarebullet   : exit($25AB);  {(U+25AB WHITE SMALL SQUARE)}
    XK_openrectbullet       : exit($25AD);  {(U+25AD WHITE RECTANGLE)}
    XK_opentribulletup      : exit($25B3);  {(U+25B3 WHITE UP-POINTING TRIANGLE)}
    XK_opentribulletdown    : exit($25BD);  {(U+25BD WHITE DOWN-POINTING TRIANGLE)}
    XK_openstar             : exit($2606);  {(U+2606 WHITE STAR)}
    XK_enfilledcircbullet   : exit($2022);  {(U+2022 BULLET)}
    XK_enfilledsqbullet     : exit($25AA);  {(U+25AA BLACK SMALL SQUARE)}
    XK_filledtribulletup    : exit($25B2);  {(U+25B2 BLACK UP-POINTING TRIANGLE)}
    XK_filledtribulletdown  : exit($25BC);  {(U+25BC BLACK DOWN-POINTING TRIANGLE)}
    XK_leftpointer          : exit($261C);  {(U+261C WHITE LEFT POINTING INDEX)}
    XK_rightpointer         : exit($261E);  {(U+261E WHITE RIGHT POINTING INDEX)}
    XK_club                 : exit($2663);  { U+2663 BLACK CLUB SUIT }
    XK_diamond              : exit($2666);  { U+2666 BLACK DIAMOND SUIT }
    XK_heart                : exit($2665);  { U+2665 BLACK HEART SUIT }
    XK_maltesecross         : exit($2720);  { U+2720 MALTESE CROSS }
    XK_dagger               : exit($2020);  { U+2020 DAGGER }
    XK_doubledagger         : exit($2021);  { U+2021 DOUBLE DAGGER }
    XK_checkmark            : exit($2713);  { U+2713 CHECK MARK }
    XK_ballotcross          : exit($2717);  { U+2717 BALLOT X }
    XK_musicalsharp         : exit($266F);  { U+266F MUSIC SHARP SIGN }
    XK_musicalflat          : exit($266D);  { U+266D MUSIC FLAT SIGN }
    XK_malesymbol           : exit($2642);  { U+2642 MALE SIGN }
    XK_femalesymbol         : exit($2640);  { U+2640 FEMALE SIGN }
    XK_telephone            : exit($260E);  { U+260E BLACK TELEPHONE }
    XK_telephonerecorder    : exit($2315);  { U+2315 TELEPHONE RECORDER }
    XK_phonographcopyright  : exit($2117);  { U+2117 SOUND RECORDING COPYRIGHT }
    XK_caret                : exit($2038);  { U+2038 CARET }
    XK_singlelowquotemark   : exit($201A);  { U+201A SINGLE LOW-9 QUOTATION MARK }
    XK_doublelowquotemark   : exit($201E);  { U+201E DOUBLE LOW-9 QUOTATION MARK }
{    XK_cursor               : exit($);}

    { APL }
    XK_leftcaret  : exit($003C);  {(U+003C LESS-THAN SIGN)}
    XK_rightcaret : exit($003E);  {(U+003E GREATER-THAN SIGN)}
    XK_downcaret  : exit($2228);  {(U+2228 LOGICAL OR)}
    XK_upcaret    : exit($2227);  {(U+2227 LOGICAL AND)}
    XK_overbar    : exit($00AF);  {(U+00AF MACRON)}
    XK_downtack   : exit($22A4);  { U+22A4 DOWN TACK }
    XK_upshoe     : exit($2229);  {(U+2229 INTERSECTION)}
    XK_downstile  : exit($230A);  { U+230A LEFT FLOOR }
    XK_underbar   : exit($005F);  {(U+005F LOW LINE)}
    XK_jot        : exit($2218);  { U+2218 RING OPERATOR }
    XK_quad       : exit($2395);  { U+2395 APL FUNCTIONAL SYMBOL QUAD }
    XK_uptack     : exit($22A5);  { U+22A5 UP TACK }
    XK_circle     : exit($25CB);  { U+25CB WHITE CIRCLE }
    XK_upstile    : exit($2308);  { U+2308 LEFT CEILING }
    XK_downshoe   : exit($222A);  {(U+222A UNION)}
    XK_rightshoe  : exit($2283);  {(U+2283 SUPERSET OF)}
    XK_leftshoe   : exit($2282);  {(U+2282 SUBSET OF)}
    XK_lefttack   : exit($22A3);  { U+22A3 LEFT TACK }
    XK_righttack  : exit($22A2);  { U+22A2 RIGHT TACK }

    { Hebrew }
    XK_hebrew_doublelowline : exit($2017);  { U+2017 DOUBLE LOW LINE }
    XK_hebrew_aleph         : exit($05D0);  { U+05D0 HEBREW LETTER ALEF }
    XK_hebrew_bet           : exit($05D1);  { U+05D1 HEBREW LETTER BET }
    XK_hebrew_gimel         : exit($05D2);  { U+05D2 HEBREW LETTER GIMEL }
    XK_hebrew_dalet         : exit($05D3);  { U+05D3 HEBREW LETTER DALET }
    XK_hebrew_he            : exit($05D4);  { U+05D4 HEBREW LETTER HE }
    XK_hebrew_waw           : exit($05D5);  { U+05D5 HEBREW LETTER VAV }
    XK_hebrew_zain          : exit($05D6);  { U+05D6 HEBREW LETTER ZAYIN }
    XK_hebrew_chet          : exit($05D7);  { U+05D7 HEBREW LETTER HET }
    XK_hebrew_tet           : exit($05D8);  { U+05D8 HEBREW LETTER TET }
    XK_hebrew_yod           : exit($05D9);  { U+05D9 HEBREW LETTER YOD }
    XK_hebrew_finalkaph     : exit($05DA);  { U+05DA HEBREW LETTER FINAL KAF }
    XK_hebrew_kaph          : exit($05DB);  { U+05DB HEBREW LETTER KAF }
    XK_hebrew_lamed         : exit($05DC);  { U+05DC HEBREW LETTER LAMED }
    XK_hebrew_finalmem      : exit($05DD);  { U+05DD HEBREW LETTER FINAL MEM }
    XK_hebrew_mem           : exit($05DE);  { U+05DE HEBREW LETTER MEM }
    XK_hebrew_finalnun      : exit($05DF);  { U+05DF HEBREW LETTER FINAL NUN }
    XK_hebrew_nun           : exit($05E0);  { U+05E0 HEBREW LETTER NUN }
    XK_hebrew_samech        : exit($05E1);  { U+05E1 HEBREW LETTER SAMEKH }
    XK_hebrew_ayin          : exit($05E2);  { U+05E2 HEBREW LETTER AYIN }
    XK_hebrew_finalpe       : exit($05E3);  { U+05E3 HEBREW LETTER FINAL PE }
    XK_hebrew_pe            : exit($05E4);  { U+05E4 HEBREW LETTER PE }
    XK_hebrew_finalzade     : exit($05E5);  { U+05E5 HEBREW LETTER FINAL TSADI }
    XK_hebrew_zade          : exit($05E6);  { U+05E6 HEBREW LETTER TSADI }
    XK_hebrew_qoph          : exit($05E7);  { U+05E7 HEBREW LETTER QOF }
    XK_hebrew_resh          : exit($05E8);  { U+05E8 HEBREW LETTER RESH }
    XK_hebrew_shin          : exit($05E9);  { U+05E9 HEBREW LETTER SHIN }
    XK_hebrew_taw           : exit($05EA);  { U+05EA HEBREW LETTER TAV }

    { Thai }
    XK_Thai_kokai             : exit($0E01);  { U+0E01 THAI CHARACTER KO KAI }
    XK_Thai_khokhai           : exit($0E02);  { U+0E02 THAI CHARACTER KHO KHAI }
    XK_Thai_khokhuat          : exit($0E03);  { U+0E03 THAI CHARACTER KHO KHUAT }
    XK_Thai_khokhwai          : exit($0E04);  { U+0E04 THAI CHARACTER KHO KHWAI }
    XK_Thai_khokhon           : exit($0E05);  { U+0E05 THAI CHARACTER KHO KHON }
    XK_Thai_khorakhang        : exit($0E06);  { U+0E06 THAI CHARACTER KHO RAKHANG }
    XK_Thai_ngongu            : exit($0E07);  { U+0E07 THAI CHARACTER NGO NGU }
    XK_Thai_chochan           : exit($0E08);  { U+0E08 THAI CHARACTER CHO CHAN }
    XK_Thai_choching          : exit($0E09);  { U+0E09 THAI CHARACTER CHO CHING }
    XK_Thai_chochang          : exit($0E0A);  { U+0E0A THAI CHARACTER CHO CHANG }
    XK_Thai_soso              : exit($0E0B);  { U+0E0B THAI CHARACTER SO SO }
    XK_Thai_chochoe           : exit($0E0C);  { U+0E0C THAI CHARACTER CHO CHOE }
    XK_Thai_yoying            : exit($0E0D);  { U+0E0D THAI CHARACTER YO YING }
    XK_Thai_dochada           : exit($0E0E);  { U+0E0E THAI CHARACTER DO CHADA }
    XK_Thai_topatak           : exit($0E0F);  { U+0E0F THAI CHARACTER TO PATAK }
    XK_Thai_thothan           : exit($0E10);  { U+0E10 THAI CHARACTER THO THAN }
    XK_Thai_thonangmontho     : exit($0E11);  { U+0E11 THAI CHARACTER THO NANGMONTHO }
    XK_Thai_thophuthao        : exit($0E12);  { U+0E12 THAI CHARACTER THO PHUTHAO }
    XK_Thai_nonen             : exit($0E13);  { U+0E13 THAI CHARACTER NO NEN }
    XK_Thai_dodek             : exit($0E14);  { U+0E14 THAI CHARACTER DO DEK }
    XK_Thai_totao             : exit($0E15);  { U+0E15 THAI CHARACTER TO TAO }
    XK_Thai_thothung          : exit($0E16);  { U+0E16 THAI CHARACTER THO THUNG }
    XK_Thai_thothahan         : exit($0E17);  { U+0E17 THAI CHARACTER THO THAHAN }
    XK_Thai_thothong          : exit($0E18);  { U+0E18 THAI CHARACTER THO THONG }
    XK_Thai_nonu              : exit($0E19);  { U+0E19 THAI CHARACTER NO NU }
    XK_Thai_bobaimai          : exit($0E1A);  { U+0E1A THAI CHARACTER BO BAIMAI }
    XK_Thai_popla             : exit($0E1B);  { U+0E1B THAI CHARACTER PO PLA }
    XK_Thai_phophung          : exit($0E1C);  { U+0E1C THAI CHARACTER PHO PHUNG }
    XK_Thai_fofa              : exit($0E1D);  { U+0E1D THAI CHARACTER FO FA }
    XK_Thai_phophan           : exit($0E1E);  { U+0E1E THAI CHARACTER PHO PHAN }
    XK_Thai_fofan             : exit($0E1F);  { U+0E1F THAI CHARACTER FO FAN }
    XK_Thai_phosamphao        : exit($0E20);  { U+0E20 THAI CHARACTER PHO SAMPHAO }
    XK_Thai_moma              : exit($0E21);  { U+0E21 THAI CHARACTER MO MA }
    XK_Thai_yoyak             : exit($0E22);  { U+0E22 THAI CHARACTER YO YAK }
    XK_Thai_rorua             : exit($0E23);  { U+0E23 THAI CHARACTER RO RUA }
    XK_Thai_ru                : exit($0E24);  { U+0E24 THAI CHARACTER RU }
    XK_Thai_loling            : exit($0E25);  { U+0E25 THAI CHARACTER LO LING }
    XK_Thai_lu                : exit($0E26);  { U+0E26 THAI CHARACTER LU }
    XK_Thai_wowaen            : exit($0E27);  { U+0E27 THAI CHARACTER WO WAEN }
    XK_Thai_sosala            : exit($0E28);  { U+0E28 THAI CHARACTER SO SALA }
    XK_Thai_sorusi            : exit($0E29);  { U+0E29 THAI CHARACTER SO RUSI }
    XK_Thai_sosua             : exit($0E2A);  { U+0E2A THAI CHARACTER SO SUA }
    XK_Thai_hohip             : exit($0E2B);  { U+0E2B THAI CHARACTER HO HIP }
    XK_Thai_lochula           : exit($0E2C);  { U+0E2C THAI CHARACTER LO CHULA }
    XK_Thai_oang              : exit($0E2D);  { U+0E2D THAI CHARACTER O ANG }
    XK_Thai_honokhuk          : exit($0E2E);  { U+0E2E THAI CHARACTER HO NOKHUK }
    XK_Thai_paiyannoi         : exit($0E2F);  { U+0E2F THAI CHARACTER PAIYANNOI }
    XK_Thai_saraa             : exit($0E30);  { U+0E30 THAI CHARACTER SARA A }
    XK_Thai_maihanakat        : exit($0E31);  { U+0E31 THAI CHARACTER MAI HAN-AKAT }
    XK_Thai_saraaa            : exit($0E32);  { U+0E32 THAI CHARACTER SARA AA }
    XK_Thai_saraam            : exit($0E33);  { U+0E33 THAI CHARACTER SARA AM }
    XK_Thai_sarai             : exit($0E34);  { U+0E34 THAI CHARACTER SARA I }
    XK_Thai_saraii            : exit($0E35);  { U+0E35 THAI CHARACTER SARA II }
    XK_Thai_saraue            : exit($0E36);  { U+0E36 THAI CHARACTER SARA UE }
    XK_Thai_sarauee           : exit($0E37);  { U+0E37 THAI CHARACTER SARA UEE }
    XK_Thai_sarau             : exit($0E38);  { U+0E38 THAI CHARACTER SARA U }
    XK_Thai_sarauu            : exit($0E39);  { U+0E39 THAI CHARACTER SARA UU }
    XK_Thai_phinthu           : exit($0E3A);  { U+0E3A THAI CHARACTER PHINTHU }
{    XK_Thai_maihanakat_maitho : exit($);}
    XK_Thai_baht              : exit($0E3F);  { U+0E3F THAI CURRENCY SYMBOL BAHT }
    XK_Thai_sarae             : exit($0E40);  { U+0E40 THAI CHARACTER SARA E }
    XK_Thai_saraae            : exit($0E41);  { U+0E41 THAI CHARACTER SARA AE }
    XK_Thai_sarao             : exit($0E42);  { U+0E42 THAI CHARACTER SARA O }
    XK_Thai_saraaimaimuan     : exit($0E43);  { U+0E43 THAI CHARACTER SARA AI MAIMUAN }
    XK_Thai_saraaimaimalai    : exit($0E44);  { U+0E44 THAI CHARACTER SARA AI MAIMALAI }
    XK_Thai_lakkhangyao       : exit($0E45);  { U+0E45 THAI CHARACTER LAKKHANGYAO }
    XK_Thai_maiyamok          : exit($0E46);  { U+0E46 THAI CHARACTER MAIYAMOK }
    XK_Thai_maitaikhu         : exit($0E47);  { U+0E47 THAI CHARACTER MAITAIKHU }
    XK_Thai_maiek             : exit($0E48);  { U+0E48 THAI CHARACTER MAI EK }
    XK_Thai_maitho            : exit($0E49);  { U+0E49 THAI CHARACTER MAI THO }
    XK_Thai_maitri            : exit($0E4A);  { U+0E4A THAI CHARACTER MAI TRI }
    XK_Thai_maichattawa       : exit($0E4B);  { U+0E4B THAI CHARACTER MAI CHATTAWA }
    XK_Thai_thanthakhat       : exit($0E4C);  { U+0E4C THAI CHARACTER THANTHAKHAT }
    XK_Thai_nikhahit          : exit($0E4D);  { U+0E4D THAI CHARACTER NIKHAHIT }
    XK_Thai_leksun            : exit($0E50);  { U+0E50 THAI DIGIT ZERO }
    XK_Thai_leknung           : exit($0E51);  { U+0E51 THAI DIGIT ONE }
    XK_Thai_leksong           : exit($0E52);  { U+0E52 THAI DIGIT TWO }
    XK_Thai_leksam            : exit($0E53);  { U+0E53 THAI DIGIT THREE }
    XK_Thai_leksi             : exit($0E54);  { U+0E54 THAI DIGIT FOUR }
    XK_Thai_lekha             : exit($0E55);  { U+0E55 THAI DIGIT FIVE }
    XK_Thai_lekhok            : exit($0E56);  { U+0E56 THAI DIGIT SIX }
    XK_Thai_lekchet           : exit($0E57);  { U+0E57 THAI DIGIT SEVEN }
    XK_Thai_lekpaet           : exit($0E58);  { U+0E58 THAI DIGIT EIGHT }
    XK_Thai_lekkao            : exit($0E59);  { U+0E59 THAI DIGIT NINE }

    { Korean }
    XK_Korean_Won : exit($20A9);  {(U+20A9 WON SIGN)}

    { Currency }
    XK_EuroSign : exit($20ac);     { U+20AC EURO SIGN }

  end;
  X11ConvertKeySymToUnicode := -1;
end;

