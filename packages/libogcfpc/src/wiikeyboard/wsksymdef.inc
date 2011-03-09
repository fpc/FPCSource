
{$IFDEF OGC_INTERFACE}
const
  KS_BackSpace = $08;
  KS_Tab = $09;
  KS_Linefeed = $0a;
  KS_Clear = $0b;
  KS_Return = $0d;
  KS_Escape = $1b;
  KS_space = $20;
  KS_exclam = $21;
  KS_quotedbl = $22;
  KS_numbersign = $23;
  KS_dollar = $24;
  KS_percent = $25;
  KS_ampersand = $26;
  KS_apostrophe = $27;
  KS_parenleft = $28;
  KS_parenright = $29;
  KS_asterisk = $2a;
  KS_plus = $2b;
  KS_comma = $2c;
  KS_minus = $2d;
  KS_period = $2e;
  KS_slash = $2f;
  KS_0 = $30;
  KS_1 = $31;
  KS_2 = $32;
  KS_3 = $33;
  KS_4 = $34;
  KS_5 = $35;
  KS_6 = $36;
  KS_7 = $37;
  KS_8 = $38;
  KS_9 = $39;
  KS_colon = $3a;
  KS_semicolon = $3b;
  KS_less = $3c;
  KS_equal = $3d;
  KS_greater = $3e;
  KS_question = $3f;
  KS_at = $40;
  KS_A = $41;
  KS_B = $42;
  KS_C = $43;
  KS_D = $44;
  KS_E = $45;
  KS_F = $46;
  KS_G = $47;
  KS_H = $48;
  KS_I = $49;
  KS_J = $4a;
  KS_K = $4b;
  KS_L = $4c;
  KS_M = $4d;
  KS_N = $4e;
  KS_O = $4f;
  KS_P = $50;
  KS_Q = $51;
  KS_R = $52;
  KS_S = $53;
  KS_T = $54;
  KS_U = $55;
  KS_V = $56;
  KS_W = $57;
  KS_X = $58;
  KS_Y = $59;
  KS_Z = $5a;
  KS_bracketleft = $5b;
  KS_backslash = $5c;
  KS_bracketright = $5d;
  KS_asciicircum = $5e;
  KS_underscore = $5f;
  KS_grave = $60;
  KS_a = $61;
  KS_b = $62;
  KS_c = $63;
  KS_d = $64;
  KS_e = $65;
  KS_f = $66;
  KS_g = $67;
  KS_h = $68;
  KS_i = $69;
  KS_j = $6a;
  KS_k = $6b;
  KS_l = $6c;
  KS_m = $6d;
  KS_n = $6e;
  KS_o = $6f;
  KS_p = $70;
  KS_q = $71;
  KS_r = $72;
  KS_s = $73;
  KS_t = $74;
  KS_u = $75;
  KS_v = $76;
  KS_w = $77;
  KS_x = $78;
  KS_y = $79;
  KS_z = $7a;
  KS_braceleft = $7b;
  KS_bar = $7c;
  KS_braceright = $7d;
  KS_asciitilde = $7e;
  KS_Delete = $7f;
  KS_nobreakspace = $a0;
  KS_exclamdown = $a1;
  KS_cent = $a2;
  KS_sterling = $a3;
  KS_currency = $a4;
  KS_yen = $a5;
  KS_brokenbar = $a6;
  KS_section = $a7;
  KS_diaeresis = $a8;
  KS_copyright = $a9;
  KS_ordfeminine = $aa;
  KS_guillemotleft = $ab;
  KS_notsign = $ac;
  KS_hyphen = $ad;
  KS_registered = $ae;
  KS_macron = $af;
  KS_degree = $b0;
  KS_plusminus = $b1;
  KS_twosuperior = $b2;
  KS_threesuperior = $b3;
  KS_acute = $b4;
  KS_mu = $b5;
  KS_paragraph = $b6;
  KS_periodcentered = $b7;
  KS_cedilla = $b8;
  KS_onesuperior = $b9;
  KS_masculine = $ba;
  KS_guillemotright = $bb;
  KS_onequarter = $bc;
  KS_onehalf = $bd;
  KS_threequarters = $be;
  KS_questiondown = $bf;
  KS_Agrave = $c0;
  KS_Aacute = $c1;
  KS_Acircumflex = $c2;
  KS_Atilde = $c3;
  KS_Adiaeresis = $c4;
  KS_Aring = $c5;
  KS_AE = $c6;
  KS_Ccedilla = $c7;
  KS_Egrave = $c8;
  KS_Eacute = $c9;
  KS_Ecircumflex = $ca;
  KS_Ediaeresis = $cb;
  KS_Igrave = $cc;
  KS_Iacute = $cd;
  KS_Icircumflex = $ce;
  KS_Idiaeresis = $cf;
  KS_ETH = $d0;
  KS_Ntilde = $d1;
  KS_Ograve = $d2;
  KS_Oacute = $d3;
  KS_Ocircumflex = $d4;
  KS_Otilde = $d5;
  KS_Odiaeresis = $d6;
  KS_multiply = $d7;
  KS_Ooblique = $d8;
  KS_Ugrave = $d9;
  KS_Uacute = $da;
  KS_Ucircumflex = $db;
  KS_Udiaeresis = $dc;
  KS_Yacute = $dd;
  KS_THORN = $de;
  KS_ssharp = $df;
  KS_agrave = $e0;
  KS_aacute = $e1;
  KS_acircumflex = $e2;
  KS_atilde = $e3;
  KS_adiaeresis = $e4;
  KS_aring = $e5;
  KS_ae = $e6;
  KS_ccedilla = $e7;
  KS_egrave = $e8;
  KS_eacute = $e9;
  KS_ecircumflex = $ea;
  KS_ediaeresis = $eb;
  KS_igrave = $ec;
  KS_iacute = $ed;
  KS_icircumflex = $ee;
  KS_idiaeresis = $ef;
  KS_eth = $f0;
  KS_ntilde = $f1;
  KS_ograve = $f2;
  KS_oacute = $f3;
  KS_ocircumflex = $f4;
  KS_otilde = $f5;
  KS_odiaeresis = $f6;
  KS_division = $f7;
  KS_oslash = $f8;
  KS_ugrave = $f9;
  KS_uacute = $fa;
  KS_ucircumflex = $fb;
  KS_udiaeresis = $fc;
  KS_yacute = $fd;
  KS_thorn = $fe;
  KS_ydiaeresis = $ff;
  KS_Odoubleacute = $150;
  KS_odoubleacute = $151;
  KS_Udoubleacute = $170;
  KS_udoubleacute = $171;
  (*
 * Group Dead (dead accents)
  *)


  KS_dead_grave = $0300;
  KS_dead_acute = $0301;
  KS_dead_circumflex = $0302;
  KS_dead_tilde = $0303;
  KS_dead_diaeresis = $0308;
  KS_dead_abovering = $030a;
  KS_dead_cedilla = $0327;
  (*
 * Group Cyrillic (koi8-r)
  *)


  KS_Cyrillic_A = $e1;
  KS_Cyrillic_BE = $e2;
  KS_Cyrillic_VE = $f7;
  KS_Cyrillic_GE = $e7;
  KS_Cyrillic_DE = $e4;
  KS_Cyrillic_IE = $e5;
  KS_Cyrillic_YO = $b3;
  KS_Cyrillic_ZHE = $f6;
  KS_Cyrillic_ZE = $fa;
  KS_Cyrillic_I = $e9;
  KS_Cyrillic_ISHORT = $ea;
  KS_Cyrillic_IUKR = $b6;
  KS_Cyrillic_YI = $b7;
  KS_Cyrillic_KA = $eb;
  KS_Cyrillic_EL = $ec;
  KS_Cyrillic_EM = $ed;
  KS_Cyrillic_EN = $ee;
  KS_Cyrillic_O = $ef;
  KS_Cyrillic_PE = $f0;
  KS_Cyrillic_ER = $f2;
  KS_Cyrillic_ES = $f3;
  KS_Cyrillic_TE = $f4;
  KS_Cyrillic_U = $f5;
  KS_Cyrillic_EF = $e6;
  KS_Cyrillic_HA = $e8;
  KS_Cyrillic_TSE = $e3;
  KS_Cyrillic_CHE = $fe;
  KS_Cyrillic_SHA = $fb;
  KS_Cyrillic_SCHA = $fd;
  KS_Cyrillic_HSIGHN = $ff;
  KS_Cyrillic_YERU = $f9;
  KS_Cyrillic_SSIGHN = $f8;
  KS_Cyrillic_E = $fc;
  KS_Cyrillic_YU = $e0;
  KS_Cyrillic_YA = $f1;
  KS_Cyrillic_a = $c1;
  KS_Cyrillic_be = $c2;
  KS_Cyrillic_ve = $d7;
  KS_Cyrillic_ge = $c7;
  KS_Cyrillic_de = $c4;
  KS_Cyrillic_ie = $c5;
  KS_Cyrillic_yo = $a3;
  KS_Cyrillic_zhe = $d6;
  KS_Cyrillic_ze = $da;
  KS_Cyrillic_i = $c9;
  KS_Cyrillic_ishort = $ca;
  KS_Cyrillic_iukr = $a6;
  KS_Cyrillic_yi = $a7;
  KS_Cyrillic_ka = $cb;
  KS_Cyrillic_el = $cc;
  KS_Cyrillic_em = $cd;
  KS_Cyrillic_en = $ce;
  KS_Cyrillic_o = $cf;
  KS_Cyrillic_pe = $d0;
  KS_Cyrillic_er = $d2;
  KS_Cyrillic_es = $d3;
  KS_Cyrillic_te = $d4;
  KS_Cyrillic_u = $d5;
  KS_Cyrillic_ef = $c6;
  KS_Cyrillic_ha = $c8;
  KS_Cyrillic_tse = $c3;
  KS_Cyrillic_che = $de;
  KS_Cyrillic_sha = $db;
  KS_Cyrillic_scha = $dd;
  KS_Cyrillic_hsighn = $df;
  KS_Cyrillic_yeru = $d9;
  KS_Cyrillic_ssighn = $d8;
  KS_Cyrillic_e = $dc;
  KS_Cyrillic_yu = $c0;
  KS_Cyrillic_ya = $d1;
  KS_Cyrillic_gheukr = $ad;
  KS_Cyrillic_GHEUKR = $bd;
  KS_Cyrillic_yeukr = $a4;
  KS_Cyrillic_YEUKR = $b4;
  (*
 * Group Latin-2 (iso8859-2)
  *)


  KS_L2_Aogonek = $a1;
  KS_L2_breve = $a2;
  KS_L2_Lstroke = $a3;
  KS_L2_Lcaron = $a5;
  KS_L2_Sacute = $a6;
  KS_L2_Scaron = $a9;
  KS_L2_Scedilla = $aa;
  KS_L2_Tcaron = $ab;
  KS_L2_Zacute = $ac;
  KS_L2_Zcaron = $ae;
  KS_L2_Zdotabove = $af;
  KS_L2_aogonek = $b1;
  KS_L2_ogonek = $b2;
  KS_L2_lstroke = $b3;
  KS_L2_lcaron = $b5;
  KS_L2_sacute = $b6;
  KS_L2_caron = $b7;
  KS_L2_scaron = $b9;
  KS_L2_scedilla = $ba;
  KS_L2_tcaron = $bb;
  KS_L2_zacute = $bc;
  KS_L2_dblacute = $bd;
  KS_L2_zcaron = $be;
  KS_L2_zdotabove = $bf;
  KS_L2_Racute = $c0;
  KS_L2_Abreve = $c3;
  KS_L2_Lacute = $c5;
  KS_L2_Cacute = $c6;
  KS_L2_Ccaron = $c8;
  KS_L2_Eogonek = $ca;
  KS_L2_Ecaron = $cc;
  KS_L2_Dcaron = $cf;
  KS_L2_Dstroke = $d0;
  KS_L2_Nacute = $d1;
  KS_L2_Ncaron = $d2;
  KS_L2_Odoubleacute = $d5;
  KS_L2_Rcaron = $d8;
  KS_L2_Uring = $d9;
  KS_L2_Udoubleacute = $db;
  KS_L2_Tcedilla = $de;
  KS_L2_racute = $e0;
  KS_L2_abreve = $e3;
  KS_L2_lacute = $e5;
  KS_L2_cacute = $e6;
  KS_L2_ccaron = $e8;
  KS_L2_eogonek = $ea;
  KS_L2_ecaron = $ec;
  KS_L2_dcaron = $ef;
  KS_L2_dstroke = $f0;
  KS_L2_nacute = $f1;
  KS_L2_odoubleacute = $f5;
  KS_L2_rcaron = $f8;
  KS_L2_uring = $f9;
  KS_L2_udoubleacute = $fb;
  KS_L2_tcedilla = $fe;
  KS_L2_dotabove = $ff;
  (*
 * Group Latin-5 (iso8859-9)
  *)


  KS_L5_Gbreve = $d0;
  KS_L5_Idotabove = $dd;
  KS_L5_Scedilla = $de;
  KS_L5_gbreve = $f0;
  KS_L5_idotless = $fd;
  KS_L5_scedilla = $fe;
  (*
 * Group Latin-7 (iso8859-13)
  *)


  KS_L7_rightdblquot = $a1;
  KS_L7_dbllow9quot = $a5;
  KS_L7_Ostroke = $a8;
  KS_L7_Rcedilla = $aa;
  KS_L7_AE = $af;
  KS_L7_leftdblquot = $b4;
  KS_L7_ostroke = $b8;
  KS_L7_rcedilla = $ba;
  KS_L7_ae = $bf;
  KS_L7_Aogonek = $c0;
  KS_L7_Iogonek = $c1;
  KS_L7_Amacron = $c2;
  KS_L7_Cacute = $c3;
  KS_L7_Eogonek = $c6;
  KS_L7_Emacron = $c7;
  KS_L7_Ccaron = $c8;
  KS_L7_Zacute = $ca;
  KS_L7_Edot = $cb;
  KS_L7_Gcedilla = $cc;
  KS_L7_Kcedilla = $cd;
  KS_L7_Imacron = $ce;
  KS_L7_Lcedilla = $cf;
  KS_L7_Scaron = $d0;
  KS_L7_Nacute = $d1;
  KS_L7_Ncedilla = $d2;
  KS_L7_Omacron = $d4;
  KS_L7_Uogonek = $d8;
  KS_L7_Lstroke = $d9;
  KS_L7_Sacute = $da;
  KS_L7_Umacron = $db;
  KS_L7_Zdot = $dd;
  KS_L7_Zcaron = $de;
  KS_L7_aogonek = $e0;
  KS_L7_iogonek = $e1;
  KS_L7_amacron = $e2;
  KS_L7_cacute = $e3;
  KS_L7_eogonek = $e6;
  KS_L7_emacron = $e7;
  KS_L7_ccaron = $e8;
  KS_L7_zacute = $ea;
  KS_L7_edot = $eb;
  KS_L7_gcedilla = $ec;
  KS_L7_kcedilla = $ed;
  KS_L7_imacron = $ee;
  KS_L7_lcedilla = $ef;
  KS_L7_scaron = $f0;
  KS_L7_nacute = $f1;
  KS_L7_ncedilla = $f2;
  KS_L7_omacron = $f4;
  KS_L7_uogonek = $f8;
  KS_L7_lstroke = $f9;
  KS_L7_sacute = $fa;
  KS_L7_umacron = $fb;
  KS_L7_zdot = $fd;
  KS_L7_zcaron = $fe;
  KS_L7_rightsnglquot = $ff;
  (*
 * Group 1 (modifiers)
  *)


  KS_Shift_L = $f101;
  KS_Shift_R = $f102;
  KS_Control_L = $f103;
  KS_Control_R = $f104;
  KS_Caps_Lock = $f105;
  KS_Shift_Lock = $f106;
  KS_Alt_L = $f107;
  KS_Alt_R = $f108;
  KS_Multi_key = $f109;
  KS_Mode_switch = $f10a;
  KS_Num_Lock = $f10b;
  KS_Hold_Screen = $f10c;
  KS_Cmd = $f10d;
  KS_Cmd1 = $f10e;
  KS_Cmd2 = $f10f;
  KS_Meta_L = $f110;
  KS_Meta_R = $f111;
  KS_Zenkaku_Hankaku = $f112;  (* Zenkaku/Hankaku toggle  *)
  KS_Hiragana_Katakana = $f113;  (* Hiragana/Katakana toggle  *)
  KS_Henkan_Mode = $f114;  (* Start/Stop Conversion  *)
  KS_Henkan = $f115;  (* Alias for Henkan_Mode  *)
  KS_Muhenkan = $f116;  (* Cancel Conversion  *)
  KS_Mode_Lock = $f117;
  (*
 * Group 2 (keypad) character in low byte
  *)


  KS_KP_F1 = $f291;
  KS_KP_F2 = $f292;
  KS_KP_F3 = $f293;
  KS_KP_F4 = $f294;
  KS_KP_Home = $f295;
  KS_KP_Left = $f296;
  KS_KP_Up = $f297;
  KS_KP_Right = $f298;
  KS_KP_Down = $f299;
  KS_KP_Prior = $f29a;
  KS_KP_Next = $f29b;
  KS_KP_End = $f29c;
  KS_KP_Begin = $f29d;
  KS_KP_Insert = $f29e;
  KS_KP_Delete = $f29f;
  KS_KP_Space = $f220;
  KS_KP_Tab = $f209;
  KS_KP_Enter = $f20d;
  KS_KP_Equal = $f23d;
  KS_KP_Numbersign = $f223;
  KS_KP_Multiply = $f22a;
  KS_KP_Add = $f22b;
  KS_KP_Separator = $f22c;
  KS_KP_Subtract = $f22d;
  KS_KP_Decimal = $f22e;
  KS_KP_Divide = $f22f;
  KS_KP_0 = $f230;
  KS_KP_1 = $f231;
  KS_KP_2 = $f232;
  KS_KP_3 = $f233;
  KS_KP_4 = $f234;
  KS_KP_5 = $f235;
  KS_KP_6 = $f236;
  KS_KP_7 = $f237;
  KS_KP_8 = $f238;
  KS_KP_9 = $f239;
  (*
 * Group 3 (function)
  *)


  KS_f1 = $f300;
  KS_f2 = $f301;
  KS_f3 = $f302;
  KS_f4 = $f303;
  KS_f5 = $f304;
  KS_f6 = $f305;
  KS_f7 = $f306;
  KS_f8 = $f307;
  KS_f9 = $f308;
  KS_f10 = $f309;
  KS_f11 = $f30a;
  KS_f12 = $f30b;
  KS_f13 = $f30c;
  KS_f14 = $f30d;
  KS_f15 = $f30e;
  KS_f16 = $f30f;
  KS_f17 = $f310;
  KS_f18 = $f311;
  KS_f19 = $f312;
  KS_f20 = $f313;
  KS_F1 = $f340;
  KS_F2 = $f341;
  KS_F3 = $f342;
  KS_F4 = $f343;
  KS_F5 = $f344;
  KS_F6 = $f345;
  KS_F7 = $f346;
  KS_F8 = $f347;
  KS_F9 = $f348;
  KS_F10 = $f349;
  KS_F11 = $f34a;
  KS_F12 = $f34b;
  KS_F13 = $f34c;
  KS_F14 = $f34d;
  KS_F15 = $f34e;
  KS_F16 = $f34f;
  KS_F17 = $f350;
  KS_F18 = $f351;
  KS_F19 = $f352;
  KS_F20 = $f353;
  KS_Home = $f381;
  KS_Prior = $f382;
  KS_Next = $f383;
  KS_Up = $f384;
  KS_Down = $f385;
  KS_Left = $f386;
  KS_Right = $f387;
  KS_End = $f388;
  KS_Insert = $f389;
  KS_Help = $f38a;
  KS_Execute = $f38b;
  KS_Find = $f38c;
  KS_Select = $f38d;
  KS_Again = $f38e;
  KS_Props = $f38f;
  KS_Undo = $f390;
  KS_Front = $f391;
  KS_Copy = $f392;
  KS_Open = $f393;
  KS_Paste = $f394;
  KS_Cut = $f395;
  KS_Menu = $f3c0;
  KS_Pause = $f3c1;
  KS_Print_Screen = $f3c2;
  KS_AudioMute = $f3d1;
  KS_AudioLower = $f3d2;
  KS_AudioRaise = $f3d3;
  (*
 * Group 4 (command)
  *)


  KS_Cmd_Screen0 = $f400;
  KS_Cmd_Screen1 = $f401;
  KS_Cmd_Screen2 = $f402;
  KS_Cmd_Screen3 = $f403;
  KS_Cmd_Screen4 = $f404;
  KS_Cmd_Screen5 = $f405;
  KS_Cmd_Screen6 = $f406;
  KS_Cmd_Screen7 = $f407;
  KS_Cmd_Screen8 = $f408;
  KS_Cmd_Screen9 = $f409;
  KS_Cmd_Screen10 = $f40a;
  KS_Cmd_Screen11 = $f40b;
  KS_Cmd_Debugger = $f420;
  KS_Cmd_ResetEmul = $f421;
  KS_Cmd_ResetClose = $f422;
  KS_Cmd_BacklightOn = $f423;
  KS_Cmd_BacklightOff = $f424;
  KS_Cmd_BacklightToggle = $f425;
  KS_Cmd_BrightnessUp = $f426;
  KS_Cmd_BrightnessDown = $f427;
  KS_Cmd_BrightnessRotate = $f428;
  KS_Cmd_ContrastUp = $f429;
  KS_Cmd_ContrastDown = $f42a;
  KS_Cmd_ContrastRotate = $f42b;
  KS_Cmd_ScrollBack = $f42c;
  KS_Cmd_ScrollFwd = $f42d;
  KS_Cmd_KbdReset = $f42e;
  (*
 * Group 5 (internal)
  *)


  KS_voidSymbol = $f500;
  (*ENDKEYSYMDECL *)


  (*
 * keysym groups
  *)


  KS_GROUP_Mod = $f100;
  KS_GROUP_Keypad = $f200;
  KS_GROUP_Function = $f300;
  KS_GROUP_Command = $f400;
  KS_GROUP_Internal = $f500;
  KS_GROUP_Dead = $f801;  (* not encoded in keysym  *)
  KS_GROUP_Ascii = $f802;  (* not encoded in keysym  *)
  KS_GROUP_Keycode = $f803;  (* not encoded in keysym  *)
  KS_NUMKEYCODES = $1000;

function KS_KEYCODE(v: cint): cint; inline;
function KS_KEYCODE(v: cint): cint; inline;
function KS_GROUP(k: cint): cint; inline;
function KS_VALUE(k: cint): cint; inline;
function KB_ENCODING(e: cint): cint; inline;
function KB_VARIANT(e: cint): cint; inline;

const
  KB_NONE = $0000;
  KB_USER = $0100;
  KB_US = $0200;
  KB_DE = $0300;
  KB_DK = $0400;
  KB_IT = $0500;
  KB_FR = $0600;
  KB_UK = $0700;
  KB_JP = $0800;
  KB_SV = $0900;
  KB_NO = $0a00;
  KB_ES = $0b00;
  KB_HU = $0c00;
  KB_BE = $0d00;
  KB_RU = $0e00;
  KB_SG = $0f00;
  KB_SF = $1000;
  KB_PT = $1100;
  KB_UA = $1200;
  KB_LT = $1300;
  KB_LA = $1400;
  KB_BR = $1500;
  KB_NL = $1600;
  KB_TR = $1700;
  KB_PL = $1800;
  KB_SI = $1900;
  KB_CF = $1a00;
  KB_LV = $1b00;
  KB_NODEAD = $000001;  (* disable dead accents  *)
  KB_DECLK = $000002;  (* DEC LKnnn layout  *)
  KB_LK401 = $000004;  (* DEC LK401 instead LK201  *)
  KB_SWAPCTRLCAPS = $000008;  (* swap Left-Control and Caps-Lock  *)
  KB_DVORAK = $000010;  (* Dvorak layout  *)
  KB_METAESC = $000020;  (* generate ESC prefix on ALT-key  *)
  KB_IOPENER = $000040;  (* f1-f12 -> ESC,f1-f11  *)
  KB_MACHDEP = $000080;  (* machine dependent  *)
  KB_APPLE = $010000;  (* Apple specific layout  *)

  KB_ENCTAB = (( KB_USER,	'user' ),
  	( KB_US,	'us' ),
  	( KB_DE,	'de' ),
  	( KB_DK,	'dk' ),
  	( KB_IT,	'it' ),
  	( KB_FR,	'fr' ),
  	( KB_UK,	'uk' ),
  	( KB_JP,	'jp' ),
  	( KB_SV,	'sv' ),
  	( KB_NO,	'no' ),
  	( KB_ES,	'es' ),
  	( KB_HU,	'hu' ),
  	( KB_BE,	'be' ),
  	( KB_RU,	'ru' ),
  	( KB_UA,	'ua' ),
  	( KB_SG,	'sg' ),
  	( KB_SF,	'sf' ),
  	( KB_PT,	'pt' ),
  	( KB_LT,	'lt' ),
  	( KB_LA,	'la' ),
  	( KB_BR,	'br' ),
  	( KB_NL,	'nl' ),
  	( KB_TR,	'tr' ),
  	( KB_PL,	'pl' ),
  	( KB_SI,	'si' ),
  	( KB_CF,	'cf' ),
  	( KB_LV,	'lv' ));

  KB_VARTAB = (( KB_NODEAD,	'nodead' ),
  	( KB_DECLK,	'declk' ),
  	( KB_LK401,	'lk401' ),
  	( KB_SWAPCTRLCAPS, 'swapctrlcaps' ),
  	( KB_DVORAK,	'dvorak' ),
  	( KB_METAESC,	'metaesc' ),
  	( KB_IOPENER,	'iopener' ),
  	( KB_MACHDEP,	'machdep' ),
  	( KB_APPLE,	'apple' ));
{$ENDIF}



{$IFDEF OGC_IMPLEMENTATION}
function KS_KEYCODE(v: cint): cint; inline;
begin
  KS_KEYCODE := v or $e000;
end;

function KS_GROUP(k: cint): cint; inline;
begin
  if (k >= $0300) and (k < $0370) then
    KS_GROUP := KS_GROUP_Dead
  else
	if (k and $f000) = $e000 then
    KS_GROUP := KS_GROUP_Keycode
  else
	if (k and $f800) = $f000 then
    KS_GROUP := (k and $ff00)
  else
		KS_GROUP := KS_GROUP_Ascii;
end;

function KS_VALUE(k: cint): cint; inline;
begin
  if (k and $f000) = $e000 then
    KS_VALUE := (k and $0fff)
  else
	if (k and $f800) = $f000 then
    KS_VALUE := (k and $00ff)
  else
    KS_VALUE := k;
end;

function KB_ENCODING(e: cint): cint; inline;
begin
  KB_ENCODING := (e and $0000ff00);
end;

function KB_VARIANT(e: cint): cint; inline;
begin
  KB_VARIANT := (e and $ffff00ff);
end;
{$ENDIF}

