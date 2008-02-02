{
Converted from X11/keysym.h and X11/keysymdef.h

Capital letter consts renamed from XK_... to XKc_...
 (since Pascal isn't case-sensitive)

i.e.
C      Pascal
XK_a   XK_a
XK_A   XKc_A
}

Unit keysym;

Interface

{* default keysyms *}
{$DEFINE XK_MISCELLANY}
{$DEFINE XK_XKB_KEYS}
{$DEFINE XK_3270}
{$DEFINE XK_LATIN1}
{$DEFINE XK_LATIN2}
{$DEFINE XK_LATIN3}
{$DEFINE XK_LATIN4}
{$DEFINE XK_LATIN8}
{$DEFINE XK_LATIN9}
{$DEFINE XK_KATAKANA}
{$DEFINE XK_ARABIC}
{$DEFINE XK_CYRILLIC}
{$DEFINE XK_GREEK}
{$DEFINE XK_TECHNICAL}
{$DEFINE XK_SPECIAL}
{$DEFINE XK_PUBLISHING}
{$DEFINE XK_APL}
{$DEFINE XK_HEBREW}
{$DEFINE XK_THAI}
{$DEFINE XK_KOREAN}
{$DEFINE XK_ARMENIAN}
{$DEFINE XK_GEORGIAN}
{$DEFINE XK_CAUCASUS}
{$DEFINE XK_VIETNAMESE}
{$DEFINE XK_CURRENCY}

Const
  XK_VoidSymbol         = $FFFFFF;      { void symbol }

{$IFDEF XK_MISCELLANY}
{*
 * TTY Functions, cleverly chosen to map to ascii, for convenience of
 * programming, but could have been arbitrary (at the cost of lookup
 * tables in client code.
 *}

  XK_BackSpace          = $FF08;        { back space, back char }
  XK_Tab                = $FF09;
  XK_Linefeed           = $FF0A;        { Linefeed, LF }
  XK_Clear              = $FF0B;
  XK_Return             = $FF0D;        { Return, enter }
  XK_Pause              = $FF13;        { Pause, hold }
  XK_Scroll_Lock        = $FF14;
  XK_Sys_Req            = $FF15;
  XK_Escape             = $FF1B;
  XK_Delete             = $FFFF;        { Delete, rubout }



{ International & multi-key character composition }

  XK_Multi_key          = $FF20;  { Multi-key character compose }
  XK_Codeinput          = $FF37;
  XK_SingleCandidate    = $FF3C;
  XK_MultipleCandidate  = $FF3D;
  XK_PreviousCandidate  = $FF3E;

{ Japanese keyboard support }

  XK_Kanji              = $FF21;        { Kanji, Kanji convert }
  XK_Muhenkan           = $FF22;  { Cancel Conversion }
  XK_Henkan_Mode        = $FF23;  { Start/Stop Conversion }
  XK_Henkan             = $FF23;  { Alias for Henkan_Mode }
  XK_Romaji             = $FF24;  { to Romaji }
  XK_Hiragana           = $FF25;  { to Hiragana }
  XK_Katakana           = $FF26;  { to Katakana }
  XK_Hiragana_Katakana  = $FF27;  { Hiragana/Katakana toggle }
  XK_Zenkaku            = $FF28;  { to Zenkaku }
  XK_Hankaku            = $FF29;  { to Hankaku }
  XK_Zenkaku_Hankaku    = $FF2A;  { Zenkaku/Hankaku toggle }
  XK_Touroku            = $FF2B;  { Add to Dictionary }
  XK_Massyo             = $FF2C;  { Delete from Dictionary }
  XK_Kana_Lock          = $FF2D;  { Kana Lock }
  XK_Kana_Shift         = $FF2E;  { Kana Shift }
  XK_Eisu_Shift         = $FF2F;  { Alphanumeric Shift }
  XK_Eisu_toggle        = $FF30;  { Alphanumeric toggle }
  XK_Kanji_Bangou       = $FF37;  { Codeinput }
  XK_Zen_Koho           = $FF3D;  { Multiple/All Candidate(s) }
  XK_Mae_Koho           = $FF3E;  { Previous Candidate }

{ = $FF31 thru = $FF3F are under XK_KOREAN }

{ Cursor control & motion }

  XK_Home               = $FF50;
  XK_Left               = $FF51;        { Move left, left arrow }
  XK_Up                 = $FF52;        { Move up, up arrow }
  XK_Right              = $FF53;        { Move right, right arrow }
  XK_Down               = $FF54;        { Move down, down arrow }
  XK_Prior              = $FF55;        { Prior, previous }
  XK_Page_Up            = $FF55;
  XK_Next               = $FF56;        { Next }
  XK_Page_Down          = $FF56;
  XK_End                = $FF57;        { EOL }
  XK_Begin              = $FF58;        { BOL }


{ Misc Functions }

  XK_Select             = $FF60;        { Select, mark }
  XK_Print              = $FF61;
  XK_Execute            = $FF62;        { Execute, run, do }
  XK_Insert             = $FF63;        { Insert, insert here }
  XK_Undo               = $FF65;        { Undo, oops }
  XK_Redo               = $FF66;        { redo, again }
  XK_Menu               = $FF67;
  XK_Find               = $FF68;        { Find, search }
  XK_Cancel             = $FF69;        { Cancel, stop, abort, exit }
  XK_Help               = $FF6A;        { Help }
  XK_Break              = $FF6B;
  XK_Mode_switch        = $FF7E;        { Character set switch }
  XK_script_switch      = $FF7E;        { Alias for mode_switch }
  XK_Num_Lock           = $FF7F;

{ Keypad Functions, keypad numbers cleverly chosen to map to ascii }

  XK_KP_Space           = $FF80;        { space }
  XK_KP_Tab             = $FF89;
  XK_KP_Enter           = $FF8D;        { enter }
  XK_KP_F1              = $FF91;        { PF1, KP_A, ... }
  XK_KP_F2              = $FF92;
  XK_KP_F3              = $FF93;
  XK_KP_F4              = $FF94;
  XK_KP_Home            = $FF95;
  XK_KP_Left            = $FF96;
  XK_KP_Up              = $FF97;
  XK_KP_Right           = $FF98;
  XK_KP_Down            = $FF99;
  XK_KP_Prior           = $FF9A;
  XK_KP_Page_Up         = $FF9A;
  XK_KP_Next            = $FF9B;
  XK_KP_Page_Down       = $FF9B;
  XK_KP_End             = $FF9C;
  XK_KP_Begin           = $FF9D;
  XK_KP_Insert          = $FF9E;
  XK_KP_Delete          = $FF9F;
  XK_KP_Equal           = $FFBD;        { equals }
  XK_KP_Multiply        = $FFAA;
  XK_KP_Add             = $FFAB;
  XK_KP_Separator       = $FFAC;        { separator, often comma }
  XK_KP_Subtract        = $FFAD;
  XK_KP_Decimal         = $FFAE;
  XK_KP_Divide          = $FFAF;

  XK_KP_0               = $FFB0;
  XK_KP_1               = $FFB1;
  XK_KP_2               = $FFB2;
  XK_KP_3               = $FFB3;
  XK_KP_4               = $FFB4;
  XK_KP_5               = $FFB5;
  XK_KP_6               = $FFB6;
  XK_KP_7               = $FFB7;
  XK_KP_8               = $FFB8;
  XK_KP_9               = $FFB9;



{*
 * Auxilliary Functions; note the duplicate definitions for left and right
 * function keys;  Sun keyboards and a few other manufactures have such
 * function key groups on the left and/or right sides of the keyboard.
 * We've not found a keyboard with more than 35 function keys total.
 *}

  XK_F1                 = $FFBE;
  XK_F2                 = $FFBF;
  XK_F3                 = $FFC0;
  XK_F4                 = $FFC1;
  XK_F5                 = $FFC2;
  XK_F6                 = $FFC3;
  XK_F7                 = $FFC4;
  XK_F8                 = $FFC5;
  XK_F9                 = $FFC6;
  XK_F10                = $FFC7;
  XK_F11                = $FFC8;
  XK_L1                 = $FFC8;
  XK_F12                = $FFC9;
  XK_L2                 = $FFC9;
  XK_F13                = $FFCA;
  XK_L3                 = $FFCA;
  XK_F14                = $FFCB;
  XK_L4                 = $FFCB;
  XK_F15                = $FFCC;
  XK_L5                 = $FFCC;
  XK_F16                = $FFCD;
  XK_L6                 = $FFCD;
  XK_F17                = $FFCE;
  XK_L7                 = $FFCE;
  XK_F18                = $FFCF;
  XK_L8                 = $FFCF;
  XK_F19                = $FFD0;
  XK_L9                 = $FFD0;
  XK_F20                = $FFD1;
  XK_L10                = $FFD1;
  XK_F21                = $FFD2;
  XK_R1                 = $FFD2;
  XK_F22                = $FFD3;
  XK_R2                 = $FFD3;
  XK_F23                = $FFD4;
  XK_R3                 = $FFD4;
  XK_F24                = $FFD5;
  XK_R4                 = $FFD5;
  XK_F25                = $FFD6;
  XK_R5                 = $FFD6;
  XK_F26                = $FFD7;
  XK_R6                 = $FFD7;
  XK_F27                = $FFD8;
  XK_R7                 = $FFD8;
  XK_F28                = $FFD9;
  XK_R8                 = $FFD9;
  XK_F29                = $FFDA;
  XK_R9                 = $FFDA;
  XK_F30                = $FFDB;
  XK_R10                = $FFDB;
  XK_F31                = $FFDC;
  XK_R11                = $FFDC;
  XK_F32                = $FFDD;
  XK_R12                = $FFDD;
  XK_F33                = $FFDE;
  XK_R13                = $FFDE;
  XK_F34                = $FFDF;
  XK_R14                = $FFDF;
  XK_F35                = $FFE0;
  XK_R15                = $FFE0;

{ Modifiers }

  XK_Shift_L            = $FFE1;        { Left shift }
  XK_Shift_R            = $FFE2;        { Right shift }
  XK_Control_L          = $FFE3;        { Left control }
  XK_Control_R          = $FFE4;        { Right control }
  XK_Caps_Lock          = $FFE5;        { Caps lock }
  XK_Shift_Lock         = $FFE6;        { Shift lock }

  XK_Meta_L             = $FFE7;        { Left meta }
  XK_Meta_R             = $FFE8;        { Right meta }
  XK_Alt_L              = $FFE9;        { Left alt }
  XK_Alt_R              = $FFEA;        { Right alt }
  XK_Super_L            = $FFEB;        { Left super }
  XK_Super_R            = $FFEC;        { Right super }
  XK_Hyper_L            = $FFED;        { Left hyper }
  XK_Hyper_R            = $FFEE;        { Right hyper }
{$ENDIF} { XK_MISCELLANY }

{*
 * ISO 9995 Function and Modifier Keys
 * Byte 3 = = $FE
 *}

{$IFDEF XK_XKB_KEYS}
  XK_ISO_Lock                                   = $FE01;
  XK_ISO_Level2_Latch                           = $FE02;
  XK_ISO_Level3_Shift                           = $FE03;
  XK_ISO_Level3_Latch                           = $FE04;
  XK_ISO_Level3_Lock                            = $FE05;
  XK_ISO_Group_Shift                            = $FF7E;        { Alias for mode_switch }
  XK_ISO_Group_Latch                            = $FE06;
  XK_ISO_Group_Lock                             = $FE07;
  XK_ISO_Next_Group                             = $FE08;
  XK_ISO_Next_Group_Lock                                = $FE09;
  XK_ISO_Prev_Group                             = $FE0A;
  XK_ISO_Prev_Group_Lock                                = $FE0B;
  XK_ISO_First_Group                            = $FE0C;
  XK_ISO_First_Group_Lock                               = $FE0D;
  XK_ISO_Last_Group                             = $FE0E;
  XK_ISO_Last_Group_Lock                                = $FE0F;

  XK_ISO_Left_Tab                                       = $FE20;
  XK_ISO_Move_Line_Up                           = $FE21;
  XK_ISO_Move_Line_Down                         = $FE22;
  XK_ISO_Partial_Line_Up                                = $FE23;
  XK_ISO_Partial_Line_Down                      = $FE24;
  XK_ISO_Partial_Space_Left                     = $FE25;
  XK_ISO_Partial_Space_Right                    = $FE26;
  XK_ISO_Set_Margin_Left                                = $FE27;
  XK_ISO_Set_Margin_Right                               = $FE28;
  XK_ISO_Release_Margin_Left                    = $FE29;
  XK_ISO_Release_Margin_Right                   = $FE2A;
  XK_ISO_Release_Both_Margins                   = $FE2B;
  XK_ISO_Fast_Cursor_Left                               = $FE2C;
  XK_ISO_Fast_Cursor_Right                      = $FE2D;
  XK_ISO_Fast_Cursor_Up                         = $FE2E;
  XK_ISO_Fast_Cursor_Down                               = $FE2F;
  XK_ISO_Continuous_Underline                   = $FE30;
  XK_ISO_Discontinuous_Underline                        = $FE31;
  XK_ISO_Emphasize                              = $FE32;
  XK_ISO_Center_Object                          = $FE33;
  XK_ISO_Enter                                  = $FE34;

  XK_dead_grave                                 = $FE50;
  XK_dead_acute                                 = $FE51;
  XK_dead_circumflex                            = $FE52;
  XK_dead_tilde                                 = $FE53;
  XK_dead_macron                                        = $FE54;
  XK_dead_breve                                 = $FE55;
  XK_dead_abovedot                              = $FE56;
  XK_dead_diaeresis                             = $FE57;
  XK_dead_abovering                             = $FE58;
  XK_dead_doubleacute                           = $FE59;
  XK_dead_caron                                 = $FE5A;
  XK_dead_cedilla                                       = $FE5B;
  XK_dead_ogonek                                        = $FE5C;
  XK_dead_iota                                  = $FE5D;
  XK_dead_voiced_sound                          = $FE5E;
  XK_dead_semivoiced_sound                      = $FE5F;
  XK_dead_belowdot                              = $FE60;
  XK_dead_hook                                  = $FE61;
  XK_dead_horn                                  = $FE62;

  XK_First_Virtual_Screen                               = $FED0;
  XK_Prev_Virtual_Screen                                = $FED1;
  XK_Next_Virtual_Screen                                = $FED2;
  XK_Last_Virtual_Screen                                = $FED4;
  XK_Terminate_Server                           = $FED5;

  XK_AccessX_Enable                             = $FE70;
  XK_AccessX_Feedback_Enable                    = $FE71;
  XK_RepeatKeys_Enable                          = $FE72;
  XK_SlowKeys_Enable                            = $FE73;
  XK_BounceKeys_Enable                          = $FE74;
  XK_StickyKeys_Enable                          = $FE75;
  XK_MouseKeys_Enable                           = $FE76;
  XK_MouseKeys_Accel_Enable                     = $FE77;
  XK_Overlay1_Enable                            = $FE78;
  XK_Overlay2_Enable                            = $FE79;
  XK_AudibleBell_Enable                         = $FE7A;

  XK_Pointer_Left                                       = $FEE0;
  XK_Pointer_Right                              = $FEE1;
  XK_Pointer_Up                                 = $FEE2;
  XK_Pointer_Down                                       = $FEE3;
  XK_Pointer_UpLeft                             = $FEE4;
  XK_Pointer_UpRight                            = $FEE5;
  XK_Pointer_DownLeft                           = $FEE6;
  XK_Pointer_DownRight                          = $FEE7;
  XK_Pointer_Button_Dflt                                = $FEE8;
  XK_Pointer_Button1                            = $FEE9;
  XK_Pointer_Button2                            = $FEEA;
  XK_Pointer_Button3                            = $FEEB;
  XK_Pointer_Button4                            = $FEEC;
  XK_Pointer_Button5                            = $FEED;
  XK_Pointer_DblClick_Dflt                      = $FEEE;
  XK_Pointer_DblClick1                          = $FEEF;
  XK_Pointer_DblClick2                          = $FEF0;
  XK_Pointer_DblClick3                          = $FEF1;
  XK_Pointer_DblClick4                          = $FEF2;
  XK_Pointer_DblClick5                          = $FEF3;
  XK_Pointer_Drag_Dflt                          = $FEF4;
  XK_Pointer_Drag1                              = $FEF5;
  XK_Pointer_Drag2                              = $FEF6;
  XK_Pointer_Drag3                              = $FEF7;
  XK_Pointer_Drag4                              = $FEF8;
  XK_Pointer_Drag5                              = $FEFD;

  XK_Pointer_EnableKeys                         = $FEF9;
  XK_Pointer_Accelerate                         = $FEFA;
  XK_Pointer_DfltBtnNext                                = $FEFB;
  XK_Pointer_DfltBtnPrev                                = $FEFC;

{$ENDIF}

{*
 * 3270 Terminal Keys
 * Byte 3 = = $FD
 *}

{$IFDEF XK_3270}
  XK_3270_Duplicate      = $FD01;
  XK_3270_FieldMark      = $FD02;
  XK_3270_Right2         = $FD03;
  XK_3270_Left2          = $FD04;
  XK_3270_BackTab        = $FD05;
  XK_3270_EraseEOF       = $FD06;
  XK_3270_EraseInput     = $FD07;
  XK_3270_Reset          = $FD08;
  XK_3270_Quit           = $FD09;
  XK_3270_PA1            = $FD0A;
  XK_3270_PA2            = $FD0B;
  XK_3270_PA3            = $FD0C;
  XK_3270_Test           = $FD0D;
  XK_3270_Attn           = $FD0E;
  XK_3270_CursorBlink    = $FD0F;
  XK_3270_AltCursor      = $FD10;
  XK_3270_KeyClick       = $FD11;
  XK_3270_Jump           = $FD12;
  XK_3270_Ident          = $FD13;
  XK_3270_Rule           = $FD14;
  XK_3270_Copy           = $FD15;
  XK_3270_Play           = $FD16;
  XK_3270_Setup          = $FD17;
  XK_3270_Record         = $FD18;
  XK_3270_ChangeScreen   = $FD19;
  XK_3270_DeleteWord     = $FD1A;
  XK_3270_ExSelect       = $FD1B;
  XK_3270_CursorSelect   = $FD1C;
  XK_3270_PrintScreen    = $FD1D;
  XK_3270_Enter          = $FD1E;
{$ENDIF}

{*
 *  Latin 1
 *  Byte 3 = 0
 *}
{$IFDEF XK_LATIN1}
  XK_space               = $020;
  XK_exclam              = $021;
  XK_quotedbl            = $022;
  XK_numbersign          = $023;
  XK_dollar              = $024;
  XK_percent             = $025;
  XK_ampersand           = $026;
  XK_apostrophe          = $027;
  XK_quoteright          = $027;        { deprecated }
  XK_parenleft           = $028;
  XK_parenright          = $029;
  XK_asterisk            = $02a;
  XK_plus                = $02b;
  XK_comma               = $02c;
  XK_minus               = $02d;
  XK_period              = $02e;
  XK_slash               = $02f;
  XK_0                   = $030;
  XK_1                   = $031;
  XK_2                   = $032;
  XK_3                   = $033;
  XK_4                   = $034;
  XK_5                   = $035;
  XK_6                   = $036;
  XK_7                   = $037;
  XK_8                   = $038;
  XK_9                   = $039;
  XK_colon               = $03a;
  XK_semicolon           = $03b;
  XK_less                = $03c;
  XK_equal               = $03d;
  XK_greater             = $03e;
  XK_question            = $03f;
  XK_at                  = $040;
  XKc_A                   = $041;
  XKc_B                   = $042;
  XKc_C                   = $043;
  XKc_D                   = $044;
  XKc_E                   = $045;
  XKc_F                   = $046;
  XKc_G                   = $047;
  XKc_H                   = $048;
  XKc_I                   = $049;
  XKc_J                   = $04a;
  XKc_K                   = $04b;
  XKc_L                   = $04c;
  XKc_M                   = $04d;
  XKc_N                   = $04e;
  XKc_O                   = $04f;
  XKc_P                   = $050;
  XKc_Q                   = $051;
  XKc_R                   = $052;
  XKc_S                   = $053;
  XKc_T                   = $054;
  XKc_U                   = $055;
  XKc_V                   = $056;
  XKc_W                   = $057;
  XKc_X                   = $058;
  XKc_Y                   = $059;
  XKc_Z                   = $05a;
  XK_bracketleft         = $05b;
  XK_backslash           = $05c;
  XK_bracketright        = $05d;
  XK_asciicircum         = $05e;
  XK_underscore          = $05f;
  XK_grave               = $060;
  XK_quoteleft           = $060;        { deprecated }
  XK_a                   = $061;
  XK_b                   = $062;
  XK_c                   = $063;
  XK_d                   = $064;
  XK_e                   = $065;
  XK_f                   = $066;
  XK_g                   = $067;
  XK_h                   = $068;
  XK_i                   = $069;
  XK_j                   = $06a;
  XK_k                   = $06b;
  XK_l                   = $06c;
  XK_m                   = $06d;
  XK_n                   = $06e;
  XK_o                   = $06f;
  XK_p                   = $070;
  XK_q                   = $071;
  XK_r                   = $072;
  XK_s                   = $073;
  XK_t                   = $074;
  XK_u                   = $075;
  XK_v                   = $076;
  XK_w                   = $077;
  XK_x                   = $078;
  XK_y                   = $079;
  XK_z                   = $07a;
  XK_braceleft           = $07b;
  XK_bar                 = $07c;
  XK_braceright          = $07d;
  XK_asciitilde          = $07e;

  XK_nobreakspace        = $0a0;
  XK_exclamdown          = $0a1;
  XK_cent                = $0a2;
  XK_sterling            = $0a3;
  XK_currency            = $0a4;
  XK_yen                 = $0a5;
  XK_brokenbar           = $0a6;
  XK_section             = $0a7;
  XK_diaeresis           = $0a8;
  XK_copyright           = $0a9;
  XK_ordfeminine         = $0aa;
  XK_guillemotleft       = $0ab;        { left angle quotation mark }
  XK_notsign             = $0ac;
  XK_hyphen              = $0ad;
  XK_registered          = $0ae;
  XK_macron              = $0af;
  XK_degree              = $0b0;
  XK_plusminus           = $0b1;
  XK_twosuperior         = $0b2;
  XK_threesuperior       = $0b3;
  XK_acute               = $0b4;
  XK_mu                  = $0b5;
  XK_paragraph           = $0b6;
  XK_periodcentered      = $0b7;
  XK_cedilla             = $0b8;
  XK_onesuperior         = $0b9;
  XK_masculine           = $0ba;
  XK_guillemotright      = $0bb;        { right angle quotation mark }
  XK_onequarter          = $0bc;
  XK_onehalf             = $0bd;
  XK_threequarters       = $0be;
  XK_questiondown        = $0bf;
  XKc_Agrave              = $0c0;
  XKc_Aacute              = $0c1;
  XKc_Acircumflex         = $0c2;
  XKc_Atilde              = $0c3;
  XKc_Adiaeresis          = $0c4;
  XKc_Aring               = $0c5;
  XKc_AE                  = $0c6;
  XKc_Ccedilla            = $0c7;
  XKc_Egrave              = $0c8;
  XKc_Eacute              = $0c9;
  XKc_Ecircumflex         = $0ca;
  XKc_Ediaeresis          = $0cb;
  XKc_Igrave              = $0cc;
  XKc_Iacute              = $0cd;
  XKc_Icircumflex         = $0ce;
  XKc_Idiaeresis          = $0cf;
  XKc_ETH                 = $0d0;
  XKc_Ntilde              = $0d1;
  XKc_Ograve              = $0d2;
  XKc_Oacute              = $0d3;
  XKc_Ocircumflex         = $0d4;
  XKc_Otilde              = $0d5;
  XKc_Odiaeresis          = $0d6;
  XK_multiply            = $0d7;
  XKc_Ooblique            = $0d8;
  XKc_Oslash              = XKc_Ooblique;
  XKc_Ugrave              = $0d9;
  XKc_Uacute              = $0da;
  XKc_Ucircumflex         = $0db;
  XKc_Udiaeresis          = $0dc;
  XKc_Yacute              = $0dd;
  XKc_THORN               = $0de;
  XK_ssharp              = $0df;
  XK_agrave              = $0e0;
  XK_aacute              = $0e1;
  XK_acircumflex         = $0e2;
  XK_atilde              = $0e3;
  XK_adiaeresis          = $0e4;
  XK_aring               = $0e5;
  XK_ae                  = $0e6;
  XK_ccedilla            = $0e7;
  XK_egrave              = $0e8;
  XK_eacute              = $0e9;
  XK_ecircumflex         = $0ea;
  XK_ediaeresis          = $0eb;
  XK_igrave              = $0ec;
  XK_iacute              = $0ed;
  XK_icircumflex         = $0ee;
  XK_idiaeresis          = $0ef;
  XK_eth                 = $0f0;
  XK_ntilde              = $0f1;
  XK_ograve              = $0f2;
  XK_oacute              = $0f3;
  XK_ocircumflex         = $0f4;
  XK_otilde              = $0f5;
  XK_odiaeresis          = $0f6;
  XK_division            = $0f7;
  XK_oslash              = $0f8;
  XK_ooblique            = XK_oslash;
  XK_ugrave              = $0f9;
  XK_uacute              = $0fa;
  XK_ucircumflex         = $0fb;
  XK_udiaeresis          = $0fc;
  XK_yacute              = $0fd;
  XK_thorn               = $0fe;
  XK_ydiaeresis          = $0ff;
{$ENDIF} { XK_LATIN1 }

{*
 *   Latin 2
 *   Byte 3 = 1
 *}

{$IFDEF XK_LATIN2}
  XKc_Aogonek             = $1a1;
  XK_breve               = $1a2;
  XKc_Lstroke             = $1a3;
  XKc_Lcaron              = $1a5;
  XKc_Sacute              = $1a6;
  XKc_Scaron              = $1a9;
  XKc_Scedilla            = $1aa;
  XKc_Tcaron              = $1ab;
  XKc_Zacute              = $1ac;
  XKc_Zcaron              = $1ae;
  XKc_Zabovedot           = $1af;
  XK_aogonek             = $1b1;
  XK_ogonek              = $1b2;
  XK_lstroke             = $1b3;
  XK_lcaron              = $1b5;
  XK_sacute              = $1b6;
  XK_caron               = $1b7;
  XK_scaron              = $1b9;
  XK_scedilla            = $1ba;
  XK_tcaron              = $1bb;
  XK_zacute              = $1bc;
  XK_doubleacute         = $1bd;
  XK_zcaron              = $1be;
  XK_zabovedot           = $1bf;
  XKc_Racute              = $1c0;
  XKc_Abreve              = $1c3;
  XKc_Lacute              = $1c5;
  XKc_Cacute              = $1c6;
  XKc_Ccaron              = $1c8;
  XKc_Eogonek             = $1ca;
  XKc_Ecaron              = $1cc;
  XKc_Dcaron              = $1cf;
  XKc_Dstroke             = $1d0;
  XKc_Nacute              = $1d1;
  XKc_Ncaron              = $1d2;
  XKc_Odoubleacute        = $1d5;
  XKc_Rcaron              = $1d8;
  XKc_Uring               = $1d9;
  XKc_Udoubleacute        = $1db;
  XKc_Tcedilla            = $1de;
  XK_racute              = $1e0;
  XK_abreve              = $1e3;
  XK_lacute              = $1e5;
  XK_cacute              = $1e6;
  XK_ccaron              = $1e8;
  XK_eogonek             = $1ea;
  XK_ecaron              = $1ec;
  XK_dcaron              = $1ef;
  XK_dstroke             = $1f0;
  XK_nacute              = $1f1;
  XK_ncaron              = $1f2;
  XK_odoubleacute        = $1f5;
  XK_udoubleacute        = $1fb;
  XK_rcaron              = $1f8;
  XK_uring               = $1f9;
  XK_tcedilla            = $1fe;
  XK_abovedot            = $1ff;
{$ENDIF} { XK_LATIN2 }

{*
 *   Latin 3
 *   Byte 3 = 2
 *}

{$IFDEF XK_LATIN3}
  XKc_Hstroke             = $2a1;
  XKc_Hcircumflex         = $2a6;
  XKc_Iabovedot           = $2a9;
  XKc_Gbreve              = $2ab;
  XKc_Jcircumflex         = $2ac;
  XK_hstroke             = $2b1;
  XK_hcircumflex         = $2b6;
  XK_idotless            = $2b9;
  XK_gbreve              = $2bb;
  XK_jcircumflex         = $2bc;
  XKc_Cabovedot           = $2c5;
  XKc_Ccircumflex         = $2c6;
  XKc_Gabovedot           = $2d5;
  XKc_Gcircumflex         = $2d8;
  XKc_Ubreve              = $2dd;
  XKc_Scircumflex         = $2de;
  XK_cabovedot           = $2e5;
  XK_ccircumflex         = $2e6;
  XK_gabovedot           = $2f5;
  XK_gcircumflex         = $2f8;
  XK_ubreve              = $2fd;
  XK_scircumflex         = $2fe;
{$ENDIF} { XK_LATIN3 }


{*
 *   Latin 4
 *   Byte 3 = 3
 *}

{$IFDEF XK_LATIN4}
  XK_kra                 = $3a2;
  XK_kappa               = $3a2;{ deprecated }
  XKc_Rcedilla            = $3a3;
  XKc_Itilde              = $3a5;
  XKc_Lcedilla            = $3a6;
  XKc_Emacron             = $3aa;
  XKc_Gcedilla            = $3ab;
  XKc_Tslash              = $3ac;
  XK_rcedilla            = $3b3;
  XK_itilde              = $3b5;
  XK_lcedilla            = $3b6;
  XK_emacron             = $3ba;
  XK_gcedilla            = $3bb;
  XK_tslash              = $3bc;
  XKc_ENG                 = $3bd;
  XK_eng                 = $3bf;
  XKc_Amacron             = $3c0;
  XKc_Iogonek             = $3c7;
  XKc_Eabovedot           = $3cc;
  XKc_Imacron             = $3cf;
  XKc_Ncedilla            = $3d1;
  XKc_Omacron             = $3d2;
  XKc_Kcedilla            = $3d3;
  XKc_Uogonek             = $3d9;
  XKc_Utilde              = $3dd;
  XKc_Umacron             = $3de;
  XK_amacron             = $3e0;
  XK_iogonek             = $3e7;
  XK_eabovedot           = $3ec;
  XK_imacron             = $3ef;
  XK_ncedilla            = $3f1;
  XK_omacron             = $3f2;
  XK_kcedilla            = $3f3;
  XK_uogonek             = $3f9;
  XK_utilde              = $3fd;
  XK_umacron             = $3fe;
{$ENDIF} { XK_LATIN4 }

{*
 * Latin-8
 * Byte 3 = 18
 *}
{$IFDEF XK_LATIN8}
  XKc_Babovedot           = $12a1;
  XK_babovedot           = $12a2;
  XKc_Dabovedot           = $12a6;
  XKc_Wgrave              = $12a8;
  XKc_Wacute              = $12aa;
  XK_dabovedot           = $12ab;
  XKc_Ygrave              = $12ac;
  XKc_Fabovedot           = $12b0;
  XK_fabovedot           = $12b1;
  XKc_Mabovedot           = $12b4;
  XK_mabovedot           = $12b5;
  XKc_Pabovedot           = $12b7;
  XK_wgrave              = $12b8;
  XK_pabovedot           = $12b9;
  XK_wacute              = $12ba;
  XKc_Sabovedot           = $12bb;
  XK_ygrave              = $12bc;
  XKc_Wdiaeresis          = $12bd;
  XK_wdiaeresis          = $12be;
  XK_sabovedot           = $12bf;
  XKc_Wcircumflex         = $12d0;
  XKc_Tabovedot           = $12d7;
  XKc_Ycircumflex         = $12de;
  XK_wcircumflex         = $12f0;
  XK_tabovedot           = $12f7;
  XK_ycircumflex         = $12fe;
{$ENDIF} { XK_LATIN8 }

{*
 * Latin-9 (a.k.a. Latin-0)
 * Byte 3 = 19
 *}

{$IFDEF XK_LATIN9}
  XKc_OE                  = $13bc;
  XK_oe                  = $13bd;
  XKc_Ydiaeresis          = $13be;
{$ENDIF} { XK_LATIN9 }

{*
 * Katakana
 * Byte 3 = 4
 *}

{$IFDEF XK_KATAKANA}
  XK_overline                                    = $47e;
  XK_kana_fullstop                               = $4a1;
  XK_kana_openingbracket                         = $4a2;
  XK_kana_closingbracket                         = $4a3;
  XK_kana_comma                                  = $4a4;
  XK_kana_conjunctive                            = $4a5;
  XK_kana_middledot                              = $4a5; { deprecated }
  XKc_kana_WO                                     = $4a6;
  XK_kana_a                                      = $4a7;
  XK_kana_i                                      = $4a8;
  XK_kana_u                                      = $4a9;
  XK_kana_e                                      = $4aa;
  XK_kana_o                                      = $4ab;
  XK_kana_ya                                     = $4ac;
  XK_kana_yu                                     = $4ad;
  XK_kana_yo                                     = $4ae;
  XK_kana_tsu                                    = $4af;
  XK_kana_tu                                     = $4af; { deprecated }
  XK_prolongedsound                              = $4b0;
  XKc_kana_A                                      = $4b1;
  XKc_kana_I                                      = $4b2;
  XKc_kana_U                                      = $4b3;
  XKc_kana_E                                      = $4b4;
  XKc_kana_O                                      = $4b5;
  XKc_kana_KA                                     = $4b6;
  XKc_kana_KI                                     = $4b7;
  XKc_kana_KU                                     = $4b8;
  XKc_kana_KE                                     = $4b9;
  XKc_kana_KO                                     = $4ba;
  XKc_kana_SA                                     = $4bb;
  XKc_kana_SHI                                    = $4bc;
  XKc_kana_SU                                     = $4bd;
  XKc_kana_SE                                     = $4be;
  XKc_kana_SO                                     = $4bf;
  XKc_kana_TA                                     = $4c0;
  XKc_kana_CHI                                    = $4c1;
  XKc_kana_TI                                     = $4c1;  { deprecated }
  XKc_kana_TSU                                    = $4c2;
  XKc_kana_TU                                     = $4c2;  { deprecated }
  XKc_kana_TE                                     = $4c3;
  XKc_kana_TO                                     = $4c4;
  XKc_kana_NA                                     = $4c5;
  XKc_kana_NI                                     = $4c6;
  XKc_kana_NU                                     = $4c7;
  XKc_kana_NE                                     = $4c8;
  XKc_kana_NO                                     = $4c9;
  XKc_kana_HA                                     = $4ca;
  XKc_kana_HI                                     = $4cb;
  XKc_kana_FU                                     = $4cc;
  XKc_kana_HU                                     = $4cc; { deprecated }
  XKc_kana_HE                                     = $4cd;
  XKc_kana_HO                                     = $4ce;
  XKc_kana_MA                                     = $4cf;
  XKc_kana_MI                                     = $4d0;
  XKc_kana_MU                                     = $4d1;
  XKc_kana_ME                                     = $4d2;
  XKc_kana_MO                                     = $4d3;
  XKc_kana_YA                                     = $4d4;
  XKc_kana_YU                                     = $4d5;
  XKc_kana_YO                                     = $4d6;
  XKc_kana_RA                                     = $4d7;
  XKc_kana_RI                                     = $4d8;
  XKc_kana_RU                                     = $4d9;
  XKc_kana_RE                                     = $4da;
  XKc_kana_RO                                     = $4db;
  XKc_kana_WA                                     = $4dc;
  XKc_kana_N                                      = $4dd;
  XK_voicedsound                                 = $4de;
  XK_semivoicedsound                             = $4df;
  XK_kana_switch          = $FF7E;  { Alias for mode_switch }
{$ENDIF} { XK_KATAKANA }

{*
 *  Arabic
 *  Byte 3 = 5
 *}

{$IFDEF XK_ARABIC}
  XK_Farsi_0                                     = $590;
  XK_Farsi_1                                     = $591;
  XK_Farsi_2                                     = $592;
  XK_Farsi_3                                     = $593;
  XK_Farsi_4                                     = $594;
  XK_Farsi_5                                     = $595;
  XK_Farsi_6                                     = $596;
  XK_Farsi_7                                     = $597;
  XK_Farsi_8                                     = $598;
  XK_Farsi_9                                     = $599;
  XK_Arabic_percent                              = $5a5;
  XK_Arabic_superscript_alef                     = $5a6;
  XK_Arabic_tteh                                 = $5a7;
  XK_Arabic_peh                                  = $5a8;
  XK_Arabic_tcheh                                = $5a9;
  XK_Arabic_ddal                                 = $5aa;
  XK_Arabic_rreh                                 = $5ab;
  XK_Arabic_comma                                = $5ac;
  XK_Arabic_fullstop                             = $5ae;
  XK_Arabic_0                                    = $5b0;
  XK_Arabic_1                                    = $5b1;
  XK_Arabic_2                                    = $5b2;
  XK_Arabic_3                                    = $5b3;
  XK_Arabic_4                                    = $5b4;
  XK_Arabic_5                                    = $5b5;
  XK_Arabic_6                                    = $5b6;
  XK_Arabic_7                                    = $5b7;
  XK_Arabic_8                                    = $5b8;
  XK_Arabic_9                                    = $5b9;
  XK_Arabic_semicolon                            = $5bb;
  XK_Arabic_question_mark                        = $5bf;
  XK_Arabic_hamza                                = $5c1;
  XK_Arabic_maddaonalef                          = $5c2;
  XK_Arabic_hamzaonalef                          = $5c3;
  XK_Arabic_hamzaonwaw                           = $5c4;
  XK_Arabic_hamzaunderalef                       = $5c5;
  XK_Arabic_hamzaonyeh                           = $5c6;
  XK_Arabic_alef                                 = $5c7;
  XK_Arabic_beh                                  = $5c8;
  XK_Arabic_tehmarbuta                           = $5c9;
  XK_Arabic_teh                                  = $5ca;
  XK_Arabic_theh                                 = $5cb;
  XK_Arabic_jeem                                 = $5cc;
  XK_Arabic_hah                                  = $5cd;
  XK_Arabic_khah                                 = $5ce;
  XK_Arabic_dal                                  = $5cf;
  XK_Arabic_thal                                 = $5d0;
  XK_Arabic_ra                                   = $5d1;
  XK_Arabic_zain                                 = $5d2;
  XK_Arabic_seen                                 = $5d3;
  XK_Arabic_sheen                                = $5d4;
  XK_Arabic_sad                                  = $5d5;
  XK_Arabic_dad                                  = $5d6;
  XK_Arabic_tah                                  = $5d7;
  XK_Arabic_zah                                  = $5d8;
  XK_Arabic_ain                                  = $5d9;
  XK_Arabic_ghain                                = $5da;
  XK_Arabic_tatweel                              = $5e0;
  XK_Arabic_feh                                  = $5e1;
  XK_Arabic_qaf                                  = $5e2;
  XK_Arabic_kaf                                  = $5e3;
  XK_Arabic_lam                                  = $5e4;
  XK_Arabic_meem                                 = $5e5;
  XK_Arabic_noon                                 = $5e6;
  XK_Arabic_ha                                   = $5e7;
  XK_Arabic_heh                                  = $5e7;  { deprecated }
  XK_Arabic_waw                                  = $5e8;
  XK_Arabic_alefmaksura                          = $5e9;
  XK_Arabic_yeh                                  = $5ea;
  XK_Arabic_fathatan                             = $5eb;
  XK_Arabic_dammatan                             = $5ec;
  XK_Arabic_kasratan                             = $5ed;
  XK_Arabic_fatha                                = $5ee;
  XK_Arabic_damma                                = $5ef;
  XK_Arabic_kasra                                = $5f0;
  XK_Arabic_shadda                               = $5f1;
  XK_Arabic_sukun                                = $5f2;
  XK_Arabic_madda_above                          = $5f3;
  XK_Arabic_hamza_above                          = $5f4;
  XK_Arabic_hamza_below                          = $5f5;
  XK_Arabic_jeh                                  = $5f6;
  XK_Arabic_veh                                  = $5f7;
  XK_Arabic_keheh                                = $5f8;
  XK_Arabic_gaf                                  = $5f9;
  XK_Arabic_noon_ghunna                          = $5fa;
  XK_Arabic_heh_doachashmee                      = $5fb;
  XK_Farsi_yeh                                   = $5fc;
  XK_Arabic_farsi_yeh                    = XK_Farsi_yeh;
  XK_Arabic_yeh_baree                            = $5fd;
  XK_Arabic_heh_goal                             = $5fe;
  XK_Arabic_switch        = $FF7E;  { Alias for mode_switch }
{$ENDIF} { XK_ARABIC }

{*
 * Cyrillic
 * Byte 3 = 6
 *}
{$IFDEF XK_CYRILLIC}
  XKc_Cyrillic_GHE_bar                             = $680;
  XK_Cyrillic_ghe_bar                              = $690;
  XKc_Cyrillic_ZHE_descender                       = $681;
  XK_Cyrillic_zhe_descender                        = $691;
  XKc_Cyrillic_KA_descender                        = $682;
  XK_Cyrillic_ka_descender                         = $692;
  XKc_Cyrillic_KA_vertstroke                       = $683;
  XK_Cyrillic_ka_vertstroke                        = $693;
  XKc_Cyrillic_EN_descender                        = $684;
  XK_Cyrillic_en_descender                         = $694;
  XKc_Cyrillic_U_straight                              = $685;
  XK_Cyrillic_u_straight                               = $695;
  XKc_Cyrillic_U_straight_bar                      = $686;
  XK_Cyrillic_u_straight_bar                       = $696;
  XKc_Cyrillic_HA_descender                        = $687;
  XK_Cyrillic_ha_descender                         = $697;
  XKc_Cyrillic_CHE_descender                       = $688;
  XK_Cyrillic_che_descender                        = $698;
  XKc_Cyrillic_CHE_vertstroke                      = $689;
  XK_Cyrillic_che_vertstroke                       = $699;
  XKc_Cyrillic_SHHA                                = $68a;
  XK_Cyrillic_shha                                 = $69a;

  XKc_Cyrillic_SCHWA                               = $68c;
  XK_Cyrillic_schwa                                = $69c;
  XKc_Cyrillic_I_macron                            = $68d;
  XK_Cyrillic_i_macron                             = $69d;
  XKc_Cyrillic_O_bar                               = $68e;
  XK_Cyrillic_o_bar                                = $69e;
  XKc_Cyrillic_U_macron                            = $68f;
  XK_Cyrillic_u_macron                             = $69f;

  XK_Serbian_dje                                 = $6a1;
  XK_Macedonia_gje                               = $6a2;
  XK_Cyrillic_io                                 = $6a3;
  XK_Ukrainian_ie                                = $6a4;
  XK_Ukranian_je                                 = $6a4;  { deprecated }
  XK_Macedonia_dse                               = $6a5;
  XK_Ukrainian_i                                 = $6a6;
  XK_Ukranian_i                                  = $6a6;  { deprecated }
  XK_Ukrainian_yi                                = $6a7;
  XK_Ukranian_yi                                 = $6a7;  { deprecated }
  XK_Cyrillic_je                                 = $6a8;
  XK_Serbian_je                                  = $6a8;  { deprecated }
  XK_Cyrillic_lje                                = $6a9;
  XK_Serbian_lje                                 = $6a9;  { deprecated }
  XK_Cyrillic_nje                                = $6aa;
  XK_Serbian_nje                                 = $6aa;  { deprecated }
  XK_Serbian_tshe                                = $6ab;
  XK_Macedonia_kje                               = $6ac;
  XK_Ukrainian_ghe_with_upturn                   = $6ad;
  XK_Byelorussian_shortu                         = $6ae;
  XK_Cyrillic_dzhe                               = $6af;
  XK_Serbian_dze                                 = $6af;  { deprecated }
  XK_numerosign                                  = $6b0;
  XKc_Serbian_DJE                                 = $6b1;
  XKc_Macedonia_GJE                               = $6b2;
  XKc_Cyrillic_IO                                 = $6b3;
  XKc_Ukrainian_IE                                = $6b4;
  XKc_Ukranian_JE                                 = $6b4;  { deprecated }
  XKc_Macedonia_DSE                               = $6b5;
  XKc_Ukrainian_I                                 = $6b6;
  XKc_Ukranian_I                                  = $6b6;  { deprecated }
  XKc_Ukrainian_YI                                = $6b7;
  XKc_Ukranian_YI                                 = $6b7;  { deprecated }
  XKc_Cyrillic_JE                                 = $6b8;
  XKc_Serbian_JE                                  = $6b8;  { deprecated }
  XKc_Cyrillic_LJE                                = $6b9;
  XKc_Serbian_LJE                                 = $6b9;  { deprecated }
  XKc_Cyrillic_NJE                                = $6ba;
  XKc_Serbian_NJE                                 = $6ba;  { deprecated }
  XKc_Serbian_TSHE                                = $6bb;
  XKc_Macedonia_KJE                               = $6bc;
  XKc_Ukrainian_GHE_WITH_UPTURN                   = $6bd;
  XKc_Byelorussian_SHORTU                         = $6be;
  XKc_Cyrillic_DZHE                               = $6bf;
  XKc_Serbian_DZE                                 = $6bf;  { deprecated }
  XK_Cyrillic_yu                                 = $6c0;
  XK_Cyrillic_a                                  = $6c1;
  XK_Cyrillic_be                                 = $6c2;
  XK_Cyrillic_tse                                = $6c3;
  XK_Cyrillic_de                                 = $6c4;
  XK_Cyrillic_ie                                 = $6c5;
  XK_Cyrillic_ef                                 = $6c6;
  XK_Cyrillic_ghe                                = $6c7;
  XK_Cyrillic_ha                                 = $6c8;
  XK_Cyrillic_i                                  = $6c9;
  XK_Cyrillic_shorti                             = $6ca;
  XK_Cyrillic_ka                                 = $6cb;
  XK_Cyrillic_el                                 = $6cc;
  XK_Cyrillic_em                                 = $6cd;
  XK_Cyrillic_en                                 = $6ce;
  XK_Cyrillic_o                                  = $6cf;
  XK_Cyrillic_pe                                 = $6d0;
  XK_Cyrillic_ya                                 = $6d1;
  XK_Cyrillic_er                                 = $6d2;
  XK_Cyrillic_es                                 = $6d3;
  XK_Cyrillic_te                                 = $6d4;
  XK_Cyrillic_u                                  = $6d5;
  XK_Cyrillic_zhe                                = $6d6;
  XK_Cyrillic_ve                                 = $6d7;
  XK_Cyrillic_softsign                           = $6d8;
  XK_Cyrillic_yeru                               = $6d9;
  XK_Cyrillic_ze                                 = $6da;
  XK_Cyrillic_sha                                = $6db;
  XK_Cyrillic_e                                  = $6dc;
  XK_Cyrillic_shcha                              = $6dd;
  XK_Cyrillic_che                                = $6de;
  XK_Cyrillic_hardsign                           = $6df;
  XKc_Cyrillic_YU                                 = $6e0;
  XKc_Cyrillic_A                                  = $6e1;
  XKc_Cyrillic_BE                                 = $6e2;
  XKc_Cyrillic_TSE                                = $6e3;
  XKc_Cyrillic_DE                                 = $6e4;
  XKc_Cyrillic_IE                                 = $6e5;
  XKc_Cyrillic_EF                                 = $6e6;
  XKc_Cyrillic_GHE                                = $6e7;
  XKc_Cyrillic_HA                                 = $6e8;
  XKc_Cyrillic_I                                  = $6e9;
  XKc_Cyrillic_SHORTI                             = $6ea;
  XKc_Cyrillic_KA                                 = $6eb;
  XKc_Cyrillic_EL                                 = $6ec;
  XKc_Cyrillic_EM                                 = $6ed;
  XKc_Cyrillic_EN                                 = $6ee;
  XKc_Cyrillic_O                                  = $6ef;
  XKc_Cyrillic_PE                                 = $6f0;
  XKc_Cyrillic_YA                                 = $6f1;
  XKc_Cyrillic_ER                                 = $6f2;
  XKc_Cyrillic_ES                                 = $6f3;
  XKc_Cyrillic_TE                                 = $6f4;
  XKc_Cyrillic_U                                  = $6f5;
  XKc_Cyrillic_ZHE                                = $6f6;
  XKc_Cyrillic_VE                                 = $6f7;
  XKc_Cyrillic_SOFTSIGN                           = $6f8;
  XKc_Cyrillic_YERU                               = $6f9;
  XKc_Cyrillic_ZE                                 = $6fa;
  XKc_Cyrillic_SHA                                = $6fb;
  XKc_Cyrillic_E                                  = $6fc;
  XKc_Cyrillic_SHCHA                              = $6fd;
  XKc_Cyrillic_CHE                                = $6fe;
  XKc_Cyrillic_HARDSIGN                           = $6ff;
{$ENDIF} { XK_CYRILLIC }

{*
 * Greek
 * Byte 3 = 7
 *}

{$IFDEF XK_GREEK}
  XKc_Greek_ALPHAaccent                           = $7a1;
  XKc_Greek_EPSILONaccent                         = $7a2;
  XKc_Greek_ETAaccent                             = $7a3;
  XKc_Greek_IOTAaccent                            = $7a4;
  XKc_Greek_IOTAdieresis                          = $7a5;
  XKc_Greek_IOTAdiaeresis = XKc_Greek_IOTAdieresis; { old typo }
  XKc_Greek_OMICRONaccent                         = $7a7;
  XKc_Greek_UPSILONaccent                         = $7a8;
  XKc_Greek_UPSILONdieresis                       = $7a9;
  XKc_Greek_OMEGAaccent                           = $7ab;
  XK_Greek_accentdieresis                        = $7ae;
  XK_Greek_horizbar                              = $7af;
  XK_Greek_alphaaccent                           = $7b1;
  XK_Greek_epsilonaccent                         = $7b2;
  XK_Greek_etaaccent                             = $7b3;
  XK_Greek_iotaaccent                            = $7b4;
  XK_Greek_iotadieresis                          = $7b5;
  XK_Greek_iotaaccentdieresis                    = $7b6;
  XK_Greek_omicronaccent                         = $7b7;
  XK_Greek_upsilonaccent                         = $7b8;
  XK_Greek_upsilondieresis                       = $7b9;
  XK_Greek_upsilonaccentdieresis                 = $7ba;
  XK_Greek_omegaaccent                           = $7bb;
  XKc_Greek_ALPHA                                 = $7c1;
  XKc_Greek_BETA                                  = $7c2;
  XKc_Greek_GAMMA                                 = $7c3;
  XKc_Greek_DELTA                                 = $7c4;
  XKc_Greek_EPSILON                               = $7c5;
  XKc_Greek_ZETA                                  = $7c6;
  XKc_Greek_ETA                                   = $7c7;
  XKc_Greek_THETA                                 = $7c8;
  XKc_Greek_IOTA                                  = $7c9;
  XKc_Greek_KAPPA                                 = $7ca;
  XKc_Greek_LAMDA                                 = $7cb;
  XKc_Greek_LAMBDA                                = $7cb;
  XKc_Greek_MU                                    = $7cc;
  XKc_Greek_NU                                    = $7cd;
  XKc_Greek_XI                                    = $7ce;
  XKc_Greek_OMICRON                               = $7cf;
  XKc_Greek_PI                                    = $7d0;
  XKc_Greek_RHO                                   = $7d1;
  XKc_Greek_SIGMA                                 = $7d2;
  XKc_Greek_TAU                                   = $7d4;
  XKc_Greek_UPSILON                               = $7d5;
  XKc_Greek_PHI                                   = $7d6;
  XKc_Greek_CHI                                   = $7d7;
  XKc_Greek_PSI                                   = $7d8;
  XKc_Greek_OMEGA                                 = $7d9;
  XK_Greek_alpha                                 = $7e1;
  XK_Greek_beta                                  = $7e2;
  XK_Greek_gamma                                 = $7e3;
  XK_Greek_delta                                 = $7e4;
  XK_Greek_epsilon                               = $7e5;
  XK_Greek_zeta                                  = $7e6;
  XK_Greek_eta                                   = $7e7;
  XK_Greek_theta                                 = $7e8;
  XK_Greek_iota                                  = $7e9;
  XK_Greek_kappa                                 = $7ea;
  XK_Greek_lamda                                 = $7eb;
  XK_Greek_lambda                                = $7eb;
  XK_Greek_mu                                    = $7ec;
  XK_Greek_nu                                    = $7ed;
  XK_Greek_xi                                    = $7ee;
  XK_Greek_omicron                               = $7ef;
  XK_Greek_pi                                    = $7f0;
  XK_Greek_rho                                   = $7f1;
  XK_Greek_sigma                                 = $7f2;
  XK_Greek_finalsmallsigma                       = $7f3;
  XK_Greek_tau                                   = $7f4;
  XK_Greek_upsilon                               = $7f5;
  XK_Greek_phi                                   = $7f6;
  XK_Greek_chi                                   = $7f7;
  XK_Greek_psi                                   = $7f8;
  XK_Greek_omega                                 = $7f9;
  XK_Greek_switch         = $FF7E;  { Alias for mode_switch }
{$ENDIF} { XK_GREEK }

{*
 * Technical
 * Byte 3 = 8
 *}

{$IFDEF XK_TECHNICAL}
  XK_leftradical                                 = $8a1;
  XK_topleftradical                              = $8a2;
  XK_horizconnector                              = $8a3;
  XK_topintegral                                 = $8a4;
  XK_botintegral                                 = $8a5;
  XK_vertconnector                               = $8a6;
  XK_topleftsqbracket                            = $8a7;
  XK_botleftsqbracket                            = $8a8;
  XK_toprightsqbracket                           = $8a9;
  XK_botrightsqbracket                           = $8aa;
  XK_topleftparens                               = $8ab;
  XK_botleftparens                               = $8ac;
  XK_toprightparens                              = $8ad;
  XK_botrightparens                              = $8ae;
  XK_leftmiddlecurlybrace                        = $8af;
  XK_rightmiddlecurlybrace                       = $8b0;
  XK_topleftsummation                            = $8b1;
  XK_botleftsummation                            = $8b2;
  XK_topvertsummationconnector                   = $8b3;
  XK_botvertsummationconnector                   = $8b4;
  XK_toprightsummation                           = $8b5;
  XK_botrightsummation                           = $8b6;
  XK_rightmiddlesummation                        = $8b7;
  XK_lessthanequal                               = $8bc;
  XK_notequal                                    = $8bd;
  XK_greaterthanequal                            = $8be;
  XK_integral                                    = $8bf;
  XK_therefore                                   = $8c0;
  XK_variation                                   = $8c1;
  XK_infinity                                    = $8c2;
  XK_nabla                                       = $8c5;
  XK_approximate                                 = $8c8;
  XK_similarequal                                = $8c9;
  XK_ifonlyif                                    = $8cd;
  XK_implies                                     = $8ce;
  XK_identical                                   = $8cf;
  XK_radical                                     = $8d6;
  XK_includedin                                  = $8da;
  XK_includes                                    = $8db;
  XK_intersection                                = $8dc;
  XK_union                                       = $8dd;
  XK_logicaland                                  = $8de;
  XK_logicalor                                   = $8df;
  XK_partialderivative                           = $8ef;
  XK_function                                    = $8f6;
  XK_leftarrow                                   = $8fb;
  XK_uparrow                                     = $8fc;
  XK_rightarrow                                  = $8fd;
  XK_downarrow                                   = $8fe;
{$ENDIF} { XK_TECHNICAL }

{*
 *  Special
 *  Byte 3 = 9
 *}

{$IFDEF XK_SPECIAL}
  XK_blank                                       = $9df;
  XK_soliddiamond                                = $9e0;
  XK_checkerboard                                = $9e1;
  XK_ht                                          = $9e2;
  XK_ff                                          = $9e3;
  XK_cr                                          = $9e4;
  XK_lf                                          = $9e5;
  XK_nl                                          = $9e8;
  XK_vt                                          = $9e9;
  XK_lowrightcorner                              = $9ea;
  XK_uprightcorner                               = $9eb;
  XK_upleftcorner                                = $9ec;
  XK_lowleftcorner                               = $9ed;
  XK_crossinglines                               = $9ee;
  XK_horizlinescan1                              = $9ef;
  XK_horizlinescan3                              = $9f0;
  XK_horizlinescan5                              = $9f1;
  XK_horizlinescan7                              = $9f2;
  XK_horizlinescan9                              = $9f3;
  XK_leftt                                       = $9f4;
  XK_rightt                                      = $9f5;
  XK_bott                                        = $9f6;
  XK_topt                                        = $9f7;
  XK_vertbar                                     = $9f8;
{$ENDIF} { XK_SPECIAL }

{*
 *  Publishing
 *  Byte 3 = a
 *}

{$IFDEF XK_PUBLISHING}
  XK_emspace                                     = $aa1;
  XK_enspace                                     = $aa2;
  XK_em3space                                    = $aa3;
  XK_em4space                                    = $aa4;
  XK_digitspace                                  = $aa5;
  XK_punctspace                                  = $aa6;
  XK_thinspace                                   = $aa7;
  XK_hairspace                                   = $aa8;
  XK_emdash                                      = $aa9;
  XK_endash                                      = $aaa;
  XK_signifblank                                 = $aac;
  XK_ellipsis                                    = $aae;
  XK_doubbaselinedot                             = $aaf;
  XK_onethird                                    = $ab0;
  XK_twothirds                                   = $ab1;
  XK_onefifth                                    = $ab2;
  XK_twofifths                                   = $ab3;
  XK_threefifths                                 = $ab4;
  XK_fourfifths                                  = $ab5;
  XK_onesixth                                    = $ab6;
  XK_fivesixths                                  = $ab7;
  XK_careof                                      = $ab8;
  XK_figdash                                     = $abb;
  XK_leftanglebracket                            = $abc;
  XK_decimalpoint                                = $abd;
  XK_rightanglebracket                           = $abe;
  XK_marker                                      = $abf;
  XK_oneeighth                                   = $ac3;
  XK_threeeighths                                = $ac4;
  XK_fiveeighths                                 = $ac5;
  XK_seveneighths                                = $ac6;
  XK_trademark                                   = $ac9;
  XK_signaturemark                               = $aca;
  XK_trademarkincircle                           = $acb;
  XK_leftopentriangle                            = $acc;
  XK_rightopentriangle                           = $acd;
  XK_emopencircle                                = $ace;
  XK_emopenrectangle                             = $acf;
  XK_leftsinglequotemark                         = $ad0;
  XK_rightsinglequotemark                        = $ad1;
  XK_leftdoublequotemark                         = $ad2;
  XK_rightdoublequotemark                        = $ad3;
  XK_prescription                                = $ad4;
  XK_minutes                                     = $ad6;
  XK_seconds                                     = $ad7;
  XK_latincross                                  = $ad9;
  XK_hexagram                                    = $ada;
  XK_filledrectbullet                            = $adb;
  XK_filledlefttribullet                         = $adc;
  XK_filledrighttribullet                        = $add;
  XK_emfilledcircle                              = $ade;
  XK_emfilledrect                                = $adf;
  XK_enopencircbullet                            = $ae0;
  XK_enopensquarebullet                          = $ae1;
  XK_openrectbullet                              = $ae2;
  XK_opentribulletup                             = $ae3;
  XK_opentribulletdown                           = $ae4;
  XK_openstar                                    = $ae5;
  XK_enfilledcircbullet                          = $ae6;
  XK_enfilledsqbullet                            = $ae7;
  XK_filledtribulletup                           = $ae8;
  XK_filledtribulletdown                         = $ae9;
  XK_leftpointer                                 = $aea;
  XK_rightpointer                                = $aeb;
  XK_club                                        = $aec;
  XK_diamond                                     = $aed;
  XK_heart                                       = $aee;
  XK_maltesecross                                = $af0;
  XK_dagger                                      = $af1;
  XK_doubledagger                                = $af2;
  XK_checkmark                                   = $af3;
  XK_ballotcross                                 = $af4;
  XK_musicalsharp                                = $af5;
  XK_musicalflat                                 = $af6;
  XK_malesymbol                                  = $af7;
  XK_femalesymbol                                = $af8;
  XK_telephone                                   = $af9;
  XK_telephonerecorder                           = $afa;
  XK_phonographcopyright                         = $afb;
  XK_caret                                       = $afc;
  XK_singlelowquotemark                          = $afd;
  XK_doublelowquotemark                          = $afe;
  XK_cursor                                      = $aff;
{$ENDIF} { XK_PUBLISHING }

{*
 *  APL
 *  Byte 3 = b
 *}

{$IFDEF XK_APL}
  XK_leftcaret                                   = $ba3;
  XK_rightcaret                                  = $ba6;
  XK_downcaret                                   = $ba8;
  XK_upcaret                                     = $ba9;
  XK_overbar                                     = $bc0;
  XK_downtack                                    = $bc2;
  XK_upshoe                                      = $bc3;
  XK_downstile                                   = $bc4;
  XK_underbar                                    = $bc6;
  XK_jot                                         = $bca;
  XK_quad                                        = $bcc;
  XK_uptack                                      = $bce;
  XK_circle                                      = $bcf;
  XK_upstile                                     = $bd3;
  XK_downshoe                                    = $bd6;
  XK_rightshoe                                   = $bd8;
  XK_leftshoe                                    = $bda;
  XK_lefttack                                    = $bdc;
  XK_righttack                                   = $bfc;
{$ENDIF} { XK_APL }

{*
 * Hebrew
 * Byte 3 = c
 *}

{$IFDEF XK_HEBREW}
  XK_hebrew_doublelowline                        = $cdf;
  XK_hebrew_aleph                                = $ce0;
  XK_hebrew_bet                                  = $ce1;
  XK_hebrew_beth                                 = $ce1;  { deprecated }
  XK_hebrew_gimel                                = $ce2;
  XK_hebrew_gimmel                               = $ce2;  { deprecated }
  XK_hebrew_dalet                                = $ce3;
  XK_hebrew_daleth                               = $ce3;  { deprecated }
  XK_hebrew_he                                   = $ce4;
  XK_hebrew_waw                                  = $ce5;
  XK_hebrew_zain                                 = $ce6;
  XK_hebrew_zayin                                = $ce6;  { deprecated }
  XK_hebrew_chet                                 = $ce7;
  XK_hebrew_het                                  = $ce7;  { deprecated }
  XK_hebrew_tet                                  = $ce8;
  XK_hebrew_teth                                 = $ce8;  { deprecated }
  XK_hebrew_yod                                  = $ce9;
  XK_hebrew_finalkaph                            = $cea;
  XK_hebrew_kaph                                 = $ceb;
  XK_hebrew_lamed                                = $cec;
  XK_hebrew_finalmem                             = $ced;
  XK_hebrew_mem                                  = $cee;
  XK_hebrew_finalnun                             = $cef;
  XK_hebrew_nun                                  = $cf0;
  XK_hebrew_samech                               = $cf1;
  XK_hebrew_samekh                               = $cf1;  { deprecated }
  XK_hebrew_ayin                                 = $cf2;
  XK_hebrew_finalpe                              = $cf3;
  XK_hebrew_pe                                   = $cf4;
  XK_hebrew_finalzade                            = $cf5;
  XK_hebrew_finalzadi                            = $cf5;  { deprecated }
  XK_hebrew_zade                                 = $cf6;
  XK_hebrew_zadi                                 = $cf6;  { deprecated }
  XK_hebrew_qoph                                 = $cf7;
  XK_hebrew_kuf                                  = $cf7;  { deprecated }
  XK_hebrew_resh                                 = $cf8;
  XK_hebrew_shin                                 = $cf9;
  XK_hebrew_taw                                  = $cfa;
  XK_hebrew_taf                                  = $cfa;  { deprecated }
  XK_Hebrew_switch        = $FF7E;  { Alias for mode_switch }
{$ENDIF} { XK_HEBREW }

{*
 * Thai
 * Byte 3 = d
 *}

{$IFDEF XK_THAI}
  XK_Thai_kokai                                 = $da1;
  XK_Thai_khokhai                                       = $da2;
  XK_Thai_khokhuat                              = $da3;
  XK_Thai_khokhwai                              = $da4;
  XK_Thai_khokhon                                       = $da5;
  XK_Thai_khorakhang                            = $da6;
  XK_Thai_ngongu                                        = $da7;
  XK_Thai_chochan                                       = $da8;
  XK_Thai_choching                              = $da9;
  XK_Thai_chochang                              = $daa;
  XK_Thai_soso                                  = $dab;
  XK_Thai_chochoe                                       = $dac;
  XK_Thai_yoying                                        = $dad;
  XK_Thai_dochada                                       = $dae;
  XK_Thai_topatak                                       = $daf;
  XK_Thai_thothan                                       = $db0;
  XK_Thai_thonangmontho                         = $db1;
  XK_Thai_thophuthao                            = $db2;
  XK_Thai_nonen                                 = $db3;
  XK_Thai_dodek                                 = $db4;
  XK_Thai_totao                                 = $db5;
  XK_Thai_thothung                              = $db6;
  XK_Thai_thothahan                             = $db7;
  XK_Thai_thothong                              = $db8;
  XK_Thai_nonu                                  = $db9;
  XK_Thai_bobaimai                              = $dba;
  XK_Thai_popla                                 = $dbb;
  XK_Thai_phophung                              = $dbc;
  XK_Thai_fofa                                  = $dbd;
  XK_Thai_phophan                                       = $dbe;
  XK_Thai_fofan                                 = $dbf;
  XK_Thai_phosamphao                            = $dc0;
  XK_Thai_moma                                  = $dc1;
  XK_Thai_yoyak                                 = $dc2;
  XK_Thai_rorua                                 = $dc3;
  XK_Thai_ru                                    = $dc4;
  XK_Thai_loling                                        = $dc5;
  XK_Thai_lu                                    = $dc6;
  XK_Thai_wowaen                                        = $dc7;
  XK_Thai_sosala                                        = $dc8;
  XK_Thai_sorusi                                        = $dc9;
  XK_Thai_sosua                                 = $dca;
  XK_Thai_hohip                                 = $dcb;
  XK_Thai_lochula                                       = $dcc;
  XK_Thai_oang                                  = $dcd;
  XK_Thai_honokhuk                              = $dce;
  XK_Thai_paiyannoi                             = $dcf;
  XK_Thai_saraa                                 = $dd0;
  XK_Thai_maihanakat                            = $dd1;
  XK_Thai_saraaa                                        = $dd2;
  XK_Thai_saraam                                        = $dd3;
  XK_Thai_sarai                                 = $dd4;
  XK_Thai_saraii                                        = $dd5;
  XK_Thai_saraue                                        = $dd6;
  XK_Thai_sarauee                                       = $dd7;
  XK_Thai_sarau                                 = $dd8;
  XK_Thai_sarauu                                        = $dd9;
  XK_Thai_phinthu                                       = $dda;
  XK_Thai_maihanakat_maitho                     = $dde;
  XK_Thai_baht                                  = $ddf;
  XK_Thai_sarae                                 = $de0;
  XK_Thai_saraae                                        = $de1;
  XK_Thai_sarao                                 = $de2;
  XK_Thai_saraaimaimuan                         = $de3;
  XK_Thai_saraaimaimalai                                = $de4;
  XK_Thai_lakkhangyao                           = $de5;
  XK_Thai_maiyamok                              = $de6;
  XK_Thai_maitaikhu                             = $de7;
  XK_Thai_maiek                                 = $de8;
  XK_Thai_maitho                                        = $de9;
  XK_Thai_maitri                                        = $dea;
  XK_Thai_maichattawa                           = $deb;
  XK_Thai_thanthakhat                           = $dec;
  XK_Thai_nikhahit                              = $ded;
  XK_Thai_leksun                                        = $df0;
  XK_Thai_leknung                                       = $df1;
  XK_Thai_leksong                                       = $df2;
  XK_Thai_leksam                                        = $df3;
  XK_Thai_leksi                                 = $df4;
  XK_Thai_lekha                                 = $df5;
  XK_Thai_lekhok                                        = $df6;
  XK_Thai_lekchet                                       = $df7;
  XK_Thai_lekpaet                                       = $df8;
  XK_Thai_lekkao                                        = $df9;
{$ENDIF} { XK_THAI }

{*
 *   Korean
 *   Byte 3 = e
 *}

{$IFDEF XK_KOREAN}

  XK_Hangul             = $ff31;    { Hangul start/stop(toggle) }
  XK_Hangul_Start               = $ff32;    { Hangul start }
  XK_Hangul_End         = $ff33;    { Hangul end, English start }
  XK_Hangul_Hanja               = $ff34;    { Start Hangul->Hanja Conversion }
  XK_Hangul_Jamo                = $ff35;    { Hangul Jamo mode }
  XK_Hangul_Romaja      = $ff36;    { Hangul Romaja mode }
  XK_Hangul_Codeinput   = $ff37;    { Hangul code input mode }
  XK_Hangul_Jeonja      = $ff38;    { Jeonja mode }
  XK_Hangul_Banja               = $ff39;    { Banja mode }
  XK_Hangul_PreHanja    = $ff3a;    { Pre Hanja conversion }
  XK_Hangul_PostHanja   = $ff3b;    { Post Hanja conversion }
  XK_Hangul_SingleCandidate     = $ff3c;    { Single candidate }
  XK_Hangul_MultipleCandidate   = $ff3d;    { Multiple candidate }
  XK_Hangul_PreviousCandidate   = $ff3e;    { Previous candidate }
  XK_Hangul_Special     = $ff3f;    { Special symbols }
  XK_Hangul_switch      = $FF7E;    { Alias for mode_switch }

{ Hangul Consonant Characters }
  XK_Hangul_Kiyeog                              = $ea1;
  XK_Hangul_SsangKiyeog                         = $ea2;
  XK_Hangul_KiyeogSios                          = $ea3;
  XK_Hangul_Nieun                                       = $ea4;
  XK_Hangul_NieunJieuj                          = $ea5;
  XK_Hangul_NieunHieuh                          = $ea6;
  XK_Hangul_Dikeud                              = $ea7;
  XK_Hangul_SsangDikeud                         = $ea8;
  XK_Hangul_Rieul                                       = $ea9;
  XK_Hangul_RieulKiyeog                         = $eaa;
  XK_Hangul_RieulMieum                          = $eab;
  XK_Hangul_RieulPieub                          = $eac;
  XK_Hangul_RieulSios                           = $ead;
  XK_Hangul_RieulTieut                          = $eae;
  XK_Hangul_RieulPhieuf                         = $eaf;
  XK_Hangul_RieulHieuh                          = $eb0;
  XK_Hangul_Mieum                                       = $eb1;
  XK_Hangul_Pieub                                       = $eb2;
  XK_Hangul_SsangPieub                          = $eb3;
  XK_Hangul_PieubSios                           = $eb4;
  XK_Hangul_Sios                                        = $eb5;
  XK_Hangul_SsangSios                           = $eb6;
  XK_Hangul_Ieung                                       = $eb7;
  XK_Hangul_Jieuj                                       = $eb8;
  XK_Hangul_SsangJieuj                          = $eb9;
  XK_Hangul_Cieuc                                       = $eba;
  XK_Hangul_Khieuq                              = $ebb;
  XK_Hangul_Tieut                                       = $ebc;
  XK_Hangul_Phieuf                              = $ebd;
  XK_Hangul_Hieuh                                       = $ebe;

{ Hangul Vowel Characters }
  XK_Hangul_A                                   = $ebf;
  XK_Hangul_AE                                  = $ec0;
  XK_Hangul_YA                                  = $ec1;
  XK_Hangul_YAE                                 = $ec2;
  XK_Hangul_EO                                  = $ec3;
  XK_Hangul_E                                   = $ec4;
  XK_Hangul_YEO                                 = $ec5;
  XK_Hangul_YE                                  = $ec6;
  XK_Hangul_O                                   = $ec7;
  XK_Hangul_WA                                  = $ec8;
  XK_Hangul_WAE                                 = $ec9;
  XK_Hangul_OE                                  = $eca;
  XK_Hangul_YO                                  = $ecb;
  XK_Hangul_U                                   = $ecc;
  XK_Hangul_WEO                                 = $ecd;
  XK_Hangul_WE                                  = $ece;
  XK_Hangul_WI                                  = $ecf;
  XK_Hangul_YU                                  = $ed0;
  XK_Hangul_EU                                  = $ed1;
  XK_Hangul_YI                                  = $ed2;
  XK_Hangul_I                                   = $ed3;

{ Hangul syllable-final (JongSeong) Characters }
  XK_Hangul_J_Kiyeog                            = $ed4;
  XK_Hangul_J_SsangKiyeog                               = $ed5;
  XK_Hangul_J_KiyeogSios                                = $ed6;
  XK_Hangul_J_Nieun                             = $ed7;
  XK_Hangul_J_NieunJieuj                                = $ed8;
  XK_Hangul_J_NieunHieuh                                = $ed9;
  XK_Hangul_J_Dikeud                            = $eda;
  XK_Hangul_J_Rieul                             = $edb;
  XK_Hangul_J_RieulKiyeog                               = $edc;
  XK_Hangul_J_RieulMieum                                = $edd;
  XK_Hangul_J_RieulPieub                                = $ede;
  XK_Hangul_J_RieulSios                         = $edf;
  XK_Hangul_J_RieulTieut                                = $ee0;
  XK_Hangul_J_RieulPhieuf                               = $ee1;
  XK_Hangul_J_RieulHieuh                                = $ee2;
  XK_Hangul_J_Mieum                             = $ee3;
  XK_Hangul_J_Pieub                             = $ee4;
  XK_Hangul_J_PieubSios                         = $ee5;
  XK_Hangul_J_Sios                              = $ee6;
  XK_Hangul_J_SsangSios                         = $ee7;
  XK_Hangul_J_Ieung                             = $ee8;
  XK_Hangul_J_Jieuj                             = $ee9;
  XK_Hangul_J_Cieuc                             = $eea;
  XK_Hangul_J_Khieuq                            = $eeb;
  XK_Hangul_J_Tieut                             = $eec;
  XK_Hangul_J_Phieuf                            = $eed;
  XK_Hangul_J_Hieuh                             = $eee;

{ Ancient Hangul Consonant Characters }
  XK_Hangul_RieulYeorinHieuh                    = $eef;
  XK_Hangul_SunkyeongeumMieum                   = $ef0;
  XK_Hangul_SunkyeongeumPieub                   = $ef1;
  XK_Hangul_PanSios                             = $ef2;
  XK_Hangul_KkogjiDalrinIeung                   = $ef3;
  XK_Hangul_SunkyeongeumPhieuf                  = $ef4;
  XK_Hangul_YeorinHieuh                         = $ef5;

{ Ancient Hangul Vowel Characters }
  XK_Hangul_AraeA                                       = $ef6;
  XK_Hangul_AraeAE                              = $ef7;

{ Ancient Hangul syllable-final (JongSeong) Characters }
  XK_Hangul_J_PanSios                           = $ef8;
  XK_Hangul_J_KkogjiDalrinIeung                 = $ef9;
  XK_Hangul_J_YeorinHieuh                               = $efa;

{ Korean currency symbol }
  XK_Korean_Won                                 = $eff;

{$ENDIF} { XK_KOREAN }

{*
 *   Armenian
 *   Byte 3 = = $14
 *}

{$IFDEF XK_ARMENIAN}
  XK_Armenian_eternity                          = $14a1;
  XK_Armenian_ligature_ew                               = $14a2;
  XK_Armenian_full_stop                         = $14a3;
  XK_Armenian_verjaket                          = $14a3;
  XK_Armenian_parenright                                = $14a4;
  XK_Armenian_parenleft                         = $14a5;
  XK_Armenian_guillemotright                    = $14a6;
  XK_Armenian_guillemotleft                     = $14a7;
  XK_Armenian_em_dash                           = $14a8;
  XK_Armenian_dot                                       = $14a9;
  XK_Armenian_mijaket                           = $14a9;
  XK_Armenian_separation_mark                   = $14aa;
  XK_Armenian_but                                       = $14aa;
  XK_Armenian_comma                             = $14ab;
  XK_Armenian_en_dash                           = $14ac;
  XK_Armenian_hyphen                            = $14ad;
  XK_Armenian_yentamna                          = $14ad;
  XK_Armenian_ellipsis                          = $14ae;
  XK_Armenian_exclam                            = $14af;
  XK_Armenian_amanak                            = $14af;
  XK_Armenian_accent                            = $14b0;
  XK_Armenian_shesht                            = $14b0;
  XK_Armenian_question                          = $14b1;
  XK_Armenian_paruyk                            = $14b1;
  XKc_Armenian_AYB                                      = $14b2;
  XK_Armenian_ayb                                       = $14b3;
  XKc_Armenian_BEN                                      = $14b4;
  XK_Armenian_ben                                       = $14b5;
  XKc_Armenian_GIM                                      = $14b6;
  XK_Armenian_gim                                       = $14b7;
  XKc_Armenian_DA                                       = $14b8;
  XK_Armenian_da                                        = $14b9;
  XKc_Armenian_YECH                             = $14ba;
  XK_Armenian_yech                              = $14bb;
  XKc_Armenian_ZA                                       = $14bc;
  XK_Armenian_za                                        = $14bd;
  XKc_Armenian_E                                        = $14be;
  XK_Armenian_e                                 = $14bf;
  XKc_Armenian_AT                                       = $14c0;
  XK_Armenian_at                                        = $14c1;
  XKc_Armenian_TO                                       = $14c2;
  XK_Armenian_to                                        = $14c3;
  XKc_Armenian_ZHE                                      = $14c4;
  XK_Armenian_zhe                                       = $14c5;
  XKc_Armenian_INI                                      = $14c6;
  XK_Armenian_ini                                       = $14c7;
  XKc_Armenian_LYUN                             = $14c8;
  XK_Armenian_lyun                              = $14c9;
  XKc_Armenian_KHE                                      = $14ca;
  XK_Armenian_khe                                       = $14cb;
  XKc_Armenian_TSA                                      = $14cc;
  XK_Armenian_tsa                                       = $14cd;
  XKc_Armenian_KEN                                      = $14ce;
  XK_Armenian_ken                                       = $14cf;
  XKc_Armenian_HO                                       = $14d0;
  XK_Armenian_ho                                        = $14d1;
  XKc_Armenian_DZA                                      = $14d2;
  XK_Armenian_dza                                       = $14d3;
  XKc_Armenian_GHAT                             = $14d4;
  XK_Armenian_ghat                              = $14d5;
  XKc_Armenian_TCHE                             = $14d6;
  XK_Armenian_tche                              = $14d7;
  XKc_Armenian_MEN                                      = $14d8;
  XK_Armenian_men                                       = $14d9;
  XKc_Armenian_HI                                       = $14da;
  XK_Armenian_hi                                        = $14db;
  XKc_Armenian_NU                                       = $14dc;
  XK_Armenian_nu                                        = $14dd;
  XKc_Armenian_SHA                                      = $14de;
  XK_Armenian_sha                                       = $14df;
  XKc_Armenian_VO                                       = $14e0;
  XK_Armenian_vo                                        = $14e1;
  XKc_Armenian_CHA                                      = $14e2;
  XK_Armenian_cha                                       = $14e3;
  XKc_Armenian_PE                                       = $14e4;
  XK_Armenian_pe                                        = $14e5;
  XKc_Armenian_JE                                       = $14e6;
  XK_Armenian_je                                        = $14e7;
  XKc_Armenian_RA                                       = $14e8;
  XK_Armenian_ra                                        = $14e9;
  XKc_Armenian_SE                                       = $14ea;
  XK_Armenian_se                                        = $14eb;
  XKc_Armenian_VEV                                      = $14ec;
  XK_Armenian_vev                                       = $14ed;
  XKc_Armenian_TYUN                             = $14ee;
  XK_Armenian_tyun                              = $14ef;
  XKc_Armenian_RE                                       = $14f0;
  XK_Armenian_re                                        = $14f1;
  XKc_Armenian_TSO                                      = $14f2;
  XK_Armenian_tso                                       = $14f3;
  XKc_Armenian_VYUN                             = $14f4;
  XK_Armenian_vyun                              = $14f5;
  XKc_Armenian_PYUR                             = $14f6;
  XK_Armenian_pyur                              = $14f7;
  XKc_Armenian_KE                                       = $14f8;
  XK_Armenian_ke                                        = $14f9;
  XKc_Armenian_O                                        = $14fa;
  XK_Armenian_o                                 = $14fb;
  XKc_Armenian_FE                                       = $14fc;
  XK_Armenian_fe                                        = $14fd;
  XK_Armenian_apostrophe                                = $14fe;
  XK_Armenian_section_sign                      = $14ff;
{$ENDIF} { XK_ARMENIAN }

{*
 *   Georgian
 *   Byte 3 = = $15
 *}

{$IFDEF XK_GEORGIAN}
  XK_Georgian_an                                        = $15d0;
  XK_Georgian_ban                                       = $15d1;
  XK_Georgian_gan                                       = $15d2;
  XK_Georgian_don                                       = $15d3;
  XK_Georgian_en                                        = $15d4;
  XK_Georgian_vin                                       = $15d5;
  XK_Georgian_zen                                       = $15d6;
  XK_Georgian_tan                                       = $15d7;
  XK_Georgian_in                                        = $15d8;
  XK_Georgian_kan                                       = $15d9;
  XK_Georgian_las                                       = $15da;
  XK_Georgian_man                                       = $15db;
  XK_Georgian_nar                                       = $15dc;
  XK_Georgian_on                                        = $15dd;
  XK_Georgian_par                                       = $15de;
  XK_Georgian_zhar                              = $15df;
  XK_Georgian_rae                                       = $15e0;
  XK_Georgian_san                                       = $15e1;
  XK_Georgian_tar                                       = $15e2;
  XK_Georgian_un                                        = $15e3;
  XK_Georgian_phar                              = $15e4;
  XK_Georgian_khar                              = $15e5;
  XK_Georgian_ghan                              = $15e6;
  XK_Georgian_qar                                       = $15e7;
  XK_Georgian_shin                              = $15e8;
  XK_Georgian_chin                              = $15e9;
  XK_Georgian_can                                       = $15ea;
  XK_Georgian_jil                                       = $15eb;
  XK_Georgian_cil                                       = $15ec;
  XK_Georgian_char                              = $15ed;
  XK_Georgian_xan                                       = $15ee;
  XK_Georgian_jhan                              = $15ef;
  XK_Georgian_hae                                       = $15f0;
  XK_Georgian_he                                        = $15f1;
  XK_Georgian_hie                                       = $15f2;
  XK_Georgian_we                                        = $15f3;
  XK_Georgian_har                                       = $15f4;
  XK_Georgian_hoe                                       = $15f5;
  XK_Georgian_fi                                        = $15f6;
{$ENDIF} { XK_GEORGIAN }

{*
 * Azeri (and other Turkic or Caucasian languages of ex-USSR)
 * Byte 3 = = $16
 *}

{$IFDEF XK_CAUCASUS}
{ latin }
  XKc_Ccedillaabovedot  = $16a2;
  XKc_Xabovedot         = $16a3;
  XKc_Qabovedot         = $16a5;
  XKc_Ibreve            = $16a6;
  XKc_IE                        = $16a7;
  XKc_UO                        = $16a8;
  XKc_Zstroke           = $16a9;
  XKc_Gcaron            = $16aa;
  XKc_Obarred           = $16af;
  XK_ccedillaabovedot   = $16b2;
  XK_xabovedot          = $16b3;
  XKc_Ocaron            = $16b4;
  XK_qabovedot          = $16b5;
  XK_ibreve             = $16b6;
  XK_ie                 = $16b7;
  XK_uo                 = $16b8;
  XK_zstroke            = $16b9;
  XK_gcaron             = $16ba;
  XK_ocaron             = $16bd;
  XK_obarred            = $16bf;
  XKc_SCHWA             = $16c6;
  XK_schwa              = $16f6;
{ those are not really Caucasus, but I put them here for now }
{ For Inupiak }
  XKc_Lbelowdot         = $16d1;
  XKc_Lstrokebelowdot   = $16d2;
  XK_lbelowdot          = $16e1;
  XK_lstrokebelowdot    = $16e2;
{ For Guarani }
  XKc_Gtilde            = $16d3;
  XK_gtilde             = $16e3;
{$ENDIF} { XK_CAUCASUS }

{*
 *   Vietnamese
 *   Byte 3 = = $1e
 *}

{$IFDEF XK_VIETNAMESE}
  XKc_Abelowdot                                 = $1ea0;
  XK_abelowdot                                  = $1ea1;
  XKc_Ahook                                     = $1ea2;
  XK_ahook                                      = $1ea3;
  XKc_Acircumflexacute                          = $1ea4;
  XK_acircumflexacute                           = $1ea5;
  XKc_Acircumflexgrave                          = $1ea6;
  XK_acircumflexgrave                           = $1ea7;
  XKc_Acircumflexhook                           = $1ea8;
  XK_acircumflexhook                            = $1ea9;
  XKc_Acircumflextilde                          = $1eaa;
  XK_acircumflextilde                           = $1eab;
  XKc_Acircumflexbelowdot                               = $1eac;
  XK_acircumflexbelowdot                                = $1ead;
  XKc_Abreveacute                                       = $1eae;
  XK_abreveacute                                        = $1eaf;
  XKc_Abrevegrave                                       = $1eb0;
  XK_abrevegrave                                        = $1eb1;
  XKc_Abrevehook                                        = $1eb2;
  XK_abrevehook                                 = $1eb3;
  XKc_Abrevetilde                                       = $1eb4;
  XK_abrevetilde                                        = $1eb5;
  XKc_Abrevebelowdot                            = $1eb6;
  XK_abrevebelowdot                             = $1eb7;
  XKc_Ebelowdot                                 = $1eb8;
  XK_ebelowdot                                  = $1eb9;
  XKc_Ehook                                     = $1eba;
  XK_ehook                                      = $1ebb;
  XKc_Etilde                                    = $1ebc;
  XK_etilde                                     = $1ebd;
  XKc_Ecircumflexacute                          = $1ebe;
  XK_ecircumflexacute                           = $1ebf;
  XKc_Ecircumflexgrave                          = $1ec0;
  XK_ecircumflexgrave                           = $1ec1;
  XKc_Ecircumflexhook                           = $1ec2;
  XK_ecircumflexhook                            = $1ec3;
  XKc_Ecircumflextilde                          = $1ec4;
  XK_ecircumflextilde                           = $1ec5;
  XKc_Ecircumflexbelowdot                               = $1ec6;
  XK_ecircumflexbelowdot                                = $1ec7;
  XKc_Ihook                                     = $1ec8;
  XK_ihook                                      = $1ec9;
  XKc_Ibelowdot                                 = $1eca;
  XK_ibelowdot                                  = $1ecb;
  XKc_Obelowdot                                 = $1ecc;
  XK_obelowdot                                  = $1ecd;
  XKc_Ohook                                     = $1ece;
  XK_ohook                                      = $1ecf;
  XKc_Ocircumflexacute                          = $1ed0;
  XK_ocircumflexacute                           = $1ed1;
  XKc_Ocircumflexgrave                          = $1ed2;
  XK_ocircumflexgrave                           = $1ed3;
  XKc_Ocircumflexhook                           = $1ed4;
  XK_ocircumflexhook                            = $1ed5;
  XKc_Ocircumflextilde                          = $1ed6;
  XK_ocircumflextilde                           = $1ed7;
  XKc_Ocircumflexbelowdot                               = $1ed8;
  XK_ocircumflexbelowdot                                = $1ed9;
  XKc_Ohornacute                                        = $1eda;
  XK_ohornacute                                 = $1edb;
  XKc_Ohorngrave                                        = $1edc;
  XK_ohorngrave                                 = $1edd;
  XKc_Ohornhook                                 = $1ede;
  XK_ohornhook                                  = $1edf;
  XKc_Ohorntilde                                        = $1ee0;
  XK_ohorntilde                                 = $1ee1;
  XKc_Ohornbelowdot                             = $1ee2;
  XK_ohornbelowdot                              = $1ee3;
  XKc_Ubelowdot                                 = $1ee4;
  XK_ubelowdot                                  = $1ee5;
  XKc_Uhook                                     = $1ee6;
  XK_uhook                                      = $1ee7;
  XKc_Uhornacute                                        = $1ee8;
  XK_uhornacute                                 = $1ee9;
  XKc_Uhorngrave                                        = $1eea;
  XK_uhorngrave                                 = $1eeb;
  XKc_Uhornhook                                 = $1eec;
  XK_uhornhook                                  = $1eed;
  XKc_Uhorntilde                                        = $1eee;
  XK_uhorntilde                                 = $1eef;
  XKc_Uhornbelowdot                             = $1ef0;
  XK_uhornbelowdot                              = $1ef1;
  XKc_Ybelowdot                                 = $1ef4;
  XK_ybelowdot                                  = $1ef5;
  XKc_Yhook                                     = $1ef6;
  XK_yhook                                      = $1ef7;
  XKc_Ytilde                                    = $1ef8;
  XK_ytilde                                     = $1ef9;
  XKc_Ohorn                                     = $1efa; { U+01a0 }
  XK_ohorn                                      = $1efb; { U+01a1 }
  XKc_Uhorn                                     = $1efc; { U+01af }
  XK_uhorn                                      = $1efd; { U+01b0 }

  XK_combining_tilde                            = $1e9f; { U+0303 }
  XK_combining_grave                            = $1ef2; { U+0300 }
  XK_combining_acute                            = $1ef3; { U+0301 }
  XK_combining_hook                             = $1efe; { U+0309 }
  XK_combining_belowdot                         = $1eff; { U+0323 }
{$ENDIF} { XK_VIETNAMESE }

{$IFDEF XK_CURRENCY}
  XK_EcuSign                                    = $20a0;
  XK_ColonSign                                  = $20a1;
  XK_CruzeiroSign                                       = $20a2;
  XK_FFrancSign                                 = $20a3;
  XK_LiraSign                                   = $20a4;
  XK_MillSign                                   = $20a5;
  XK_NairaSign                                  = $20a6;
  XK_PesetaSign                                 = $20a7;
  XK_RupeeSign                                  = $20a8;
  XK_WonSign                                    = $20a9;
  XK_NewSheqelSign                              = $20aa;
  XK_DongSign                                   = $20ab;
  XK_EuroSign                                   = $20ac;
{$ENDIF}
Implementation
End.
