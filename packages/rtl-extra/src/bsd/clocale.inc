{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    Init rtl formating variables based on BSD like libc locales

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

{ Initial FreeBSD Implementation by Marco van de Voort.
  Crosschecked with OS X 10.4/ppc headers.
  To some symbols leading underscores were added. If these values are
   ever exposed in an interface this has to be reevaluated.

  OpenBSD's langinfo came from NetBSD which is different to FreeBSD. CODESET is
  at the end, shifting the older constants one up, the ERA block
  is missing and the order after RADIXCHAR is different, first -EXPR then -STR on
  FreeBSD and -EXPR paired with corresponding -STR on Open/NetBSD
}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  __LC_ALL          = 0;
  __LC_COLLATE      = 1;
  __LC_CTYPE        = 2;
  __LC_MONETARY     = 3;
  __LC_NUMERIC      = 4;
  __LC_TIME         = 5;
  __LC_MESSAGES     = 6;
  _LC_LAST          = 7;              { marks end }


Type
     lconv = record
          decimal_point     : pchar;
          thousands_sep     : pchar;
          grouping          : pchar;
          int_curr_symbol   : pchar;
          currency_symbol   : pchar;
          mon_decimal_point : pchar;
          mon_thousands_sep : pchar;
          mon_grouping      : pchar;
          positive_sign     : pchar;
          negative_sign     : pchar;
          int_frac_digits   : char;
          frac_digits       : char;
          p_cs_precedes     : char;
          p_sep_by_space    : char;
          n_cs_precedes     : char;
          n_sep_by_space    : char;
          p_sign_posn       : char;
          n_sign_posn       : char;
          int_p_cs_precedes : char;
          int_n_cs_precedes : char;
          int_p_sep_by_space: char;
          int_n_sep_by_space: char;
          int_p_sign_posn   : char;
          int_n_sign_posn   : char;
       end;
     Tlconv = lconv;
     plconv = ^lconv;

Const
{$if defined(openbsd) or defined(netbsd)}
        D_T_FIRST       = 0;
{$else}
        // FreeBSD and Mac OS X?
        D_T_FIRST       = 1;
        CODESET         = 0;               { codeset name }
{$endif}
        D_T_FMT         = D_T_FIRST;       { string for formatting date and time }
        D_FMT           = D_T_FIRST+1;     { date format string }
        T_FMT           = D_T_FIRST+2;     { time format string }
        T_FMT_AMPM      = D_T_FIRST+3;     { a.m. or p.m. time formatting string }
        AM_STR          = D_T_FIRST+4;     { Ante Meridian affix }
        PM_STR          = D_T_FIRST+5;     { Post Meridian affix }

{ week day names }
        DAY_1           = PM_STR+1;
        DAY_2           = DAY_1+1;
        DAY_3           = DAY_1+2;
        DAY_4           = DAY_1+3;
        DAY_5           = DAY_1+4;
        DAY_6           = DAY_1+5;
        DAY_7           = DAY_1+6;

{ abbreviated week day names }
        ABDAY_1         = DAY_7+1;
        ABDAY_2         = ABDAY_1+1;
        ABDAY_3         = ABDAY_1+2;
        ABDAY_4         = ABDAY_1+3;
        ABDAY_5         = ABDAY_1+4;
        ABDAY_6         = ABDAY_1+5;
        ABDAY_7         = ABDAY_1+6;

{ month names }
        MON_1           = ABDAY_7+1;
        MON_2           = MON_1+1;
        MON_3           = MON_1+2;
        MON_4           = MON_1+3;
        MON_5           = MON_1+4;
        MON_6           = MON_1+5;
        MON_7           = MON_1+6;
        MON_8           = MON_1+7;
        MON_9           = MON_1+8;
        MON_10          = MON_1+9;
        MON_11          = MON_1+10;
        MON_12          = MON_1+11;

{ abbreviated month names }
        ABMON_1         = MON_12+1;
        ABMON_2         = ABMON_1+1;
        ABMON_3         = ABMON_1+2;
        ABMON_4         = ABMON_1+3;
        ABMON_5         = ABMON_1+4;
        ABMON_6         = ABMON_1+5;
        ABMON_7         = ABMON_1+6;
        ABMON_8         = ABMON_1+7;
        ABMON_9         = ABMON_1+8;
        ABMON_10        = ABMON_1+9;
        ABMON_11        = ABMON_1+10;
        ABMON_12        = ABMON_1+11;
        {$if defined(openbsd) or defined(netbsd)}
        RADIXCHAR       = ABMON_1+12;   { Radix char}
        THOUSEP         = RADIXCHAR+1;  { separator for thousands }
        YESSTR          = RADIXCHAR+2;  { affirmative response for yes/no queries }
        YESEXPR         = RADIXCHAR+3;  { affirmative response expression }
        NOSTR           = RADIXCHAR+4;  { negative response for yes/no queries }
        NOEXPR          = RADIXCHAR+5;  { negative response expression }
        CRNCYSTR        = RADIXCHAR+6;  { currency symbol }
        {$else}
        ERA             = ABMON_12+1;           { era description segments }
        ERA_D_FMT       = ERA+1;        { era date format string }
        ERA_D_T_FMT     = ERA+2;        { era date and time format string }
        ERA_T_FMT       = ERA+3;        { era time format string }
        ALT_DIGITS      = ERA+4;        { alternative symbols for digits }
        RADIXCHAR       = ERA+5;        { radix char }
        THOUSEP         = RADIXCHAR+1;  { separator for thousands }
        YESEXPR         = RADIXCHAR+2;  { affirmative response expression }
        NOEXPR          = RADIXCHAR+3;  { negative response expression }
        YESSTR          = RADIXCHAR+4;  { affirmative response for yes/no queries }
        NOSTR           = RADIXCHAR+5;  { negative response for yes/no queries }
        CRNCYSTR        = RADIXCHAR+6;  { currency symbol }
        {$endif}

        {$if defined(openbsd) or defined(netbsd)}
        CODESET         = 51;
        {$else}
        D_MD_ORDER      = 57;      { month/day order (local extension) }
        {$endif}

function localeconv:plconv; cdecl; external clib name 'localeconv';
