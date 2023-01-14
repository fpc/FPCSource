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
}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  __LC_ALL          = -1;
  __LC_COLLATE      = 0;
  __LC_CTYPE        = 1;
  __LC_MONETARY     = 2;
  __LC_NUMERIC      = 3;
  __LC_TIME         = 4;
  __LC_MESSAGES     = 5;
  _LC_LAST         = 6;              { marks end }


Type
     lconv = record
          decimal_point     : PAnsiChar;
          thousands_sep     : PAnsiChar;
          grouping          : PAnsiChar;
          int_curr_symbol   : PAnsiChar;
          currency_symbol   : PAnsiChar;
          mon_decimal_point : PAnsiChar;
          mon_thousands_sep : PAnsiChar;
          mon_grouping      : PAnsiChar;
          positive_sign     : PAnsiChar;
          negative_sign     : PAnsiChar;
          int_frac_digits   : AnsiChar;
          frac_digits       : AnsiChar;
          p_cs_precedes     : AnsiChar;
          p_sep_by_space    : AnsiChar;
          n_cs_precedes     : AnsiChar;
          n_sep_by_space    : AnsiChar;
          p_sign_posn       : AnsiChar;
          n_sign_posn       : AnsiChar;
          left_parenthesis  : AnsiChar;
          right_parenthesis : AnsiChar;
          int_p_cs_precedes : AnsiChar;
          int_p_sep_by_space: AnsiChar;
          int_n_cs_precedes : AnsiChar;
          int_n_sep_by_space: AnsiChar;
          int_p_sign_posn   : AnsiChar;
          int_n_sign_posn   : AnsiChar;
       end;
     Tlconv = lconv;
     plconv = ^lconv;

Const
        CODESET         = 49;     { codeset name }
        D_T_FMT         = 1;     { string for formatting date and time }
        D_FMT           = 2;     { date format string }
        T_FMT           = 3;     { time format string }
        T_FMT_AMPM      = 55;    { a.m. or p.m. time formatting string }
        AM_STR          = 4;     { Ante Meridian affix }
        PM_STR          = 5;     { Post Meridian affix }

{ week day names }
        DAY_1           = 13;
        DAY_2           = 14;
        DAY_3           = 15;
        DAY_4           = 16;
        DAY_5           = 17;
        DAY_6           = 18;
        DAY_7           = 19;

{ abbreviated week day names }
        ABDAY_1         = 6;
        ABDAY_2         = 7;
        ABDAY_3         = 8;
        ABDAY_4         = 9;
        ABDAY_5         = 10;
        ABDAY_6         = 11;
        ABDAY_7         = 22;

{ month names }
        MON_1           = 32;
        MON_2           = 33;
        MON_3           = 34;
        MON_4           = 35;
        MON_5           = 36;
        MON_6           = 37;
        MON_7           = 38;
        MON_8           = 39;
        MON_9           = 40;
        MON_10          = 41;
        MON_11          = 42;
        MON_12          = 43;

{ abbreviated month names }
        ABMON_1         = 20;
        ABMON_2         = 21;
        ABMON_3         = 22;
        ABMON_4         = 23;
        ABMON_5         = 24;
        ABMON_6         = 25;
        ABMON_7         = 26;
        ABMON_8         = 27;
        ABMON_9         = 28;
        ABMON_10        = 29;
        ABMON_11        = 30;
        ABMON_12        = 31;
                        
        ERA             = 56;      { era description segments }
        ERA_D_FMT       = 57;      { era date format string }
        ERA_D_T_FMT     = 58;      { era date and time format string }
        ERA_T_FMT       = 59;      { era time format string }
        ALT_DIGITS      = 60;      { alternative symbols for digits }
        RADIXCHAR       = 44;      { radix char }
        THOUSEP         = 45;      { separator for thousands }

        YESEXPR         = 61;      { affirmative response expression }
        NOEXPR          = 62;      { negative response expression }
        YESSTR          = 46;      { affirmative response for yes/no queries }
        NOSTR           = 47;      { negative response for yes/no queries }
        CRNCYSTR        = 48;      { currency symbol }

function localeconv:plconv; cdecl; external clib name 'localeconv';
