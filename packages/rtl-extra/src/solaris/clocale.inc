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
  Updated by Ludo Brands from Solaris 10 headers files.
}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


// from /usr/include/iso/locale_iso.h
      __LC_CTYPE    = 0;
      __LC_NUMERIC  = 1;
      __LC_TIME     = 2;
      __LC_COLLATE  = 3;
      __LC_MONETARY = 4;
      __LC_MESSAGES = 5;
      __LC_ALL      = 6;


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
          int_p_sep_by_space: char;
          int_n_cs_precedes : char;
          int_n_sep_by_space: char;
          int_p_sign_posn   : char;
          int_n_sign_posn   : char;
       end;
     Tlconv = lconv;
     plconv = ^lconv;

// imported from /usr/include/langinfo.h

Const
  {
   * The seven days of the week in their full beauty
    }
  { sunday  }
    DAY_1 = 1;
  { monday  }
    DAY_2 = 2;
  { tuesday  }
    DAY_3 = 3;
  { wednesday  }
    DAY_4 = 4;
  { thursday  }
    DAY_5 = 5;
  { friday  }
    DAY_6 = 6;
  { saturday  }
    DAY_7 = 7;
  {
   * The abbreviated seven days of the week
    }
  { sun  }
    ABDAY_1 = 8;
  { mon  }
    ABDAY_2 = 9;
  { tue  }
    ABDAY_3 = 10;
  { wed  }
    ABDAY_4 = 11;
  { thu  }
    ABDAY_5 = 12;
  { fri  }
    ABDAY_6 = 13;
  { sat  }
    ABDAY_7 = 14;
  {
   * The full names of the twelve months...
    }
  { january  }
    MON_1 = 15;
  { february  }
    MON_2 = 16;
  { march  }
    MON_3 = 17;
  { april  }
    MON_4 = 18;
  { may  }
    MON_5 = 19;
  { june  }
    MON_6 = 20;
  { july  }
    MON_7 = 21;
  { august  }
    MON_8 = 22;
  { september  }
    MON_9 = 23;
  { october  }
    MON_10 = 24;
  { november  }
    MON_11 = 25;
  { december  }
    MON_12 = 26;
  {
   * ... and their abbreviated form
    }
  { jan  }
    ABMON_1 = 27;
  { feb  }
    ABMON_2 = 28;
  { mar  }
    ABMON_3 = 29;
  { apr  }
    ABMON_4 = 30;
  { may  }
    ABMON_5 = 31;
  { jun  }
    ABMON_6 = 32;
  { jul  }
    ABMON_7 = 33;
  { aug  }
    ABMON_8 = 34;
  { sep  }
    ABMON_9 = 35;
  { oct  }
    ABMON_10 = 36;
  { nov  }
    ABMON_11 = 37;
  { dec  }
    ABMON_12 = 38;
  {
   * plus some special strings you might need to know
    }
  { radix character  }
    RADIXCHAR = 39;
  { separator for thousand  }
    THOUSEP = 40;
  { YESSTR and NOSTR marked as legacy in XPG5 and removed in SUSv3  }
  { affirmative response for yes/no queries  }

    YESSTR = 41;
  { negative response for yes/no queries  }
    NOSTR = 42;
  { !defined(_XPG6) || defined(__EXTENSIONS__  }
  { currency symbol  }

    CRNCYSTR = 43;
  {
   * Default string used to format date and time
   *	e.g. Sunday, August 24 21:08:38 MET 1986
    }
  { string for formatting date and time  }
    D_T_FMT = 44;
  { date format  }
    D_FMT = 45;
  { time format  }
    T_FMT = 46;
  { am string  }
    AM_STR = 47;
  { pm string  }
    PM_STR = 48;
  {
   * Additions for XPG4 (XSH4) Compliance
    }
  { codeset name  }
    CODESET = 49;
  { am or pm time format string  }
    T_FMT_AMPM = 50;
  { era description segments  }
    ERA = 51;
  { era date format string  }
    ERA_D_FMT = 52;
  { era date and time format string  }
    ERA_D_T_FMT = 53;
  { era time format string  }
    ERA_T_FMT = 54;
  { alternative symbols for digits  }
    ALT_DIGITS = 55;
  { affirmative response expression  }
    YESEXPR = 56;
  { negative response expression  }
    NOEXPR = 57;
  { strftime format for date(1)  }
    _DATE_FMT = 58;

function localeconv:plconv; cdecl; external clib name 'localeconv';
