{*************************************************
*       Perl-Compatible Regular Expressions      *
*************************************************}

{ Copyright (c) 1997-2000 University of Cambridge }

{
 * @file include/pcreposix.h
 * @brief PCRE definitions
 }

{ This is the header for the POSIX wrapper interface to the PCRE Perl-
Compatible Regular Expression library. It defines the things POSIX says should
be there. I hope. }

{ Have to include stdlib.h in order to ensure that size_t is defined. }

{ Options defined by POSIX. }

const
  { Ignore case }
  REG_ICASE    = $01;
  { Don't match newlines with wildcards }
  REG_NEWLINE  = $02;
  { Don't match BOL }
  REG_NOTBOL   = $04;
  { Don't match EOL }
  REG_NOTEOL   = $08;

{ These are not used by PCRE, but by defining them we make it easier
to slot PCRE into existing programs that make POSIX calls. }

  { UNUSED! }
  REG_EXTENDED = 0;
  { UNUSED! }
  REG_NOSUB    = 0;

{ Error values. Not all these are relevant or used by the wrapper. }

type
 pc_error = (
  REG_ASSERT = 1,  { internal error ? }
  REG_BADBR,       { invalid repeat counts in } {}
  REG_BADPAT,      { pattern error }
  REG_BADRPT,      { ? * + invalid }
  REG_EBRACE,      { unbalanced } {}
  REG_EBRACK,      { unbalanced [] }
  REG_ECOLLATE,    { collation error - not relevant }
  REG_ECTYPE,      { bad class }
  REG_EESCAPE,     { bad escape sequence }
  REG_EMPTY,       { empty expression }
  REG_EPAREN,      { unbalanced () }
  REG_ERANGE,      { bad range inside [] }
  REG_ESIZE,       { expression too big }
  REG_ESPACE,      { failed to get memory }
  REG_ESUBREG,     { bad back reference }
  REG_INVARG,      { bad argument }
  REG_NOMATCH      { match failed }
 );


{ The structure representing a compiled regular expression. }

  regex_t = record
    re_pcre: Pointer;
    re_nsub, re_erroffset: size_t;
  end;
  
  Pregex_t = ^regex_t;

{ The structure in which a captured offset is returned. }

  regoff_t = Integer;

  regmatch_t = record
    rm_so, rm_eo: regoff_t;
  end;

{ The functions }

{extern int regcomp(regex_t *, const char *, int);
extern int regexec(regex_t *, const char *, size_t, regmatch_t *, int);
extern size_t regerror(int, const regex_t *, char *, size_t);
extern void regfree(regex_t *);}

