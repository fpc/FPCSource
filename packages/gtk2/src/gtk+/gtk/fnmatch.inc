// included gtk2.pas

{$IFDEF read_interface_rest}

function __P(protos : longint) : longint;

{ We #undef these before defining them because some losing systems
   (HP-UX A.08.07 for example) define these in <unistd.h>.   }
{$undef FNM_PATHNAME}
{$undef FNM_NOESCAPE}
{$undef FNM_PERIOD}
{ Bits set in the FLAGS argument to `fnmatch'.   }
{ No wildcard can ever match `/'.   }

const
   FNM_PATHNAME = 1 shl 0;
{ Backslashes don't quote special chars.   }
   FNM_NOESCAPE = 1 shl 1;
{ Leading `.' is matched only explicitly.   }
   FNM_PERIOD = 1 shl 2;

{ Preferred GNU name.   }
const
   FNM_FILE_NAME = FNM_PATHNAME;
{ Ignore `/...' after a match.   }
   FNM_LEADING_DIR = 1 shl 3;
{ Compare without regard to case.   }
   FNM_CASEFOLD = 1 shl 4;

{ Value returned by `fnmatch' if STRING does not match PATTERN.   }
const
   FNM_NOMATCH = 1;

{ Match STRING against the filename pattern PATTERN,
   returning zero if it matches, FNM_NOMATCH if not.   }
function fnmatch(__pattern: char; __string: char;
  __flags: gint): gint; cdecl; external gtklib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function __P(protos : longint) : longint;
begin
   __P:=protos;
end;
{$ENDIF read_implementation}

// included gtk2.pas

