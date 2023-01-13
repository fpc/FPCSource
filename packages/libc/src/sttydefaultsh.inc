

const
   TTYDEF_IFLAG = ((((BRKINT or ISTRIP) or ICRNL) or IMAXBEL) or IXON) or IXANY;
   TTYDEF_OFLAG = (OPOST or ONLCR) or XTABS;
   TTYDEF_LFLAG = (((((ECHO or ICANON) or ISIG) or IEXTEN) or ECHOE) or ECHOKE) or ECHOCTL;
   TTYDEF_CFLAG = ((CREAD or CS7) or PARENB) or HUPCL;
   TTYDEF_SPEED = B9600;

function CTRL(x : AnsiChar) : AnsiChar;

Const
  CEOF       = Chr(Ord('d') and $1F);
  CEOL       = #0; { XXX avoid _POSIX_VDISABLE }

  CERASE  = 0177;
  CINTR   = AnsiChar(Ord('c') and $1F);

  CSTATUS = #0;
  CKILL   = AnsiChar(Ord('u') and $1F);

  CMIN = 1;
  CQUIT = 034;

  CSUSP      = AnsiChar(Ord('z') and $1F);
  CDSUSP     = AnsiChar(Ord('y') and $1F);
  CSTART     = AnsiChar(Ord('q') and $1F);
  CSTOP      = AnsiChar(Ord('s') and $1F);
  CLNEXT     = AnsiChar(Ord('v') and $1F);
  CDISCARD   = AnsiChar(Ord('o') and $1F);
  CWERASE    = AnsiChar(Ord('w') and $1F);
  CREPRINT   = AnsiChar(Ord('r') and $1F);
  CEOT       = CEOF;

  CBRK       = CEOL;
  CRPRNT     = CREPRINT;
  CFLUSH     = CDISCARD;

  __CTIME    = #0; // renamed, ctime type conflict.


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
