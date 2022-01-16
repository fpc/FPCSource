{ IHX (Intel Hex format) to TZX (ZX Spectrum tape file format) convertor tool

  This file contains various definitions, constants and utility functions for
  dealing with BASIC programs on the ZX Spectrum.

  Copyright (C) 2020 Nikolay Nikolov <nickysn@users.sourceforg.net>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit zxbasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  { the ZX BASIC special keyword character set }
  BC_RND       = #165;  { RND       }
  BC_INKEYS    = #166;  { INKEY$    }
  BC_PI        = #167;  { PI        }
  BC_FN        = #168;  { FN        }
  BC_POINT     = #169;  { POINT     }
  BC_SCREENS   = #170;  { SCREEN$   }
  BC_ATTR      = #171;  { ATTR      }
  BC_AT        = #172;  { AT        }
  BC_TAB       = #173;  { TAB       }
  BC_VALS      = #174;  { VAL$      }
  BC_CODE      = #175;  { CODE      }
  BC_VAL       = #176;  { VAL       }
  BC_LEN       = #177;  { LEN       }
  BC_SIN       = #178;  { SIN       }
  BC_COS       = #179;  { COS       }
  BC_TAN       = #180;  { TAN       }
  BC_ASN       = #181;  { ASN       }
  BC_ACS       = #182;  { ACS       }
  BC_ATN       = #183;  { ATN       }
  BC_LN        = #184;  { LN        }
  BC_EXP       = #185;  { EXP       }
  BC_INT       = #186;  { INT       }
  BC_SOR       = #187;  { SOR       }
  BC_SGN       = #188;  { SGN       }
  BC_ABS       = #189;  { ABS       }
  BC_PEEK      = #190;  { PEEK      }
  BC_IN        = #191;  { IN        }
  BC_USR       = #192;  { USR       }
  BC_STRS      = #193;  { STR$      }
  BC_CHRS      = #194;  { CHR$      }
  BC_NOT       = #195;  { NOT       }
  BC_BIN       = #196;  { BIN       }
  BC_OR        = #197;  { OR        }
  BC_AND       = #198;  { AND       }
  BC_LE        = #199;  { <=        }
  BC_GE        = #200;  { >=        }
  BC_NEQ       = #201;  { <>        }
  BC_LINE      = #202;  { LINE      }
  BC_THEN      = #203;  { THEN      }
  BC_TO        = #204;  { TO        }
  BC_STEP      = #205;  { STEP      }
  BC_DEF_FN    = #206;  { DEF FN    }
  BC_CAT       = #207;  { CAT       }
  BC_FORMAT    = #208;  { FORMAT    }
  BC_MOVE      = #209;  { MOVE      }
  BC_ERASE     = #210;  { ERASE     }
  BC_OPEN      = #211;  { OPEN      }
  BC_CLOSE     = #212;  { CLOSE     }
  BC_MERGE     = #213;  { MERGE     }
  BC_VERIFY    = #214;  { VERIFY    }
  BC_BEEP      = #215;  { BEEP      }
  BC_CIRCLE    = #216;  { CIRCLE    }
  BC_INK       = #217;  { INK       }
  BC_PAPER     = #218;  { PAPER     }
  BC_FLASH     = #219;  { FLASH     }
  BC_BRIGHT    = #220;  { BRIGHT    }
  BC_INVERSE   = #221;  { INVERSE   }
  BC_OVER      = #222;  { OVER      }
  BC_OUT       = #223;  { OUT       }
  BC_LPRINT    = #224;  { LPRINT    }
  BC_LLIST     = #225;  { LLIST     }
  BC_STOP      = #226;  { STOP      }
  BC_READ      = #227;  { READ      }
  BC_DATA      = #228;  { DATA      }
  BC_RESTORE   = #229;  { RESTORE   }
  BC_NEW       = #230;  { NEW       }
  BC_BORDER    = #231;  { BORDER    }
  BC_CONTINUE  = #232;  { CONTINUE  }
  BC_DIM       = #233;  { DIM       }
  BC_REM       = #234;  { REM       }
  BC_FOR       = #235;  { FOR       }
  BC_GO_TO     = #236;  { GO TO     }
  BC_GO_SUB    = #237;  { GO SUB    }
  BC_INPUT     = #238;  { INPUT     }
  BC_LOAD      = #239;  { LOAD      }
  BC_LIST      = #240;  { LIST      }
  BC_LET       = #241;  { LET       }
  BC_PAUSE     = #242;  { PAUSE     }
  BC_NEXT      = #243;  { NEXT      }
  BC_POKE      = #244;  { POKE      }
  BC_PRINT     = #245;  { PRINT     }
  BC_PLOT      = #246;  { PLOT      }
  BC_RUN       = #247;  { RUN       }
  BC_SAVE      = #248;  { SAVE      }
  BC_RANDOMIZE = #249;  { RANDOMIZE }
  BC_IF        = #250;  { IF        }
  BC_CLS       = #251;  { CLS       }
  BC_DRAW      = #252;  { DRAW      }
  BC_CLEAR     = #253;  { CLEAR     }
  BC_RETURN    = #254;  { RETURN    }
  BC_COPY      = #255;  { COPY      }

function BAS_EncodeNumber(N: Integer): ansistring;
function BAS_EncodeNumber(N: Real): ansistring;
function BAS_EncodeLine(LineNr: Integer; const Line: string): ansistring;

implementation

function BAS_EncodeNumber(N: Integer): ansistring;
begin
  if (N >= -65535) and (N <= 65535) then
  begin
    Str(N, Result);
    N := Abs(N);
    Result := Result + #14#0#0 + Chr(Byte(N)) + Chr(Byte(N shr 8)) + #0;
  end
  else
    Result := BAS_EncodeNumber(Real(N));
end;

function BAS_EncodeNumber(N: Real): ansistring;
begin
  raise ENotImplemented.Create('Real number support not yet implemented');
end;

function BAS_EncodeLine(LineNr: Integer; const Line: string): ansistring;
begin
  Result := Chr(Byte(LineNr shr 8)) + Chr(Byte(LineNr)) +
            Chr(Byte((Length(Line) + 1))) + Chr(Byte((Length(Line) + 1) shr 8)) +
            Line + #13;
end;

end.

