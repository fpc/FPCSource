
unit oratypes;
interface

{
  Automatically converted by H2Pas 1.0.0 from oratypes.h
  The following command line parameters were used:
    -p
    -D
    -l
    jojo
    oratypes.h
}

    { Pointers to basic pascal types, inserted by h2pas conversion program.}
    Type
      PLongint  = ^Longint;
      PSmallInt = ^SmallInt;
      PByte     = ^Byte;
      PWord     = ^Word;
      PDWord    = ^DWord;
      PDouble   = ^Double;


{$PACKRECORDS C}


  { $RCSfile: oratypes.h $ $Date: 20-jul-00.13:44:19 
     ----------------------------------------------------------------------
     Copyright (c) 1982, 2003, Oracle.  All rights reserved.
     ----------------------------------------------------------------------  }
{$define ORATYPES}
{$define SX_ORACLE}  
{$define SX3_ORACLE}  

{$ifndef ORASTDDEF}
{ $include <stddef.h>}
{ $define ORASTDDEF}
{$endif}

{$ifndef ORALIMITS}
{ $include <limits.h>}
{ $define ORALIMITS}
{$endif}


// Not needed for pascal
{  const
     TRUE = 1;     
     FALSE = 0;     }

  { --- Signed/Unsigned one-byte scalar (sb1/ub1) ---  }

  type

     Pub1 = ^ub1;
     ub1 = byte;

     Psb1 = ^sb1;
     sb1 = shortint;

     Peb1 = ^eb1;
     eb1 = char;

  const
     UB1MAXVAL:ub1 = 255;
     UB1MINVAL:ub1 = 0;
     SB1MAXVAL:sb1 = 127;
     SB1MINVAL:sb1 = -128;
     MINUB1MAXVAL:ub1 = 255;
     MAXUB1MINVAL:ub1 = 0;
     MINSB1MAXVAL:sb1 = 127;
     MAXSB1MINVAL:sb1 = -127;
     EB1MAXVAL:eb1 = #255;
     EB1MINVAL:eb1 = #0;
     MINEB1MAXVAL:eb1 =#127;
     MAXEB1MINVAL:eb1 =#0;

  const
     UB1BITS = 8;

  { was #define dname def_expr }
  function UB1MASK : longint;
      { return type might be wrong }

  { backwards compatibility  }
  type

     Pb1 = ^b1;
     b1 = sb1;

  const
     B1MAXVAL:b1 = 127;  // was SB1MAXVAL
     B1MINVAL:b1 = -128; // was SB1MINVAL
  { --- Signed/Unsigned two-byte scalar (sb2/ub2) ---  }

  type

     Pub2 = ^ub2;
     ub2 = word;

     Psb2 = ^sb2;
     sb2 = smallint;

     Peb2 = ^eb2;
     eb2 = smallint;

  const
     UB2MAXVAL:ub2 = 65535;
     UB2MINVAL:ub2 = 0;
     SB2MAXVAL:sb2 = 32767;
     SB2MINVAL:sb2 = -32768;
     MINUB2MAXVAL:ub2 = 65535;
     MAXUB2MINVAL:ub2 = 0;
     MINSB2MAXVAL:sb2 = 32767;
     MAXSB2MINVAL:sb2 = -32767;
     EB2MAXVAL:eb2 = 32767;
     EB2MINVAL:eb2 = 0;
     MINEB2MAXVAL:eb2 = 32767;
     MAXEB2MINVAL:eb2 = 0;

  { backwards compatibility  }
  type

     Pb2 = ^b2;
     b2 = sb2;

  const
     B2MAXVAL = 32767;  // was SB2MAXVAL
     B2MINVAL = -32768; // was SB2MINVAL;
  { --- Signed/Unsigned four-byte scalar (sb4/ub4) ---  }

  type

     Pub4 = ^ub4;
     ub4 = dword;

     Psb4 = ^sb4;
     sb4 = longint;

     Peb4 = ^eb4;
     eb4 = longint;


  const
     UB4MAXVAL:ub4 = 4294967295;
     UB4MINVAL:ub4 = 0;
     SB4MAXVAL:sb4 = 2147483647;
     SB4MINVAL:sb4 = -2147483648;
     MINUB4MAXVAL:ub4 = 4294967295;
     MAXUB4MINVAL:ub4 = 0;
     MINSB4MAXVAL:sb4 = 2147483647;
     MAXSB4MINVAL:sb4 = -2147483647;
     EB4MAXVAL:eb4 = 2147483647;
     EB4MINVAL:eb4 =0;
     MINEB4MAXVAL:eb4 = 2147483647;
     MAXEB4MINVAL:eb4 = 0;

  { --- Signed/Unsigned eight-byte scalar (orasb8/oraub8) ---  }
  type

     Poraub8 = ^oraub8;
     oraub8 = qword;

     Porasb8 = ^orasb8;
     orasb8 = int64;

  type

     Pub8 = ^ub8;
     ub8 = oraub8;

     Psb8 = ^sb8;
     sb8 = orasb8;

  const
     ORAUB8MINVAL:oraub8=0;
     ORAUB8MAXVAL:oraub8=18446744073709551615;
     ORASB8MINVAL:orasb8=-9223372036854775808;
     ORASB8MAXVAL:orasb8= 9223372036854775807;
     MAXORAUB8MINVAL:oraub8=0;
     MINORAUB8MAXVAL:oraub8=18446744073709551615;
     MAXORASB8MINVAL:orasb8=-9223372036854775807;
     MINORASB8MAXVAL:orasb8=9223372036854775807;

  { backwards compatibility  }

  type

     Pb4 = ^b4;
     b4 = sb4;

  const
     B4MAXVAL = 2147483647;  // was SB4MAXVAL
     B4MINVAL = -2147483648; // was SB4MINVAL
  { --- Character pointer ---  }

  type
     Poratext = ^oratext;
     oratext = char;


  type

     Ptext = ^text;
     text = oratext;

    type
       Putext = ^utext;
       utext = word;

    { --- Other data types ---  }
    type

       Peword = ^eword;
       eword = longint;

       Puword = ^uword;
       uword = dword;

       Psword = ^sword;
       sword = longint;
       

  const
       EWORDMAXVAL:eword = 2147483647;
       EWORDMINVAL:eword = 0;
       UWORDMAXVAL:uword = 4294967295;
       UWORDMINVAL:uword = 0;
       SWORDMAXVAL:sword = 2147483647;
       SWORDMINVAL:sword = -2147483648;
       MINEWORDMAXVAL:eword = 2147483647;
       MAXEWORDMINVAL:eword = 0;
       MINUWORDMAXVAL:uword = 4294967295;
       MAXUWORDMINVAL:uword = 0;
       MINSWORDMAXVAL:sword = 2147483647;
       MAXSWORDMINVAL:sword = -2147483647;


    type

       Pubig_ora = ^ubig_ora;
       ubig_ora = dword;

       Psbig_ora = ^sbig_ora;
       sbig_ora = longint;

  const
       UBIG_ORAMAXVAL:ubig_ora = 2147483647;
       UBIG_ORAMINVAL:ubig_ora = 0;
       SBIG_ORAMAXVAL:sbig_ora = 2147483647;
       SBIG_ORAMINVAL:sbig_ora = -2147483648;
       MINUBIG_ORAMAXVAL:ubig_ora = 4294967295;
       MAXUBIG_ORAMINVAL:ubig_ora = 0;
       MINSBIG_ORAMAXVAL:sbig_ora = 2147483647;
       MAXSBIG_ORAMINVAL:sbig_ora = -2147483647;

(* error 
#define UBIGORABITS      (UB1BITS * sizeof(ubig_ora))
in define line 272 *)

    type

       lgenfp_t = procedure (_para1:pointer);cdecl;

    const
       SIZE_TMAXVAL = 2147483647; // was UBIG_ORAMAXVAL;

//    const
//       MINSIZE_TMAXVAL:size_t = 4294967295;

type
    POCITime = ^OCITime;
    OCITime = packed record
      OCITimeHH      : ub1;
      OCITimeMM      : ub1;
      OCITimeSS      : ub1;
    end;

    POCIDate = ^OCIDate;
    OCIDate = packed record
      OCIDateYYYY    : sb2;
      OCIDateMM      : ub1;
      OCIDateDD      : ub1;
      OCIDateTime    : OCITime;
    end;

    POCIDateTime = ^TOCIDate;
    TOCIDate = packed record
      Year           : sb2;
      Month          : ub1;
      Day            : ub1;
      Hour           : ub1;
      Min            : ub1;
      Sec            : ub1;
    end;

implementation

  function UB1MASK : longint;
      { return type might be wrong }
      begin
//         UB1MASK:=(1 shl (uword(CHAR_BIT)))-1;
      end;



end.
