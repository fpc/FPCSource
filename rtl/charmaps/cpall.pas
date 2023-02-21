{$IFNDEF FPC_DOTTEDUNITS}
unit cpall;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  { cyrillic code pages }
  System.CodePages.CP1251,System.CodePages.CP855,System.CodePages.CP866,System.CodePages.CP8859_5,System.CodePages.CP3021,System.CodePages.CPkoi8_r,System.CodePages.CPkoi8_u,
  { greek code pages }
  System.CodePages.CP1253,System.CodePages.CP737,System.CodePages.CP869,System.CodePages.CP8859_7,
  { other code pages }
  System.CodePages.CP8859_1,System.CodePages.CP850,System.CodePages.CP437,System.CodePages.CP1252,System.CodePages.CP646,System.CodePages.CP775,
  System.CodePages.CP874,System.CodePages.CP856,System.CodePages.CP857,System.CodePages.CP860,System.CodePages.CP861,System.CodePages.CP862,System.CodePages.CP863,System.CodePages.CP864,System.CodePages.CP865,
{$ifndef cpu16}
  { these are too big for a 16-bit System.CPU }
  {  cp932,cp936,cp949,cp950, -> to rtl-unicode}
{$endif not cpu16}
  System.CodePages.CP1250,System.CodePages.CP1254,System.CodePages.CP1255,System.CodePages.CP1256,System.CodePages.CP1257,System.CodePages.CP1258,System.CodePages.CP852,System.CodePages.CP8859_2,System.CodePages.CP8859_3,System.CodePages.CP8859_4,
  System.CodePages.CP8859_6,System.CodePages.CP8859_8,System.CodePages.CP8859_9,System.CodePages.CP8859_10,System.CodePages.CP8859_11,System.CodePages.CP8859_13,System.CodePages.CP8859_14,System.CodePages.CP8859_15,
  System.CodePages.CP8859_16;
{$ELSE FPC_DOTTEDUNITS}
uses
  { cyrillic code pages }
  cp1251,cp855,cp866,cp8859_5,cp3021,cpkoi8_r,cpkoi8_u,
  { greek code pages }
  cp1253,cp737,cp869,cp8859_7,
  { other code pages }
  cp8859_1,cp850,cp437,cp1252,cp646,cp775,
  cp874,cp856,cp857,cp860,cp861,cp862,cp863,cp864,cp865,
{$ifndef cpu16}
  { these are too big for a 16-bit CPU }
  {  cp932,cp936,cp949,cp950, -> to rtl-unicode}
{$endif not cpu16}
  cp1250,cp1254,cp1255,cp1256,cp1257,cp1258,cp852,cp8859_2,cp8859_3,cp8859_4,
  cp8859_6,cp8859_8,cp8859_9,cp8859_10,cp8859_11,cp8859_13,cp8859_14,cp8859_15,
  cp8859_16;
{$ENDIF FPC_DOTTEDUNITS}


implementation

end.
