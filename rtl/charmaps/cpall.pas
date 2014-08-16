unit cpall;

interface

uses
  { cyrillic code pages }
  cp1251,cp866,cp8859_5,
  { greek code page }
  cp1253,
  { other code pages }
  cp8859_1,cp850,cp437,cp1252,cp646,
  cp874, cp856,
{$ifndef cpu16}
  { these are too big for a 16-bit CPU }
  {  cp932,cp936,cp949,cp950, -> to rtl-unicode}
{$endif not cpu16}
  cp1250,cp1254,cp1255,cp1256,cp1257,cp1258,cp852,cp8859_2;


implementation

end.
