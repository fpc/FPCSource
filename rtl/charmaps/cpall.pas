unit cpall;

interface

uses
  { cyrillic code pages }
  cp1251,cp855,cp866,cp8859_5,
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


implementation

end.
