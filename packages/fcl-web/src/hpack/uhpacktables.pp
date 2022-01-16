unit uhpacktables;

interface

const

  HPACK_HUFFMAN_CODES_LENGTH=257;

  HPackHuffmanCodes: array [0..HPACK_HUFFMAN_CODES_LENGTH-1] of DWORD =(
       $1ff8,  $7fffd8,    $fffffe2,    $fffffe3,    $fffffe4,    $fffffe5,    $fffffe6,    $fffffe7,
    $fffffe8,  $ffffea,   $3ffffffc,    $fffffe9,    $fffffea,   $3ffffffd,    $fffffeb,    $fffffec,
    $fffffed, $fffffee,    $fffffef,    $ffffff0,    $ffffff1,    $ffffff2,   $3ffffffe,    $ffffff3,
    $ffffff4, $ffffff5,    $ffffff6,    $ffffff7,    $ffffff8,    $ffffff9,    $ffffffa,    $ffffffb,
         $14,     $3f8,        $3f9,        $ffa,       $1ff9,         $15,         $f8,        $7fa,
        $3fa,     $3fb,         $f9,        $7fb,         $fa,         $16,         $17,         $18,
          $0,       $1,          $2,         $19,         $1a,         $1b,         $1c,         $1d,
         $1e,      $1f,         $5c,         $fb,       $7ffc,         $20,        $ffb,        $3fc,
       $1ffa,      $21,         $5d,         $5e,         $5f,         $60,         $61,         $62,
         $63,      $64,         $65,         $66,         $67,         $68,         $69,         $6a,
         $6b,      $6c,         $6d,         $6e,         $6f,         $70,         $71,         $72,
         $fc,      $73,         $fd,       $1ffb,      $7fff0,       $1ffc,       $3ffc,         $22,
       $7ffd,       $3,         $23,          $4,         $24,          $5,         $25,         $26,
         $27,       $6,         $74,         $75,         $28,         $29,         $2a,          $7,
         $2b,      $76,         $2c,          $8,          $9,         $2d,         $77,         $78,
         $79,      $7a,         $7b,       $7ffe,        $7fc,       $3ffd,       $1ffd,    $ffffffc,
      $fffe6,  $3fffd2,      $fffe7,      $fffe8,     $3fffd3,     $3fffd4,     $3fffd5,     $7fffd9,
     $3fffd6,  $7fffda,     $7fffdb,     $7fffdc,     $7fffdd,     $7fffde,     $ffffeb,     $7fffdf,
     $ffffec,  $ffffed,     $3fffd7,     $7fffe0,     $ffffee,     $7fffe1,     $7fffe2,     $7fffe3,
     $7fffe4,  $1fffdc,     $3fffd8,     $7fffe5,     $3fffd9,     $7fffe6,     $7fffe7,     $ffffef,
     $3fffda,  $1fffdd,      $fffe9,     $3fffdb,     $3fffdc,     $7fffe8,     $7fffe9,     $1fffde,
     $7fffea,  $3fffdd,     $3fffde,     $fffff0,     $1fffdf,     $3fffdf,     $7fffeb,     $7fffec,
     $1fffe0,  $1fffe1,     $3fffe0,     $1fffe2,     $7fffed,     $3fffe1,     $7fffee,     $7fffef,
      $fffea,  $3fffe2,     $3fffe3,     $3fffe4,     $7ffff0,     $3fffe5,     $3fffe6,     $7ffff1,
    $3ffffe0, $3ffffe1,      $fffeb,      $7fff1,     $3fffe7,     $7ffff2,     $3fffe8,    $1ffffec,
    $3ffffe2, $3ffffe3,    $3ffffe4,    $7ffffde,    $7ffffdf,    $3ffffe5,     $fffff1,    $1ffffed,
      $7fff2,  $1fffe3,    $3ffffe6,    $7ffffe0,    $7ffffe1,    $3ffffe7,    $7ffffe2,     $fffff2,
     $1fffe4,  $1fffe5,    $3ffffe8,    $3ffffe9,    $ffffffd,    $7ffffe3,    $7ffffe4,    $7ffffe5,
      $fffec,  $fffff3,      $fffed,     $1fffe6,     $3fffe9,     $1fffe7,     $1fffe8,     $7ffff3,
     $3fffea,  $3fffeb,    $1ffffee,    $1ffffef,     $fffff4,     $fffff5,    $3ffffea,     $7ffff4,
    $3ffffeb, $7ffffe6,    $3ffffec,    $3ffffed,    $7ffffe7,    $7ffffe8,    $7ffffe9,    $7ffffea,
    $7ffffeb, $ffffffe,    $7ffffec,    $7ffffed,    $7ffffee,    $7ffffef,    $7fffff0,    $3ffffee,
   $3fffffff // EOS
  );

  HPackHuffmanCodeLength: array [0..256] of byte =(
     13, 23, 28, 28, 28, 28, 28, 28, 28, 24, 30, 28, 28, 30, 28, 28,
     28, 28, 28, 28, 28, 28, 30, 28, 28, 28, 28, 28, 28, 28, 28, 28,
      6, 10, 10, 12, 13,  6,  8, 11, 10, 10,  8, 11,  8,  6,  6,  6,
      5,  5,  5,  6,  6,  6,  6,  6,  6,  6,  7,  8, 15,  6, 12, 10,
     13,  6,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
      7,  7,  7,  7,  7,  7,  7,  7,  8,  7,  8, 13, 19, 13, 14,  6,
     15,  5,  6,  5,  6,  5,  6,  6,  6,  5,  7,  7,  6,  6,  6,  5,
      6,  7,  6,  5,  5,  6,  7,  7,  7,  7,  7, 15, 11, 14, 13, 28,
     20, 22, 20, 20, 22, 22, 22, 23, 22, 23, 23, 23, 23, 23, 24, 23,
     24, 24, 22, 23, 24, 23, 23, 23, 23, 21, 22, 23, 22, 23, 23, 24,
     22, 21, 20, 22, 22, 23, 23, 21, 23, 22, 22, 24, 21, 22, 23, 23,
     21, 21, 22, 21, 23, 22, 23, 23, 20, 22, 22, 22, 23, 22, 22, 23,
     26, 26, 20, 19, 22, 23, 22, 25, 26, 26, 26, 27, 27, 26, 24, 25,
     19, 21, 26, 27, 27, 26, 27, 24, 21, 21, 26, 26, 28, 27, 27, 27,
     20, 24, 20, 21, 22, 21, 21, 23, 22, 22, 25, 25, 24, 24, 26, 23,
     26, 27, 26, 26, 27, 27, 27, 27, 27, 28, 27, 27, 27, 27, 27, 26,
     30 // EOS
  );

  HPACK_HUFFMAN_EOS: integer = 256;

  HPACK_HEADER_ENTRY_OVERHEAD = 32;

type
  THPackIndexType=(
    eHPackINCREMENTAL, // Section 6.2.1. Literal Header Field with Incremental Indexing
    eHPackNONE,        // Section 6.2.2. Literal Header Field without Indexing
    eHPackNEVER        // Section 6.2.3. Literal Header Field never Indexed
  );

  THPackState =(
    READ_HEADER_REPRESENTATION,
    READ_MAX_DYNAMIC_TABLE_SIZE,
    READ_INDEXED_HEADER,
    READ_INDEXED_HEADER_NAME,
    READ_LITERAL_HEADER_NAME_LENGTH_PREFIX,
    READ_LITERAL_HEADER_NAME_LENGTH,
    READ_LITERAL_HEADER_NAME,
    SKIP_LITERAL_HEADER_NAME,
    READ_LITERAL_HEADER_VALUE_LENGTH_PREFIX,
    READ_LITERAL_HEADER_VALUE_LENGTH,
    READ_LITERAL_HEADER_VALUE,
    SKIP_LITERAL_HEADER_VALUE
  );

implementation

end.

