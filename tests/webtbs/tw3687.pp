{ Source provided for Free Pascal Bug Report 3687 }
{ Submitted by "marco" on  2005-02-24 }
{ e-mail:  }
unit tw3687;

{$WRITEABLECONST OFF}

interface

type
  PByte = ^Byte;
  PWord = ^Word;
  PLongWord = ^LongWord;
  T128BitDigest = record end;

implementation

{                                                                              }
{ MD5 hashing                                                                  }
{                                                                              }
const
  MD5Table_1 : Array [0..15] of LongWord = (
      $D76AA478, $E8C7B756, $242070DB, $C1BDCEEE,
      $F57C0FAF, $4787C62A, $A8304613, $FD469501,
      $698098D8, $8B44F7AF, $FFFF5BB1, $895CD7BE,
      $6B901122, $FD987193, $A679438E, $49B40821);

Procedure TransformMD5Buffer (var Digest : T128BitDigest);
var A, B, C, D : LongWord;
    P2          : PLongWord;
    I          : Integer;
    J          : Byte;
  Begin
    P2 := @MD5Table_1;
  End;
end.
