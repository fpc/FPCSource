unit uw3103;

interface

const MaxArt = 2000;

type
   WordArr = array[1..MaxArt] of word;

var
   RefList  : array[1..MaxArt] of string;
   Theorems : array[- 1..MaxArt,1..2] of byte;

implementation

procedure P;
var  lThNbr : WordArr;
     sArr   : array[- 1..MaxArt] of Integer;
     pctTh  : array[- 1..MaxArt] of real;
     k   : integer;
     lThArr : array[- 1..MaxArt] of Integer;
 begin
    sArr[k] := k;
    if RefList[k] = 'A' then lThArr[- 1] := lThNbr[k];
    pctTh[k] := Theorems[k,1];
  end;

begin
end.
