{ %CPU=i386,powerpc,arm,sparc,mips }
{ %fail }

{ test should fail on 32 bit targets but not crash }

{$MODE DELPHI}  // (1) _NOT_ using delphi mode works
unit tb0254a;

{==============================================================================}
interface

type
  pSExp = pointer;
  aSExpArr = array[0..((MaxInt div SizeOf(pSExp)) - 1)] of pSExp;
  // aSExpArr = array[0..((MaxInt div 2*SizeOf(pSExp)) - 1)] of pSExp;
  // (2) using a _shorter_ array, eg. ^^ works on Ubuntu (Mac still Error)

  pFoo = ^aFoo;
  aFoo = record
    rec: aSExpArr;
  end;

function ahoppla(_x: pFoo): aSExpArr;

{==============================================================================}
implementation

type
  pData = ^aData;
  aData = record
    offset: array[1..24] of byte;  // (3) uncommenting offset works
    SExpArr: aSExpArr;
  end;

function ahoppla(_x: pFoo): aSExpArr;
  begin
    ahoppla:= pData(_x)^.SExpArr;
    // ahoppla:= _x.rec;           // (4) _not_ casting works on Ubuntu (Mac error)
  end;

end {a}.
