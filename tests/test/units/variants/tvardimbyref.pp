{ %skiptarget=qnx,wdosx,palmos,macosclassic,embedded,zxspectrum,msxdos,amstradcpc,ps1 }
{ %skipcpu=jvm }
{ VarArrayDimCount of a variant array held by reference (varArray or
  varByRef) counts the dimensions of the array the reference points to. The
  references are made by hand, and emptied again, because copying or clearing
  one is not the same on every target }
program tvardimbyref;

{$mode objfpc}

uses
  variants;

procedure Check(got, expected, code: longint);
begin
  if got<>expected then
    begin
      writeln('error ',code,': ',got,', expected ',expected);
      halt(code);
    end;
end;

procedure MakeRef(var ref: variant; var arr: variant);
begin
  TVarData(ref).vType:=TVarData(arr).vType or varByRef;
  TVarData(ref).vPointer:=@TVarData(arr).vArray;
end;

var
  one, two, ref1, ref2, outer: variant;
begin
  one:=VarArrayCreate([0,3],varByte);
  two:=VarArrayCreate([0,1,5,7],varInteger);
  Check(VarArrayDimCount(one),1,1);
  Check(VarArrayDimCount(two),2,2);
  MakeRef(ref1,one);
  MakeRef(ref2,two);
  Check(VarArrayDimCount(ref1),1,4);
  Check(VarArrayDimCount(ref2),2,5);
  { the bounds already follow the reference }
  Check(VarArrayLowBound(ref2,2),5,6);
  Check(VarArrayHighBound(ref2,2),7,7);
  { a reference to a variant holding the reference }
  TVarData(outer).vType:=varVariant or varByRef;
  TVarData(outer).vPointer:=@ref2;
  Check(VarArrayDimCount(outer),2,8);
  TVarData(outer).vType:=varEmpty;
  TVarData(ref2).vType:=varEmpty;
  TVarData(ref1).vType:=varEmpty;
  Check(VarArrayDimCount(Null),0,9);
  writeln('ok');
end.
