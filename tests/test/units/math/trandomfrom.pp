{ RandomFrom overloads for Single, Double, Extended, Integer and Int64 arrays, issue #41975 }
program trandomfrom;

{$mode objfpc}{$h+}

uses
  Math;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


var
  lSingles: array[0..2] of Single = (1.5, 2.5, 3.5);
  lDoubles: array[0..2] of Double = (1.25, 2.25, 3.25);
  lExtendeds: array[0..2] of Extended = (1.75, 2.75, 3.75);
  lIntegers: array[0..2] of Integer = (10, 20, 30);
  lInt64s: array[0..2] of Int64 = (100, 200, 300);
  lSeen: array[0..2] of Boolean;
  lSingle: Single;
  lDouble: Double;
  lExtended: Extended;
  i, j: Integer;

begin
  RandSeed:=41975;
  FillChar(lSeen,SizeOf(lSeen),0);
  for i:=1 to 200 do
    begin
    lSingle:=RandomFrom(lSingles);
    j:=0;
    while (j<=High(lSingles)) and (lSingles[j]<>lSingle) do
      Inc(j);
    Check(1,j<=High(lSingles),'RandomFrom(array of Single) returns an element');
    lSeen[j]:=True;
    end;
  Check(2,lSeen[0] and lSeen[1] and lSeen[2],'RandomFrom(array of Single) returns every element');
  for i:=1 to 50 do
    begin
    lDouble:=RandomFrom(lDoubles);
    Check(3,(lDouble=1.25) or (lDouble=2.25) or (lDouble=3.25),'RandomFrom(array of Double) returns an element');
    lExtended:=RandomFrom(lExtendeds);
    Check(4,(lExtended=1.75) or (lExtended=2.75) or (lExtended=3.75),'RandomFrom(array of Extended) returns an element');
    j:=RandomFrom(lIntegers);
    Check(5,(j=10) or (j=20) or (j=30),'RandomFrom(array of Integer) returns an element');
    Check(6,RandomFrom(lInt64s) mod 100=0,'RandomFrom(array of Int64) returns an element');
    end;
  Check(10,SizeOf(RandomFrom(lSingles))=SizeOf(Single),'RandomFrom(array of Single) returns a Single');
  Check(11,SizeOf(RandomFrom(lDoubles))=SizeOf(Double),'RandomFrom(array of Double) returns a Double');
  Check(12,SizeOf(RandomFrom(lExtendeds))=SizeOf(Extended),'RandomFrom(array of Extended) returns an Extended');
  Check(13,RandomFrom([42.5])=42.5,'RandomFrom of a one-element array constructor');
  writeln('ok');
end.
