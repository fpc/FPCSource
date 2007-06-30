{$mode macpas}

program packed_offset_bug;

type
	SInt16 = integer;
	Point = record
		case SInt16 of
		0: (
			v: SInt16;
			h: SInt16;
		   );
		1: (
			vh: array [0..1] of SInt16;
			);
	end;
	PointPtr = ^Point;
	Rect = record
		case SInt16 of
		0: (
			top: SInt16;
			left: SInt16;
			bottom: SInt16;
			right: SInt16;
		   );
		1: (
			topLeft: Point;
			botRight: Point;
		   );
	end;
	RectPtr = ^Rect;

var
	gMeasStrs: packed array[1..64] of string[4];
	gMeasStrRects: array[1..64] of Rect;

var
  i,j,k: longint;
begin
	writeln( 'SizeOf gMeasStrs)        = ', SizeOf( gMeasStrs));
	writeln( 'SizeOf gMeasStrRects)    = ', SizeOf( gMeasStrRects));
	writeln( 'Offset gMeasStrs[  1   ] = ', ptruint( @gMeasStrs[      1]) -ptruint( @gMeasStrs));
	writeln( 'Offset gMeasStrs[ 64, 4] = ', ptruint( @gMeasStrs[ 64,  4]) - ptruint( @gMeasStrs));
	writeln( 'Offset gMeasStrRects     = ', ptruint( @gMeasStrRects)      - ptruint( @gMeasStrs));
        i:=1;
        j:=64;
        k:=4;
        if (SizeOf( gMeasStrs) <> 320) or
           (SizeOf( gMeasStrRects) <> 512) or
           (ptruint( @gMeasStrs[      1]) - ptruint( @gMeasStrs) <> 0) or
           (ptruint( @gMeasStrs[ 64,  4])- ptruint( @gMeasStrs)<>319) or
           (ptruint( @gMeasStrs[      i]) - ptruint( @gMeasStrs) <> 0) or
           (ptruint( @gMeasStrs[ j,  k])- ptruint( @gMeasStrs)<>319) then
          halt(1);
end.

