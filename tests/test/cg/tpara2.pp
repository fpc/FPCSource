type
  sint16 = smallint;
  uint16 = word;

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

        RGBColor = record
                red:                                    UInt16;                 
                                                { magnitude of red component }
                green:                                  UInt16;                 
                                                { magnitude of green component }
                blue:                                   UInt16;                 
                                                { magnitude of blue component }
        end;

function test(r: Rect; c1, c2: RGBColor): Rect;
begin
  test:= r;
end;

var 
  r: rect;
  c1,c2: rgbcolor;
begin
  test(r,c1,c2);
end.
