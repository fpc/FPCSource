{$mode objfpc}
type
   float = double;


function ConvertRealToPixel(Axis     : integer;
                            HelpReal : real) : real;

   begin   { function ConvertRealToPixel }
      ConvertRealToPixel := HelpReal;
   end;    { function ConvertRealToPixel }


var
   HelpFloat1,HelpFloat2,HelpFloat3  : float;
   SegmentStartPos        : float;
   SegmentLength          : float;


begin
   SegmentStartPos := 0.5;
   SegmentLength := 0.5;
   HelpFloat1 := SegmentStartPos - SegmentLength / 2;
   HelpFloat2 := ConvertRealToPixel(1,HelpFloat1);
   writeln('Function result = ',HelpFloat2,'  This is OK');

   HelpFloat3 := ConvertRealToPixel(1,SegmentStartPos - SegmentLength / 2);
   writeln('Function result = ',HelpFloat3,'  THIS IS WRONG !');
   if HelpFloat2<>HelpFloat3 then
    begin
      Writeln('ERROR!');
      Halt(1);
    end;
end.
