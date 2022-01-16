{ %CPU=i388,x86_64 }
{$OPTIMIZATION ON}
{$FPUTYPE SSE3}

uses
  cpu;

var  map    : array [0..63,0..63,0..63] of smallint;

procedure makeMap( ) ;
var x,y,z,i : longword;
    yd,zd,th : single;
    begin
    // add random blocks to the map
    for x := 0 to 63 do begin
        for y := 0 to 63 do begin
             for z := 0 to 63 do begin
               yd := (y - 32.5) * 0.4;
               zd := (z - 32.5) * 0.4;
               map[z,y,x] := random( 16 );
               th := random;
              if th > sqrt( sqrt( yd * yd + zd * zd ) ) - 0.8 then
                 map[z,y,x] := 0;
              end;
        end;
    end;
end;

procedure init( );
begin
  makeMap( );
end;


begin
 if is_sse3_cpu then
   init ();
end.


