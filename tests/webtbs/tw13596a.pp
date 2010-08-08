{$inline on}

unit tw13596a;

interface

const
  LUT_SIZE = 512;
  LUT_MASK = 511;

procedure gluPerspective(fovy, aspect, zNear, zFar: single); inline;
procedure gluPerspectivef32(fovy: longint; aspect, zNear, zFar: longint); inline;
function mulf32(a, b: longint): longint; inline;
function floattof32(n: single): longint; inline;        

implementation

function floattof32(n: single): longint; inline;        
begin
  floattof32 := trunc((n) * (1 shl 12)); 
end;

function mulf32(a, b: longint): longint; inline;
var
  rslt: int64;
begin
        rslt := int64(a) * int64(b);
        mulf32 := longint(rslt shr 12);
end;

procedure gluPerspectivef32(fovy: longint; aspect, zNear, zFar: longint); inline;
var
  xmin, xmax, ymin, ymax: longint;
  TAN_bin: array [0..511] of smallint;
begin

  ymax := mulf32(zNear, TAN_bin[(fovy shr 1) and LUT_MASK]);
        ymin := -ymax;
        xmin := mulf32(ymin, aspect);
        xmax := mulf32(ymax, aspect);
        
//        glFrustumf32(xmin, xmax, ymin, ymax, zNear, zFar);

end;

procedure gluPerspective(fovy, aspect, zNear, zFar: single); inline;
begin
 gluPerspectivef32(trunc(fovy * LUT_SIZE / 360.0), floattof32(aspect), floattof32(zNear), floattof32(zFar));
end;


end.
