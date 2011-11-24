{%norun}
program project1;
{$mode delphi}
uses
  SysUtils, Math;


type
  TVector3 = record
    public
      // constructor
//      constructor create(const x: Single; const y: Single; const z: Single);

      // overloaded operators
      class operator negative(const a : TVector3)             : TVector3; inline;

      class operator add(const a, b : TVector3)               : TVector3; inline;
      class operator subtract(const a, b : TVector3)          : TVector3; inline;
      class operator multiply(const a, b : TVector3)          : TVector3; inline;
      class operator multiply(const a : single; b : TVector3) : TVector3; inline;
      class operator multiply(const a : TVector3; b : single) : TVector3; inline;

      class operator equal(const a, b : TVector3)             : boolean; inline;


      // methods
      function normalize  : TVector3; inline;                 // normalized vector
      function length     : single;   inline;                 // length of a vector

      function dot(const v : TVector3)   : single;   inline;  // dot product
      function cross(const v : TVector3) : TVector3; inline;  // cross product
	  // variant record part
      case Boolean of
        TRUE:        (x : single;
                      y : single;
                      z : single; );
        FALSE:       (e : array[0..2] of single; );
  end;

// constants
const
  X_AXIS : TVector3 = (x: 1.0; y: 1.0; z: 1.0);
  Y_AXIS : TVector3 = (x: 0.0; y: 1.0; z: 0.0);
  Z_AXIS : TVector3 = (x: 0.0; y: 0.0; z: 1.0);

  ZERO   : TVector3 = (x: 0.0; y: 0.0; z: 0.0);
  ONE    : TVector3 = (x: 1.0; y: 1.0; z: 1.0);

  JITTERED_X_AXIS : TVector3 = (x: 1.0;    y: 0.0072; z: 0.0034);
  JITTERED_Y_AXIS : TVector3 = (x: 0.0072; y: 1.0;    z: 0.0034);
  JITTERED_Z_AXIS : TVector3 = (x: 0.0034; y: 0.0072; z: 1.0);

// constructor
{
constructor TVector3.create(const x: Single; const y: Single; const z: Single);
begin
  self.x := x;
  self.y := y;
  self.z := z;
end;
}
// overloaded operators
class operator TVector3.negative(const a : TVector3) : TVector3;
begin
  Result.x := -a.x;
  Result.y := -a.y;
  Result.z := -a.z;
end;


class operator TVector3.add(const a, b : TVector3) : TVector3;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
  Result.z := a.z + b.z;
end;


class operator TVector3.subtract(const a, b : TVector3) : TVector3;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
  Result.z := a.z - b.z;
end;


class operator TVector3.multiply(const a, b : TVector3) : TVector3;
begin
  Result.x := a.x * b.x;
  Result.y := a.y * b.y;
  Result.z := a.z * b.z;
end;


class operator TVector3.multiply(const a : single; b : TVector3) : TVector3;
begin
  Result.x := a * b.x;
  Result.y := a * b.y;
  Result.z := a * b.z;
end;


class operator TVector3.multiply(const a : TVector3; b : single) : TVector3;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
  Result.z := a.z * b;
end;


class operator TVector3.equal(const a, b : TVector3) : boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y) and (a.z = b.z);
end;


// methods
function TVector3.normalize : TVector3;
var
  invLen : single;
begin
  invLen := 1.0 / sqrt(x * x + y * y + z * z);

  Result.x := x * invLen;
  Result.y := y * invLen;
  Result.z := z * invLen;
end;


function TVector3.length : single;
begin
  Result := sqrt(x * x + y * y + z * z);
end;


function TVector3.dot(const v : TVector3) : single;
begin
  Result := x * v.x + y * v.y + z * v.z;
end;


function TVector3.cross(const v : TVector3) : TVector3;
begin
  Result.x := y * v.z - z * v.y;
  Result.y := z * v.x - x * v.z;
  Result.z := x * v.y - y * v.x;
end;


// functions
function init(vx, vy, vz : single) : TVector3;
begin
  Result.x := vx;
  Result.y := vy;
  Result.z := vz;
end;


function unitize(const v : TVector3)  : TVector3;
var
  invLen : single;
begin
  invLen := 1.0 / sqrt(v.x * v.x + v.y * v.y + v.z * v.z);

  Result.x := v.x * invLen;
  Result.y := v.y * invLen;
  Result.z := v.z * invLen;
end;


function dot(const a, b : TVector3)   : single;
begin
  Result := a.x * b.x + a.y * b.y + a.z * b.z;
end;


function cross(const a, b : TVector3) : TVector3;
begin
  Result.x := a.y * b.z - a.z * b.y;
  Result.y := a.z * b.x - a.x * b.z;
  Result.z := a.x * b.y - a.y * b.x;
end;


// output functions
function str(v : TVector3) : string; overload;
begin
  Result := '[ '
            + FloatToStr(v.x) + ', '
            + FloatToStr(v.y) + ', '
            + FloatToStr(v.z)
            + '] ';
end;

begin
end.

