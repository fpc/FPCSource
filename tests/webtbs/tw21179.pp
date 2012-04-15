{ %NORUN }

{$mode OBJFPC}
{$modeswitch ADVANCEDRECORDS}
program generic_record_op_bug;

type generic GVec3<T> = record
  D : Byte;
  class operator *( const A, B : GVec3 ) : GVec3;
  class operator *( const A : GVec3; Scalar : T ) : GVec3;
end;

class operator GVec3.*( const A, B : GVec3 ) : GVec3;
begin
end;

class operator GVec3.*( const A : GVec3; Scalar : T ) : GVec3;
begin
end;

begin
end.
