{ original: peter5c.pas from the GNU Pascal testsuite }

{$mode macpas}

program peter5c(output);

   type
     ObjectA = object
       procedure Doit;
     end;
     ObjectB = object
       obj: ObjectA;
       function GetA: ObjectA;
     end;

var
   ok: boolean;

   procedure ObjectA.Doit;
   begin
     WriteLn( 'OK' );
     ok := true;
   end;

   function ObjectB.GetA: ObjectA;
   begin
     return obj;
   end;

var
   a: ObjectA;
   b: ObjectB;
begin
   New(a);
   New(b);
   b.obj := a;
   b.GetA.Doit;
   if not ok then
     halt(1);
end.

