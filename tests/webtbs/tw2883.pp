{ Source provided for Free Pascal Bug Report 2883 }
{ Submitted by "Den Jean" on  2004-01-06 }
{ e-mail: Den.Jean@pandora.be }
unit tw2883;

{$mode objfpc}

interface
   type TRec = record
      Field1 : smallint;
      Field2 : smallint;
   end;

   function Func1 : integer;
   function Func2 (DefParam : integer = 1) : TRec;

Implementation

   function Func1 : integer;
   begin
   Result := Func2.Field1;
   end;

   function Func2 (DefParam : integer = 1) : TRec;
   begin
   Result.Field1 := DefParam;
   Result.Field2 := 2;
   end;

end.
