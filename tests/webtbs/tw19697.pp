{ %NORUN }

program tw19697;

{$mode objfpc}{$H+}

uses
  uw19697;

type
  TSpecialisedClass = specialize TGenericClass<Integer>;

begin
  TSpecialisedClass.Init;
end.

