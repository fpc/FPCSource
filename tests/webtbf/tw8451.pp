{ %fail }
{$mode objfpc}
program bug8303;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type proptype1=record
                record_member:integer;
                end;

const records_array: array[0..3] of proptype1 = ((record_member:0),(record_member:0),(record_member:0),(record_member:0));
      array_idx=2;



type
              tsomeclass = class
                        procedure setprop1(p:proptype1);
                        property prop1: proptype1  read records_array[array_idx].record_member write setprop1;
                        end;


procedure tsomeclass.setprop1(p>proptype1);

begin

end;

begin
end.



