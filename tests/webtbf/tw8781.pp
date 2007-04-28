{ %fail }
program packer;
type
    HeaderStr=record
    Name:Array[1..20] of char;
    Objects:byte;
    FirstObject:longint;
    end;

    ObjStr=record
    Name:array[1..8] of char;
    Size:longint;
    Width:integer;
    Height:integer;
    ObjType:byte;
    NextObject:longint
    end;

function openfile(filepath:string):file;
begin
end;

begin

end.
