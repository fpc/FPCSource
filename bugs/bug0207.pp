{$mode delphi}

type
   tmyobject = class(tobject)
	procedure free;
    end;

   procedure tmyobject.free;
     begin
        if self<>nil then
          destroy;
     end; 

   var t : tmyobject;
begin
   t:=tmyobject.create;
   t.destroy;
   while true do tmyobject.create.free;
end.
