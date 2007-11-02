{$mode objfpc}

type
    ta = byte;
    pa = ^byte;
    panother = pa;

type
    tRec = record
        a : ta;
        p : panother;
    end;

    tNestedObj = object
        arec : tRec;
        p : panother;
    end;

    tChildObj = object(tNestedObj)
        dummy : byte;
    end;

type

    tObj = object
        arec : tRec;
        aobj : tNestedObj;
        achild : tChildObj;

        property a_rec : byte read arec.a;
        property a_obj : byte read aobj.arec.a;
        property p_obj : pa read aobj.p;
        property dummy_child : byte read achild.dummy;
        property a_child : byte read achild.arec.a;
{ Error: Unknown record field identifier "arec" ^
            Error: Unknown record field identifier "a" ^
}
        property p_child : pa read achild.p;
{ Error: Unknown record field identifier "p" ^
}
    end;

var
  Obj : tObj;

begin
  Obj.achild.p:=panother($deadbeef);
  if Obj.p_child<>panother($deadbeef) then
    halt(1);
  writeln('ok');
end.
