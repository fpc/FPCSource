{ %NORUN }

program tb0585;

{$mode objfpc}

type
  tobjectoptions = set of (oo_is_external, oo_is_forward, oo_is_formal);

  tobjectdef = class
    objectoptions: tobjectoptions;
  end;

procedure finalize_class_external_status(od: tobjectdef);
  begin
    if  [oo_is_external,oo_is_forward] <= od.objectoptions then
      begin
        { formal definition: x = objcclass external; }
        exclude(od.objectoptions,oo_is_forward);
        include(od.objectoptions,oo_is_formal);
      end;
  end;


begin

end.
