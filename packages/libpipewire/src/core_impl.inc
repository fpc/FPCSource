{%MainUnit pipewire.pas}

function pw_core_add_listener(pw_core: Ppw_core; aListener: Pspa_hook; aEvents: Ppw_core_events; aData: Pointer) : cint;

var
  intf: Pspa_interface;
  F: Ppw_core_methods;

begin
  intf := Pspa_interface(pw_core);
  F := Ppw_core_methods(Intf^.cb.funcs);
  Result := F^.add_listener(Intf^.cb.Data, aListener, aEvents, aData);
end;



function pw_core_sync(pw_core: Ppw_core; aID, aSync: cint) : cint;

var
  intf: Pspa_interface;
  F: Ppw_core_methods;

begin
  intf := Pspa_interface(pw_core);
  F := Ppw_core_methods(intf^.cb.funcs);
  Result := f^.sync(intf^.cb.Data, aID, aSync);
end;

