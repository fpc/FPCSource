{$mode objfpc}

uses
  sysutils;

type
{$interfaces corba}
  icorbainterface1 = interface
    ['STRING_UID']
  end;

  icorbainterface2 = interface
    ['{95B5633F-38A8-4D5F-A7FA-A2EA2664C670}']
  end;

{$interfaces com}
  icominterface = interface
    ['{04B6AB72-8F86-45F8-8D49-393E799F51A8}']
  end;

const
  iid_corba1: shortstring = icorbainterface1;
  iid_corba2: shortstring = icorbainterface2;
  iid_com: tguid = icominterface;
  iid_comref: tguid = '{04B6AB72-8F86-45F8-8D49-393E799F51A8}';

begin
  if iid_corba1 <> 'STRING_UID' then
    halt(1);

  if iid_corba2 <> '{95B5633F-38A8-4D5F-A7FA-A2EA2664C670}' then
    halt(1);

  if not IsEqualGUID(iid_com, iid_comref) then
    halt(1);
end.
