{ %VERSION=1.1 }
{$ifdef fpc}{$mode objfpc}{$endif}
{$J+}

type
   imyinterface = interface
      // this program isn't supposed to run so the guid doesn't matter }
      ['{00000000-0000-0000-0000-000000000000}']
      procedure p;
   end;

const
   iid_imyinterface = imyinterface;
   iid2 : tguid = '{00000000-0000-0000-0000-000000000000}';

var
   g : tguid;
begin
   g:=imyinterface;
   g:=iid_imyinterface;
   g:=iid2;
   iid2:=iid_imyinterface;
end.
