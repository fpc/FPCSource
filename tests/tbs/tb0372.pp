{ %VERSION=1.1 }
type
   imyinterface = interface
      // this program isn't supposed to run so the guid doesn't matter }
      ['{00000000-0000-0000-0000-000000000000}']
      procedure p;
   end;

const
   iid_imyinterface = imyinterface;

var
   g : tguid;

begin
   g:=imyinterface;
end.
