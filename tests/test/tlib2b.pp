{ %target=linux,android }
{ %needlibrary }
{ %delfiles=tlib2a }

uses dl;

var
   hdl : Pointer;

begin
   WriteLn('dlopen');
   hdl := dlopen('./libtlib2a.so', RTLD_LAZY);
   if hdl = nil then
      WriteLn(dlerror())
   else
   begin
      WriteLn('dlclose');
      dlclose(hdl);
   end;
   WriteLn('exit');
end.
