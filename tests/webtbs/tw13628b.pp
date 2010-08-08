{ %needlibrary }

program loadmodule; 

uses
  dynlibs;

const
  {$ifdef unix}
  {$ifdef darwin}
  libname = './libtw13628a.dylib';
  {$else darwin}
  libname = './libtw13628a.so';
  {$endif darwin}
  {$endif unix}

  {$ifdef windows}
  libname = 'tw13628a.dll';
  {$endif windows}

var
  hdl: TLibHandle; 
begin
  hdl := loadlibrary(libname);
  if (hdl=nilhandle) then
    halt(1);
  if not UnloadLibrary(hdl) then
    halt(2);;
end.
