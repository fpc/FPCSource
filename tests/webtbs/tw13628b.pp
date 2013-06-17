{ %needlibrary }
{ %delfiles=tw13628a }

program loadmodule;

uses
  popuperr,
  dynlibs;

const
  {$ifdef unix}
  libname = './libtw13628a.'+SharedSuffix;
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
