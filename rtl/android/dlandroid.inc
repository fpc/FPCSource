
// dladdr() function is available in Android API 8+.
// Get dladdr() dynamically at run-time to support older versions of Android.

function dladdr_stub(Lib: pointer; info: Pdl_info): Longint; cdecl;
begin
  dladdr_stub:=0;
  FillChar(info^, SizeOf(info^), 0);
end;

var
  _dladdr: function(lib: pointer; info: Pdl_info): longint; cdecl;

function dladdr(Lib: pointer; info: Pdl_info): Longint; cdecl;
var
  libdl: pointer;
begin
  if not assigned(_dladdr) then
    begin
      libdl:=dlopen('libdl.so',RTLD_LAZY);
      if assigned(libdl) then
        pointer(_dladdr):=dlsym(libdl,'dladdr');
      if not assigned(_dladdr) then
        _dladdr:=@dladdr_stub;
      { can't be the last reference that causes it to be unloaded, since
        most functions from this unit come from it }
      if assigned(libdl) then
        dlclose(libdl);
    end;
  dladdr:=_dladdr(Lib,info);
end;
