library some_plugin;

function get_plugin_info: PAnsiChar; export; cdecl;
begin
  get_plugin_info:= 'GLib2 Plugin-Test Plugin';
end;

exports
  get_plugin_info;

begin
end.
