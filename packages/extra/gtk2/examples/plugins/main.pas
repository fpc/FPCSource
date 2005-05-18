program plugin_test;

uses
  glib2;

const
  PLUGIN_NAME = 'plugin';
  SYMBOL_NAME = 'get_plugin_info';

var
  module   : PGModule;
  func     : function : pgchar;
  id       : pgchar;
  filename : pgchar;

begin
  if not g_module_supported then
  begin
    g_error ('No GModule support on this platform.'#13#10);
    exit;
  end;
  filename := g_module_build_path ('.',PLUGIN_NAME);
  g_print ('Trying to locate module; using %s as filename'#13#10,
            [filename]);

  module := g_module_open (filename, G_MODULE_BIND_MASK);

  if module = NULL then
  begin
    g_error ('Couldn''t find Module %s!'#13#10, [PLUGIN_NAME]);
    exit;
  end;

  if not g_module_symbol (module, SYMBOL_NAME, @func) then
  begin
    g_error ('No symbol %s in %s found!'#13#10, [SYMBOL_NAME, PLUGIN_NAME]);
    g_module_close (module);
    exit;
  end;

  id := func();

  g_print ('Plugin defined itself as "%s"'#13#10, [id]);

  g_module_close (module);
end.
