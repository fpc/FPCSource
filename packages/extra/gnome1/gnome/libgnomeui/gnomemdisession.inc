{$IfDef read_interface}
type
   TGnomeMDIChildCreator = Function (const name : gchar) : TGnomeMDIChild; cdecl;

function gnome_mdi_restore_state(mdi:PGnomeMDI; section:Pgchar; child_create_func:TGnomeMDIChildCreator):gboolean;cdecl;external libgnomeuidll name 'gnome_mdi_restore_state';
procedure gnome_mdi_save_state(mdi:PGnomeMDI; section:Pgchar);cdecl;external libgnomeuidll name 'gnome_mdi_save_state';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
