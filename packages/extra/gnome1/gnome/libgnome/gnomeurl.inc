{$IfDef read_interface}

procedure gnome_url_show(url:Pchar);cdecl;external libgnomedll name 'gnome_url_show';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
