// included by libglade.pas

{ left for compatibility.  Libglade will now automatically initialise  }

procedure glade_init; cdecl; external LibGladeLib;
{ handle dynamic loading of libglade extensions  }
procedure glade_require(TheLibrary:Pgchar); cdecl; external LibGladeLib;
procedure glade_provide(TheLibrary:Pgchar); cdecl; external LibGladeLib;

{$ifndef LIBGLADE_DISABLE_DEPRECATED}
{ libglade now loads modules based on information
   in the glade file - these are deprecated  }
procedure glade_gnome_init;
procedure glade_bonobo_init;
{$endif}

// included by libglade.pas

