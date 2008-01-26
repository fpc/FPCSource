// included by glib2.pp

{$IFDEF read_forward_definitions}
  PGQuark = ^TGQuark;
  TGQuark = guint32;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{ Quarks (string<->id association)  }

function g_quark_try_string(_string:Pgchar):TGQuark;cdecl;external gliblib name 'g_quark_try_string';

function g_quark_from_static_string(_string:Pgchar):TGQuark;cdecl;external gliblib name 'g_quark_from_static_string';

function g_quark_from_string(_string:Pgchar):TGQuark;cdecl;external gliblib name 'g_quark_from_string';

function g_quark_to_string  (quark: TGQuark): Pgchar;cdecl;external gliblib name 'g_quark_to_string';
{$ENDIF read_interface_rest}

