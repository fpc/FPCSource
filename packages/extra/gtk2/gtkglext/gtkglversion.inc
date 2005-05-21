{ Original file is /usr/include/gtkglext-1.0/gtk/gtkglversion.h }

{ Comments for Pascal translation of gtkglext:
  I prefixed constants (describing header version) and
  macro (checking versus header version) with "HEADER_",
  since Pascal is not case sensitive.
}

{$ifdef read_interface_rest}

{*
 * Compile time version.
 *}
const
  HEADER_GTKGLEXT_MAJOR_VERSION = 1;
  HEADER_GTKGLEXT_MINOR_VERSION = 0;
  HEADER_GTKGLEXT_MICRO_VERSION = 6;
  HEADER_GTKGLEXT_INTERFACE_AGE = 4;
  HEADER_GTKGLEXT_BINARY_AGE    = 6;

{*
 * Check whether a GtkGLExt version equal to or greater than
 * major.minor.micro is present.
 *}
function HEADER_GTKGLEXT_CHECK_VERSION(major, minor, micro: guint): boolean;

{*
 * Library version.
 *}
var
  gtkglext_major_version: guint; cvar; external;
  gtkglext_minor_version: guint; cvar; external;
  gtkglext_micro_version: guint; cvar; external;
  gtkglext_interface_age: guint; cvar; external;
  gtkglext_binary_age   : guint; cvar; external;

{$endif read_interface_rest}

{$ifdef read_implementation}

function HEADER_GTKGLEXT_CHECK_VERSION(major, minor, micro: guint): boolean;
begin
 HEADER_GTKGLEXT_CHECK_VERSION :=
   (HEADER_GTKGLEXT_MAJOR_VERSION > major) or
   ( (HEADER_GTKGLEXT_MAJOR_VERSION = major) and (HEADER_GTKGLEXT_MINOR_VERSION > minor) ) or
   ( (HEADER_GTKGLEXT_MAJOR_VERSION = major) and (HEADER_GTKGLEXT_MINOR_VERSION = minor) and
     (HEADER_GTKGLEXT_MICRO_VERSION >= micro) );
end;

{$endif read_implementation}
