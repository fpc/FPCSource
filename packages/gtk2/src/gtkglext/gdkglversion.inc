{ Original file is /usr/include/gtkglext-1.0/gdk/gdkglversion.h }

{ Comments for Pascal translation of gdkglext:
  I prefixed constants (describing header version) and
  macro (checking versus header version) with "HEADER_",
  since Pascal is not case sensitive.
}

{$ifdef read_interface_rest}

{*
 * Compile time version.
 *}
const
  HEADER_GDKGLEXT_MAJOR_VERSION = 1;
  HEADER_GDKGLEXT_MINOR_VERSION = 0;
  HEADER_GDKGLEXT_MICRO_VERSION = 6;
  HEADER_GDKGLEXT_INTERFACE_AGE = 4;
  HEADER_GDKGLEXT_BINARY_AGE    = 6;

{*
 * Check whether a GdkGLExt version equal to or greater than
 * major.minor.micro is present.
 *}
function HEADER_GDKGLEXT_CHECK_VERSION(major, minor, micro: guint): boolean;

{*
 * Library version.
 *}
var
  gdkglext_major_version: guint; cvar; external;
  gdkglext_minor_version: guint; cvar; external;
  gdkglext_micro_version: guint; cvar; external;
  gdkglext_interface_age: guint; cvar; external;
  gdkglext_binary_age   : guint; cvar; external;

{$endif read_interface_rest}

{$ifdef read_implementation}

function HEADER_GDKGLEXT_CHECK_VERSION(major, minor, micro: guint): boolean;
begin
 HEADER_GDKGLEXT_CHECK_VERSION :=
   (HEADER_GDKGLEXT_MAJOR_VERSION > major) or
   ( (HEADER_GDKGLEXT_MAJOR_VERSION = major) and (HEADER_GDKGLEXT_MINOR_VERSION > minor) ) or
   ( (HEADER_GDKGLEXT_MAJOR_VERSION = major) and (HEADER_GDKGLEXT_MINOR_VERSION = minor) and
     (HEADER_GDKGLEXT_MICRO_VERSION >= micro) );
end;

{$endif read_implementation}
