{$IfDef read_interface}
type
  TGConfError = (GCONF_ERROR_SUCCESS := 0,GCONF_ERROR_FAILED := 1,
     GCONF_ERROR_NO_SERVER := 2,GCONF_ERROR_NO_PERMISSION := 3,
     GCONF_ERROR_BAD_ADDRESS := 4,GCONF_ERROR_BAD_KEY := 5,
     GCONF_ERROR_PARSE_ERROR := 6,GCONF_ERROR_CORRUPT := 7,
     GCONF_ERROR_TYPE_MISMATCH := 8,GCONF_ERROR_IS_DIR := 9,
     GCONF_ERROR_IS_KEY := 10,GCONF_ERROR_OVERRIDDEN := 11,
     GCONF_ERROR_OAF_ERROR := 12,GCONF_ERROR_LOCAL_ENGINE := 13,
     GCONF_ERROR_LOCK_FAILED := 14,GCONF_ERROR_NO_WRITABLE_DATABASE := 15,
     GCONF_ERROR_IN_SHUTDOWN := 16);

function gconf_error_quark:TGQuark;cdecl;external gconfdll name 'gconf_error_quark';

function GCONF_ERROR : TGConfError;

{$EndIf read_interface}

{$Ifdef read_implementation}

function GCONF_ERROR : TGConfError;
begin
  GCONF_ERROR := TGConfError(gconf_error_quark);
end;

{$Endif read_implementation}
