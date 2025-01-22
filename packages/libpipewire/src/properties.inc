
type
  Ppw_properties = ^Tpw_properties;

  Tpw_properties = record
    dict: Tspa_dict;
    flags: Tuint32_t;
  end;


function pw_properties_new(key: PChar; args: array of const) : Ppw_properties; cdecl; external libpipewirename Name 'pw_properties_new';

function pw_properties_new(key: PChar) : Ppw_properties; cdecl; external libpipewirename Name 'pw_properties_new';

function pw_properties_new_dict(dict: Pspa_dict) : Ppw_properties; cdecl; external libpipewirename Name 'pw_properties_new_dict';

function pw_properties_new_string(args: PChar) : Ppw_properties; cdecl; external libpipewirename Name 'pw_properties_new_string';

function pw_properties_copy(properties: Ppw_properties) : Ppw_properties; cdecl; external libpipewirename Name 'pw_properties_copy';

function pw_properties_update_keys(props: Ppw_properties; dict: Pspa_dict; keys: PPchar) : longint; cdecl; external libpipewirename Name 'pw_properties_update_keys';

function pw_properties_update_ignore(props: Ppw_properties; dict: Pspa_dict; ignore: PPchar) : longint; cdecl; external libpipewirename Name 'pw_properties_update_ignore';

function pw_properties_update(props: Ppw_properties; dict: Pspa_dict) : longint; cdecl; external libpipewirename Name 'pw_properties_update';

function pw_properties_update_string(props: Ppw_properties; str: PChar; size: Tsize_t) : longint; cdecl; external libpipewirename Name 'pw_properties_update_string';

function pw_properties_add(oldprops: Ppw_properties; dict: Pspa_dict) : longint; cdecl; external libpipewirename Name 'pw_properties_add';

function pw_properties_add_keys(oldprops: Ppw_properties; dict: Pspa_dict; keys: PPchar) : longint; cdecl; external libpipewirename Name 'pw_properties_add_keys';

procedure pw_properties_clear(properties: Ppw_properties); cdecl; external libpipewirename Name 'pw_properties_clear';

procedure pw_properties_free(properties: Ppw_properties); cdecl; external libpipewirename Name 'pw_properties_free';

function pw_properties_set(properties: Ppw_properties; key: PChar; Value: PChar) : longint; cdecl; external libpipewirename Name 'pw_properties_set';

function pw_properties_setf(properties: Ppw_properties; key: PChar; format: PChar; args: array of const) : longint; cdecl; external libpipewirename Name 'pw_properties_setf';

function pw_properties_setf(properties: Ppw_properties; key: PChar; format: PChar) : longint; cdecl; external libpipewirename Name 'pw_properties_setf';

function pw_properties_setva(properties: Ppw_properties; key: PChar; format: PChar; args: pointer) : longint; cdecl; external libpipewirename Name 'pw_properties_setva';

function pw_properties_get(properties: Ppw_properties; key: PChar) : PChar; cdecl; external libpipewirename Name 'pw_properties_get';

function pw_properties_fetch_uint32(properties: Ppw_properties; key: PChar; Value: Puint32_t) : longint; cdecl; external libpipewirename Name 'pw_properties_fetch_uint32';

function pw_properties_fetch_int32(properties: Ppw_properties; key: PChar; Value: Pint32_t) : longint; cdecl; external libpipewirename Name 'pw_properties_fetch_int32';

function pw_properties_fetch_uint64(properties: Ppw_properties; key: PChar; Value: Puint64_t) : longint; cdecl; external libpipewirename Name 'pw_properties_fetch_uint64';

function pw_properties_fetch_int64(properties: Ppw_properties; key: PChar; Value: Pint64_t) : longint; cdecl; external libpipewirename Name 'pw_properties_fetch_int64';

function pw_properties_fetch_bool(properties: Ppw_properties; key: PChar; Value: P_Bool) : longint; cdecl; external libpipewirename Name 'pw_properties_fetch_bool';

  {
  
  static inline uint32_t
  pw_properties_get_uint32(const struct pw_properties *properties, const char *key, uint32_t deflt)
  
   uint32_t val = deflt;
   pw_properties_fetch_uint32(properties, key, &val);
   return val;
  
  
  static inline int32_t
  pw_properties_get_int32(const struct pw_properties *properties, const char *key, int32_t deflt)
  
   int32_t val = deflt;
   pw_properties_fetch_int32(properties, key, &val);
   return val;
  
  
  static inline uint64_t
  pw_properties_get_uint64(const struct pw_properties *properties, const char *key, uint64_t deflt)
  
   uint64_t val = deflt;
   pw_properties_fetch_uint64(properties, key, &val);
   return val;
  
  
  static inline int64_t
  pw_properties_get_int64(const struct pw_properties *properties, const char *key, int64_t deflt)
  
   int64_t val = deflt;
   pw_properties_fetch_int64(properties, key, &val);
   return val;
  
  
  
  static inline 
  # 148 "/usr/include/pipewire-0.3/pipewire/properties.h" 3 4
               _Bool
  
  # 149 "/usr/include/pipewire-0.3/pipewire/properties.h"
  pw_properties_get_bool(const struct pw_properties *properties, const char *key, 
  # 149 "/usr/include/pipewire-0.3/pipewire/properties.h" 3 4
                                                                                 _Bool 
  # 149 "/usr/include/pipewire-0.3/pipewire/properties.h"
                                                                                      deflt)
  
   
  # 151 "/usr/include/pipewire-0.3/pipewire/properties.h" 3 4
  _Bool 
  # 151 "/usr/include/pipewire-0.3/pipewire/properties.h"
       val = deflt;
   pw_properties_fetch_bool(properties, key, &val);
   return val;
  
   }
function pw_properties_iterate(properties: Ppw_properties; state: Ppointer) : PChar; cdecl; external libpipewirename Name 'pw_properties_iterate';

function pw_properties_serialize_dict(f: pointer; dict: Pspa_dict; flags: Tuint32_t) : longint; cdecl; external libpipewirename Name 'pw_properties_serialize_dict';

  {
  static inline 
  # 162 "/usr/include/pipewire-0.3/pipewire/properties.h" 3 4
               _Bool 
  # 162 "/usr/include/pipewire-0.3/pipewire/properties.h"
                    pw_properties_parse_bool(const char *value) 
   return spa_atob(value);
  
  
  static inline int pw_properties_parse_int(const char *value) 
   int v;
   return spa_atoi32(value, &v, 0) ? v: 0;
  
  
  static inline int64_t pw_properties_parse_int64(const char *value) 
   int64_t v;
   return spa_atoi64(value, &v, 0) ? v : 0;
  
  
  static inline uint64_t pw_properties_parse_uint64(const char *value) 
   uint64_t v;
   return spa_atou64(value, &v, 0) ? v : 0;
  
  
  static inline float pw_properties_parse_float(const char *value) 
   float v;
   return spa_atof(value, &v) ? v : 0.0f;
  
  
  static inline double pw_properties_parse_double(const char *value) 
   double v;
   return spa_atod(value, &v) ? v : 0.0;
  
   }

