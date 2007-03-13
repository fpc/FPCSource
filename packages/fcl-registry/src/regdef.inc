Type
  LPDWORD = ^DWord;
  LPVOID  = Pointer;
  WINBOOL = LongBool;
  LPCSTR  = PChar;
  LPSTR   = Pchar;
  LONG    = LongInt;
  LPBYTE  = ^Byte;

  ACCESS_MASK = DWORD;
       REGSAM = ACCESS_MASK;

  SECURITY_ATTRIBUTES = record
    nLength : DWORD;
    lpSecurityDescriptor : LPVOID;
    bInheritHandle : WINBOOL;
  end;
  LPSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;


  HKEY = THandle;
  PHKEY = ^HKEY;



Const
  HKEY_CLASSES_ROOT     = HKEY($80000000);
  HKEY_CURRENT_USER     = HKEY($80000001);
  HKEY_LOCAL_MACHINE    = HKEY($80000002);
  HKEY_USERS            = HKEY($80000003);
  HKEY_PERFORMANCE_DATA = HKEY($80000004);
  HKEY_CURRENT_CONFIG   = HKEY($80000005);
  HKEY_DYN_DATA         = HKEY($80000006);

  KEY_ALL_ACCESS         = $F003F;
  KEY_CREATE_LINK        = 32;
  KEY_CREATE_SUB_KEY     = 4;
  KEY_ENUMERATE_SUB_KEYS = 8;
  KEY_EXECUTE            = $20019;
  KEY_NOTIFY             = 16;
  KEY_QUERY_VALUE        = 1;
  KEY_READ               = $20019;
  KEY_SET_VALUE          = 2;
  KEY_WRITE              = $20006;

  REG_BINARY                     = 3;
  REG_DWORD                      = 4;
  REG_DWORD_LITTLE_ENDIAN        = 4;
  REG_DWORD_BIG_ENDIAN           = 5;
  REG_EXPAND_SZ                  = 2;
  REG_FULL_RESOURCE_DESCRIPTOR   = 9;
  REG_LINK                       = 6;
  REG_MULTI_SZ                   = 7;
  REG_NONE                       = 0;
  REG_RESOURCE_LIST              = 8;
  REG_RESOURCE_REQUIREMENTS_LIST = 10;
  REG_SZ                         = 1;

  REG_OPTION_VOLATILE            = 1;
  REG_OPTION_NON_VOLATILE        = 0;
  REG_CREATED_NEW_KEY            = 1;
  REG_OPENED_EXISTING_KEY        = 2;

  ERROR_SUCCESS = 0;


