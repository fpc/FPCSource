unit windirs;

{*******************************************************************************

IMPORTANT NOTES:

SHGetFolderPath function is deprecated. Only some CSIDL values are supported.

As of Windows Vista, this function is merely a wrapper for SHGetKnownFolderPath.
The CSIDL value is translated to its associated KNOWNFOLDERID and then SHGetKnownFolderPath
is called. New applications should use the known folder system rather than the older
CSIDL system, which is supported only for backward compatibility.

*******************************************************************************}

{$mode objfpc}
{$H+}

interface

uses
  windows;

// CSIDL_* contants are also declared in "ShellApi" and "shfolder" units.
const
  CSIDL_PROGRAMS                = $0002; { %SYSTEMDRIVE%\Program Files                                      }
  CSIDL_PERSONAL                = $0005; { %USERPROFILE%\My Documents                                       }
  CSIDL_FAVORITES               = $0006; { %USERPROFILE%\Favorites                                          }
  CSIDL_STARTUP                 = $0007; { %USERPROFILE%\Start menu\Programs\Startup                        }
  CSIDL_RECENT                  = $0008; { %USERPROFILE%\Recent                                             }
  CSIDL_SENDTO                  = $0009; { %USERPROFILE%\Sendto                                             }
  CSIDL_STARTMENU               = $000B; { %USERPROFILE%\Start menu                                         }
  CSIDL_MYMUSIC                 = $000D; { %USERPROFILE%\Documents\My Music                                 }
  CSIDL_MYVIDEO                 = $000E; { %USERPROFILE%\Documents\My Videos                                }
  CSIDL_DESKTOPDIRECTORY        = $0010; { %USERPROFILE%\Desktop                                            }
  CSIDL_NETHOOD                 = $0013; { %USERPROFILE%\NetHood                                            }
  CSIDL_TEMPLATES               = $0015; { %USERPROFILE%\Templates                                          }
  CSIDL_COMMON_STARTMENU        = $0016; { %PROFILEPATH%\All users\Start menu                               }
  CSIDL_COMMON_PROGRAMS         = $0017; { %PROFILEPATH%\All users\Start menu\Programs                      }
  CSIDL_COMMON_STARTUP          = $0018; { %PROFILEPATH%\All users\Start menu\Programs\Startup              }
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019; { %PROFILEPATH%\All users\Desktop                                  }
  CSIDL_APPDATA                 = $001A; { %USERPROFILE%\Application Data (roaming)                         }
  CSIDL_PRINTHOOD               = $001B; { %USERPROFILE%\Printhood                                          }
  CSIDL_LOCAL_APPDATA           = $001C; { %USERPROFILE%\Local Settings\Application Data (non roaming)      }
  CSIDL_COMMON_FAVORITES        = $001F; { %PROFILEPATH%\All users\Favorites                                }
  CSIDL_INTERNET_CACHE          = $0020; { %USERPROFILE%\Local Settings\Temporary Internet Files            }
  CSIDL_COOKIES                 = $0021; { %USERPROFILE%\Cookies                                            }
  CSIDL_HISTORY                 = $0022; { %USERPROFILE%\Local settings\History                             }
  CSIDL_COMMON_APPDATA          = $0023; { %PROFILESPATH%\All Users\Application Data                        }
  CSIDL_WINDOWS                 = $0024; { %SYSTEMROOT%                                                     }
  CSIDL_SYSTEM                  = $0025; { %SYSTEMROOT%\SYSTEM32 (may be system on 95/98/ME)                }
  CSIDL_PROGRAM_FILES           = $0026; { %SYSTEMDRIVE%\Program Files                                      }
  CSIDL_MYPICTURES              = $0027; { %USERPROFILE%\My Documents\My Pictures                           }
  CSIDL_PROFILE                 = $0028; { %USERPROFILE%                                                    }
  CSIDL_PROGRAM_FILES_COMMON    = $002B; { %SYSTEMDRIVE%\Program Files\Common                               }
  CSIDL_COMMON_TEMPLATES        = $002D; { %PROFILEPATH%\All Users\Templates                                }
  CSIDL_COMMON_DOCUMENTS        = $002E; { %PROFILEPATH%\All Users\Documents                                }
  CSIDL_COMMON_ADMINTOOLS       = $002F; { %PROFILEPATH%\All Users\Start Menu\Programs\Administrative Tools }
  CSIDL_ADMINTOOLS              = $0030; { %USERPROFILE%\Start Menu\Programs\Administrative Tools           }
  CSIDL_COMMON_MUSIC            = $0035; { %PROFILEPATH%\All Users\Documents\my music                       }
  CSIDL_COMMON_PICTURES         = $0036; { %PROFILEPATH%\All Users\Documents\my pictures                    }
  CSIDL_COMMON_VIDEO            = $0037; { %PROFILEPATH%\All Users\Documents\my videos                      }
  CSIDL_CDBURN_AREA             = $003B; { %USERPROFILE%\Local Settings\Application Data\Microsoft\CD Burning }
  CSIDL_PROFILES                = $003E; { %PROFILEPATH%                                                    }

  CSIDL_FLAG_CREATE             = $8000; { (force creation of requested folder if it doesn't exist yet)     }


function GetWindowsSpecialDir(ID: Integer; CreateIfNotExists: Boolean = True): String;
function GetWindowsSpecialDirUnicode(ID: Integer; CreateIfNotExists: Boolean = True): UnicodeString;

function GetWindowsSystemDirectory: String;
function GetWindowsSystemDirectoryUnicode: UnicodeString;

implementation

uses
  sysutils;

type
  // HRESULT SHGetFolderPath(
  //  _In_  HWND   hwndOwner,
  //  _In_  int    nFolder,
  //  _In_  HANDLE hToken,
  //  _In_  DWORD  dwFlags,
  //  _Out_ LPTSTR pszPath
  // );
  TSHGetFolderPathW = function(Ahwnd: HWND; Csidl: Integer; Token: THandle;
    Flags: DWORD; Path: PWideChar): HRESULT; stdcall;

const
  SSHGetFolderPathW = 'SHGetFolderPathW';
  SLibName = 'shell32.dll';

var
  _SHGetFolderPathW : TSHGetFolderPathW = nil;
  DLLHandle: THandle = 0;

procedure InitDLL;
var
  DLLPath: UnicodeString;
begin
  if DLLHandle = 0 then
  begin
    // Load DLL using a full path, in order to prevent spoofing (Mantis #18185)
    DLLPath := GetWindowsSystemDirectoryUnicode;
    if Length(DLLPath) > 0 then
    begin
      DLLPath := IncludeTrailingPathDelimiter(DLLPath) + SLibName;
      DLLHandle := LoadLibraryW(PWideChar(DLLPath));
      if DLLHandle <> 0 then
        Pointer(_SHGetFolderPathW) := GetProcAddress(DLLHandle, SSHGetFolderPathW);
    end;
  end;
  if @_SHGetFolderPathW = nil then
    raise Exception.Create('Could not locate SHGetFolderPath function');
end;

procedure FinitDLL;
begin
  if DLLHandle <> 0 then
  begin
    FreeLibrary(DLLHandle);
    DLLHandle := 0;
  end;
end;

function GetWindowsSystemDirectoryUnicode: UnicodeString;
var
  Buffer: array [0..MAX_PATH] of WideChar;
  CharCount: Integer;
begin
  CharCount := GetSystemDirectoryW(@Buffer[0], MAX_PATH);
  // CharCount is length in TCHARs not including the terminating null character.
  // If result did not fit, CharCount will be bigger than buffer size.
  if (CharCount > 0) and (CharCount < MAX_PATH) then
    Result := StrPas(Buffer)
  else
    Result := '';
end;

function GetWindowsSystemDirectory: String;
begin
  Result := String(GetWindowsSystemDirectoryUnicode);
end;

function GetWindowsSpecialDirUnicode(ID: Integer; CreateIfNotExists: Boolean = True): UnicodeString;
var
  Buffer: array [0..MAX_PATH] of WideChar;
begin
  InitDLL;
  Result := '';
  if CreateIfNotExists then
    ID := ID or CSIDL_FLAG_CREATE;
  if _SHGetFolderPathW(0, ID, 0, 0, @Buffer[0]) = S_OK then
    Result := IncludeTrailingPathDelimiter(StrPas(Buffer));
end;

function GetWindowsSpecialDir(ID: Integer; CreateIfNotExists: Boolean = True): String;
begin
  Result := String(GetWindowsSpecialDirUnicode(ID, CreateIfNotExists));
end;

finalization
  FinitDLL;

end.

