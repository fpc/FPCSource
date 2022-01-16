unit WinDirs;

{*******************************************************************************

IMPORTANT NOTES:

SHGetFolderPath function is deprecated. Only some CSIDL values are supported.

As of Windows Vista, this function is merely a wrapper for SHGetKnownFolderPath.
The CSIDL value is translated to its associated KNOWNFOLDERID and then SHGetKnownFolderPath
is called. New applications should use the known folder system rather than the older
CSIDL system, which is supported only for backward compatibility.

Official list of CSIDL and FOLDERID constants:
https://msdn.microsoft.com/en-us/library/windows/desktop/bb762494.aspx
https://msdn.microsoft.com/en-us/library/windows/desktop/dd378457.aspx

*******************************************************************************}

{$mode objfpc}
{$H+}

interface

// CSIDL_* contants are also declared in "ShellApi" and "shfolder" units.
// If changed, remember to add an appropriate mapping in CSIDLtoFOLDERID list.
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
  CSIDL_FONTS                   = $0014; { %SYSTEMROOT%\Fonts                                               }
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
  // NOTE: CSIDL_PROFILES has been removed from Microsoft official documentation!

  CSIDL_FLAG_CREATE             = $8000; { (force creation of requested folder if it doesn't exist yet)     }

// FOLDERID_* constants parsed from KnownFolders.h file in
// Windows 10 SDK for Fall Creators Update (10.0.16299.91).
// If changed, remember to add an appropriate mapping in CSIDLtoFOLDERID list.
const
  FOLDERID_AccountPictures         : TGUID = '{008CA0B1-55B4-4C56-B8A8-4DE4B299D3BE}';
  FOLDERID_AddNewPrograms          : TGUID = '{DE61D971-5EBC-4F02-A3A9-6C82895E5C04}';
  FOLDERID_AdminTools              : TGUID = '{724EF170-A42D-4FEF-9F26-B60E846FBA4F}';
  FOLDERID_AllAppMods              : TGUID = '{7AD67899-66AF-43BA-9156-6AAD42E6C596}';
  FOLDERID_AppCaptures             : TGUID = '{EDC0FE71-98D8-4F4A-B920-C8DC133CB165}';
  FOLDERID_AppDataDesktop          : TGUID = '{B2C5E279-7ADD-439F-B28C-C41FE1BBF672}';
  FOLDERID_AppDataDocuments        : TGUID = '{7BE16610-1F7F-44AC-BFF0-83E15F2FFCA1}';
  FOLDERID_AppDataFavorites        : TGUID = '{7CFBEFBC-DE1F-45AA-B843-A542AC536CC9}';
  FOLDERID_AppDataProgramData      : TGUID = '{559D40A3-A036-40FA-AF61-84CB430A4D34}';
  FOLDERID_AppUpdates              : TGUID = '{A305CE99-F527-492B-8B1A-7E76FA98D6E4}';
  FOLDERID_ApplicationShortcuts    : TGUID = '{A3918781-E5F2-4890-B3D9-A7E54332328C}';
  FOLDERID_AppsFolder              : TGUID = '{1E87508D-89C2-42F0-8A7E-645A0F50CA58}';
  FOLDERID_CDBurning               : TGUID = '{9E52AB10-F80D-49DF-ACB8-4330F5687855}';
  FOLDERID_CameraRoll              : TGUID = '{AB5FB87B-7CE2-4F83-915D-550846C9537B}';
  FOLDERID_CameraRollLibrary       : TGUID = '{2B20DF75-1EDA-4039-8097-38798227D5B7}';
  FOLDERID_ChangeRemovePrograms    : TGUID = '{DF7266AC-9274-4867-8D55-3BD661DE872D}';
  FOLDERID_CommonAdminTools        : TGUID = '{D0384E7D-BAC3-4797-8F14-CBA229B392B5}';
  FOLDERID_CommonOEMLinks          : TGUID = '{C1BAE2D0-10DF-4334-BEDD-7AA20B227A9D}';
  FOLDERID_CommonPrograms          : TGUID = '{0139D44E-6AFE-49F2-8690-3DAFCAE6FFB8}';
  FOLDERID_CommonStartMenu         : TGUID = '{A4115719-D62E-491D-AA7C-E74B8BE3B067}';
  FOLDERID_CommonStartMenuPlaces   : TGUID = '{A440879F-87A0-4F7D-B700-0207B966194A}';
  FOLDERID_CommonStartup           : TGUID = '{82A5EA35-D9CD-47C5-9629-E15D2F714E6E}';
  FOLDERID_CommonTemplates         : TGUID = '{B94237E7-57AC-4347-9151-B08C6C32D1F7}';
  FOLDERID_ComputerFolder          : TGUID = '{0AC0837C-BBF8-452A-850D-79D08E667CA7}';
  FOLDERID_ConflictFolder          : TGUID = '{4BFEFB45-347D-4006-A5BE-AC0CB0567192}';
  FOLDERID_ConnectionsFolder       : TGUID = '{6F0CD92B-2E97-45D1-88FF-B0D186B8DEDD}';
  FOLDERID_Contacts                : TGUID = '{56784854-C6CB-462B-8169-88E350ACB882}';
  FOLDERID_ControlPanelFolder      : TGUID = '{82A74AEB-AEB4-465C-A014-D097EE346D63}';
  FOLDERID_Cookies                 : TGUID = '{2B0F765D-C0E9-4171-908E-08A611B84FF6}';
  FOLDERID_CurrentAppMods          : TGUID = '{3DB40B20-2A30-4DBE-917E-771DD21DD099}';
  FOLDERID_Desktop                 : TGUID = '{B4BFCC3A-DB2C-424C-B029-7FE99A87C641}';
  FOLDERID_DevelopmentFiles        : TGUID = '{DBE8E08E-3053-4BBC-B183-2A7B2B191E59}';
  FOLDERID_Device                  : TGUID = '{1C2AC1DC-4358-4B6C-9733-AF21156576F0}';
  FOLDERID_DeviceMetadataStore     : TGUID = '{5CE4A5E9-E4EB-479D-B89F-130C02886155}';
  FOLDERID_Documents               : TGUID = '{FDD39AD0-238F-46AF-ADB4-6C85480369C7}';
  FOLDERID_DocumentsLibrary        : TGUID = '{7B0DB17D-9CD2-4A93-9733-46CC89022E7C}';
  FOLDERID_Downloads               : TGUID = '{374DE290-123F-4565-9164-39C4925E467B}';
  FOLDERID_Favorites               : TGUID = '{1777F761-68AD-4D8A-87BD-30B759FA33DD}';
  FOLDERID_Fonts                   : TGUID = '{FD228CB7-AE11-4AE3-864C-16F3910AB8FE}';
  FOLDERID_GameTasks               : TGUID = '{054FAE61-4DD8-4787-80B6-090220C4B700}';
  FOLDERID_Games                   : TGUID = '{CAC52C1A-B53D-4EDC-92D7-6B2E8AC19434}';
  FOLDERID_History                 : TGUID = '{D9DC8A3B-B784-432E-A781-5A1130A75963}';
  FOLDERID_HomeGroup               : TGUID = '{52528A6B-B9E3-4ADD-B60D-588C2DBA842D}';
  FOLDERID_HomeGroupCurrentUser    : TGUID = '{9B74B6A3-0DFD-4F11-9E78-5F7800F2E772}';
  FOLDERID_ImplicitAppShortcuts    : TGUID = '{BCB5256F-79F6-4CEE-B725-DC34E402FD46}';
  FOLDERID_InternetCache           : TGUID = '{352481E8-33BE-4251-BA85-6007CAEDCF9D}';
  FOLDERID_InternetFolder          : TGUID = '{4D9F7874-4E0C-4904-967B-40B0D20C3E4B}';
  FOLDERID_Libraries               : TGUID = '{1B3EA5DC-B587-4786-B4EF-BD1DC332AEAE}';
  FOLDERID_Links                   : TGUID = '{BFB9D5E0-C6A9-404C-B2B2-AE6DB6AF4968}';
  FOLDERID_LocalAppData            : TGUID = '{F1B32785-6FBA-4FCF-9D55-7B8E7F157091}';
  FOLDERID_LocalAppDataLow         : TGUID = '{A520A1A4-1780-4FF6-BD18-167343C5AF16}';
  FOLDERID_LocalDocuments          : TGUID = '{F42EE2D3-909F-4907-8871-4C22FC0BF756}';
  FOLDERID_LocalDownloads          : TGUID = '{7D83EE9B-2244-4E70-B1F5-5393042AF1E4}';
  FOLDERID_LocalMusic              : TGUID = '{A0C69A99-21C8-4671-8703-7934162FCF1D}';
  FOLDERID_LocalPictures           : TGUID = '{0DDD015D-B06C-45D5-8C4C-F59713854639}';
  FOLDERID_LocalVideos             : TGUID = '{35286A68-3C57-41A1-BBB1-0EAE73D76C95}';
  FOLDERID_LocalizedResourcesDir   : TGUID = '{2A00375E-224C-49DE-B8D1-440DF7EF3DDC}';
  FOLDERID_Music                   : TGUID = '{4BD8D571-6D19-48D3-BE97-422220080E43}';
  FOLDERID_MusicLibrary            : TGUID = '{2112AB0A-C86A-4FFE-A368-0DE96E47012E}';
  FOLDERID_NetHood                 : TGUID = '{C5ABBF53-E17F-4121-8900-86626FC2C973}';
  FOLDERID_NetworkFolder           : TGUID = '{D20BEEC4-5CA8-4905-AE3B-BF251EA09B53}';
  FOLDERID_Objects3D               : TGUID = '{31C0DD25-9439-4F12-BF41-7FF4EDA38722}';
  FOLDERID_OneDrive                : TGUID = '{A52BBA46-E9E1-435F-B3D9-28DAA648C0F6}';
  FOLDERID_OriginalImages          : TGUID = '{2C36C0AA-5812-4B87-BFD0-4CD0DFB19B39}';
  FOLDERID_PhotoAlbums             : TGUID = '{69D2CF90-FC33-4FB7-9A0C-EBB0F0FCB43C}';
  FOLDERID_Pictures                : TGUID = '{33E28130-4E1E-4676-835A-98395C3BC3BB}';
  FOLDERID_PicturesLibrary         : TGUID = '{A990AE9F-A03B-4E80-94BC-9912D7504104}';
  FOLDERID_Playlists               : TGUID = '{DE92C1C7-837F-4F69-A3BB-86E631204A23}';
  FOLDERID_PrintHood               : TGUID = '{9274BD8D-CFD1-41C3-B35E-B13F55A758F4}';
  FOLDERID_PrintersFolder          : TGUID = '{76FC4E2D-D6AD-4519-A663-37BD56068185}';
  FOLDERID_Profile                 : TGUID = '{5E6C858F-0E22-4760-9AFE-EA3317B67173}';
  FOLDERID_ProgramData             : TGUID = '{62AB5D82-FDC1-4DC3-A9DD-070D1D495D97}';
  FOLDERID_ProgramFiles            : TGUID = '{905E63B6-C1BF-494E-B29C-65B732D3D21A}';
  FOLDERID_ProgramFilesCommon      : TGUID = '{F7F1ED05-9F6D-47A2-AAAE-29D317C6F066}';
  FOLDERID_ProgramFilesCommonX64   : TGUID = '{6365D5A7-0F0D-45E5-87F6-0DA56B6A4F7D}';
  FOLDERID_ProgramFilesCommonX86   : TGUID = '{DE974D24-D9C6-4D3E-BF91-F4455120B917}';
  FOLDERID_ProgramFilesX64         : TGUID = '{6D809377-6AF0-444B-8957-A3773F02200E}';
  FOLDERID_ProgramFilesX86         : TGUID = '{7C5A40EF-A0FB-4BFC-874A-C0F2E0B9FA8E}';
  FOLDERID_Programs                : TGUID = '{A77F5D77-2E2B-44C3-A6A2-ABA601054A51}';
  FOLDERID_Public                  : TGUID = '{DFDF76A2-C82A-4D63-906A-5644AC457385}';
  FOLDERID_PublicDesktop           : TGUID = '{C4AA340D-F20F-4863-AFEF-F87EF2E6BA25}';
  FOLDERID_PublicDocuments         : TGUID = '{ED4824AF-DCE4-45A8-81E2-FC7965083634}';
  FOLDERID_PublicDownloads         : TGUID = '{3D644C9B-1FB8-4F30-9B45-F670235F79C0}';
  FOLDERID_PublicGameTasks         : TGUID = '{DEBF2536-E1A8-4C59-B6A2-414586476AEA}';
  FOLDERID_PublicLibraries         : TGUID = '{48DAF80B-E6CF-4F4E-B800-0E69D84EE384}';
  FOLDERID_PublicMusic             : TGUID = '{3214FAB5-9757-4298-BB61-92A9DEAA44FF}';
  FOLDERID_PublicPictures          : TGUID = '{B6EBFB86-6907-413C-9AF7-4FC2ABF07CC5}';
  FOLDERID_PublicRingtones         : TGUID = '{E555AB60-153B-4D17-9F04-A5FE99FC15EC}';
  FOLDERID_PublicUserTiles         : TGUID = '{0482AF6C-08F1-4C34-8C90-E17EC98B1E17}';
  FOLDERID_PublicVideos            : TGUID = '{2400183A-6185-49FB-A2D8-4A392A602BA3}';
  FOLDERID_QuickLaunch             : TGUID = '{52A4F021-7B75-48A9-9F6B-4B87A210BC8F}';
  FOLDERID_Recent                  : TGUID = '{AE50C081-EBD2-438A-8655-8A092E34987A}';
  FOLDERID_RecordedCalls           : TGUID = '{2F8B40C2-83ED-48EE-B383-A1F157EC6F9A}';
  FOLDERID_RecordedTVLibrary       : TGUID = '{1A6FDBA2-F42D-4358-A798-B74D745926C5}';
  FOLDERID_RecycleBinFolder        : TGUID = '{B7534046-3ECB-4C18-BE4E-64CD4CB7D6AC}';
  FOLDERID_ResourceDir             : TGUID = '{8AD10C31-2ADB-4296-A8F7-E4701232C972}';
  FOLDERID_RetailDemo              : TGUID = '{12D4C69E-24AD-4923-BE19-31321C43A767}';
  FOLDERID_Ringtones               : TGUID = '{C870044B-F49E-4126-A9C3-B52A1FF411E8}';
  FOLDERID_RoamedTileImages        : TGUID = '{AAA8D5A5-F1D6-4259-BAA8-78E7EF60835E}';
  FOLDERID_RoamingAppData          : TGUID = '{3EB685DB-65F9-4CF6-A03A-E3EF65729F3D}';
  FOLDERID_RoamingTiles            : TGUID = '{00BCFC5A-ED94-4E48-96A1-3F6217F21990}';
  FOLDERID_SEARCH_CSC              : TGUID = '{EE32E446-31CA-4ABA-814F-A5EBD2FD6D5E}';
  FOLDERID_SEARCH_MAPI             : TGUID = '{98EC0E18-2098-4D44-8644-66979315A281}';
  FOLDERID_SampleMusic             : TGUID = '{B250C668-F57D-4EE1-A63C-290EE7D1AA1F}';
  FOLDERID_SamplePictures          : TGUID = '{C4900540-2379-4C75-844B-64E6FAF8716B}';
  FOLDERID_SamplePlaylists         : TGUID = '{15CA69B3-30EE-49C1-ACE1-6B5EC372AFB5}';
  FOLDERID_SampleVideos            : TGUID = '{859EAD94-2E85-48AD-A71A-0969CB56A6CD}';
  FOLDERID_SavedGames              : TGUID = '{4C5C32FF-BB9D-43B0-B5B4-2D72E54EAAA4}';
  FOLDERID_SavedPictures           : TGUID = '{3B193882-D3AD-4EAB-965A-69829D1FB59F}';
  FOLDERID_SavedPicturesLibrary    : TGUID = '{E25B5812-BE88-4BD9-94B0-29233477B6C3}';
  FOLDERID_SavedSearches           : TGUID = '{7D1D3A04-DEBB-4115-95CF-2F29DA2920DA}';
  FOLDERID_Screenshots             : TGUID = '{B7BEDE81-DF94-4682-A7D8-57A52620B86F}';
  FOLDERID_SearchHistory           : TGUID = '{0D4C3DB6-03A3-462F-A0E6-08924C41B5D4}';
  FOLDERID_SearchHome              : TGUID = '{190337D1-B8CA-4121-A639-6D472D16972A}';
  FOLDERID_SearchTemplates         : TGUID = '{7E636BFE-DFA9-4D5E-B456-D7B39851D8A9}';
  FOLDERID_SendTo                  : TGUID = '{8983036C-27C0-404B-8F08-102D10DCFD74}';
  FOLDERID_SidebarDefaultParts     : TGUID = '{7B396E54-9EC5-4300-BE0A-2482EBAE1A26}';
  FOLDERID_SidebarParts            : TGUID = '{A75D362E-50FC-4FB7-AC2C-A8BEAA314493}';
  FOLDERID_SkyDrive                : TGUID = '{A52BBA46-E9E1-435F-B3D9-28DAA648C0F6}';
  FOLDERID_SkyDriveCameraRoll      : TGUID = '{767E6811-49CB-4273-87C2-20F355E1085B}';
  FOLDERID_SkyDriveDocuments       : TGUID = '{24D89E24-2F19-4534-9DDE-6A6671FBB8FE}';
  FOLDERID_SkyDriveMusic           : TGUID = '{C3F2459E-80D6-45DC-BFEF-1F769F2BE730}';
  FOLDERID_SkyDrivePictures        : TGUID = '{339719B5-8C47-4894-94C2-D8F77ADD44A6}';
  FOLDERID_StartMenu               : TGUID = '{625B53C3-AB48-4EC1-BA1F-A1EF4146FC19}';
  FOLDERID_StartMenuAllPrograms    : TGUID = '{F26305EF-6948-40B9-B255-81453D09C785}';
  FOLDERID_Startup                 : TGUID = '{B97D20BB-F46A-4C97-BA10-5E3608430854}';
  FOLDERID_SyncManagerFolder       : TGUID = '{43668BF8-C14E-49B2-97C9-747784D784B7}';
  FOLDERID_SyncResultsFolder       : TGUID = '{289A9A43-BE44-4057-A41B-587A76D7E7F9}';
  FOLDERID_SyncSetupFolder         : TGUID = '{0F214138-B1D3-4A90-BBA9-27CBC0C5389A}';
  FOLDERID_System                  : TGUID = '{1AC14E77-02E7-4E5D-B744-2EB1AE5198B7}';
  FOLDERID_SystemX86               : TGUID = '{D65231B0-B2F1-4857-A4CE-A8E7C6EA7D27}';
  FOLDERID_Templates               : TGUID = '{A63293E8-664E-48DB-A079-DF759E0509F7}';
  FOLDERID_UserPinned              : TGUID = '{9E3995AB-1F9C-4F13-B827-48B24B6C7174}';
  FOLDERID_UserProfiles            : TGUID = '{0762D272-C50A-4BB0-A382-697DCD729B80}';
  FOLDERID_UserProgramFiles        : TGUID = '{5CD7AEE2-2219-4A67-B85D-6C9CE15660CB}';
  FOLDERID_UserProgramFilesCommon  : TGUID = '{BCBD3057-CA5C-4622-B42D-BC56DB0AE516}';
  FOLDERID_UsersFiles              : TGUID = '{F3CE0F7C-4901-4ACC-8648-D5D44B04EF8F}';
  FOLDERID_UsersLibraries          : TGUID = '{A302545D-DEFF-464B-ABE8-61C8648D939B}';
  FOLDERID_Videos                  : TGUID = '{18989B1D-99B5-455B-841C-AB7C74E4DDFC}';
  FOLDERID_VideosLibrary           : TGUID = '{491E922F-5643-4AF4-A7EB-4E7A138D8174}';
  FOLDERID_Windows                 : TGUID = '{F38BF404-1D43-42F2-9305-67DE0B28FC23}';

// KNOWN_FOLDER_FLAG enumeration
const
  KF_FLAG_DEFAULT = DWORD(0);
  KF_FLAG_CREATE  = DWORD($8000);

function GetWindowsSpecialDir(ID: Integer; CreateIfNotExists: Boolean = True): String;
function GetWindowsSpecialDir(const GUID: TGUID; CreateIfNotExists: Boolean = True): String;
function GetWindowsSpecialDirUnicode(ID: Integer; CreateIfNotExists: Boolean = True): UnicodeString;
function GetWindowsSpecialDirUnicode(const GUID: TGUID; CreateIfNotExists: Boolean = True): UnicodeString;

function GetWindowsSystemDirectory: String;
function GetWindowsSystemDirectoryUnicode: UnicodeString;

function ConvertCSIDLtoFOLDERID(CSIDL: Integer; out FOLDERID: TGUID): Boolean;
function ConvertFOLDERIDtoCSIDL(const FOLDERID: TGUID; out CSIDL: Integer): Boolean;

implementation

uses
  Windows, SysUtils;

type
  TCSIDLtoFOLDERID = record
    CSIDL: Integer;
    FOLDERID: PGUID;
  end;

const
  // Mapping of legacy CSIDL_* constants to their appropriate FOLDERID_* counterparts.
  CSIDLtoFOLDERID: array [1..40] of TCSIDLtoFOLDERID = (
    (CSIDL: CSIDL_PROGRAMS;                 FOLDERID: @FOLDERID_Programs),
    (CSIDL: CSIDL_PERSONAL;                 FOLDERID: @FOLDERID_Documents),
    (CSIDL: CSIDL_FAVORITES;                FOLDERID: @FOLDERID_Favorites),
    (CSIDL: CSIDL_STARTUP;                  FOLDERID: @FOLDERID_Startup),
    (CSIDL: CSIDL_RECENT;                   FOLDERID: @FOLDERID_Recent),
    (CSIDL: CSIDL_SENDTO;                   FOLDERID: @FOLDERID_SendTo),
    (CSIDL: CSIDL_STARTMENU;                FOLDERID: @FOLDERID_StartMenu),
    (CSIDL: CSIDL_MYMUSIC;                  FOLDERID: @FOLDERID_Music),
    (CSIDL: CSIDL_MYVIDEO;                  FOLDERID: @FOLDERID_Videos),
    (CSIDL: CSIDL_DESKTOPDIRECTORY;         FOLDERID: @FOLDERID_Desktop),
    (CSIDL: CSIDL_NETHOOD;                  FOLDERID: @FOLDERID_NetHood),
    (CSIDL: CSIDL_FONTS;                    FOLDERID: @FOLDERID_Fonts),
    (CSIDL: CSIDL_TEMPLATES;                FOLDERID: @FOLDERID_Templates),
    (CSIDL: CSIDL_COMMON_STARTMENU;         FOLDERID: @FOLDERID_CommonStartMenu),
    (CSIDL: CSIDL_COMMON_PROGRAMS;          FOLDERID: @FOLDERID_CommonPrograms),
    (CSIDL: CSIDL_COMMON_STARTUP;           FOLDERID: @FOLDERID_CommonStartup),
    (CSIDL: CSIDL_COMMON_DESKTOPDIRECTORY;  FOLDERID: @FOLDERID_PublicDesktop),
    (CSIDL: CSIDL_APPDATA;                  FOLDERID: @FOLDERID_RoamingAppData),
    (CSIDL: CSIDL_PRINTHOOD;                FOLDERID: @FOLDERID_PrintHood),
    (CSIDL: CSIDL_LOCAL_APPDATA;            FOLDERID: @FOLDERID_LocalAppData),
    (CSIDL: CSIDL_COMMON_FAVORITES;         FOLDERID: @FOLDERID_Favorites),
    (CSIDL: CSIDL_INTERNET_CACHE;           FOLDERID: @FOLDERID_InternetCache),
    (CSIDL: CSIDL_COOKIES;                  FOLDERID: @FOLDERID_Cookies),
    (CSIDL: CSIDL_HISTORY;                  FOLDERID: @FOLDERID_History),
    (CSIDL: CSIDL_COMMON_APPDATA;           FOLDERID: @FOLDERID_ProgramData),
    (CSIDL: CSIDL_WINDOWS;                  FOLDERID: @FOLDERID_Windows),
    (CSIDL: CSIDL_SYSTEM;                   FOLDERID: @FOLDERID_System),
    (CSIDL: CSIDL_PROGRAM_FILES;            FOLDERID: @FOLDERID_ProgramFiles),
    (CSIDL: CSIDL_MYPICTURES;               FOLDERID: @FOLDERID_Pictures),
    (CSIDL: CSIDL_PROFILE;                  FOLDERID: @FOLDERID_Profile),
    (CSIDL: CSIDL_PROGRAM_FILES_COMMON;     FOLDERID: @FOLDERID_ProgramFilesCommon),
    (CSIDL: CSIDL_COMMON_TEMPLATES;         FOLDERID: @FOLDERID_CommonTemplates),
    (CSIDL: CSIDL_COMMON_DOCUMENTS;         FOLDERID: @FOLDERID_PublicDocuments),
    (CSIDL: CSIDL_COMMON_ADMINTOOLS;        FOLDERID: @FOLDERID_CommonAdminTools),
    (CSIDL: CSIDL_ADMINTOOLS;               FOLDERID: @FOLDERID_AdminTools),
    (CSIDL: CSIDL_COMMON_MUSIC;             FOLDERID: @FOLDERID_PublicMusic),
    (CSIDL: CSIDL_COMMON_PICTURES;          FOLDERID: @FOLDERID_PublicPictures),
    (CSIDL: CSIDL_COMMON_VIDEO;             FOLDERID: @FOLDERID_PublicVideos),
    (CSIDL: CSIDL_CDBURN_AREA;              FOLDERID: @FOLDERID_CDBurning),
    (CSIDL: CSIDL_PROFILES;                 FOLDERID: @FOLDERID_UserProfiles)
  );

// CoTaskMemFree is required for the use of SHGetKnownFolderPath function.
// CoTaskMemFree function signature was copied from ActiveX unit.
procedure CoTaskMemFree(_para1:PVOID); stdcall; external 'ole32.dll' name 'CoTaskMemFree';

type
  KNOWNFOLDERID = TGUID;
  PWSTR = PWideChar;

type
  // HRESULT SHGetFolderPath(
  //  _In_  HWND   hwndOwner,
  //  _In_  int    nFolder,
  //  _In_  HANDLE hToken,
  //  _In_  DWORD  dwFlags,
  //  _Out_ LPTSTR pszPath
  // );
  // DLL: Shell32.dll (version 5.0 or later)
  // OS: Windows 2000 and newer
  TSHGetFolderPathW = function(Ahwnd: HWND; Csidl: Integer; Token: THandle;
    Flags: DWORD; Path: PWideChar): HRESULT; stdcall;

  // HRESULT SHGetKnownFolderPath(
  //  _In_     REFKNOWNFOLDERID rfid,
  //  _In_     DWORD            dwFlags,
  //  _In_opt_ HANDLE           hToken,
  //  _Out_    PWSTR            *ppszPath
  // );
  // DLL: Shell32.dll (version 6.0.6000 or later)
  // OS: Windows Vista / Server 2008 and newer
  TSHGetKnownFolderPathW = function(const rfid: KNOWNFOLDERID; dwFlags: DWORD;
    hToken: THandle; out ppszPath: PWSTR): HRESULT; stdcall;

const
  SSHGetFolderPathW = 'SHGetFolderPathW';
  SSHGetKnownFolderPathW = 'SHGetKnownFolderPath';
  SLibName = 'shell32.dll';

var
  _SHGetFolderPathW : TSHGetFolderPathW = nil;
  _SHGetKnownFolderPathW: TSHGetKnownFolderPathW = nil;
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
      begin
        Pointer(_SHGetFolderPathW) := GetProcAddress(DLLHandle, SSHGetFolderPathW);
        Pointer(_SHGetKnownFolderPathW) := GetProcAddress(DLLHandle, SSHGetKnownFolderPathW);
      end;
    end;
  end;
  // At least one of SHGetFolderPath or SHGetKnownFolderPath functions is required
  if (@_SHGetFolderPathW = nil) and (@_SHGetKnownFolderPathW = nil) then
    raise Exception.Create('Could not locate '+SSHGetFolderPathW+' / '+SSHGetKnownFolderPathW+' functions');
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
  FOLDERID: TGUID;
begin
  InitDLL;
  Result := '';
  if @_SHGetFolderPathW = nil then
  begin
    if ConvertCSIDLtoFOLDERID(ID, FOLDERID) then
      Result := GetWindowsSpecialDirUnicode(FOLDERID, CreateIfNotExists);
  end
  else
  begin
    if CreateIfNotExists then
      ID := ID or CSIDL_FLAG_CREATE;
    if _SHGetFolderPathW(0, ID, 0, 0, @Buffer[0]) = S_OK then
      Result := IncludeTrailingPathDelimiter(StrPas(Buffer));
  end;
end;

function GetWindowsSpecialDirUnicode(const GUID: TGUID; CreateIfNotExists: Boolean = True): UnicodeString;
var
  Flags: DWORD;
  Path: PWSTR;
  CSIDL: Integer;
begin
  InitDLL;
  Result := '';
  if @_SHGetKnownFolderPathW = nil then
  begin
    if ConvertFOLDERIDtoCSIDL(GUID, CSIDL) then
      Result := GetWindowsSpecialDirUnicode(CSIDL, CreateIfNotExists);
  end
  else
  begin
    Path := nil;
    Flags := KF_FLAG_DEFAULT;
    if CreateIfNotExists then
      Flags := Flags or KF_FLAG_CREATE;
    if _SHGetKnownFolderPathW(GUID, Flags, 0, Path) = S_OK then
    begin
      Result := StrPas(Path);
      CoTaskMemFree(Path);
    end;
  end;
end;

function GetWindowsSpecialDir(ID: Integer; CreateIfNotExists: Boolean = True): String;
begin
  Result := String(GetWindowsSpecialDirUnicode(ID, CreateIfNotExists));
end;

function GetWindowsSpecialDir(const GUID: TGUID; CreateIfNotExists: Boolean = True): String;
begin
  Result := String(GetWindowsSpecialDirUnicode(GUID, CreateIfNotExists));
end;

function ConvertCSIDLtoFOLDERID(CSIDL: Integer; out FOLDERID: TGUID): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(CSIDLtoFOLDERID) to High(CSIDLtoFOLDERID) do
  begin
    if CSIDLtoFOLDERID[I].CSIDL = CSIDL then
    begin
      if CSIDLtoFOLDERID[I].FOLDERID <> nil then
      begin
        FOLDERID := CSIDLtoFOLDERID[I].FOLDERID^;
        Result := True;
        Break;
      end
      else
        Break;
    end;
  end;
end;

function ConvertFOLDERIDtoCSIDL(const FOLDERID: TGUID; out CSIDL: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(CSIDLtoFOLDERID) to High(CSIDLtoFOLDERID) do
  begin
    if CSIDLtoFOLDERID[I].FOLDERID <> nil then
    begin
      if IsEqualGUID(CSIDLtoFOLDERID[I].FOLDERID^, FOLDERID) then
      begin
        CSIDL := CSIDLtoFOLDERID[I].CSIDL;
        Result := True;
        Break;
      end;
    end;
  end;
end;

finalization
  FinitDLL;

end.

