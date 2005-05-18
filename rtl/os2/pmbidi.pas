{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Yuri Prokushev (prokushev@freemail.ru).

    OS/2 Presentation Manager Window Manager - Bidirectional
    support include file.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Warning: This code is alfa. Future versions
 of this unit might not be compatible.}

unit pmbidi;

interface

uses os2def, pmwin;


{$PACKRECORDS C}

  {                                                                       }
  { Bidirectional languages support window messages                       }
  {                                                                       }

  const
     WM_SETBIDIATTR = $0BD0;
     WM_QUERYBIDIATTR = $0BD1;
     WM_SETBIDISTAT = $0BD2;
     WM_QUERYBIDISTAT = $0BD3;
     WM_KBDLAYERCHANGED = $0BD4;
  {                                                                       }
  {  Language Viewer messages                                             }
  {                                                                       }
     WM_LANGVIEWINFOCHANGED = $0BE0;
     WM_LANG_OPTIONS_DIALOG = $0BE2;
     WM_LANGOPTIONSDIALOG = $0BE2;
  { LVI values - indicating which Bidi information has changed  }
     LVI_WND_BIDI_ATTR = 1;
     LVI_WND_BIDI_STAT = 2;
     LVI_FOCUS_CHANGE = 3;
     LVI_KBD_LAYER = 4;
     LVI_CSD = 5;
     LVI_SET_KBD_LAYER = 6;
     LVI_ALL = -(1);
  { Possible return values from WM_SETBIDIATTR/WM_SETBIDISTAT  }
     SBI_MSG_NOT_PROCESSED = 0;
     SBI_MSG_PROCESSED = 1;
     SBI_MSG_PROCESSED_SELF = 2;
  {                                                                       }
  { Bidirectional attributes masks                                        }
  {                                                                       }
     BDAM_INIT = $80000000;
     BDAM_LEVEL = $70000000;
     BDAM_NATIVE = $08000000;
     BDAM_TEXTTYPE = $01000000;
     BDAM_TEXT_ORIENTATION = $00030000;
     BDAM_WND_ORIENTATION = $00100000;
     BDAM_NUMERALS = $00003000;
     BDAM_SYM_SWAP = $00000100;
     BDAM_WORD_BREAK = $00000200;
     BDAM_TEXT_SHAPE = $000000FF;
     BDAM_ALL=(BDAM_INIT or
   BDAM_LEVEL            or
   BDAM_TEXTTYPE         or
   BDAM_NATIVE           or
   BDAM_TEXT_ORIENTATION or
   BDAM_WND_ORIENTATION  or
   BDAM_NUMERALS         or
   BDAM_SYM_SWAP         or
   BDAM_WORD_BREAK       or
   BDAM_TEXT_SHAPE         );
    {                                                                       }
    { Bidirectional attributes values (in Bidi attributes word)             }
    {                                                                       }
    { Note: Value of the attributes must match the values in layout.h.      }
    {                                                                       }
       BDA_INIT = $80000000;
       BDA_LEVEL = $30000000;
       BDA_NATIVE_OFF = $00000000;
       BDA_NATIVE_ON = $08000000;
       BDA_TEXTTYPE_VISUAL = $00000000;
       BDA_TEXTTYPE_IMPLICIT = $01000000;
       BDA_WND_ORIENT_LTR = $00000000;
       BDA_WND_ORIENT_RTL = $00100000;
       BDA_TEXT_ORIENT_LTR = $00000000;
       BDA_TEXT_ORIENT_RTL = $00010000;
       BDA_TEXT_ORIENT_DYNAMIC = $00020000;
       BDA_TEXT_ORIENT_CONTEXT = $00020000;
       BDA_NUMERALS_NOMINAL = $00000000;
       BDA_NUMERALS_PASSTHRU = $00001000;
       BDA_NUMERALS_NATIONAL = $00002000;
       BDA_NUMERALS_CONTEXTUAL = $00003000;
       BDA_SYM_SWAP_OFF = $00000000;
       BDA_SYM_SWAP_ON = $00000100;
       BDA_WORD_BREAK_OFF = $00000000;
       BDA_WORD_BREAK_ON = $00000200;
       BDA_TEXT_DISPLAY_SHAPED = $00000000;
       BDA_TEXT_SAVE_SHAPED = $00000001;
       BDA_TEXT_NOMINAL = $00000010;
       BDA_TEXT_INITIAL = $00000011;
       BDA_TEXT_MIDDLE = $00000012;
       BDA_TEXT_FINAL = $00000013;
       BDA_TEXT_ISOLATED = $00000014;
    {                                                                       }
    { Bidirectional attribute (BIDIATTR) as specified in a window template  }
    { or in the resource script.                                            }
    {                                                                       }
type
       BIDIPARAM = PRESPARAMS;

const
    { First BidiAttr PresParam   }
       PP_BDATTR_FIRST = $100;
    {                            }
    { Set ALL Bidi attrs         }
       PP_BDATTR_ALL = $101;
    {                            }
    { Text/Data type             }
       PP_BDATTR_TEXTTYPE = $102;
    { Text Orientation           }
       PP_BDATTR_TEXT_ORIENTATION = $103;
    { Window Orientation         }
       PP_BDATTR_WND_ORIENTATION = $104;
    { Arabic/Hindi Numerals      }
       PP_BDATTR_NUMERALS = $105;
    { Symetric Swapping          }
       PP_BDATTR_SYM_SWAP = $106;
    { Word break                 }
       PP_BDATTR_WORD_BREAK = $107;
    { Char Shape Determination   }
       PP_BDATTR_TEXT_SHAPE = $108;
    {                            }
    { Last BidiAttr PresParam    }
       PP_BDATTR_LAST = $108;
    {                            }
    { Bidirectional status flags }
       PP_BDSTATUS = $110;
    {                                                                       }
    { Values used in Set/Query Bidirectional status word                    }
    {                                                                       }
       BDS_HKFLAG_ENG_LAYER = $00010000;
       BDS_HKFLAG_NAT_LAYER = $00020000;
       BDS_HKFLAG_PUSH = $00040000;
       BDS_HKFLAG_END_PUSH = $00080000;
       BDS_HKFLAG_AUTO_PUSH = $00100000;
       BDS_HKFLAG_FIELD_REV = $00200000;
       BDS_HKFLAG_SCREEN_REV = $00400000;
       BDS_HKFLAG_STATUS_INDICATOR = $02000000;
       BDS_HKFLAG_DISPLAY_SHAPED = $04000000;
       BDS_HKFLAG_INITIAL = $08000000;
       BDS_HKFLAG_MIDDLE = $10000000;
       BDS_HKFLAG_FINAL = $20000000;
       BDS_HKFLAG_ISOLATED = $40000000;
       BDS_HKFLAG_SAVE_SHAPED = $80000000;
       BDS_HKFLAG_ENTRY_SWITCH_UI = $00800000;
       BDS_FAUTOPUSH_RTL_ON = $00000001;
       BDS_FAUTOPUSH_LTR_ON = $00000002;
       BDS_FPUSH_ON = $00000004;
       BDS_DISABLE_INPUT_PROCESSING = $00000020;
    {                                                                       }
    { Bidirectional status masks                                            }
    {                                                                       }
       BDSM_HKFLAGS = $FE7F0000;
       BDSM_AUTOPUSH_RTL = $00000001;
       BDSM_AUTOPUSH_LTR = $00000002;
       BDSM_PUSH_ON = $00000004;
       BDSM_DISABLE_INPUT_PROCESSING = $00000020;
       BDSM_ALL     =     ( BDSM_HKFLAGS      or
                            BDSM_AUTOPUSH_RTL or
                            BDSM_AUTOPUSH_LTR or
                            BDSM_PUSH_ON      or
                            BDSM_DISABLE_INPUT_PROCESSING );
    {                                                                       }
    { Functions to Set/Query Bidirectional Langugage Information            }
    {                                                                       }
    { Process Bidi attributes     }
       LI_BD_PROCESS_ATTR = $00000010;
    { Process Bidi attributes     }
       LI_BD_PROCESS_STAT = $00000011;
    { Window Bidi Attributes      }
       LI_BD_WND_ATTR = $00000020;
    { Window Bidi Status          }
       LI_BD_WND_STAT = $00000021;
    { Clipboard Bidi Attributes   }
       LI_BD_CLIP_ATTR = $00000030;
    { Clipboard Conversion Bidi Attributes   }
       LI_BD_CLIP_CONV_ATTR = $00000031;
    { Values used in WinSet/QueryLangInfo (in flFlags);  }
    { No message is sent to the  }
       LIF_NO_SENDMSG = $00000001;
    { window to inform it of the }
    { change                     }
    { Send SetBidiAttr msg to    }
       LIF_CHILD_INHERIT = $00000002;
    { all child windows.         }
    { Refresh all windows whose  }
       LIF_WND_REFRESH = $00000004;
    { bidi attributes are set    }
    { directly by the API (and   }
    { not by a sent message.     }
    {                                                                       }
    { Flags for WinSetKbdLayer                                              }
    {                                                                       }
       SKLF_SENDMSG = $00000001;
    {                                                                       }
    { Keyboard layers for WinSetKbdLayer                                    }
    {                                                                       }
       KL_LATIN = $00000000;
       KL_NATIONAL = $00000001;
    {                                                                       }
    { Keyboard layouts for WinSetKbdLayout                                  }
    {                                                                       }
    { VKey                               }
       KBDL_VKEY = 1;
    { Belgium        - 120               }
       KBDL_BE = 2;
    { Canadian       - 058 French        }
       KBDL_CF = 3;
    { Denmark        - 159               }
       KBDL_DK = 4;
    { France         - 189               }
       KBDL_FR = 5;
    { Germany        - 129               }
       KBDL_GR = 6;
    { Germany        - 129               }
       KBDL_DE = 6;
    { Italy          - 141               }
       KBDL_IT = 7;
    { Latin America  - 171               }
       KBDL_LA = 8;
    { Netherlands    - 143               }
       KBDL_NL = 9;
    { Norway         - 155               }
       KBDL_NO = 10;
    { Portugal       - 163               }
       KBDL_PO = 11;
    { Swiss-French   - 150f              }
       KBDL_SF = 12;
    { Swiss-German   - 150d              }
       KBDL_SG = 13;
    { Spain          - 172               }
       KBDL_ES = 14;
    { Finland        - 153               }
       KBDL_FI = 15;
    { Sweden         - 153               }
       KBDL_SV = 16;
    { United Kingdom - 166               }
       KBDL_UK = 17;
    { United States  - 103p              }
       KBDL_US = 18;
    { French         - 120               }
       KBDL_FR120 = 19;
    { Italian        - 142               }
       KBDL_IT142 = 20;
    { United Kingdom - 168               }
       KBDL_UK168W = 21;
    { Turkey         - 179               }
       KBDL_TR = 22;
    { Czech          - 243               }
       KBDL_CZ = 23;
    { Slovakia       - 245               }
       KBDL_SK = 24;
    { Hungarian      - 208               }
       KBDL_HU = 25;
    { Croatia        - 234               }
       KBDL_HR = 26;
    { Poland         - 163               }
       KBDL_PL = 27;
    { Iceland        - 197               }
       KBDL_IS = 28;
    { Brazil         - 275               }
       KBDL_BR = 29;
    { Hebrew         - 212 Latin         }
       KBDL_HE_LATIN = 30;
    { Hebrew         - 212 Hebrew        }
       KBDL_HE_NATIONAL = 31;
    { Arabic         - 238 Latin         }
       KBDL_AR_LATIN = 32;
    { Arabic         - 238 Arabic        }
       KBDL_AR_NATIONAL = 33;
    { Brazil         - 274               }
       KBDL_BR274 = 34;
    { Greek          - 319 Latin         }
       KBDL_GK_LAT319 = 35;
    { Greek          - 319 Greek         }
       KBDL_GK_NAT319 = 36;
    { Greek          - 220 Latin         }
       KBDL_GK_LAT220 = 37;
    { Greek          - 220 Greek         }
       KBDL_GK_NAT220 = 38;
    { Arabic         - 470 Latin         }
       KBDL_AR_LAT470 = 39;
    { Arabic         - 470 Arabic        }
       KBDL_AR_NAT470 = 40;
    { Turkey         - 440               }
       KBDL_TR440 = 41;
    { Slovenia       - 234               }
       KBDL_SL = 42;
    { Romania        - 446               }
       KBDL_RO = 43;
    { Bulgaria       - 442 Cyrillic      }
       KBDL_BG_NATIONAL = 44;
    { Bulgaria       - 442 Latin         }
       KBDL_BG_LATIN = 45;
    { Macedonia      - 449 Cyrillic      }
       KBDL_MK_NATIONAL = 46;
    { Macedonia      - 449 Latin         }
       KBDL_MK_LATIN = 47;
    { Serbia         - 450 Cyrillic      }
       KBDL_SR_NATIONAL = 48;
    { Serbia         - 450 Latin         }
       KBDL_SR_LATIN = 49;
    { Russia         - 441 Cyrillic      }
       KBDL_RU_NATIONAL = 50;
    { Russia         - 441 Latin         }
       KBDL_RU_LATIN = 51;
    { Poland         - 274 Programmer    }
       KBDL_PL274 = 52;
    { Russia         - 443 Cyrillic      }
       KBDL_RU_NAT443 = 53;
    { Russia         - 443 Latin         }
       KBDL_RU_LAT443 = 54;
    { Bosnia         - 234               }
       KBDL_BA = 55;
    { Albania        - 452               }
       KBDL_SQ = 56;
    { International  - 103               }
       KBDL_US_INTER = 57;
    { Canadian       - 445               }
       KBDL_CA = 58;
    { Canadian       - 501               }
       KBDL_CA_EXTRA = 59;
    { German         - 453               }
       KBDL_DE453 = 60;
    { German         - 500               }
       KBDL_DE_EXTRA = 61;
    { Iceland        - 458               }
       KBDL_IS458 = 62;
    { Estonia        - 454               }
       KBDL_EE = 63;
    { Thai Kbd       - Latin for 874     }
       KBDL_TH_LATIN = 64;
    { Thai Kbd       - Pattachot for 874 }
       KBDL_TH_PAT = 65;
    { Thai Kbd                           }
       KBDL_TH_PAT_CAP = 66;
    { Thai Kbd       - Kesmanee for 874  }
       KBDL_TH_KES = 67;
    { Thai Kbd                           }
       KBDL_TH_KES_CAP = 68;
    { Thai Kbd       - Pattachot for 850 }
       KBDL_TH_COMP_PAT = 69;
    { Thai Kbd                           }
       KBDL_TH_COMP_PAT_CAP = 70;
    { Thai Kbd       - Kesmanee for 850  }
       KBDL_TH_COMP_KES = 71;
    { Thai Kbd                           }
       KBDL_TH_COMP_KES_CAP = 72;
    { US Dvorak      -                   }
       KBDL_US_DV = 73;
    { US Left        -                   }
       KBDL_US_LEFT = 74;
    { US Right       -                   }
       KBDL_US_RIGHT = 75;
    { Lithuania      - 456 - National    }
       KBDL_LTL = 76;
    { Lithuania      - 456 - Programmer  }
       KBDL_LTP = 77;
    { Latvia 455     - 455 - National    }
       KBDL_LVL = 78;
    { Latvia 455     - 455 - Programmer  }
       KBDL_LVP = 79;
    { Japan Latin                        }
       KBDL_JALPHANUMERIC = 80;
       KBDL_JP = 80;
    { Japan Katakana                     }
       KBDL_JKATAKANA = 81;
    { Japan Katakana Romanji             }
       KBDL_JKATAKANAROMAN = 82;
    { Japan Hiragana                     }
       KBDL_JHIRAGANA = 83;
    { Japan Hiragana Romanji             }
       KBDL_JHIRAGANAROMAN = 84;
       KBDL_JCAPSALPHANUMERIC = 85;
    { Korean                             }
       KBDL_KALPHANUMERIC = 86;
       KBDL_KR = 86;
    { Korean national layer              }
       KBDL_KJAMO = 87;
    { Simplified Chinese                 }
       KBDL_SALPHANUMERIC = 88;
    { Traditional Chinese                }
       KBDL_TALPHANUMERIC = 89;
       KBDL_TW = 89;
    { Belarus 463    - 463 - Latin       }
       KBDL_BYL = 90;
    { Belarus 463    - 463 - Cyrillic    }
       KBDL_BYC = 91;
    { Ukraine 465    - 465 - Latin       }
       KBDL_UAL = 92;
    { Ukraine 465    - 465 - Ukraine     }
       KBDL_UAU = 93;
    {                                                                       }
    { Defines for use in WinQueryCpType                                     }
    {                                                                       }
    { Latin 1  }
       CPTYPE_OTHER = 1;
       CPTYPE_ARABIC = 2;
       CPTYPE_BALTIC = 3;
       CPTYPE_CYRILLIC = 4;
       CPTYPE_GREEK = 5;
       CPTYPE_HEBREW = 6;
       CPTYPE_JAPANESE = 7;
       CPTYPE_KOREAN = 8;
       CPTYPE_LATIN2 = 9;
       CPTYPE_SCHINESE = 10;
       CPTYPE_TCHINESE = 11;
       CPTYPE_THAI = 12;
       CPTYPE_TURKISH = 13;
       CPTYPE_UNICODE = 14;
    { was #define dname def_expr }
    function HMQ_SYSTEM : THMQ;

    {                                                                       }
    { PM Bidirectional support - function prototypes.                       }
    {                                                                       }

Function WinSetLangInfo(aHWND: HWND; ulEffect, ulData, flMask, flFlags,
                        ulReserved: Cardinal ): Cardinal; cdecl;
    external 'pmbidi' index 20;

Function WinQueryLangInfo(ahwnd: HWND; ulEffect, flFlags, ulReserved: Cardinal): Cardinal; cdecl;
    external 'pmbidi' index 21;

Function WinSetKbdLayer(ahwnd: HWND; idKbdLayer, flFlags: Cardinal): Cardinal; cdecl;
    external 'pmbidi' index 22;

Function WinQueryKbdLayer(ahwnd: HWND): Cardinal; cdecl;
    external 'pmbidi' index 23;

Function WinQueryKbdLayout(hwndDesktop: HWND): Cardinal; cdecl;
    external 'pmbidi' index 23;

//Function WinSetKbdLayout(hwndDesktop: HWND; idKbdLayout: Cardinal): Longbool; cdecl;
//    external '???';

Function WinSetLangViewer(ahab, hwndLangViewer: HAB; Codepage: Cardinal): HWND; cdecl;
    external 'pmbidi' index 24;

Function WinQueryLangViewer(ahab: HAB; Codepage: Cardinal): HWND; cdecl;
    external 'pmbidi' index 25;

Function GpiSetBidiAttr(ahps: HPS; BidiAttr: Cardinal): Cardinal; cdecl;
    external 'pmbidi' index 50;

Function GpiQueryBidiAttr(ahps: HPS): Cardinal; cdecl;
    external 'pmbidi' index 51;

Function WinQueryCpType(ahmq: HMQ): Cardinal; cdecl;
    external 'pmbidi' index 60;

    {                                                                            }
    {  Macros to manipulate Bidi values                                          }
    {                                                                            }
    {
         Macro to make a BidiAttribute/Status ULONG from several fields

         Example :      SET_BD_VALUE(BidiAtts,
                                     BDA_TEXT_ORIENT_RTL   | BDA_TEXTTYPE_IMPLICIT,
                                     BDAM_TEXT_ORIENTATION | BDAM_TEXTTYPE)

        }
//   #define SET_BD_VALUE(BidiValue,NewBidiValue,Mask) \
//   (BidiValue = ((BidiValue & (~(Mask))) | (NewBidiValue & (Mask))))

    {
         Macro to extract one or more fields from a BidiAttribute/Status ULONG

         Example :      Orientation = QUERY_BD_VALUE(BidiAtts,BDAM_TEXT_ORIENTATION)

        }
//   ( ((ULONG)BidiValue) & ((ULONG)Mask) )
const
    {                                                                       }
    { New CURSOR flags for Left-To-Right and Right-To-Left cursors          }
    { These are in addtion to those in the CURSOR section in PMWIN.H        }
    {                                                                       }
       CURSOR_DIR_LTR = $0100;
       CURSOR_DIR_RTL = $0300;
    {                                                                            }
    {  PM Bidi Error codes.                                                      }
    {                                                                            }
       PMERR_BIDI_FIRST = $10F0;
       PMERR_BIDI_TEXT_CONV_FAILED = $10F0;
       PMERR_BIDI_LAST = $10FF;
    {                                                                            }
    { Bidi virtual key definitions                                               }
    {                                                                            }
       VK_BIDI_FIRST = $E0;
       VK_START_PUSH = $E0;
       VK_END_PUSH = $E1;
       VK_REVERSE_FIELD = $E2;
       VK_REVERSE_WINDOW = $E3;
       VK_AUTOPUSH = $E4;
       VK_STATUS_INDICATOR = $E5;
       VK_TEXT_DISPLAY_SHAPED = $E6;
       VK_TEXT_INITIAL = $E7;
       VK_TEXT_MIDDLE = $E8;
       VK_TEXT_FINAL = $E9;
       VK_TEXT_ISOLATED = $EA;
       VK_TEXT_SAVE_SHAPED = $EB;
       VK_REQUIRED_SPACE = $EC;
       VK_LTR_MARKER = $ED;
       VK_RTL_MARKER = $EE;
       VK_ENTRY_SWITCH_UI = $EF;
       VK_LAYER0 = $F0;
       VK_LAYER1 = $F1;
       VK_LAYER2 = $F2;
       VK_LAYER3 = $F3;
       VK_LATIN_LAYER = VK_LAYER0;
       VK_NATIONAL_LAYER = VK_LAYER1;
       VK_BIDI_LAST = $FF;
    {                                                                       }
    {  Language-sensitive definition for standard File and Font dialogs.    }
    {                                                                       }
    { Use National Language  }

    const
       FNTS_NATIONAL_LANGUAGE = $80000000;
    { Use National Language  }
       FDS_NATIONAL_LANGUAGE = $80000000;
    {                                                                       }
    {  WM_ messages related to bidirectional language support for           }
    {      CUATOOLS components.                                             }
    {                                                                       }
       CM_SETITEMBIDIATTR = $0390;
       CM_SETFIELDBIDIATTR = $0391;
       CM_QUERYITEMBIDIATTR = $0392;
       CM_QUERYFIELDBIDIATTR = $0393;
       BKM_SETSTATUSLINEBIDIATTR = $0394;
       BKM_QUERYSTATUSLINEBIDIATTR = $0395;
       BKM_SETTABTEXTBIDIATTR = $0396;
       BKM_QUERYTABTEXTBIDIATTR = $0397;
       VM_SETITEMBIDIATTR = $0398;
       VM_QUERYITEMBIDIATTR = $0399;
    { bam  }
    { Bidirectional Attributes word  }
    { Bitmask to define which bidi   }

    type

       TBD_ATTR_MASK = record
            ulBdAttr : Cardinal;
            ulBdMask : Cardinal;
         end;
    { attributes are to be used.     }

       PBD_ATTR_MASK = ^TBD_ATTR_MASK;
    {                                                                     }
    { BOOKPAGEBIDIINFO structure is pointed to from the BOOKPAGEINFO      }
    { structure (BKM_SETPAGEINFO notebook message)                        }
    {                                                                     }
    { bkpgbdi  }
    { Major Tab Text BD_ATTR_MASK struct.    }
    { Minor Tab Text BD_ATTR_MASK struct.    }
    { Status Line Text BD_ATTR_MASK struct.  }

       TBOOKPAGEBIDIINFO = record
            bamMajorTab : TBD_ATTR_MASK;
            bamMinorTab : TBD_ATTR_MASK;
            bamStatusLine : TBD_ATTR_MASK;
         end;

       PBOOKPAGEBIDIINFO = ^TBOOKPAGEBIDIINFO;

implementation

    { was #define dname def_expr }
    function HMQ_SYSTEM : THMQ;
        begin
           HMQ_SYSTEM:=THMQ(0);
        end;


end.
