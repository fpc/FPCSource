(*
 * XFree86 vendor specific keysyms.
 *
 * The XFree86 keysym range is 0x10080001 - 0x1008FFFF.
 *
 * X.Org will not be adding to the XF86 set of keysyms, though they have
 * been adopted and are considered a "standard" part of X keysym definitions.
 * XFree86 never properly commented these keysyms, so we have done our
 * best to explain the semantic meaning of these keys.
 *
 * XFree86 has removed their mail archives of the period, that might have
 * shed more light on some of these definitions. Until/unless we resurrect
 * these archives, these are from memory and usage.
 *)

unit xf86keysym;

interface

(*
 * ModeLock
 *
 * This one is old, and not really used any more since XKB offers this
 * functionality.
 *)
const
  XF86XK_ModeLock          = $1008FF01;   { Mode Switch Lock }

{ Backlight controls. }
  XF86XK_MonBrightnessUp   = $1008FF02;   { Monitor/panel brightness }
  XF86XK_MonBrightnessDown = $1008FF03;   { Monitor/panel brightness }
  XF86XK_KbdLightOnOff     = $1008FF04;   { Keyboards may be lit     }
  XF86XK_KbdBrightnessUp   = $1008FF05;   { Keyboards may be lit     }
  XF86XK_KbdBrightnessDown = $1008FF06;   { Keyboards may be lit     }

{*
 * Keys found on some "Internet" keyboards.
 *}
  XF86XK_Standby           = $1008FF10;   { System into standby mode   }
  XF86XK_AudioLowerVolume  = $1008FF11;   { Volume control down        }
  XF86XK_AudioMute         = $1008FF12;   { Mute sound from the system }
  XF86XK_AudioRaiseVolume  = $1008FF13;   { Volume control up          }
  XF86XK_AudioPlay         = $1008FF14;   { Start playing of audio >   }
  XF86XK_AudioStop         = $1008FF15;   { Stop playing audio         }
  XF86XK_AudioPrev         = $1008FF16;   { Previous track             }
  XF86XK_AudioNext         = $1008FF17;   { Next track                 }
  XF86XK_HomePage          = $1008FF18;   { Display user's home page   }
  XF86XK_Mail              = $1008FF19;   { Invoke user's mail program }
  XF86XK_Start             = $1008FF1A;   { Start application          }
  XF86XK_Search            = $1008FF1B;   { Search                     }
  XF86XK_AudioRecord       = $1008FF1C;   { Record audio application   }

{ These are sometimes found on PDA's (e.g. Palm, PocketPC or elsewhere)   }
  XF86XK_Calculator        = $1008FF1D;   { Invoke calculator program  }
  XF86XK_Memo              = $1008FF1E;   { Invoke Memo taking program }
  XF86XK_ToDoList          = $1008FF1F;   { Invoke To Do List program  }
  XF86XK_Calendar          = $1008FF20;   { Invoke Calendar program    }
  XF86XK_PowerDown         = $1008FF21;   { Deep sleep the system      }
  XF86XK_ContrastAdjust    = $1008FF22;   { Adjust screen contrast     }
  XF86XK_RockerUp          = $1008FF23;   { Rocker switches exist up   }
  XF86XK_RockerDown        = $1008FF24;   { and down                   }
  XF86XK_RockerEnter       = $1008FF25;   { and let you press them     }

{ Some more "Internet" keyboard symbols }
  XF86XK_Back              = $1008FF26;   { Like back on a browser     }
  XF86XK_Forward           = $1008FF27;   { Like forward on a browser  }
  XF86XK_Stop              = $1008FF28;   { Stop current operation     }
  XF86XK_Refresh           = $1008FF29;   { Refresh the page           }
  XF86XK_PowerOff          = $1008FF2A;   { Power off system entirely  }
  XF86XK_WakeUp            = $1008FF2B;   { Wake up system from sleep  }
  XF86XK_Eject             = $1008FF2C;   { Eject device (e.g. DVD)    }
  XF86XK_ScreenSaver       = $1008FF2D;   { Invoke screensaver         }
  XF86XK_WWW               = $1008FF2E;   { Invoke web browser         }
  XF86XK_Sleep             = $1008FF2F;   { Put system to sleep        }
  XF86XK_Favorites         = $1008FF30;   { Show favorite locations    }
  XF86XK_AudioPause        = $1008FF31;   { Pause audio playing        }
  XF86XK_AudioMedia        = $1008FF32;   { Launch media collection app }
  XF86XK_MyComputer        = $1008FF33;   { Display "My Computer" window }
  XF86XK_VendorHome        = $1008FF34;   { Display vendor home web site }
  XF86XK_LightBulb         = $1008FF35;   { Light bulb keys exist       }
  XF86XK_Shop              = $1008FF36;   { Display shopping web site   }
  XF86XK_History           = $1008FF37;   { Show history of web surfing }
  XF86XK_OpenURL           = $1008FF38;   { Open selected URL           }
  XF86XK_AddFavorite       = $1008FF39;   { Add URL to favorites list   }
  XF86XK_HotLinks          = $1008FF3A;   { Show "hot" links            }
  XF86XK_BrightnessAdjust  = $1008FF3B;   { Invoke brightness adj. UI   }
  XF86XK_Finance           = $1008FF3C;   { Display financial site      }
  XF86XK_Community         = $1008FF3D;   { Display user's community    }
  XF86XK_AudioRewind       = $1008FF3E;   { "rewind" audio track        }
  XF86XK_BackForward       = $1008FF3F;   { ??? }
  XF86XK_Launch0           = $1008FF40;   { Launch Application          }
  XF86XK_Launch1           = $1008FF41;   { Launch Application          }
  XF86XK_Launch2           = $1008FF42;   { Launch Application          }
  XF86XK_Launch3           = $1008FF43;   { Launch Application          }
  XF86XK_Launch4           = $1008FF44;   { Launch Application          }
  XF86XK_Launch5           = $1008FF45;   { Launch Application          }
  XF86XK_Launch6           = $1008FF46;   { Launch Application          }
  XF86XK_Launch7           = $1008FF47;   { Launch Application          }
  XF86XK_Launch8           = $1008FF48;   { Launch Application          }
  XF86XK_Launch9           = $1008FF49;   { Launch Application          }
  XF86XK_LaunchA           = $1008FF4A;   { Launch Application          }
  XF86XK_LaunchB           = $1008FF4B;   { Launch Application          }
  XF86XK_LaunchC           = $1008FF4C;   { Launch Application          }
  XF86XK_LaunchD           = $1008FF4D;   { Launch Application          }
  XF86XK_LaunchE           = $1008FF4E;   { Launch Application          }
  XF86XK_LaunchF           = $1008FF4F;   { Launch Application          }

  XF86XK_ApplicationLeft   = $1008FF50;   { switch to application, left }
  XF86XK_ApplicationRight  = $1008FF51;   { switch to application, right}
  XF86XK_Book              = $1008FF52;   { Launch bookreader           }
  XF86XK_CD                = $1008FF53;   { Launch CD/DVD player        }
  XF86XK_Calculater        = $1008FF54;   { Launch Calculater           }
  XF86XK_Clear             = $1008FF55;   { Clear window, screen        }
  XF86XK_Close             = $1008FF56;   { Close window                }
  XF86XK_Copy              = $1008FF57;   { Copy selection              }
  XF86XK_Cut               = $1008FF58;   { Cut selection               }
  XF86XK_Display           = $1008FF59;   { Output switch key           }
  XF86XK_DOS               = $1008FF5A;   { Launch DOS (emulation)      }
  XF86XK_Documents         = $1008FF5B;   { Open documents window       }
  XF86XK_Excel             = $1008FF5C;   { Launch spread sheet         }
  XF86XK_Explorer          = $1008FF5D;   { Launch file explorer        }
  XF86XK_Game              = $1008FF5E;   { Launch game                 }
  XF86XK_Go                = $1008FF5F;   { Go to URL                   }
  XF86XK_iTouch            = $1008FF60;   { Logitch iTouch- don't use   }
  XF86XK_LogOff            = $1008FF61;   { Log off system              }
  XF86XK_Market            = $1008FF62;   { ??                          }
  XF86XK_Meeting           = $1008FF63;   { enter meeting in calendar   }
  XF86XK_MenuKB            = $1008FF65;   { distingush keyboard from PB }
  XF86XK_MenuPB            = $1008FF66;   { distinuish PB from keyboard }
  XF86XK_MySites           = $1008FF67;   { Favourites                  }
  XF86XK_New               = $1008FF68;   { New (folder, document...    }
  XF86XK_News              = $1008FF69;   { News                        }
  XF86XK_OfficeHome        = $1008FF6A;   { Office home (old Staroffice)}
  XF86XK_Open              = $1008FF6B;   { Open                        }
  XF86XK_Option            = $1008FF6C;   { ?? }
  XF86XK_Paste             = $1008FF6D;   { Paste                       }
  XF86XK_Phone             = $1008FF6E;   { Launch phone; dial number   }
  XF86XK_Q                 = $1008FF70;   { Compaq's Q - don't use      }
  XF86XK_Reply             = $1008FF72;   { Reply e.g., mail            }
  XF86XK_Reload            = $1008FF73;   { Reload web page, file, etc. }
  XF86XK_RotateWindows     = $1008FF74;   { Rotate windows e.g. xrandr  }
  XF86XK_RotationPB        = $1008FF75;   { don't use                   }
  XF86XK_RotationKB        = $1008FF76;   { don't use                   }
  XF86XK_Save              = $1008FF77;   { Save (file, document, state }
  XF86XK_ScrollUp          = $1008FF78;   { Scroll window/contents up   }
  XF86XK_ScrollDown        = $1008FF79;   { Scrool window/contentd down }
  XF86XK_ScrollClick       = $1008FF7A;   { Use XKB mousekeys instead   }
  XF86XK_Send              = $1008FF7B;   { Send mail, file, object     }
  XF86XK_Spell             = $1008FF7C;   { Spell checker               }
  XF86XK_SplitScreen       = $1008FF7D;   { Split window or screen      }
  XF86XK_Support           = $1008FF7E;   { Get support (??)            }
  XF86XK_TaskPane          = $1008FF7F;   { Show tasks }
  XF86XK_Terminal          = $1008FF80;   { Launch terminal emulator    }
  XF86XK_Tools             = $1008FF81;   { toolbox of desktop/app.     }
  XF86XK_Travel            = $1008FF82;   { ?? }
  XF86XK_UserPB            = $1008FF84;   { ?? }
  XF86XK_User1KB           = $1008FF85;   { ?? }
  XF86XK_User2KB           = $1008FF86;   { ?? }
  XF86XK_Video             = $1008FF87;   { Launch video player       }
  XF86XK_WheelButton       = $1008FF88;   { button from a mouse wheel }
  XF86XK_Word              = $1008FF89;   { Launch word processor     }
  XF86XK_Xfer              = $1008FF8A;
  XF86XK_ZoomIn            = $1008FF8B;   { zoom in view, map, etc.   }
  XF86XK_ZoomOut           = $1008FF8C;   { zoom out view, map, etc.  }

  XF86XK_Away              = $1008FF8D;   { mark yourself as away     }
  XF86XK_Messenger         = $1008FF8E;   { as in instant messaging   }
  XF86XK_WebCam            = $1008FF8F;   { Launch web camera app.    }
  XF86XK_MailForward       = $1008FF90;   { Forward in mail           }
  XF86XK_Pictures          = $1008FF91;   { Show pictures             }
  XF86XK_Music             = $1008FF92;   { Launch music application  }

  XF86XK_Battery           = $1008FF93;   { Display battery information }
  XF86XK_Bluetooth         = $1008FF94;   { Enable/disable Bluetooth    }
  XF86XK_WLAN              = $1008FF95;   { Enable/disable WLAN         }
  XF86XK_UWB               = $1008FF96;   { Enable/disable UWB          }

  XF86XK_AudioForward      = $1008FF97;   { fast-forward audio track    }
  XF86XK_AudioRepeat       = $1008FF98;   { toggle repeat mode          }
  XF86XK_AudioRandomPlay   = $1008FF99;   { toggle shuffle mode         }
  XF86XK_Subtitle          = $1008FF9A;   { cycle through subtitle      }
  XF86XK_AudioCycleTrack   = $1008FF9B;   { cycle through audio tracks  }
  XF86XK_CycleAngle        = $1008FF9C;   { cycle through angles        }
  XF86XK_FrameBack         = $1008FF9D;   { video: go one frame back    }
  XF86XK_FrameForward      = $1008FF9E;   { video: go one frame forward }
  XF86XK_Time              = $1008FF9F;   { display, or shows an entry for time seeking }
  XF86XK_Select            = $1008FFA0;   { Select button on joypads and remotes }
  XF86XK_View              = $1008FFA1;   { Show a view options/properties }
  XF86XK_TopMenu           = $1008FFA2;   { Go to a top-level menu in a video }

  XF86XK_Red               = $1008FFA3;   { Red button                  }
  XF86XK_Green             = $1008FFA4;   { Green button                }
  XF86XK_Yellow            = $1008FFA5;   { Yellow button               }
  XF86XK_Blue              = $1008FFA6;   { Blue button                 }

  XF86XK_Suspend           = $1008FFA7;   { Sleep to RAM                }
  XF86XK_Hibernate         = $1008FFA8;   { Sleep to disk               }
  XF86XK_TouchpadToggle    = $1008FFA9;   { Toggle between touchpad/trackstick }
  XF86XK_TouchpadOn        = $1008FFB0;   { The touchpad got switched on }
  XF86XK_TouchpadOff       = $1008FFB1;   { The touchpad got switched off }

  XF86XK_AudioMicMute      = $1008FFB2;   { Mute the Mic from the system }

{ Keys for special action keys (hot keys) }
{ Virtual terminals on some operating systems }
  XF86XK_Switch_VT_1       = $1008FE01;
  XF86XK_Switch_VT_2       = $1008FE02;
  XF86XK_Switch_VT_3       = $1008FE03;
  XF86XK_Switch_VT_4       = $1008FE04;
  XF86XK_Switch_VT_5       = $1008FE05;
  XF86XK_Switch_VT_6       = $1008FE06;
  XF86XK_Switch_VT_7       = $1008FE07;
  XF86XK_Switch_VT_8       = $1008FE08;
  XF86XK_Switch_VT_9       = $1008FE09;
  XF86XK_Switch_VT_10      = $1008FE0A;
  XF86XK_Switch_VT_11      = $1008FE0B;
  XF86XK_Switch_VT_12      = $1008FE0C;

  XF86XK_Ungrab            = $1008FE20;   { force ungrab               }
  XF86XK_ClearGrab         = $1008FE21;   { kill application with grab }
  XF86XK_Next_VMode        = $1008FE22;   { next video mode available  }
  XF86XK_Prev_VMode        = $1008FE23;   { prev. video mode available }
  XF86XK_LogWindowTree     = $1008FE24;   { print window tree to log   }
  XF86XK_LogGrabInfo       = $1008FE25;   { print all active grabs to log }

implementation
end.
