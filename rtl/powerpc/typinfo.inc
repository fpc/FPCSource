{
    $Id$
    This file is part of the Free Pascal run time library.

    Copyright (c) 2001 by Jonas Maebe,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This unit provides the same Functionality as the TypInfo Unit }
{ of Delphi                                                     }
{ ---------------------------------------------------------------------
  This include contains cpu-specific Low-level calling of methods.
  ---------------------------------------------------------------------}

Function CallIntegerFunc(s: Pointer; Address: Pointer; Index, IValue: LongInt): Int64; assembler;
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     r3-r4: result  }
  asm
     { save current return address }
     mflr      r30
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r6
     bctrl
     { restore return address }
     mtlr      r30
  end;

Function CallIntegerProc(s : Pointer;Address : Pointer;Value : Integer; INdex,IValue : Longint) : Integer;assembler;
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     r3: result     }
  asm
     { save current return address }
     mflr      r30
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r6
     bctrl
     { restore return address }
     mtlr      r30
  end;

Function CallSingleFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Single;assembler;
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     fr1: result    }
  asm
     { save current return address }
     mflr      r30
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r6
     bctrl
     { restore return address }
     mtlr      r30
  end;

Function CallDoubleFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Double;assembler;
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     fr1: result    }
  asm
     { save current return address }
     mflr      r30
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r6
     bctrl
     { restore return address }
     mtlr      r30
  end;

Function CallExtendedFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Extended;assembler;
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     fr1: result    }
  asm
     { save current return address }
     mflr      r30
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r6
     bctrl
     { restore return address }
     mtlr      r30
  end;

Function CallExtendedProc(s : Pointer;Address : Pointer;Value : Extended; Index,IVAlue : Longint) : Integer;assembler;
  { input:              }
  {     r3:  s          }
  {     r4:  address    }
  {     fr3: value      }
  {     r7:  index      }
  {     r8:  ivalue     }
  { output:             }
  {     r3: result      }
  asm
     { save current return address }
     mflr      r30
     cmpli     r7,0
     mtctr     r4
     beq       LNoIndex
     fmr       fr2,fr3
LNoIndex:
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r8
     bctrl
     { restore return address }
     mtlr      r30
  end;

Function CallBooleanFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Boolean;assembler;
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     r3: result     }
  asm
     { save current return address }
     mflr      r30
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                       }
     mr        r4,r6
     bctrl
     { restore return address }
     mtlr      r30
  end;

Procedure CallSStringFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint;
                        Var Res: Shortstring);assembler;
  { input:                                       }
  {     r3: address of shortstring result (temp) }
  {     r4: s                                    }
  {     r5: address                              }
  {     r6: index                                }
  {     r7: ivalue                               }
  {     r8: res                                  }
  { output:                                      }
  {     none                                     }
  asm
     { save current return address }
     mflr      r30
     mtctr     r5
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                       }
     mr        r5,r7
     bctrl
     { restore return address }
     mtlr      r30
  end;

Procedure CallSStringProc(s : Pointer;Address : Pointer;Const Value : ShortString; INdex,IVAlue : Longint);assembler;
  { input:                                 }
  {     r3: s                              }
  {     r4: address                        }
  {     r5: value (address of shortstring) }
  {     r6: index                          }
  {     r7: ivalue                         }
  { output:                                }
  {     none                               }
  asm
     { save current return address }
     mflr      r30
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                       }
     mr        r4,r6
     bctrl
     { restore return address }
     mtlr      r30
  end;

{
  $Log$
  Revision 1.5  2003-04-24 11:47:21  florian
    * fixed wrong newlines

  Revision 1.4  2003/04/23 22:47:13  florian
    * fixed compilation

  Revision 1.3  2003/04/23 22:19:21  peter
    * sstringfunc para updated

  Revision 1.2  2002/09/07 16:01:26  peter
    * old logs removed and tabs fixed

}
