{
    $Id$
    This file is part of the Free Pascal run time library.

    Copyright (c) 2003 by Florian Klaempfl,
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
  {     a1: s          }
  {     a2: address    }
  {     a2: index      }
  {     a3: ivalue     }
  { output:            }
  {     a1-a2: result  }
  var
    oldlr: pointer;
  asm
     { save current return address }
     mflr      r0
     stw       r0,oldlr
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r6
     bctrl
     { restore return address }
     lwz       r0,oldlr
     mtlr      r0
  end;

Function CallIntegerProc(s : Pointer;Address : Pointer;Value : Integer; Index,IValue : Longint) : Integer;assembler;
  { input:             }
  {     a1: s          }
  {     a2: address    }
  {     a3: index      }
  {     a4: ivalue     }
  { output:            }
  {     a1: result     }
  var
    oldlr: pointer;
  asm
     { save current return address }
     mflr      r0
     stw       r0,oldlr
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r6
     bctrl
     { restore return address }
     lwz       r0,oldlr
     mtlr      r0
  end;

Function CallSingleFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Single;assembler;
  { input:             }
  {     a1: s          }
  {     a2: address    }
  {     a3: index      }
  {     a4: ivalue     }
  { output:            }
  {     f0: result    }
  var
    oldlr: pointer;
  asm
     { save current return address }
     mflr      r0
     stw       r0,oldlr
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r6
     bctrl
     { restore return address }
     lwz       r0,oldlr
     mtlr      r0
  end;

Function CallDoubleFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Double;assembler;
  { input:             }
  {     a1: s          }
  {     a2: address    }
  {     a3: index      }
  {     a4: ivalue     }
  { output:            }
  {     f0: result    }
  var
    oldlr: pointer;
  asm
     { save current return address }
     mflr      r0
     stw       r0,oldlr
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r6
     bctrl
     { restore return address }
     lwz       r0,oldlr
     mtlr      r0
  end;

Function CallExtendedFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Extended;assembler;
  { input:             }
  {     a1: s          }
  {     a2: address    }
  {     a3: index      }
  {     a4: ivalue     }
  { output:            }
  {     f0: result    }
  var
    oldlr: pointer;
  asm
     { save current return address }
     mflr      r0
     stw       r0,oldlr
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                      }
     mr        r4,r6
     bctrl
     { restore return address }
     lwz       r0,oldlr
     mtlr      r0
  end;

Function CallBooleanFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Boolean;assembler;
  { input:             }
  {     a1: s          }
  {     a2: address    }
  {     a3: index      }
  {     a4: ivalue     }
  { output:            }
  {     a1: result     }
  var
    oldlr: pointer;
  asm
     { save current return address }
     mflr      r0
     stw       r0,oldlr
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                       }
     mr        r4,r6
     bctrl
     { restore return address }
     lwz       r0, oldlr
     mtlr      r0
  end;

Procedure CallSStringFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint;
                        Var Res: Shortstring);assembler;
  { input:                                       }
  {     a1: address of shortstring result (temp) }
  {     a2: s                                    }
  {     a3: address                              }
  {     a4: index                                }
  {     stack: ivalue                            }
  {     stack: res                               }
  { output:                                      }
  {     none                                     }
  var
    oldlr: pointer;
  asm
     { save current return address }
     mflr      r0
     stw       r0,oldlr
     mtctr     r5
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                       }
     mr        r5,r7
     bctrl
     { restore return address }
     lwz       r0,oldlr
     mtlr      r0
  end;


Procedure CallSStringProc(s : Pointer;Address : Pointer;Const Value : ShortString; INdex,IVAlue : Longint);assembler;
  { input:                                 }
  {     a1: s                              }
  {     a2: address                        }
  {     a3: value (address of shortstring) }
  {     a4: index                          }
  {     stack: ivalue                      }
  { output:                                }
  {     none                               }
  var
    oldlr: pointer;
  asm
     { save current return address }
     mflr      r0
     stw       r0,oldlr
     mtctr     r4
     { always pass ivalue as second parameter, it doesn't matter if it }
     { isn't used                                                       }
     mr        r4,r6
     bctrl
     { restore return address }
     lwz       r0,oldlr
     mtlr      r0
  end;

{
  $Log$
  Revision 1.1  2003-11-30 19:48:20  florian
    * fixed some arm stuff
}
