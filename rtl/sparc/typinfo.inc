{
    $Id$
    This file is part of the Free Pascal run time library.

    Copyright (c) 2003 by Peter Vreman,
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
asm
  {$warning FIXME}
end;
(*
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     r3-r4: result  }
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
*)

Function CallIntegerProc(s : Pointer;Address : Pointer;Value : Integer; Index,IValue : Longint) : Integer;assembler;
asm
  {$warning FIXME}
end;
(*
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     r3: result     }
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
*)


Function CallSingleFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Single;assembler;
asm
  {$warning FIXME}
end;
(*
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     fr1: result    }
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
*)


Function CallDoubleFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Double;assembler;
asm
  {$warning FIXME}
end;
(*
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     fr1: result    }
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
*)


Function CallExtendedFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Extended;assembler;
asm
  {$warning FIXME}
end;
(*
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     fr1: result    }
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
*)


Function CallBooleanFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Boolean;assembler;
asm
  {$warning FIXME}
end;
(*
  { input:             }
  {     r3: s          }
  {     r4: address    }
  {     r5: index      }
  {     r6: ivalue     }
  { output:            }
  {     r3: result     }
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
*)


Procedure CallSStringFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint;
                        Var Res: Shortstring);assembler;
asm
  {$warning FIXME}
end;
(*
  { input:                                       }
  {     r3: address of shortstring result (temp) }
  {     r4: s                                    }
  {     r5: address                              }
  {     r6: index                                }
  {     r7: ivalue                               }
  {     r8: res                                  }
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
*)


Procedure CallSStringProc(s : Pointer;Address : Pointer;Const Value : ShortString; INdex,IVAlue : Longint);assembler;
asm
  {$warning FIXME}
end;
(*
  { input:                                 }
  {     r3: s                              }
  {     r4: address                        }
  {     r5: value (address of shortstring) }
  {     r6: index                          }
  {     r7: ivalue                         }
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
*)

{
  $Log$
  Revision 1.1  2003-09-01 20:46:32  peter
    * new dummies

}
