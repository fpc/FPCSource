{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2010  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

type
  { object handles }
  TPTC_KEY     = Pointer; { equivalent to Object Pascal TPTCKeyEvent    }
  TPTC_AREA    = Pointer; { equivalent to Object Pascal TPTCArea        }
  TPTC_MODE    = Pointer; { equivalent to Object Pascal TPTCMode        }
  TPTC_COPY    = Pointer; { equivalent to Object Pascal TPTCCopy        }
  TPTC_CLEAR   = Pointer; { equivalent to Object Pascal TPTCClear       }
  TPTC_TIMER   = Pointer; { equivalent to Object Pascal TPTCTimer       }
  TPTC_ERROR   = Pointer; { equivalent to Object Pascal TPTCError       }
  TPTC_COLOR   = Pointer; { equivalent to Object Pascal TPTCColor       }
  TPTC_FORMAT  = Pointer; { equivalent to Object Pascal TPTCFormat      }
  TPTC_PALETTE = Pointer; { equivalent to Object Pascal TPTCPalette     }
  TPTC_SURFACE = Pointer; { equivalent to Object Pascal TPTCBaseSurface }
  TPTC_CONSOLE = Pointer; { equivalent to Object Pascal TPTCBaseConsole }
