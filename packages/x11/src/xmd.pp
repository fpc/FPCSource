(***********************************************************

Copyright 1987, 1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from The Open Group.


Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Digital not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************)

{$IFNDEF FPC_DOTTEDUNITS}
unit xmd;
{$ENDIF FPC_DOTTEDUNITS}

interface

(*
 *  Xmd.h: MACHINE DEPENDENT DECLARATIONS.
 *)

(*
 * Bitfield suffixes for the protocol structure elements, if you
 * need them.  Note that bitfields are not guaranteed to be signed
 * (or even unsigned) according to ANSI C.
 *)
//# define B32 /* bitfield not needed on architectures with native 32-bit type */
//# define B16 /* bitfield not needed on architectures with native 16-bit type */
type
  PPCARD64 = ^PCARD64;
  PCARD64 = ^CARD64;
  CARD64 = UInt64;

  PPCARD32 = ^PCARD32;
  PCARD32 = ^CARD32;
  CARD32 = UInt32;

  PPCARD16 = ^PCARD16;
  PCARD16 = ^CARD16;
  CARD16 = UInt16;

  PPCARD8 = ^PCARD8;
  PCARD8 = ^CARD8;
  CARD8 = UInt8;

(*
 * was definitions for sign-extending bitfields on architectures without
 * native types smaller than 64-bit, now just backwards compatibility
 *)
//# define cvtINT8toInt(val) (val)
//# define cvtINT16toInt(val) (val)
//# define cvtINT32toInt(val) (val)
//# define cvtINT8toShort(val) (val)
//# define cvtINT16toShort(val) (val)
//# define cvtINT32toShort(val) (val)
//# define cvtINT8toLong(val) (val)
//# define cvtINT16toLong(val) (val)
//# define cvtINT32toLong(val) (val)

(*
 * this version should leave result of type (t * ), but that should only be
 * used when not in MUSTCOPY
 *)
//# define NEXTPTR(p,t) (((t *)(p)) + 1)

implementation
end.
