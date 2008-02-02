{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{ $TOG: poly.h /main/5 1998/02/06 17:47:27 kaleb $  }
{

Copyright 1987, 1998  The Open Group

All Rights Reserved.

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

                                                                        }
{
       This file contains a few macros to help track
       the edge of a filled anObject.  The anObject is assumed
       to be filled in scanline order, and thus the
       algorithm used is an extension of Bresenham's line
       drawing algorithm which assumes that y is always the
       major axis.
       Since these pieces of code are the same for any filled shape,
       it is more convenient to gather the library in one
       place, but since these pieces of code are also in
       the inner loops of output primitives, procedure call
       overhead is out of the question.
       See the author for a derivation if needed.
  }
{
    In scan converting polygons, we want to choose those pixels
    which are inside the polygon.  Thus, we add .5 to the starting
    x coordinate for both left and right edges.  Now we choose the
    first pixel which is inside the pgon for the left edge and the
    first pixel which is outside the pgon for the right edge.
    Draw the left pixel, but not the right.

    How to add .5 to the starting x coordinate:
        If the edge is moving to the right, then subtract dy from the
    error term from the general form of the algorithm.
        If the edge is moving to the left, then add dy to the error term.

    The reason for the difference between edges moving to the left
    and edges moving to the right is simple:  If an edge is moving
    to the right, then we want the algorithm to flip immediately.
    If it is moving to the left, then we don't want it to flip until
    we traverse an entire pixel.
  }
{#define BRESINITPGON(dy, x1, x2, xStart, d, m, m1, incr1, incr2) { \
int dx;      // local storage  \
\
// \
//  if the edge is horizontal, then it is ignored \
//  and assumed not to be processed.  Otherwise, do this stuff. \
// \
if ((dy) != 0) { \
  xStart = (x1); \
  dx = (x2) - xStart; \
  if (dx < 0) { \
      m = dx / (dy); \
      m1 = m - 1; \
      incr1 = -2   dx + 2   (dy)   m1; \
      incr2 = -2   dx + 2   (dy)   m; \
      d = 2   m   (dy) - 2   dx - 2   (dy); \
  } else { \
      m = dx / (dy); \
      m1 = m + 1; \
      incr1 = 2   dx - 2   (dy)   m1; \
      incr2 = 2   dx - 2   (dy)   m; \
      d = -2   m   (dy) + 2   dx; \
  } \
} \
}
 }
{#define BRESINCRPGON(d, minval, m, m1, incr1, incr2) { \
    if (m1 > 0) { \
        if (d > 0) { \
            minval += m1; \
            d += incr1; \
        } \
        else { \
            minval += m; \
            d += incr2; \
        } \
    } else {\
        if (d >= 0) { \
            minval += m1; \
            d += incr1; \
        } \
        else { \
            minval += m; \
            d += incr2; \
        } \
    } \
}
 }
{
       This structure contains all of the information needed
       to run the bresenham algorithm.
       The variables may be hardcoded into the declarations
       instead of using this structure to make use of
       register declarations.
  }
{ minor axis         }
{ decision variable  }
{ slope and slope+1  }
{ error increments  }
type

   PBRESINFO = ^TBRESINFO;
   TBRESINFO = record
        minor_axis : longint;
        d : longint;
        m : longint;
        m1 : longint;
        incr1 : longint;
        incr2 : longint;
     end;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function BRESINITPGONSTRUCT(dmaj,min1,min2,bres : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function BRESINCRPGONSTRUCT(bres : longint) : longint;

{
       These are the data structures needed to scan
       convert regions.  Two different scan conversion
       methods are available -- the even-odd method, and
       the winding number method.
       The even-odd rule states that a point is inside
       the polygon if a ray drawn from that point in any
       direction will pass through an odd number of
       path segments.
       By the winding number rule, a point is decided
       to be inside the polygon if a ray drawn from that
       point in any direction passes through a different
       number of clockwise and counter-clockwise path
       segments.

       These data structures are adapted somewhat from
       the algorithm in (Foley/Van Dam) for scan converting
       polygons.
       The basic algorithm is to start at the top (smallest y)
       of the polygon, stepping down to the bottom of
       the polygon by incrementing the y coordinate.  We
       keep a list of edges which the current scanline crosses,
       sorted by x.  This list is called the Active Edge Table (AET)
       As we change the y-coordinate, we update each entry in
       in the active edge table to reflect the edges new xcoord.
       This list must be sorted at each scanline in case
       two edges intersect.
       We also keep a data structure known as the Edge Table (ET),
       which keeps track of all the edges which the current
       scanline has not yet reached.  The ET is basically a
       list of ScanLineList structures containing a list of
       edges which are entered at a given scanline.  There is one
       ScanLineList per scanline at which an edge is entered.
       When we enter a new edge, we move it from the ET to the AET.

       From the AET, we can implement the even-odd rule as in
       (Foley/Van Dam).
       The winding number rule is a little trickier.  We also
       keep the EdgeTableEntries in the AET linked by the
       nextWETE (winding EdgeTableEntry) link.  This allows
       the edges to be linked just as before for updating
       purposes, but only uses the edges linked by the nextWETE
       link as edges representing spans of the polygon to
       drawn (as with the even-odd rule).
  }
{
   for the winding number rule
  }

const
   CLOCKWISE = 1;
   COUNTERCLOCKWISE = -(1);
{ ycoord at which we exit this edge.  }
{ Bresenham info to run the edge      }
{ next in the list      }
{ for insertion sort    }
{ for winding num rule  }
{ flag for winding number rule        }
type

   PEdgeTableEntry = ^TEdgeTableEntry;
   TEdgeTableEntry = record
        ymax : longint;
        bres : TBRESINFO;
        next : PEdgeTableEntry;
        back : PEdgeTableEntry;
        nextWETE : PEdgeTableEntry;
        ClockWise : longint;
     end;
{ the scanline represented  }
{ header node               }
{ next in the list        }

   PScanLineList = ^TScanLineList;
   TScanLineList = record
        scanline : longint;
        edgelist : PEdgeTableEntry;
        next : PScanLineList;
     end;
{ ymax for the polygon      }
{ ymin for the polygon      }
{ header node               }

   PEdgeTable = ^TEdgeTable;
   TEdgeTable = record
        ymax : longint;
        ymin : longint;
        scanlines : TScanLineList;
     end;
{
   Here is a struct to help with storage allocation
   so we can allocate a big chunk at a time, and then take
   pieces from this heap when we need to.
  }

const
   SLLSPERBLOCK = 25;
type

   PScanLineListBlock = ^TScanLineListBlock;
   TScanLineListBlock = record
        SLLs : array[0..(SLLSPERBLOCK)-1] of TScanLineList;
        next : PScanLineListBlock;
     end;
{

       a few macros for the inner loops of the fill code where
       performance considerations don't allow a procedure call.

       Evaluate the given edge at the given scanline.
       If the edge has expired, then we leave it and fix up
       the active edge table; otherwise, we increment the
       x value to be ready for the next scanline.
       The winding number rule is in effect, so we must notify
       the caller when the edge has been removed so he
       can reorder the Winding Active Edge Table.
  }
{
#define EVALUATEEDGEWINDING(pAET, pPrevAET, y, fixWAET) { \
   if (pAET->ymax == y) {          // leaving this edge  \
      pPrevAET->next = pAET->next; \
      pAET = pPrevAET->next; \
      fixWAET = 1; \
      if (pAET) \
         pAET->back = pPrevAET; \
   } \
   else { \
      BRESINCRPGONSTRUCT(pAET->bres); \
      pPrevAET = pAET; \
      pAET = pAET->next; \
   } \
}
 }
{
       Evaluate the given edge at the given scanline.
       If the edge has expired, then we leave it and fix up
       the active edge table; otherwise, we increment the
       x value to be ready for the next scanline.
       The even-odd rule is in effect.
  }
{#define EVALUATEEDGEEVENODD(pAET, pPrevAET, y) { \
   if (pAET->ymax == y) {          // leaving this edge  \
      pPrevAET->next = pAET->next; \
      pAET = pPrevAET->next; \
      if (pAET) \
         pAET->back = pPrevAET; \
   } \
   else { \
      BRESINCRPGONSTRUCT(pAET->bres); \
      pPrevAET = pAET; \
      pAET = pAET->next; \
   } \
}
 }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function BRESINITPGONSTRUCT(dmaj,min1,min2,bres : longint) : longint;
begin
   BRESINITPGONSTRUCT:=BRESINITPGON(dmaj,min1,min2,bres.minor_axis,bres.d,bres.m,bres.m1,bres.incr1,bres.incr2);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function BRESINCRPGONSTRUCT(bres : longint) : longint;
begin
   BRESINCRPGONSTRUCT:=BRESINCRPGON(bres.d,bres.minor_axis,bres.m,bres.m1,bres.incr1,bres.incr2);
end;


