(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Rect.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines rectangle structures and routines.
 *
 * History:
 *    November 3, 1994  Created by Roger Flores
 *       Name  Date     Description
 *       ----  ----     -----------
 *       bob   2/9/99   Use Coord abstraction, fix up consts
 *
 *****************************************************************************)
{$MACRO ON}

unit rect;

interface

uses palmos, coretraps;

type
  AbsRectType = record
    left: Coord;
    top: Coord;
    right: Coord;
    bottom: Coord;
  end;

  PointType = record
    x: Coord;
    y: Coord;
  end;
  PointPtr = ^PointType;

  RectangleType = record
    topLeft: PointType;
    extent: PointType;
  end;

  RectanglePtr = ^RectangleType;

procedure RctSetRectangle(var rP: RectangleType; left, top, width, height: Coord); syscall sysTrapRctSetRectangle;

procedure RctCopyRectangle({const} var srcRectP: RectangleType; var dstRectP: RectangleType); syscall sysTrapRctCopyRectangle;

procedure RctInsetRectangle(var rP: RectangleType; insetAmt: Coord); syscall sysTrapRctInsetRectangle;

procedure RctOffsetRectangle(var rP: RectangleType; deltaX, deltaY: Coord); syscall sysTrapRctOffsetRectangle;

function RctPtInRectangle(x, y: Coord; {const} var rP: RectangleType): Boolean; syscall sysTrapRctPtInRectangle;

procedure RctGetIntersection({const} var r1P, r2P: RectangleType; var r3P: RectangleType); syscall sysTrapRctGetIntersection;

implementation

end.
