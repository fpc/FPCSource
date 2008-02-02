// included by gdk2.pp

{$IFDEF read_interface_types}
   PGdkRegionBox = ^TGdkRegionBox;
   TGdkRegionBox = TGdkSegment;

{
     clip region
  }
   PGdkRegion = ^TGdkRegion;
   TGdkRegion = record
        size : longint;
        numRects : longint;
        rects : PGdkRegionBox;
        extents : TGdkRegionBox;
     end;

{
   used to allocate buffers for points and link
   the buffers together
  }
   PPOINTBLOCK = ^TPOINTBLOCK;
   TPOINTBLOCK = record
        pts : array[0..(NUMPTSTOBUFFER)-1] of TGdkPoint;
        next : PPOINTBLOCK;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{  1 if two BOXs overlap.
    0 if two BOXs do not overlap.
    Remember, x2 and y2 are not in the region
  }
function gdkregion_EXTENTCHECK(r1, r2: PGdkRegionBox): boolean;

{
    update region extents
  }
procedure gdkregion_EXTENTS(r: PGdkRegionBox; idRect: PGdkRegion);

{
     Check to see if there is enough memory in the present region.
  }
function gdkregion_MEMCHECK(reg: PGdkRegion; var ARect, firstrect: PGdkRegionBox): boolean;

{  this routine checks to see if the previous rectangle is the same
    or subsumes the new rectangle to add.
  }
function gdkregion_CHECK_PREVIOUS(Reg: PGdkRegion; R: PGdkRegionBox;
  Rx1,Ry1,Rx2,Ry2 : gint) : boolean;

{  add a rectangle to the given Region  }
procedure gdkregion_ADDRECT(reg: PGdkRegion; r: PGdkRegionBox;
  rx1, ry1, rx2, ry2: gint);

{  add a rectangle to the given Region  }
procedure gdkregion_ADDRECTNOX(reg: PGdkRegion; r: PGdkRegionBox; rx1,ry1,rx2,ry2: gint);

{  }
function gdkregion_EMPTY_REGION(pReg: PGdkRegion): boolean;
{pReg->numRects = 0}

function gdkregion_REGION_NOT_EMPTY(pReg : PGdkRegion) : boolean;

function gdkregion_INBOX(r: TGdkRegionBox; x,y : gint) : boolean;

{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function gdkregion_EXTENTCHECK(r1, r2: PGdkRegionBox): boolean;
begin
  gdkregion_EXTENTCHECK:=(r1^.x2 > r2^.x1) and (r1^.x1 < r2^.x2) and (r1^.y2 > r2^.y1)
               and (r1^.y1 < r2^.y2)
end;

procedure gdkregion_EXTENTS(r: PGdkRegionBox; idRect: PGdkRegion);
begin
  if (r^.x1 < idRect^.extents.x1) then idRect^.extents.x1 := r^.x1;
  if r^.y1 < idRect^.extents.y1 then idRect^.extents.y1 := r^.y1;
  if r^.x2 > idRect^.extents.x2 then idRect^.extents.x2 := r^.x2;
end;

function gdkregion_MEMCHECK(reg: PGdkRegion;
  var ARect, firstrect: PGdkRegionBox): boolean;
begin
{$IFNDEF KYLIX}
  if reg^.numRects >= reg^.size - 1 then begin
    firstrect := PGdkRegionBox(g_renew (SizeOf(TGdkRegionBox), firstrect,
                                        2 * reg^.size));
    reg^.size:=reg^.size*2;
    Arect:=@firstrect[reg^.numRects];
  end;
  Result:=true;
{$ENDIF}
end;

function gdkregion_CHECK_PREVIOUS(Reg: PGdkRegion; R: PGdkRegionBox;
  Rx1,Ry1,Rx2,Ry2 : gint) : boolean;
begin
{$IFNDEF KYLIX}
  gdkregion_CHECK_PREVIOUS:=
                    not (((((Reg^.numRects > 0) and ((R - 1)^.y1 = Ry1))
                        and ((R - 1)^.y2 = Ry2))
                          and ((R - 1)^.x1 <= Rx1))
                            and ((R - 1)^.x2 >= Rx2));
{$ENDIF}
end;

procedure gdkregion_ADDRECT(reg: PGdkRegion; r: PGdkRegionBox;
  rx1, ry1, rx2, ry2: gint);
begin
  if ((rx1 < rx2) and (ry1 < ry2)
  and gdkregion_CHECK_PREVIOUS(reg, r, rx1, ry1, rx2, ry2)) then begin
    r^.x1 := rx1;
    r^.y1 := ry1;
    r^.x2 := rx2;
    r^.y2 := ry2;
  end;

end;

procedure gdkregion_ADDRECTNOX(reg: PGdkRegion; r: PGdkRegionBox;
  rx1,ry1,rx2,ry2: gint);
begin
  if ((rx1 < rx2) and (ry1 < ry2)
  and gdkregion_CHECK_PREVIOUS(reg, r, rx1, ry1, rx2, ry2)) then begin
    r^.x1 := rx1;
    r^.y1 := ry1;
    r^.x2 := rx2;
    r^.y2 := ry2;
    inc(reg^.numRects);
  end;

end;

function gdkregion_EMPTY_REGION(pReg: PGdkRegion): boolean;
begin
  gdkregion_EMPTY_REGION:=pReg^.numRects=0;
end;

function gdkregion_REGION_NOT_EMPTY(pReg : PGdkRegion) : boolean;
begin
  gdkregion_REGION_NOT_EMPTY:=pReg^.numRects<>0;
end;

function gdkregion_INBOX(r: TGdkRegionBox; x,y : gint) : boolean;
begin
  gdkregion_INBOX:=(((r.x2 > x) and (r.x1 <= x)) and (r.y2 > y)) and (r.y1<= y);
end;
{$ENDIF}

