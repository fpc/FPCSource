
{$IFDEF OGC_INTERFACE}
const
  LWP_PRIO_MIN = 0;
  LWP_PRIO_MAX = 255;


type
  _priocntrl = record
    minor : pcuint32;
    ready_minor : cuint32;
    ready_major : cuint32;
    block_minor : cuint32;
    block_major : cuint32;
  end;
  prio_cntrl = _priocntrl;
  pprio_cntrl = ^_priocntrl;


var
  _prio_major_bitmap : cuint32; external;
  _prio_bitmap: array [0..0] of cuint32; external;

procedure __lwp_priority_init; cdecl; external;


//{$ifdef LIBOGC_INTERNAL}
//{$include libogc/lwp_priority.inl}
//{$endif}
{$ENDIF}


{$IFDEF OGC_IMPLEMENTATION}
procedure __lwp_priomap_init(theprio: pprio_cntrl; prio: cuint32);
var
	major,minor,mask: cuint32;
begin
	major := prio div 16;
	minor := prio mod 16;

	theprio^.minor := @(_prio_bitmap[major]);

	mask := $80000000 shr major;
	theprio^.ready_major := mask;
	theprio^.block_major := not mask;

	mask := $80000000 shr minor;
	theprio^.ready_minor := mask;
	theprio^.block_minor := not mask;
//#ifdef _LWPPRIO_DEBUG
//	printf("__lwp_priomap_init(%p,%d,%p,%d,%d,%d,%d)\n",theprio,prio,theprio->minor,theprio->ready_major,theprio->ready_minor,theprio->block_major,theprio->block_minor);
//#endif
end;

procedure __lwp_priomap_addto(theprio: pprio_cntrl);
begin
	theprio^.minor^ := theprio^.minor^ or theprio^.ready_minor;
	_prio_major_bitmap := _prio_major_bitmap or theprio^.ready_major;
end;

procedure __lwp_priomap_removefrom(theprio: pprio_cntrl);
begin
	theprio^.minor^ := theprio^.minor^ and theprio^.block_minor;
	if theprio^.minor^ = 0 then
		_prio_major_bitmap := _prio_major_bitmap and theprio^.block_major;
end;

function __lwp_priomap_highest(): cuint32;
var
	major, minor: cuint32;
begin
  major := cntlzw(_prio_major_bitmap);
	minor := cntlzw(_prio_bitmap[major]);
//#ifdef _LWPPRIO_DEBUG
//	printf("__lwp_priomap_highest(%d)\n",((major<<4)+minor));
//#endif
	result := ((major shl 4) + minor);
end;
{$ENDIF}

