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

{ setup }
function ptc_timer_create: TPTC_TIMER;
procedure ptc_timer_destroy(obj: TPTC_TIMER);

{ set time }
procedure ptc_timer_set(obj: TPTC_TIMER; time: Double);

{ control }
procedure ptc_timer_start(obj: TPTC_TIMER);
procedure ptc_timer_stop(obj: TPTC_TIMER);

{ time data }
function ptc_timer_time(obj: TPTC_TIMER): Double;
function ptc_timer_delta(obj: TPTC_TIMER): Double;
function ptc_timer_resolution(obj: TPTC_TIMER): Double;

{ operators }
procedure ptc_timer_assign(obj, timer: TPTC_TIMER);
function ptc_timer_equals(obj, timer: TPTC_TIMER): Boolean;
