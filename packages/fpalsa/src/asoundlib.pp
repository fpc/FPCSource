{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/asoundlib.h
 * \brief Application interface library for the ALSA driver
 * \author Jaroslav Kysela <perex@perex.cz>
 * \author Abramo Bagnara <abramo@alsa-project.org>
 * \author Takashi Iwai <tiwai@suse.de>
 * \date 1998-2001
 *
 * Application interface library for the ALSA driver
 *}
{*
 *   This library is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU Lesser General Public License as
 *   published by the Free Software Foundation; either version 2.1 of
 *   the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 *}

unit asoundlib;

{$MODE objfpc}
{$PACKRECORDS c}
{$LINKLIB c}

interface

uses
  ctypes, BaseUnix;

const
  libasound = 'asound';

{$INFO va_list ???}
type
  va_list = Pointer;

{#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <assert.h>
#include <endian.h>
#include <sys/poll.h>
#include <errno.h>
#include <stdarg.h>}

{$INCLUDE asoundef.inc}
{$INCLUDE version.inc}
{$INCLUDE global.inc}
{$INCLUDE input.inc}
{$INCLUDE output.inc}
{$INCLUDE error.inc}
{$INCLUDE conf.inc}
{$INCLUDE pcm.inc}
{$INCLUDE rawmidi.inc}
{$INCLUDE timer.inc}
{$INCLUDE hwdep.inc}
{$INCLUDE control.inc}
{$INCLUDE mixer.inc}
{$INCLUDE seq_event.inc}
{$INCLUDE seq.inc}
{$INCLUDE seqmid.inc}
{$INCLUDE seq_midi_event.inc}

implementation

{$INCLUDE pcm_i.inc}
{$INCLUDE control_i.inc}
{$INCLUDE seq_i.inc}
{$INCLUDE seqmid_i.inc}

end.
