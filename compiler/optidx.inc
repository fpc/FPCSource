{
    $Id$
    Copyright (c) 1998 by the FPC development team

    This file includes the message index types which can be used to display
    a message from the message file

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}

const
  optionhelplines=117; {amount of lines for the -?}
type
  toptionconst=(
usage,
only_one_source_support,
def_only_for_os2,
no_nested_response_file,
no_source_found,
illegal_para,
help_pages_para,
unable_open_file,
reading_further_from,
target_is_already_set,
no_shared_lib_under_dos,
too_many_ifdef,
too_many_endif,
too_less_endif,
{logo}
logo_start,
logo_end,
{info}
info_start,
info1,
info2,
info3,
info4,
info5,
info_end,
{This should be the last}
endoptionconst
);
