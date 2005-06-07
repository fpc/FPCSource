{
    Copyright (c) 2001 by Carl Eric Codere


    Implements BeOS system calls and types


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


 ****************************************************************************
}
const
      { BeOS specific calls }
      syscall_nr_create_area = $14;
      syscall_nr_resize_area = $08;
      syscall_nr_delete_area = $15;
      syscall_nr_load_image  = $34;
      syscall_nr_wait_thread = $22;
      syscall_nr_rstat       = $30;
      syscall_nr_statfs      = $5F;
      syscall_nr_get_team_info = $3b;
      syscall_nr_kill_team   = $3a;
      syscall_nr_get_system_info = $56;
      syscall_nr_kget_tzfilename = $AF;
      syscall_nr_get_next_image_info = $3C;

const
{ -----
  system-wide constants;
----- *}
  MAXPATHLEN = PATH_MAX;
  B_FILE_NAME_LENGTH = NAME_MAX;
  B_OS_NAME_LENGTH  =   32;
  B_PAGE_SIZE    =   4096;




(* -----
  types
----- *)


type area_id = longint;
type port_id = longint;
type sem_id = longint;
type thread_id = longint;
type team_id = longint;
type bigtime_t = int64;
type status_t = longint;


{*************************************************************}
{*********************** KERNEL KIT **************************}
{*************************************************************}
{ ------------------------- Areas --------------------------- }
const
      { create_area constant definitions }
      { lock type }
      B_NO_LOCK        = 0;
      B_LAZY_LOCK      = 1;
      B_FULL_LOCK      = 2;
      B_CONTIGUOUS     = 3;
      B_LOMEM          = 4;
      { address type }
      B_ANY_ADDRESS    = 0;
      B_EXACT_ADDRESS  = 1;
      B_BASE_ADDRESS   = 2;
      B_CLONE_ADDRESS  = 3;
      B_ANY_KERNEL_ADDRESS = 4;
      { protection bits }
      B_READ_AREA     = 1;
      B_WRITE_AREA    = 2;


type
    area_info = packed record
      area:       area_id;
      name:       array[0..B_OS_NAME_LENGTH-1] of char;
      size:       size_t;
      lock:       cardinal;
      protection: cardinal;
      team:       team_id;
      ram_size:   cardinal;
      copy_count: cardinal;
      in_count:   cardinal;
      out_count:  cardinal;
      address:    pointer;
    end;


    function create_area(name : pchar; var addr : longint;
      addr_typ : longint; size : longint; lock_type: longint; protection : longint): area_id;
    var
     args : SysCallArgs;
    begin
     args.param[1] := cint(name);
     args.param[2] := cint(@addr);
     args.param[3] := cint(addr_typ);
     args.param[4] := cint(size);
     args.param[5] := cint(lock_type);
     args.param[6] := cint(protection);
     create_area := SysCall(syscall_nr_create_area, args);
    end;


    function delete_area(area : area_id): status_t;
    var
     args: SysCallargs;
    begin
     args.param[1] := cint(area);
     delete_area:= SysCall(syscall_nr_delete_area, args);
    end;


    function resize_area(area: area_id; new_size: size_t): status_t;
    var
     args: SysCallArgs;
    begin
     args.param[1] := cint(area);
     args.param[2] := cint(new_size);
     resize_area := SysCall(syscall_nr_resize_area, args);
    end;

    { the buffer should at least have MAXPATHLEN+1 bytes in size }
    function kget_tzfilename(buffer:pchar): cint;
    var
     args: SysCallArgs;
    begin
      args.param[1] := cint(buffer);
      kget_tzfilename := SysCall(syscall_nr_kget_tzfilename,args);
    end;

(*
extern _IMPEXP_ROOT area_id    clone_area(const char *name, void **dest_addr,
                       uint32 addr_spec, uint32 protection,
                       area_id source);


extern _IMPEXP_ROOT area_id    find_area(const char *name);
extern _IMPEXP_ROOT area_id    area_for(void *addr);
extern _IMPEXP_ROOT status_t  set_area_protection(area_id id,
                    uint32 new_protection);


extern _IMPEXP_ROOT status_t  _get_area_info(area_id id, area_info *ainfo,
                    size_t size);
extern _IMPEXP_ROOT status_t  _get_next_area_info(team_id team, int32 *cookie,
                    area_info *ainfo, size_t size);
*)
{ ------------------------- Threads --------------------------- }




const
   { thread state }
   B_THREAD_RUNNING = 1;
   B_THREAD_READY   = 2;
   B_THREAD_RECEIVING = 3;
   B_THREAD_ASLEEP    = 4;
   B_THREAD_SUSPENDED = 5;
   B_THREAD_WAITING   = 6;
   { thread priorities }
   B_LOW_PRIORITY        =    5;
   B_NORMAL_PRIORITY     =    10;
   B_DISPLAY_PRIORITY    =    15;
   B_URGENT_DISPLAY_PRIORITY  =    20;
   B_REAL_TIME_DISPLAY_PRIORITY=    100;
   B_URGENT_PRIORITY     =    110;
   B_REAL_TIME_PRIORITY  =    120;


type
    thread_info = packed record
       thread: thread_id;
       team: team_id;
       name: array[0..B_OS_NAME_LENGTH-1] of char;
       state: longint; { thread_state enum }
       priority:longint;
       sem:sem_id;
       user_time:bigtime_t;
       kernel_time:bigtime_t;
       stack_base:pointer;
       stack_end:pointer;
    end;


{


extern _IMPEXP_ROOT thread_id spawn_thread (
  thread_func    function_name,
  const char     *thread_name,
  int32      priority,
  void      *arg
);


extern _IMPEXP_ROOT thread_id  find_thread(const char *name);
extern _IMPEXP_ROOT status_t  kill_thread(thread_id thread);
extern _IMPEXP_ROOT status_t  resume_thread(thread_id thread);
extern _IMPEXP_ROOT status_t  suspend_thread(thread_id thread);
extern _IMPEXP_ROOT status_t  rename_thread(thread_id thread, const char *new_name);
extern _IMPEXP_ROOT status_t  set_thread_priority (thread_id thread, int32 new_priority);
extern _IMPEXP_ROOT void    exit_thread(status_t status);


extern _IMPEXP_ROOT status_t  _get_thread_info(thread_id thread, thread_info *info, size_t size);
extern _IMPEXP_ROOT status_t  _get_next_thread_info(team_id tmid, int32 *cookie, thread_info *info, size_t size);






extern _IMPEXP_ROOT status_t  send_data(thread_id thread,
                    int32 code,
                    const void *buf,
                    size_t buffer_size);


extern _IMPEXP_ROOT status_t  receive_data(thread_id *sender,
                   void *buf,
                   size_t buffer_size);


extern _IMPEXP_ROOT bool    has_data(thread_id thread);




extern _IMPEXP_ROOT status_t  snooze(bigtime_t microseconds);


/*
  Right now you can only snooze_until() on a single time base, the
  system time base given by system_time().  The "time" argument is
  the time (in the future) relative to the current system_time() that
  you want to snooze until.  Eventually there will be multiple time
  bases (and a way to find out which ones exist) but for now just pass
  the value B_SYSTEM_TIMEBASE.
*/
extern _IMPEXP_ROOT status_t  snooze_until(bigtime_t time, int timebase);
#define B_SYSTEM_TIMEBASE  (0)


}




    function wait_for_thread(thread: thread_id; var status : status_t): status_t;
     var
      args: SysCallArgs;
      i: longint;
     begin
       args.param[1] := cint(thread);
       args.param[2] := cint(@status);
       wait_for_thread := SysCall(syscall_nr_wait_thread, args);
     end;


{ ------------------------- Teams --------------------------- }


const
     B_SYSTEM_TEAM  = 2;


type
    team_info = packed record
     team:    team_id;
     image_count:   longint;
     thread_count:  longint;
     area_count:    longint;
     debugger_nub_thread: thread_id;
     debugger_nub_port: port_id;
     argc:longint;     (* number of args on the command line *)
     args: array[0..63] of char;  {* abbreviated command line args *}
     uid: uid_t;
     gid: gid_t;
    end;
{
extern _IMPEXP_ROOT status_t  _get_next_team_info(int32 *cookie, team_info *info, size_t size);
}


    function get_team_info(team: team_id; var info : team_info): status_t;
     var
      args: SysCallArgs;
     begin
       args.param[1] := cint(team);
       args.param[2] := cint(@info);
       get_team_info := SysCall(syscall_nr_get_team_info, args);
     end;


    function kill_team(team: team_id): status_t;
     var
      args: SysCallArgs;
     begin
       args.param[1] := cint(team);
       kill_team := SysCall(syscall_nr_kill_team, args);
     end;


{ ------------------------- Images --------------------------- }


type image_id = longint;


    { image types }
const
   B_APP_IMAGE     = 1;
   B_LIBRARY_IMAGE = 2;
   B_ADD_ON_IMAGE  = 3;
   B_SYSTEM_IMAGE  = 4;
type
    image_info = packed record
     id      : image_id;
     _type   : longint;
     sequence: longint;
     init_order: longint;
     init_routine: pointer;
     term_routine: pointer;
     device: dev_t;
     node: ino_t;
     name: array[0..MAXPATHLEN-1] of char;
     text: pointer;
     data: pointer;
     text_size: longint;
     data_size: longint;
    end;



  function get_next_image_info(team : team_id; var cookie: longint;var info : image_info): status_t;
     var
      args: SysCallArgs;
   begin
       args.param[1] := cint(team);
       args.param[2] := cint(@cookie);
       args.param[3] := cint(@info);
       args.param[4] := cint(sizeof(image_info));
       get_next_image_info := SysCall(syscall_nr_get_next_image_info, args);
   end;

{
extern _IMPEXP_ROOT image_id  load_add_on(const char *path);
extern _IMPEXP_ROOT status_t  unload_add_on(image_id imid);


/* private; use the macros, below */
extern _IMPEXP_ROOT status_t  _get_image_info (image_id image,
                  image_info *info, size_t size);
extern _IMPEXP_ROOT status_t  _get_next_image_info (team_id team, int32 *cookie,
                  image_info *info, size_t size);


}
(*----- symbol types and functions ------------------------*)


const B_SYMBOL_TYPE_DATA = $1;
const B_SYMBOL_TYPE_TEXT = $2;
const B_SYMBOL_TYPE_ANY  = $5;
{
extern _IMPEXP_ROOT status_t  get_image_symbol(image_id imid,
                  const char *name, int32 sclass,  void **ptr);
extern _IMPEXP_ROOT status_t  get_nth_image_symbol(image_id imid, int32 index,
                  char *buf, int32 *bufsize, int32 *sclass,
                  void **ptr);
}


{*----- cache manipulation --------------------------------*}
const
  B_FLUSH_DCACHE         =$0001;  {* dcache = data cache *}
  B_FLUSH_ICACHE         =$0004;   {* icache = instruction cache *}
  B_INVALIDATE_DCACHE    =$0002;
  B_INVALIDATE_ICACHE    =$0008;


{
extern _IMPEXP_ROOT void  clear_caches(void *addr, size_t len, uint32 flags);
}


    function load_image(argc : longint; argv : ppchar; envp : ppchar): thread_id;
     var
      args: SysCallArgs;
      i: longint;
     begin
       args.param[1] := cint(argc);
       args.param[2] := cint(argv);
       args.param[3] := cint(envp);
       load_image := SysCall(syscall_nr_load_image, args);
     end;


{ ------------------------ System information --------------------------- }
{ for both intel and ppc platforms }
const B_MAX_CPU_COUNT     = 8;


type
    system_info = packed record
     id: array[0..1] of longint;  {* unique machine ID *}
     boot_time: bigtime_t;        {* time of boot (# usec since 1/1/70) *}
     cpu_count: longint;         {* # of cpus *}
     cpu_type: longint;          {* type of cpu *}
     cpu_revision:longint ;        {* revision # of cpu *}
     cpu_infos: array [0..B_MAX_CPU_COUNT-1] of bigtime_t;  {* info about individual cpus *}
     cpu_clock_speed:int64;      {* processor clock speed (Hz) *}
     bus_clock_speed:int64;      {* bus clock speed (Hz) *      }
     platform_type:longint;      {* type of machine we're on *}
     max_pages:longint;          {* total # physical pages *}
     used_pages:longint;         {* # physical pages in use *}
     page_faults:longint;        {* # of page faults *}
     max_sems:longint;           {* maximum # semaphores *}
     used_sems:longint;          {* # semaphores in use *}
     max_ports:longint;          {* maximum # ports *}
     used_ports:longint;         {* # ports in use *}
     max_threads:longint;        {* maximum # threads *}
     used_threads:longint;       {* # threads in use *}
     max_teams:longint;          {* maximum # teams *}
     used_teams:longint;         {* # teams in use *}

     kernel_name: array[0..B_FILE_NAME_LENGTH-1] of char;    {* name of kernel *}
     kernel_build_date: array[0..B_OS_NAME_LENGTH-1] of char;  {* date kernel built *}
     kernel_build_time: array[0..B_OS_NAME_LENGTH-1] of char;  {* time kernel built *}
     kernel_version:int64;               {* version of this kernel *}
     _busy_wait_time:bigtime_t;      {* reserved for Be *}
     pad:array[1..4] of longint;     {* just in case... *}
    end;


    function get_system_info(var info: system_info): status_t;
     var
      args: SysCallArgs;
      i: longint;
     begin
       args.param[1] := cint(@info);
       i := SysCall(syscall_nr_get_system_info, args);
       get_system_info := i;
     end;




{*************************************************************}
{*********************** STORAGE KIT *************************}
{*************************************************************}
const
     { file system flags }
     B_FS_IS_READONLY    = $00000001;
     B_FS_IS_REMOVABLE    = $00000002;
     B_FS_IS_PERSISTENT    = $00000004;
     B_FS_IS_SHARED      = $00000008;
     B_FS_HAS_MIME      = $00010000;
     B_FS_HAS_ATTR      = $00020000;
     B_FS_HAS_QUERY      = $00040000;


type
   fs_info = packed record
     dev   : dev_t;              { fs dev_t }
     root  : ino_t;              { root ino_t }
     flags : cardinal;           { file system flags }
     block_size:off_t;           { fundamental block size }
     io_size:off_t;              { optimal io size }
     total_blocks:off_t;         { total number of blocks }
     free_blocks:off_t;          { number of free blocks  }
     total_nodes:off_t;          { total number of nodes  }
     free_nodes:off_t;           { number of free nodes   }
     device_name: array[0..127] of char;    { device holding fs      }
     volume_name: array[0..B_FILE_NAME_LENGTH-1] of char;{ volume name            }
     fsh_name : array[0..B_OS_NAME_LENGTH-1] of char;{ name of fs handler     }
   end;


    function dev_for_path(const pathname : pchar): dev_t;
     var
      args: SysCallArgs;
      buffer: array[1..15] of longint;
      i: cint;
     begin
       args.param[1] := $FFFFFFFF;
       args.param[2] := cint(pathname);
       args.param[3] := cint(@buffer);
       args.param[4] := $01000000;
       if SysCall(syscall_nr_rstat, args)=0 then
          i:=buffer[1]
       else
          i:=-1;
       dev_for_path := i;
     end;


    function fs_stat_dev(device: dev_t; var info: fs_info): dev_t;
     var
      args: SysCallArgs;
     begin
       args.param[1] := cint(device);
       args.param[2] := 0;
       args.param[3] := $FFFFFFFF;
       args.param[4] := 0;
       args.param[5] := cint(@info);
       fs_stat_dev := SysCall(syscall_nr_statfs, args);
     end;


{
_IMPEXP_ROOT dev_t    next_dev(int32 *pos);
}


{*****************************************************************}








