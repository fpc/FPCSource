       .file   "func.s"
.text

.globl  _haltproc
.type   _haltproc,@function
_haltproc:
        xorl %ebx,%ebx
    movw operatingsystem_result,%bx
        pushl %ebx
        call sys_exit

/* int sys_open (int=0xFF000000, char * name, int mode, int=0, int close_on_exec=0); */
.globl sys_open
.type sys_open,@function
sys_open:
xorl %eax,%eax
int $0x25
ret

/* int sys_close (int handle) */
.globl sys_close
.type sys_close,@function
sys_close:
mov $0x01,%eax
int $0x25
ret

/* int sys_read (int handle, void * buffer, int length) */
.globl sys_read
.type sys_read,@function
sys_read:
movl $0x02,%eax
int $0x25
ret

/* int sys_write (int handle, void * buffer, int length) */
.globl sys_write
.type sys_write,@function
sys_write:
movl $0x3,%eax
int $0x25
ret

/* int sys_lseek (int handle, long long pos, int whence) */
.globl sys_lseek
.type sys_lseek,@function
sys_lseek:
movl $0x5,%eax
int $0x25
ret

/* int sys_time(void) */
.globl sys_time
.type sys_time,@function
sys_time:
movl $0x7,%eax
int $0x25
ret

/* int sys_resize_area */
.globl sys_resize_area
.type sys_resize_area,@function
sys_resize_area:
movl $0x8,%eax
int $0x25
ret

/* int sys_opendir (0xFF000000, chra * name, 0) */
.globl sys_opendir
.type sys_opendir,@function
sys_opendir:
movl $0xC,%eax
int $0x25
ret

/* int sys_create_area */
.globl sys_create_area
.type sys_create_area,@function
sys_create_area:
movl $0x14,%eax
int $0x25
ret

/* int sys_readdir (int handle, void * dirent, 0x11C, 0x01000000) */
.globl sys_readdir
.type sys_readdir,@function
sys_readdir:
movl $0x1C,%eax
int $0x25
ret

/* int sys_mkdir (char=0xFF, char * name, int mode) */
.globl sys_mkdir
.type sys_mkdir,@function
sys_mkdir:
movl $0x1E,%eax
int $0x25
ret

/* int sys_wait_for_thread */
.globl sys_wait_for_thread
.type sys_wait_for_thread,@function
sys_wait_for_thread:
movl $0x22,%eax
int $0x25
ret

/* int sys_rename (int=0xFF000000, char * name, int=0xFF000000, char * newname) */
.globl sys_rename
.type sys_rename,@function
sys_rename:
movl $0x26,%eax
int $0x25
ret

/* int sys_unlink (int=0xFF000000, char * name) */
.globl sys_unlink
.type sys_unlink,@function
sys_unlink:
movl $0x27,%eax
int $0x25
ret

/* int sys_stat (int=0xFF000000, char * name, struct stat * s, int=0) */
.globl sys_stat
.type sys_stat,@function
sys_stat:
movl $0x30,%eax
int $0x25
ret

/* int sys_load_image */
.globl sys_load_image
.type sys_load_image,@function
sys_load_image:
movl $0x34,%eax
int $0x25
ret

/* void sys_exit (int exitcode) */
.globl sys_exit
.type sys_exit,@function
sys_exit:
movl $0x3F,%eax
int $0x25

/* void sys_chdir (char 0xFF, char * name) */
.globl sys_chdir
.type sys_chdir,@function
sys_chdir:
movl $0x57,%eax
int $0x25
ret

/* void sys_rmdir (char 0xFF, char * name) */
.globl sys_rmdir
.type sys_rmdir,@function
sys_rmdir:
movl $0x60,%eax
int $0x25
ret
