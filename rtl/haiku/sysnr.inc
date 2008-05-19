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
      syscall_nr_exit   		= $3F;
      syscall_nr_chdir  		= $57; 
      syscall_nr_mkdir  		= $1E; 
      syscall_nr_unlink 		= $27;
      syscall_nr_rmdir  		= $60;
      syscall_nr_close  		= $01;
      syscall_nr_read   		= $02;
      syscall_nr_write  		= $03;
      syscall_nr_stat   		= $30;
      syscall_nr_fstat  		= $30;
      syscall_nr_rename 		= $26;
      syscall_nr_access 		= $58;
      syscall_nr_opendir		= $0C;
      syscall_nr_closedir		= $0F;
      syscall_nr_sigaction		= $70;
      syscall_nr_time     		= $07;
      syscall_nr_open     		= $00;
      syscall_nr_readdir  		= $1C;
      syscall_nr_lseek    		= $05;
      syscall_nr_ftruncate 		= $4B;
      syscall_nr_ioctl    		= $04;
      syscall_nr_gettimeofday 	= $A6;
      syscall_nr_fork           = $A1;
      syscall_nr_waitpid        = $A3;
      syscall_nr_fcntl          = $0B;
      syscall_nr_dup            = syscall_nr_fcntl;
      syscall_nr_dup2           = $4A;
      syscall_nr_sbrk           = syscall_nr_resize_area;
      syscall_nr_getpid         = $00; // not a syscall under BeOS
      syscall_nr_sigprocmask    = $73;
      syscall_nr_getcwd         = $00; // not a syscall under BeOS
