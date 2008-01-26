unit major;
interface

{
  Automatically converted by H2Pas 0.99.15 from major.h
  The following command line parameters were used:
    major.h
}

{$PACKRECORDS C}


  const
     MAX_CHRDEV = 255;
     MAX_BLKDEV = 255;
     UNNAMED_MAJOR = 0;
     MEM_MAJOR = 1;
     RAMDISK_MAJOR = 1;
     FLOPPY_MAJOR = 2;
     PTY_MASTER_MAJOR = 2;
     IDE0_MAJOR = 3;
     PTY_SLAVE_MAJOR = 3;
     HD_MAJOR = IDE0_MAJOR;
     TTY_MAJOR = 4;
     TTYAUX_MAJOR = 5;
     LP_MAJOR = 6;
     VCS_MAJOR = 7;
     LOOP_MAJOR = 7;
     SCSI_DISK0_MAJOR = 8;
     SCSI_TAPE_MAJOR = 9;
     MD_MAJOR = 9;
     MISC_MAJOR = 10;
     SCSI_CDROM_MAJOR = 11;
     QIC02_TAPE_MAJOR = 12;
     XT_DISK_MAJOR = 13;
     SOUND_MAJOR = 14;
     CDU31A_CDROM_MAJOR = 15;
     JOYSTICK_MAJOR = 15;
     GOLDSTAR_CDROM_MAJOR = 16;
     OPTICS_CDROM_MAJOR = 17;
     SANYO_CDROM_MAJOR = 18;
     CYCLADES_MAJOR = 19;
     CYCLADESAUX_MAJOR = 20;
     MITSUMI_X_CDROM_MAJOR = 20;
  { ARM Linux /dev/mfm  }
     MFM_ACORN_MAJOR = 21;
     SCSI_GENERIC_MAJOR = 21;
     Z8530_MAJOR = 34;
     DIGI_MAJOR = 23;
     IDE1_MAJOR = 22;
     DIGICU_MAJOR = 22;
     MITSUMI_CDROM_MAJOR = 23;
     CDU535_CDROM_MAJOR = 24;
     STL_SERIALMAJOR = 24;
     MATSUSHITA_CDROM_MAJOR = 25;
     STL_CALLOUTMAJOR = 25;
     MATSUSHITA_CDROM2_MAJOR = 26;
     QIC117_TAPE_MAJOR = 27;
     MATSUSHITA_CDROM3_MAJOR = 27;
     MATSUSHITA_CDROM4_MAJOR = 28;
     STL_SIOMEMMAJOR = 28;
     ACSI_MAJOR = 28;
     AZTECH_CDROM_MAJOR = 29;
  { SparcLinux & Linux/68k /dev/fb  }
     GRAPHDEV_MAJOR = 29;
  { Linux/mips, SGI /dev/shmiq  }
     SHMIQ_MAJOR = 85;
     CM206_CDROM_MAJOR = 32;
     IDE2_MAJOR = 33;
     IDE3_MAJOR = 34;
  { expanded storage on S/390 = "slow ram"  }
     XPRAM_MAJOR = 35;
  { proposed by Peter                       }
     NETLINK_MAJOR = 36;
     PS2ESDI_MAJOR = 36;
     IDETAPE_MAJOR = 37;
     Z2RAM_MAJOR = 37;
  { AP1000 Block device  }
     APBLOCK_MAJOR = 38;
  { AP1000 DDV block device  }
     DDV_MAJOR = 39;
  { Network block device         }
     NBD_MAJOR = 43;
     RISCOM8_NORMAL_MAJOR = 48;
  { 48..55  }
     DAC960_MAJOR = 48;
     RISCOM8_CALLOUT_MAJOR = 49;
     MKISS_MAJOR = 55;
  { DSP56001 processor device  }
     DSP56K_MAJOR = 55;
     IDE4_MAJOR = 56;
     IDE5_MAJOR = 57;
  { Logical Volume Manager  }
     LVM_BLK_MAJOR = 58;
     SCSI_DISK1_MAJOR = 65;
     SCSI_DISK2_MAJOR = 66;
     SCSI_DISK3_MAJOR = 67;
     SCSI_DISK4_MAJOR = 68;
     SCSI_DISK5_MAJOR = 69;
     SCSI_DISK6_MAJOR = 70;
     SCSI_DISK7_MAJOR = 71;
     COMPAQ_SMART2_MAJOR = 72;
     COMPAQ_SMART2_MAJOR1 = 73;
     COMPAQ_SMART2_MAJOR2 = 74;
     COMPAQ_SMART2_MAJOR3 = 75;
     COMPAQ_SMART2_MAJOR4 = 76;
     COMPAQ_SMART2_MAJOR5 = 77;
     COMPAQ_SMART2_MAJOR6 = 78;
     COMPAQ_SMART2_MAJOR7 = 79;
     SPECIALIX_NORMAL_MAJOR = 75;
     SPECIALIX_CALLOUT_MAJOR = 76;
     COMPAQ_CISS_MAJOR = 104;
     COMPAQ_CISS_MAJOR1 = 105;
     COMPAQ_CISS_MAJOR2 = 106;
     COMPAQ_CISS_MAJOR3 = 107;
     COMPAQ_CISS_MAJOR4 = 108;
     COMPAQ_CISS_MAJOR5 = 109;
     COMPAQ_CISS_MAJOR6 = 110;
     COMPAQ_CISS_MAJOR7 = 111;
     ATARAID_MAJOR = 114;
  { Official assignations from Peter  }
     DASD_MAJOR = 94;
  { Official assignations from Peter  }
     MDISK_MAJOR = 95;
  { 80->87  }
     I2O_MAJOR = 80;
     IDE6_MAJOR = 88;
     IDE7_MAJOR = 89;
     IDE8_MAJOR = 90;
     IDE9_MAJOR = 91;
     UBD_MAJOR = 98;
     AURORA_MAJOR = 79;
     JSFD_MAJOR = 99;
     PHONE_MAJOR = 100;
  { Logical Volume Manager  }
     LVM_CHAR_MAJOR = 109;
     RTF_MAJOR = 150;
     RAW_MAJOR = 162;
     USB_ACM_MAJOR = 166;
     USB_ACM_AUX_MAJOR = 167;
     USB_CHAR_MAJOR = 180;
     UNIX98_PTY_MASTER_MAJOR = 128;
     UNIX98_PTY_MAJOR_COUNT = 8;
     UNIX98_PTY_SLAVE_MAJOR = UNIX98_PTY_MASTER_MAJOR + UNIX98_PTY_MAJOR_COUNT;
  { VERITAS volume i/o driver     }
     VXVM_MAJOR = 199;
  { VERITAS volume config driver  }
     VXSPEC_MAJOR = 200;
  { VERITAS volume multipath driver  }
     VXDMP_MAJOR = 201;
     MSR_MAJOR = 202;
     CPUID_MAJOR = 203;
  { OnStream-SCx0 SCSI tape  }
     OSST_MAJOR = 206;
  { Official allocations now  }
     IBM_TTY3270_MAJOR = 227;
     IBM_FS3270_MAJOR = 228;

implementation


end.
