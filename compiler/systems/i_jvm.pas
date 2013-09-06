{
    Copyright (c) 2010 by Jonas Maebe

    This unit implements support information structures for FreeBSD/NetBSD,
    OpenBSD and Darwin (Mac OS X)

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

unit i_jvm;

{$i fpcdefs.inc}

  interface

    uses
       systems,rescmn;

    const
       res_jvmraw_info : tresinfo =
           (
             id     : res_jvm_raw;
             resbin : 'fpcjres';
             rescmd : '-o $OBJ $DBG';
             rcbin  : '';
             rccmd  : '';
             resourcefileclass : nil;
             resflags : [res_no_compile];
           );

       { The 32 only means that code written for this target behaves
         semantically as if it were written for a 32 bit target (default
         integer evaluation width = 32 bit). It will work equally well on 32
         bit and 64 bit JVM implementations. }
       system_jvm_java32_info : tsysteminfo =
          (
            system       : system_jvm_java32;
            name         : 'Java Virtual Machine';
            shortname    : 'Java';
            flags        : [tf_files_case_sensitive,tf_no_generic_stackcheck,
                            { avoid the creation of threadvar tables }
                            tf_section_threadvars];
            cpu          : cpu_jvm;
            unit_env     : '';
            extradefines : '';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.j';
            objext       : '.class';
            resext       : '';
            resobjext    : '.jar';
            sharedlibext : '.jar';
            staticlibext : '.jar';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : '.jar';
            staticClibext : '.jar';
            staticClibprefix : '';
            sharedClibprefix : '';
            importlibprefix : '';
            importlibext : '.jar';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_jvm_jasmin;
            assemextern  : as_jvm_jasmin;
            link         : ld_none;
            linkextern   : ld_jvm;
            ar           : ar_none;
            res          : res_jvm_raw;
            dbg          : dbg_jasmin;
            script       : script_unix;
            endian       : endian_big;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 4;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 0;
            stacksize   : 262144;
            stackalign   : 4;
            abi          : abi_default;
          );


       system_jvm_android32_info : tsysteminfo =
          (
            system       : system_jvm_android32;
            name         : 'Android via JVM';
            shortname    : 'android';
            flags        : [tf_files_case_sensitive,tf_no_generic_stackcheck,
                            { avoid the creation of threadvar tables }
                            tf_section_threadvars];
            cpu          : cpu_jvm;
            unit_env     : '';
            extradefines : '';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.j';
            objext       : '.class';
            resext       : '';
            resobjext    : '.jar';
            sharedlibext : '.jar';
            staticlibext : '.jar';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : '.jar';
            staticClibext : '.jar';
            staticClibprefix : '';
            sharedClibprefix : '';
            importlibprefix : '';
            importlibext : '.jar';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_jvm_jasmin;
            assemextern  : as_jvm_jasmin;
            link         : ld_none;
            linkextern   : ld_jvm;
            ar           : ar_none;
            res          : res_jvm_raw;
            dbg          : dbg_jasmin;
            script       : script_unix;
            endian       : endian_big;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 4;
                varalignmax     : 4;
                localalignmin   : 4;
                localalignmax   : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 0;
            stacksize   : 262144;
            stackalign   : 4;
            abi          : abi_default;
          );


  implementation

end.
