{
    Copyright (c) 2019 by Dmitry Boyarintsev

    This unit implements support information structures for WebAssembly

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

unit i_wasm;

{$i fpcdefs.inc}

  interface

    uses
       systems,rescmn;

    const
       res_wasmraw_info : tresinfo =
           (
             id     : res_none; // todo: not implemented. but could be as memory
             resbin : 'fpcwasmres';
             rescmd : '-o $OBJ $DBG';
             rcbin  : '';
             rccmd  : '';
             resourcefileclass : nil;
             resflags : [res_no_compile];
           );

        system_wasm_info : tsysteminfo =
          (
            system       : system_wasm_wasm32;
            name         : 'WebAssembly';
            shortname    : 'Wasm';
            flags        : [tf_files_case_sensitive,tf_no_generic_stackcheck,
                            { avoid the creation of threadvar tables }
                            tf_section_threadvars];
            cpu          : cpu_wasm;
            unit_env     : '';
            extradefines : '';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.wat';
            objext       : '.wasm';
            resext       : '';
            resobjext    : '.wasm';
            sharedlibext : ''; // keep it empty! The sharedlibext drives the export module name
                               // if this is populated, then the name should be cleared when generating import
            staticlibext : '.wasm';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : '.wasm';
            staticClibext : '.wasm';
            staticClibprefix : '';
            sharedClibprefix : '';
            importlibprefix : '';
            importlibext : '.wasm';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            assem        : as_wasm_wabt;
            assemextern  : as_wasm_binaryen;
            link         : ld_wasm;
            linkextern   : ld_wasm; // there's no linker, only object files for WASM
            ar           : ar_none;
            res          : res_none;
            dbg          : dbg_none;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                jumpalignskipmax    : 0;
                coalescealign   : 0;
                coalescealignskipmax: 0;
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
            llvmdatalayout : 'todo';
          );


  implementation

end.
