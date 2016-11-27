{
    Copyright (c) 2010, 2013, 2015 by Jonas Maebe

    Basic Processor information for LLVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit llvminfo;

Interface

uses
  globtype;

Type
   { possible supported processors for this target }
   tllvmversion =
      ({ may add older/newer versions if required/appropriate }
       llvmver_3_3,
       llvmver_3_4_0,
       llvmver_3_4_1,
       llvmver_3_4_2,
       llvmver_3_5_0,
       llvmver_3_5_1,
       llvmver_3_5_2,
       llvmver_3_6_0,
       llvmver_3_6_1,
       llvmver_3_6_2,
       llvmver_3_7_0,
       llvmver_3_8_0,
       llvmver_3_9_0,
       { Xcode versions use snapshots of LLVM and don't correspond to released
         versions of llvm (they don't ship with the llvm utilities either, but
         they do come with Clang, which can also be used to some extent instead
         of opt/llc) }
       llvmver_xc_6_4
      );

type
   tllvmversionflag = (
     llvmflag_metadata_keyword,    { use "metadata" keyword (others leave it away, except when metadata is an argument to call instructions) }
     llvmflag_linker_private,      { have linker_private linkage type (later versions use global in combination with hidden visibility) }
     llvmflag_load_getelptr_type,  { the return type of loads and the base type of getelementptr must be specified }
     llvmflag_call_no_ptr,         { with direct calls, the function type is not a function pointer }
     llvmflag_alias_double_type    { with "alias" declarations, have to print both aliasee and aliasee* types }
   );
   tllvmversionflags = set of tllvmversionflag;

Const
   llvmversionstr : array[tllvmversion] of string[14] = (
     'LLVM-3.3',
     'LLVM-3.4.0',
     'LLVM-3.4.1',
     'LLVM-3.4.2',
     'LLVM-3.5.0',
     'LLVM-3.5.1',
     'LLVM-3.5.2',
     'LLVM-3.6.0',
     'LLVM-3.6.1',
     'LLVM-3.6.2',
     'LLVM-3.7.0',
     'LLVM-3.8.0',
     'LLVM-3.9.0',
     'LLVM-Xcode-6.4' { somewhere around LLVM 3.6.0 }
   );

   llvmversion_properties: array[tllvmversion] of tllvmversionflags =
     (
       { llvmver_3_3    } [llvmflag_metadata_keyword,llvmflag_linker_private],
       { llvmver_3_4_0  } [llvmflag_metadata_keyword,llvmflag_linker_private],
       { llvmver_3_4_1  } [llvmflag_metadata_keyword,llvmflag_linker_private],
       { llvmver_3_4_2  } [llvmflag_metadata_keyword,llvmflag_linker_private],
       { llvmver_3_5_0  } [llvmflag_metadata_keyword],
       { llvmver_3_5_1  } [llvmflag_metadata_keyword],
       { llvmver_3_5_2  } [llvmflag_metadata_keyword],
       { llvmver_3_6_0  } [],
       { llvmver_3_6_1  } [],
       { llvmver_3_6_2  } [],
       { llvmver_3_7_0  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr],
       { llvmver_3_8_0  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type],
       { llvmver_3_9_0  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type],
       { llvmver_xc_6_4 } [llvmflag_metadata_keyword]
     );

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [cs_opt_loopunroll,cs_opt_nodecse];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches + [cs_opt_nodecse];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [{,cs_opt_loopunroll}];
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [];

Implementation

end.
