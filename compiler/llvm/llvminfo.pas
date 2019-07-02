{
    Copyright (c) 2010, 2013, 2015 by Jonas Maebe

    Basic Processor information for LLVM

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

{$i fpcdefs.inc}

Unit llvminfo;

Interface

uses
  globtype;

Type
   { possible supported processors for this target }
   tllvmversion =
      (llvmver_invalid,
       llvmver_3_3,
       llvmver_3_4,
       llvmver_3_5,
       { Xcode versions use snapshots of LLVM and don't correspond to released
         versions of llvm (they don't ship with the llvm utilities either, but
         they do come with Clang, which can be used instead of opt/llc) }
       llvmver_xc_6_4,
       llvmver_3_6,
       llvmver_3_7,
       llvmver_xc_7_0,
       llvmver_xc_7_1,
       llvmver_xc_7_2,
       llvmver_3_8,
       llvmver_xc_7_3,
       llvmver_3_9,
       llvmver_xc_8_0,
       llvmver_xc_8_1,
       llvmver_xc_8_2,
       llvmver_4_0,
       llvmver_xc_9_0,
       llvmver_5_0,
       llvmver_xc_9_1,
       llvmver_xc_9_2,
       llvmver_xc_9_3,
       llvmver_6_0,
       llvmver_xc_10_0,
       llvmver_xc_10_1,
       llvmver_7_0,
       llvmver_7_1,
       llvmver_8_0
      );

type
   tllvmversionflag = (
     llvmflag_metadata_keyword,    { use "metadata" keyword (others leave it away, except when metadata is an argument to call instructions) }
     llvmflag_linker_private,      { have linker_private linkage type (later versions use global in combination with hidden visibility) }
     llvmflag_load_getelptr_type,  { the return type of loads and the base type of getelementptr must be specified }
     llvmflag_call_no_ptr,         { with direct calls, the function type is not a function pointer }
     llvmflag_alias_double_type,   { with "alias" declarations, have to print both aliasee and aliasee* types }
     llvmflag_fembed_bitcode,      { support embedding bitcode in object files }
     llvmflag_memcpy_indiv_align,  { memcpy intrinsic supports separate alignment for source and dest }
     llvmflag_null_pointer_valid   { supports "llvmflag_null_pointer_valid" attribute, which indicates access to nil should not be optimized as undefined behaviour }
   );
   tllvmversionflags = set of tllvmversionflag;

Const
   llvmversionstr : array[tllvmversion] of string[14] = (
     '',
     '3.3',
     '3.4',
     '3.5',
     'Xcode-6.4',
     '3.6',
     '3.7',
     'Xcode-7.0',
     'Xcode-7.1',
     'Xcode-7.2',
     '3.8',
     'Xcode-7.3',
     '3.9',
     'Xcode-8.0',
     'Xcode-8.1',
     'Xcode-8.2',
     '4.0',
     'Xcode-9.0',
     '5.0',
     'Xcode-9.1',
     'Xcode-9.2',
     'Xcode-9.3',
     '6.0',
     'Xcode-10.0',
     'Xcode-10.1',
     '7.0',
     '7.1',
     '8.0'
   );

   llvmversion_properties: array[tllvmversion] of tllvmversionflags =
     (
       { invalid         } [],
       { llvmver_3_3     } [llvmflag_metadata_keyword,llvmflag_linker_private],
       { llvmver_3_4     } [llvmflag_metadata_keyword,llvmflag_linker_private],
       { llvmver_3_5     } [llvmflag_metadata_keyword],
       { llvmver_xc_6_4  } [llvmflag_metadata_keyword],
       { llvmver_3_6     } [],
       { llvmver_3_7     } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr],
       { llvmver_xc_7_0  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr],
       { llvmver_xc_7_1  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr],
       { llvmver_xc_7_2  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr],
       { llvmver_3_8     } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type],
       { llvmver_xc_7_3  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type],
       { llvmver_3_9     } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_xc_8_0  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_xc_8_1  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_xc_8_2  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_4_0     } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_xc_9_0  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_5_0     } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_xc_9_0  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_xc_9_1  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_xc_9_2  } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_6_0     } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_xc_10_0 } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_xc_10_1 } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode],
       { llvmver_7_0     } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode,llvmflag_memcpy_indiv_align,llvmflag_null_pointer_valid],
       { llvmver_7_1     } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode,llvmflag_memcpy_indiv_align,llvmflag_null_pointer_valid],
       { llvmver_8_0     } [llvmflag_load_getelptr_type,llvmflag_call_no_ptr,llvmflag_alias_double_type,llvmflag_fembed_bitcode,llvmflag_memcpy_indiv_align,llvmflag_null_pointer_valid]
     );

   { Supported optimizations, only used for information }
   supported_optimizerswitches = genericlevel1optimizerswitches+
                                 genericlevel2optimizerswitches+
                                 genericlevel3optimizerswitches-
                                 { no need to write info about those }
                                 [cs_opt_level1,cs_opt_level2,cs_opt_level3]+
                                 [cs_opt_loopunroll,cs_opt_stackframe,
				  cs_opt_nodecse,cs_opt_reorder_fields,cs_opt_fastmath];

   level1optimizerswitches = genericlevel1optimizerswitches;
   level2optimizerswitches = genericlevel2optimizerswitches + level1optimizerswitches + [cs_opt_nodecse,cs_opt_stackframe];
   level3optimizerswitches = genericlevel3optimizerswitches + level2optimizerswitches + [];
   level4optimizerswitches = genericlevel4optimizerswitches + level3optimizerswitches + [];

   function llvmversion2enum(const s: string): tllvmversion;

Implementation

  function llvmversion2enum(const s: string): tllvmversion;
    begin
      for result:=succ(low(llvmversionstr)) to high(llvmversionstr) do
        begin
          if s=llvmversionstr[result] then
            exit;
        end;
      result:=llvmver_invalid;
    end;

end.
