{
    Copyright (c) 2010, 2013 by Jonas Maebe

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
    globtype, cpubase;

Type
   { possible supported processors for this target }
   tllvmcputype =
      (llvmcpu_none,
       { may add older/newer versions if required/appropriate }
       llvmcpu_33
      );


Const

   llvmcputypestr : array[tllvmcputype] of string[9] = ('',
     'LLVM-3.3'
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
