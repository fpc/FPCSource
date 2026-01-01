This directory contains a generic HTML loader page for a WebAssembly module
that needs a WASI and JOB hosting environment.

Compile the .lpr with pas2js, and load the page in your browser. 
By default the page will attempt to load a demo.wasm module.

You can specify the module to load in 1 of 3 ways:

  1. from external variable wasmFilename. 
     To this end, copy hostconfig-template.js to hostconfig.js.
     In this file, change the name of the webassembly to load.

  2. from the first part of hash:  #moduleName/

  3. from query variable wasmmodule: ?wasmmodule=x

the extension .wasm must be specified.

