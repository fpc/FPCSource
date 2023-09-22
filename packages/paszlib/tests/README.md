
The WebAssembly version of the tczipper test reads/writes temporary files in the current
directory and in the TMP directory (this is the directory returned by gettempdir). 

You must pre-open these directories so the wasm environment can write in it. 

Using wasmtime this can be done as follows:

wasmtime --dir=.  --dir=/tmp tczipper.wasm

Similarly the testsingle test needs the same 2 directories:

wasmtime --dir=. --dir=/tmp testsingle.wasm
