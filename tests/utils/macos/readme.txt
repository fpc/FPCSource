Usage
-----

To perform crosstest on MacOS from Darwin, issue the following
unix command to compile the tests:

    make clean alltests TEST_OS_TARGET=macos \
        USEUNITDIR=/Projekt/Freepascal/fpc/rtl/macos TEST_OPT="-WT -st"

Then use MPW to complete compilation and run tests:

    LinkRunTests <tests directory>

Example:

    LinkRunTests "{fpcdir}:tests:" ·· {diagnostics}

Here output and error is redirected to a file, stored in a variable.

The MPW script also writes entires (with unix line endings) in the "log" file.
This can then be analyzed by the unix command:

    make rundigest

Caveats
-------
The order of entries in the log file is different from normal testing, since
all runs came after all compilations. The rundigest program thinks this
is erroneous and reports unexpected runs.

Whishlist and todo's
--------------------
Either make fpc capable of assemble and link the program via AppleEvents,
or make rundigest capable of sorting the entries before analyzing.

LinkRunTests could write more kinds of entries to log.
