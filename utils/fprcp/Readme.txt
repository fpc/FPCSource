This binary with source code is released to public domain.
The utility can be compiled by Turbo Pascal (but 16-bit version
cannot handle files greather than 64K!), Delphi or Free Pascal.
fprcp.exe extracts from C header and Pascal files included into
resource scripts numerical constants and replaces these constants
to its values in resource script. Modified resource script is writing
to stdout.

fprcp.exe can be used as preprocessor by windres GNU-win32 utility.
It was tested with windres 2.9.4 successfully.
syntax: 
windres --preprocessor fprcp.exe [another switches].

Notes:
1) current fprcp does not support typecasting and operations with
non-numeric constants;
2) Old versions of windres cannot create .res files;
3) in fprcp also source code written by Lars Fosdal 1987 and
   released to the public domain 1993 was used
4) updated to accept defines.inc
   parser was expecting body for procedure/function declaration

files:
readme.txt   - this file

USE_DEMO.BAT |
DEMO.RC      |
DEMO.PP      - demo files
DEMO.H       |

COMMENTS.PP  |
PASPREP.PP   |
FPRCP.PP      - source code
EXPR.PP      |

fprcp.exe     - executable
