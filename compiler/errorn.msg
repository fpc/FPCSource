;
; Dutch messagefile for Free Pascal Compiler
;
; The letters until the _ indicate which type of message will be displayed,
; they are listed in order of importance, combinations may be made. The
; default when no type is listed is V_Default (=Error)
;  F - Fatal Error
;  E - Error
;  W - Warning
;  N - Note
;  H - Hint
;  I - Info
;  L - Linenumber
;  U - Used
;  T - Tried
;  M - Macro
;  P - Procedure
;  C - Conditional
;  D - Debug
; Special cases:
;  S - Stop the compiler after displaying the message
;
; Warning: When this file is loaded at runtime, the comments are also
;          loaded to the memory
;
I_$1 kB vrij
L_$1 lijnen $2 kB vrij
I_$1 stream kB gebruikt
I_$1 EMS kB gebruikt
I_Compiler: $1
I_Units gezocht in: $1
D_Bron OS: $1
I_Doel OS: $1
U_Programma pad: $1
U_GNU C bibliotheek: $1
I_$1 Lijnen gecompileerd, $2 sec
;
; Scanner (also scanner part of the assembler)
;
F_Onverwacht einde van bestand
F_String langer dan regel
F_ongeldig teken
F_Taalfout: $2 verwacht in kolom $1
U_Ingevoegd bestand $1 word geopend
W_Commentaar diepte $1 gevonden
N_$F directive (FAR) genegeerd
N_Stapeloverloop test is globaal onder linux
W_Foutieve compiler schakelaar
W_Deze compiler schakelaar heeft ook een globaal effect
E_Foutieve constante voor een karakter
F_kan bestand niet openen
F_Kan ingevoerd bestand niet openen $1
E_Te veel $ENDIFs of $ELSEs
W_Record velden kunnen maar op 1,2 of 4 bytes uitgelijnd worden.
E_$ENDIF verwacht voor $1 op $2 $3
E_Taalfout bij het compileren van een conditionele compilatie uitdrukking
E_Evalueren van een conditionele compilatie uitdrukking
W_Macro inhoud is afgsneden op 255 karakters voor evaluatie
E_ENDIF zonder IF(N)DEF
F_Gebruiker definitie: $1
E_Gebruiker definitie: $1
W_Gebruiker definitie: $1
N_Gebruiker definitie: $1
H_Gebruiker definitie: $1
I_Gebruiker definitie: $1
E_Sleutelwoord herdefinieren als macro heeft geen effect
F_Macro buffer overflow bij lezen of expanderen van macro
W_Expansie van macro meer dan 16 niveus diep, waarschijnlijk een recursieve macro
E_compiler schakelaar is niet toegestaan in (* ... *) commentaar
D_Behandel schakelaar "$1"
C_ENDIF $1 gevonden
C_IFDEF $1 gevonden, $2
C_IFOPT $1 gevonden, $2
C_IF $1 gevonden, $2
C_IFNDEF $1 gevonden, $2
C_ELSE $1 gevonden, $2
C_Negeer tot...
I_Druk <return> om voort te gaan
;
; Parser
;
E_Parser - Syntax fout
W_Procedure type FAR genegeerd
W_Procedure type NEAR genegeerd
E_geen DLL File opgegeven
E_Constructor naam moet INIT zijn
E_Destructor name moet DONE zijn
E_Foutieve open parameter
E_Procedure type INLINE wordt niet ondersteund
W_Private methodes zijn niet VIRTUAL
E_Constructor mag niet private or protected zijn
E_Destructor mag niet private or protected zijn
N_Class heeft slechts 1 destructor
E_Lokale class definities zijn niet toegestaan
E_Anonieme class definities zijn niet toegestaan
E_Ordinale waarde verwacht
E_Foutieve parameter lijst
E_overladen identifieerder is geen functie identifieerder
E_overladen functies hebben identieke parameterlijst
E_functie declaratie klopt niet met voorwaartse declaratie $1
N_Slechts waarden kunnen overgeslagen worden in opsommingstypes
N_Interface en implementatie namen zijn verschillend
E_functie meer dan 31 keer genest
E_Bereikfout bij evalueren constantes
E_Pointer type verwacht
E_class type verwacht
E_Dubbel case geval
E_Getypeerde constantes van classes zijn niet toegestaan
E_functie variabelen van meervoudige functies zijn niet toegestaan
E_string lengte moet tussen 1 en 255 liggen
W_Gebruik de uitgebreide syntax van DISPOSE en NEW om nieuwe instanties van classes te genereren
E_class verwacht
E_methode verwacht
E_function declaratie komt niet overeen met een methode van deze class
P_procedure/functie $1 ($2) op lijn $3
E_Illegale vlottende komma constante
E_FAIL mag slechts in constructors gebruikt worden
E_Destructors hebben geen parameters
E_string types stemmen niet overeen, wegens $V+ modus
E_Alleen class methodes kunnen gerefereerd met classe methodes
E_Alleen class methodes togankelijk in classen
E_Constante en CASE type komen niet overeen
E_Het symbool kan niet uitgevoerd worden van een bibliotheek
E_Een virtuele methode moet met OVERRIDE overroepen worden: $1
E_Er is geen geerfde methode die overroepen kan worden: $1
E_No member is provided to access property
E_Illegal symbol for property access
E_Cannot write a protected field of an object
E_all overloaded methods must be virtual if one is virtual: $1
E_overloaded methods which are virtual must have the same return type: $1
E_EXPORT declared functions can't be nested
E_methods can't be EXPORTed
E_call by var parameters have to match exactly
E_Class isn't a super class of the current class
E_SELF is only allowed in methods
E_methods can be only in other methods called direct with type identifier of the class
E_Illegal use of ':'
E_range check error in set constructor or duplicate set element
E_Pointer to class expected
E_Expression must be constructor call
E_Expression must be destructor call
E_Type conflict between set elements
W_The operator / isn't defined for integer, the result will be real, use DIV instead
E_Illegal order of record elements
E_Expression type must by class or record type
E_Functions with void return value can't return any value
E_constructors and destructors must be methods
W_Automatic type conversion from floating type to COMP which is an integer type
E_Operator is niet hergedefineerd
E_Variable or type indentifier expected
E_Re-raise isn't possible there
E_The extended syntax of new or dispose isn't allowed for a class
E_Assembler incompatible with function return value
E_Procedure overloading is switched off
E_It is not possible to overload this operator (overload = instead)
E_Comparative operator must return a boolean value
E_Only virtual methods can be abstract
F_Use of unsupported feature!
E_The mix of CLASSES and OBJECTS isn't allowed
W_Unknown procedure directive had to be ignored: $1
E_absolute can only be associated to ONE variable
E_absolute can only be associated a var or const
E_Abtract methods shouldn't have any definition (with function body)
E_This overloaded function can't be local (must be exported)
E_succ or pred on enums with assignments not possible
W_Virtual methods are used without a constructor
M_Macro defined: $1
M_Macro undefined: $1
M_Macro $1 set to $2
I_Compiling $1
D_Compiling $1 for the second time
E_Array properties aren't allowed at this point
E_No property found to override
E_Only one default property is allowed, found inherited default property in class %1
E_The default property must be an array property
;
; Symbol-table errors/warnings
;
N_Local variable $1 doesn't seem to be initialized yet !
E_Identifier not found $1
F_Internal Error in SymTableStack()
E_Duplicate identifier $1
E_Unknown identifier $1
E_Forward declaration not solved $1
F_Identifier type already defined as type
E_Type identifier expected
E_Type mismatch
E_Error in type defenition
E_Type identifier not defined
E_Only static variables can be used in static methods or outside methods
E_Invalid call to tvarsym.mangledname()
F_record or class type expected
E_To generate an instance of a class or an object with an abtract method isn't allowed
E_Label niet gedefineerd $1
E_Label foutief gedefineerd
E_GOTO en LABEL zijn momenteel uitgeschakeld (gebruik -Sg)
E_Label niet gevonden
E_identifier is geen label
E_label is al gedefineerd
E_Type declaratie van set elementen is foutief
E_Voorwaarde definitie van klasse niet opgelost $1
H_Identifier not used $1, declared in line $2
E_Elementen van set zijn incompatible
E_Set type verwacht
W_Resultaat van functie ogeschijnlijk niet present
E_Onbekend record lid
N_Locale variabele schijnt niet gebruikt te worden: $1
E_identifier idents no member
;
; Codegenerator
;
E_BREAK niet toegestaan
E_CONTINUE niet toegestaan
E_Expressie te ingewikkeld - overloop van wiskundige processor
E_Foutieve expressie
E_Foutieve integer
E_Foutieve qualifier
E_Bovengrens < ondergrens
E_Foutive lusteller
E_Weet niet welke meervoudige procedure ik moet aanroepen
E_Parameters gebruiken meer dan 64 kilobyte
E_Foute type conversie
E_Files moeten var parameters zijn
E_U kunt daar geen far pointer gebruiken
E_illegal call by reference parameters
E_procedures die EXPORT gedeclareerd staan kunnen niet aangeroepen worden
W_Possible illegal call of constructor or destructor (doesn't match to this context)
N_Inefficiente code
W_Deze code wordt nooit uitgevoerd
E_procedure call with stackframe ESP/SP
E_Abstract methods can't be called directly
F_Internal Error in getfloatreg(), allocation failure
F_Onbekend vlottend komma type
F_SecondVecn() base defined twice
F_Extended cg68k not supported
F_32-bit unsigned not supported in MC68000 mode
F_Internal Error in secondinline()
D_Register $1 gewicht $2 $3
E_Stack limit excedeed in local routine
D_Stack frame wordt niet gemaakt
;
; Assembler parsers
;
F_Delen door nul in asm evaluator
F_Evaluator stack overloop
F_Evaluator stack leegloop
F_Invalid numeric format in asm evaluator
F_Invalid Operator in asm evaluator
F_Unknown error in asm evaluator
W_Ongeldige numerieke waarde
E_escape sequence ignored $1
E_Asm syntax error - Prefix not found
E_Asm syntax error - Trying to add more than one prefix
E_Asm syntax error - Opcode not found
E_Invalid symbol reference
W_Calling an overload function in an asm
E_Constant value out of bounds
E_Non-label pattern contains @
E_Invalid Operand: $1
W_Override operator not supported
E_Error in binary constant: $1
E_Error in octal constant: $1
E_Error in hexadecimal constant: $1
E_Error in integer constant: $1
E_Invalid labeled opcode
F_Internal error in Findtype()
E_Invalid size for MOVSX/MOVZX
E_16-bit base in 32-bit segment
E_16-bit index in 32-bit segment
E_Invalid Opcode
E_Constant reference not allowed
W_Fwait can cause emulation problems with emu387
E_Invalid combination of opcode and operands
W_Opcode $1 not in table, operands not checked
F_Internal Error in ConcatOpcode()
E_Invalid size in reference
E_Invalid middle sized operand
E_Invalid three operand opcode
E_Assembler syntax error
E_Invalid operand type
E_Segment overrides not supported
E_Invalid constant symbol $1
F_Internal Errror converting binary
F_Internal Errror converting hexadecimal
F_Internal Errror converting octal
E_Invalid constant expression
E_Unknown identifier: $1
E_Trying to define an index register more than once
E_Invalid field specifier
F_Internal Error in BuildScaling()
E_Invalid scaling factor
E_Invalid scaling value
E_Scaling value only allowed with index
E_Invalid assembler syntax. No ref with brackets)
E_Expressions of the form [sreg:reg...] are currently not supported
E_Trying to define a segment register twice
E_Trying to define a base register twice
E_Trying to use a negative index register
E_Asm syntax error - error in reference
E_Local symbols not allowed as references
E_Invalid operand in bracket expression
E_Invalid symbol name: $1
E_Invalid Reference syntax
E_Invalid string as opcode operand: $1
W_@CODE and @DATA not supported
E_Null label references are not allowed
E_Cannot use SELF outside a method
E_Asm syntax error - Should start with bracket
E_Asm syntax error - register: $1
E_SEG and OFFSET not supported
E_Asm syntax error - in opcode operand
E_Invalid String expression
E_Constant expression out of bounds
F_Internal Error in BuildConstant()
W_A repeat prefix and a segment override on <= i386 may result in errors if an interrupt occurs
E_Invalid or missing opcode
E_Invalid combination of prefix and opcode: $1
E_Invalid combination of override and opcode: $1
E_Too many operands on line
E_Duplicate local symbol: $1
E_Unknown label identifer: $1
E_Assemble node syntax error
E_Undefined local symbol: $1
D_Starting intel styled assembler parsing...
D_Finished intel styled assembler parsing...
E_Not a directive or local symbol: $1
E_/ at beginning of line not allowed
E_NOR not supported
E_Invalid floating point register name
W_Modulo not supported
E_Invalid floating point constant: $1
E_Size suffix and destination register do not match
E_Internal error in ConcatLabeledInstr()
W_Floating point binary representation ignored
W_Floating point hexadecimal representation ignored
W_Floating point octal representation ignored
E_Invalid real constant expression
E_Parenthesis are not allowed
E_Invalid Reference
E_Cannot use __SELF outside a method
E_Cannot use __OLDEBP outside a nested procedure
W_Identifier $1 supposed external
E_Invalid segment override expression
E_Strings not allowed as constants
D_Starting AT&T styled assembler parsing...
D_Finished AT&T styled assembler parsing...
E_Switching sections is not allowed in an assembler block
E_Invalid global definition
E_Line separator expected
W_globl not supported
W_align not supported
W_lcomm not supported
W_comm not supported
E_Invalid local common definition
E_Invalid global common definition
E_local symbol: $1 not found inside asm statement
E_assembler code not returned to text
F_internal error in BuildReference()
E_invalid opcode size
W_NEAR ignored
W_FAR ignored
D_Creating inline asm lookup tables
W_Using a defined name as a local label
F_internal error in HandleExtend()
E_Invalid character: <
E_Invalid character: >
E_Unsupported opcode
E_Increment and Decrement mode not allowed together
E_Invalid Register list in movem/fmovem
E_Invalid Register list for opcode
E_68020+ mode required to assemble
D_Starting Motorola styled assembler parsing...
D_Finished Motorola styled assembler parsing...
W_XDEF not supported
W_Functions with void return value can't return any value in asm code
E_Invalid suffix for intel assembler
E_Extended not supported in this mode
E_Comp not supported in this mode
W_You need GNU as version >= 2.81 to compile this MMX code
;
; Exec, calls to assembler, linker, binder, libraries
;
I_Assembling (pipe) $1
E_Can't create assember file $1
W_Assembler $1 not found, switching to external assembling
U_Using assembler: $1
W_Error while assembling
W_Can't call the assembler, switching to external assembling
I_Assembling $1
W_Linker $1 not found, switching to external linking
U_Using linker: $1
E_File $1 not found, Linking may fail !!
W_Error while linking
W_Can't call the linker, switching to external linking
I_Linking $1
W_binder not found, switching to external binding
W_ar not found, switching to external ar
E_Dynamic Libraries not supported
I_Closing script $1
;
; Unit handling, PPU file
;
U_PPU Loading $1
D_PPU Time: $1
D_PPU File too short
D_PPU Invalid Header (no PPU at the begin)
D_PPU Invalid Version $1
D_PPU Flags: $1
D_PPU Crc: $1
T_PPU Source: $1
D_objectfile and assemblerfile are older than ppufile
D_objectfile is older than assemblerfile
T_Unitsearch: $1
U_Writing $1
F_Can't Write PPU-File
F_reading PPU-File
F_Invalid PPU-File entry: $1
F_PPU Dbx count problem
E_Illegal unit name: $1
F_Too much units
F_Circular unit reference
F_Can't compile unit $1, no sources available
W_Compiling the system unit requires the -Us switch
E_$1 Errors
F_There were errors compiling module, compilation stopped
