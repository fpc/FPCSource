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
E_String types stemmen niet overeen, wegens $V+ modus
E_Alleen class methodes kunnen gerefereerd met classe methodes
E_Alleen class methodes togankelijk in classen
E_Constante en CASE type komen niet overeen
E_Het symbool kan niet uitgevoerd worden van een bibliotheek
E_Een virtuele methode moet met OVERRIDE overroepen worden: $1
E_Er is geen geerfde methode die overroepen kan worden: $1
E_Er is geen lid om de eigenschap te bereiken
E_Illegaal symbool voor eigenschap toegang
E_Kan niet in beschermd veld van een object schrijven
E_Alle overladen methodse moeten virtueel zijn als 1 virtueel is: $1
E_Virtuele Overladen methodes moeten hetzelfde resultaat type hebben: $1
E_EXPORT gedeclareerde functions kunnen niet genest worden.
E_Methodes kunnen niet geexporteerd worden.
E_Aanroep per referentie parameters moet overeenstemmende types hebben.
E_Class is geen ouder class van de huidige class
E_SELF is alleen toegestaan in methodes.
E_Methodes kunnen alleen in andere methodes worden aangeroepen met type identifieerder
E_Ongeldig gebruik van ':'
E_Bereik fout in set constructor of dubbel set element
E_Pointer naar class verwacht
E_Uitdrukking moet een constructor call zijn.
E_Expression must be destructor call
E_Type conflict tussen set elementen.
W_De operator / geeft een resultaat is van type real. Gebruik DIV voor integers.
E_Ongeldige volgorde van record leden.
E_Type van uitdrukking moet class of record type zijn.
E_Procedures kunnen geen resultaat teruggeven.
E_Constructors en Destructors moeten methodes zijn
W_Type conversion van floating type to COMP, hetgeen een integer type is.
E_Operator is niet hergedefineerd
E_Variable of type indentifieerder verwacht
E_Re-raise is hier niet mogelijk
E_De uitgebreide syntax van NEW of DISPOSE is niet toegestaan voor classes
E_Assembler stemt niet overeen met functieresultaat type
E_Procedure overladen is afgezet
E_Deze operator kan niet overladen worden.
E_Vergelijkings operator moet een boolean type als resultaat hebben.
E_Enkel virtuele methodes kunnen abstract zijn.
F_Gebruik van niet ondersteunde optie !
E_CLASSES and OBJECTS kunnen niet gemengd worden.
W_Ongekende procedure schakelaar is genegeerd: $1
E_Absolute kan maar aan een variabele verbonden worden.
E_Absolute kan slechts aan variabelen of constantes verbonden worden.
E_Abtracte methodes kunnen niet gedefineerd worden.
E_Deze overladen procedure kan niet lokaal zijn, (moet geexporteerd worden)
E_SUCC of PRED op enumeratie types met toekenningen zijn niet mogelijk
W_Gebruike van Virtuele methods zonder constructor
M_Macro gedefinieerd: $1
M_Macro ongedefinieerd: $1
M_Macro $1 wordt $2
I_Compilatie van $1
D_Compilatie van $1 voor de tweede keer.
E_Array eigenschappen zijn hier niet toegestaan.
E_Geen eigenschap gevonden om te herdefinieren
E_Slechts 1 default eigenschap is toegestaan, geerfde default eigenschap in kind gevonden.
E_Default eigenschappen moeten van type Array zijn.
;
; Symbol-table errors/warnings
;
N_Lokale variabele $1 schijnt niet geinitialiseerd te zijn !
E_Identifieerder niet gevonden $1
F_Interne fout in SymTableStack()
E_Dubbele identifieerder $1
E_Onbekende identifieerder $1
E_Voorwaartse declaratie niet opgelost $1
F_Type Identifieerder reeds gedefineerd als type
E_Type identifieerder verwacht
E_Types stemmen niet overeen
E_Fout in type defenitie
E_Type identifieerder niet gedefinieerd
E_Alleen statische variabelen kunnen gebruikt worden in statische methodes
E_Ongeldige aanroep van tvarsym.mangledname()
F_record of class type verwacht
E_U kan geen vertegenwoordiger van een klasse met abstracte methodes instantieren
E_Label niet gedefineerd $1
E_Label foutief gedefineerd
E_GOTO en LABEL zijn momenteel uitgeschakeld (gebruik -Sg)
E_Label niet gevonden
E_identifier is geen label
E_label is al gedefineerd
E_Type declaratie van set elementen is foutief
E_Voorwaartse definitie van klasse niet opgelost $1
H_Identifier niet gebruikt $1, gedeclareerd in lijn $2
E_Elementen van set zijn incompatible
E_Set type verwacht
W_Resultaat van functie ogeschijnlijk niet present
E_Onbekend record lid
N_Locale variabele schijnt niet gebruikt te worden: $1
E_Geen lid met die naam
;
; Codegenerator
;
E_BREAK niet toegestaan
E_CONTINUE niet toegestaan
E_Uitdrukking te ingewikkeld - overloop van wiskundige processor
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
E_Ongeldige aanroep met referentie parameters
E_procedures die EXPORT gedeclareerd staan kunnen niet aangeroepen worden
W_Mogelijke illegale aanroep van constructor of destructor 
N_Inefficiente code
W_Deze code wordt nooit uitgevoerd
E_procedure aanroep met stackframe ESP/SP
E_Abstracte methodes kunnen niet direct aangeroepen worden.
F_Interne fout in getfloatreg(), allocatie faling
F_Onbekend vlottende komma type
F_SecondVecn() basis tweemaal gedefinieerd.
F_Extended cg68k niet ondersteund
F_32-bit unsigned niet ondersteund in MC68000 modus
F_Interne fout in secondinline()
D_Register $1 gewicht $2 $3
E_Stack limiet excedeed in local routine
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
I_Assembleren (pijp) van $1
E_Kan geen assembler bestand $1 niet maken
W_Assembleerder $1 niet gevonden, overschakeling naar externe assemblage
U_Gebruikte assembler: $1
W_Fout bij assembleren
W_Kan assembleerder niet oproepen, overschakeling naar externe assemblage
I_Assembleren van $1
W_Linker $1 niet gevonden, oveschakeling naar externe linker
U_Gebruikte linker: $1
E_Bestand $1 niet gevonden, linken kan foutlopen !
W_Error tijdens linken
W_Kan linker niet oproepen, overschakeling naar extern linken
I_Linken van $1
W_binder niet gevonden, overschakeling naar extern binden.
W_ar niet gevonden, overschakeling naar externe ar
E_Dynamische bibliotheken niet ondersteund
I_Afsluiten batch $1
;
; Unit handling, PPU file
;
U_PPU Laden $1
D_PPU Tijd: $1
D_PPU bestand te kort
D_PPU Ongeldige hoofding (geen PPU aan het begin)
D_PPU Ongeldige Versie $1
D_PPU Vlaggen: $1
D_PPU CRC: $1
T_PPU Bronbestand: $1
D_Objectbestand en assemblerbestand zijn ouder als PPU bestand
D_Objectbestand is ouder dan assembler bestand.
T_Unit zoeken: $1
U_Schrijven van $1
F_Kan geen PPU bestand schrijven
F_Lezen van PPU bestand
F_Ongeldig PPU bestand post: $1
F_PPU Dbx aantal probleem
E_Ongeldige unit naam: $1
F_Te veel units
F_Circulaire unit referentie
F_Kan unit $1 niet compileren, geen broncode beschikbaar
W_De systeem unit compileren vereist de -Us schakelaar
E_$1 Fouten
F_Er waren fouten bij het compileren van een module, compilatie gestopt.

