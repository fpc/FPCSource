const msgtxt : array[0..00092,1..240] of char=(
  'T_Compiler: $1'#000+
  'D_Source OS: $1'#000+
  'I_Target OS: $1'#000+
  'T_Using executable path: $1'#000+
  'T_Using unit path: $1'#000+
  'T_Using include path: $1'#000+
  'T_Using library path: $1'#000+
  'T_Using object path: $1'#000+
  'I_$1 Lines compiled, $2 sec'#000+
  'F_No memory left'#000+
  'F_Unexpected end of file',#000+
  'F_String exceeds line'#000+
  'F_illegal character'#000+
  'F_Syntax error $1 expected'#000+
  'T_Start reading includefile $1'#000+
  'W_Comment level $1 found'#000+
  'N_$F directive (FAR) ignored'#000+
  'N_Stack check is global under linux'#000+
  'N_Ignored compiler switch $1'#000+
  'W_Illegal compiler s','witch $1'#000+
  'W_This compiler switch has a global effect'#000+
  'E_Illegal char constant'#000+
  'F_Can'#039't open file $1'#000+
  'F_Can'#039't open include file $1'#000+
  'E_Too many $ENDIFs or $ELSEs'#000+
  'W_Records fields can be aligned to 1,2,4 or 16 bytes only'#000+
  'W_Enumerated can be saved i','n 1,2 or 4 bytes only'#000+
  'E_$1 expected for $2 defined in line $3'#000+
  'E_Syntax error while parsing a conditional compiling expression'#000+
  'E_Evaluating a conditional compiling expression'#000+
  'W_Macro contents is cut after char 255 to evalute expression'#000+
  'E_END','IF without IF(N)DEF'#000+
  'F_User defined: $1'#000+
  'E_User defined: $1'#000+
  'W_User defined: $1'#000+
  'N_User defined: $1'#000+
  'H_User defined: $1'#000+
  'I_User defined: $1'#000+
  'E_Keyword redefined as macro has no effect'#000+
  'F_Macro buffer overflow while reading or expanding a macro'#000+
  'W_Ex','tension of macros exceeds a deep of 16.'#000+
  'E_compiler switches aren'#039't allowed in (* ... *) styled comments'#000+
  'D_Handling switch "$1"'#000+
  'C_ENDIF $1 found'#000+
  'C_IFDEF $1 found, $2'#000+
  'C_IFOPT $1 found, $2'#000+
  'C_IF $1 found, $2'#000+
  'C_IFNDEF $1 found, $2'#000+
  'C_ELSE $1 foun','d, $2'#000+
  'C_Skipping until...'#000+
  'I_Press <return> to continue'#000+
  'W_Unsupported switch $1'#000+
  'W_Illegal compiler directive $1'#000+
  'T_Back in $1'#000+
  'W_Unsupported assembler style specified $1'#000+
  'E_Wrong switch toggle, use ON/OFF or +/-'#000+
  'E_Parser - Syntax Error'#000+
  'W_Proced','ure type FAR ignored'#000+
  'W_Procedure type NEAR ignored'#000+
  'W_Procedure type REGISTER ignored'#000+
  'E_No DLL File specified'#000+
  'E_Constructor name must be INIT'#000+
  'E_Destructor name must be DONE'#000+
  'E_Illegal open parameter'#000+
  'E_Procedure type INLINE not supported'#000+
  'W_Pri','vate methods shouldn'#039't be VIRTUAL'#000+
  'W_Constructor should be public'#000+
  'W_Destructor should be public'#000+
  'N_Class should have one destructor only'#000+
  'E_Local class definitions are not allowed'#000+
  'E_Anonym class definitions are not allowed'#000+
  'E_Illegal parameter ','list'#000+
  'E_Wrong parameter type specified'#000+
  'E_Wrong amount of parameters specified'#000+
  'E_overloaded identifier $1 isn'#039't a function'#000+
  'E_overloaded functions have the same parameter list'#000+
  'E_function header doesn'#039't match the forward declaration $1'#000+
  'E_functi','on header $1 doesn'#039't match forward : var name changes $2'+
  ' => $3'#000+
  'N_only values can be jumped over in enumeration types'#000+
  'N_Interface and implementation names are different !'#000+
  'E_function nesting > 31'#000+
  'E_range check error while evaluating constants',#000+
  'W_range check error while evaluating constants'#000+
  'E_duplicate case label'#000+
  'E_typed constants of classes are not allowed'#000+
  'E_functions variables of overloaded functions are not allowed'#000+
  'E_string length must be a value from 1 to 255'#000+
  'W_use extended s','yntax of DISPOSE and NEW to generate instances of ob'+
  'jects'#000+
  'E_class identifier expected'#000+
  'E_type identifier not allowed here'#000+
  'E_method identifier expected'#000+
  'E_function header doesn'#039't match any method of this class'#000+
  'P_procedure/function $1'#000+
  'E_Illegal',' floating point constant'#000+
  'E_FAIL can be used in constructors only'#000+
  'E_Destructors can'#039't have parameters'#000+
  'E_Only class methods can be referred with class references'#000+
  'E_Only class methods can be accessed in class methods'#000+
  'E_Constant and CASE types ','do not match'#000+
  'E_The symbol can'#039't be exported from a library'#000+
  'W_A inherited method is hidden by $1'#000+
  'E_There is no method in an ancestor class to be overridden: $1'#000+
  'E_No member is provided to access property'#000+
  'W_Stored prorperty directive is not ye','t implemented'#000+
  'E_Illegal symbol for property access'#000+
  'E_Cannot access a protected field of an object here'#000+
  'E_Cannot access a private field of an object here'#000+
  'W_overloaded of virtual method must be virtual: $1'#000+
  'E_overloaded methods which are virtu','al must have the same return typ'+
  'e: $1'#000+
  'E_EXPORT declared functions can'#039't be nested'#000+
  'E_methods can'#039't be EXPORTed'#000+
  'E_call by var parameters have to match exactly'#000+
  'E_Class isn'#039't a parent class of the current class'#000+
  'E_SELF is only allowed in methods',#000+
  'E_methods can be only in other methods called direct with type identif'+
  'ier of the class'#000+
  'E_Illegal use of '#039':'#039#000+
  'E_range check error in set constructor or duplicate set element'#000+
  'E_Pointer to object expected'#000+
  'E_Expression must be constructor call'#000,+
  'E_Expression must be destructor call'#000+
  'E_Illegal order of record elements'#000+
  'E_Expression type must be class or record type'#000+
  'E_Functions with void return value can'#039't return any value'#000+
  'E_constructors and destructors must be methods'#000+
  'E_Operator is no','t overloaded'#000+
  'E_Re-raise isn'#039't possible there'#000+
  'E_The extended syntax of new or dispose isn'#039't allowed for a class'#000+
  'E_Assembler incompatible with function return value'#000+
  'E_Procedure overloading is switched off'#000+
  'E_It is not possible to overload this',' operator (overload = instead)'#000+
  'E_Comparative operator must return a boolean value'#000+
  'E_Only virtual methods can be abstract'#000+
  'F_Use of unsupported feature!'#000+
  'E_The mix of CLASSES and OBJECTS isn'#039't allowed'#000+
  'W_Unknown procedure directive had to be ig','nored: $1'#000+
  'E_absolute can only be associated to ONE variable'#000+
  'E_absolute can only be associated a var or const'#000+
  'E_Abtract methods shouldn'#039't have any definition (with function bod'+
  'y)'#000+
  'E_This overloaded function can'#039't be local (must be exported)'#000+
  'W','_Virtual methods are used without a constructor in $1'#000+
  'M_Macro defined: $1'#000+
  'M_Macro undefined: $1'#000+
  'M_Macro $1 set to $2'#000+
  'I_Compiling $1'#000+
  'D_Compiling $1 for the second time'#000+
  'E_Array properties aren'#039't allowed at this point'#000+
  'E_No property found to ov','erride'#000+
  'E_Only one default property is allowed, found inherited default proper'+
  'ty in class $1'#000+
  'E_The default property must be an array property'#000+
  'E_Virtual constructors are only supported in class object model'#000+
  'E_No default property available'#000+
  'E_T','he class can'#039't have a published section, use the {$M+} switch'+
  #000+
  'E_Forward declaration of class $1 must be resolved here to use the cla'+
  'ss as ancestor'#000+
  'E_Local operators not supported'#000+
  'E_Procedure directive $1 not allowed in interface section'#000+
  'E_P','rocedure directive $1 not allowed in implementation section'#000+
  'E_Procedure directive $1 not allowed in procvar declaration'#000+
  'E_Function is already declared Public/Forward $1'#000+
  'E_Can'#039't use both EXPORT and EXTERNAL'#000+
  'E_NAME keyword expected'#000+
  'W_$1 not y','et supported inside inline procedure/function'#000+
  'W_Inlining disabled'#000+
  'I_Writing Browser log $1'#000+
  'H_may be pointer dereference is missing'#000+
  'F_Selected assembler reader not supported'#000+
  'E_Procedure directive $1 has conflicts with other directives'#000+
  'E_Call','ing convention doesn'#039't match forward'#000+
  'E_Register calling (fastcall) not supported'#000+
  'E_Property can'#039't have a default value'#000+
  'E_The default value of a property must be constant'#000+
  'E_Symbol can'#039't be published, can be only a class'#000+
  'E_That kind of proper','ty can'#039't be published'#000+
  'W_Empty import name specified'#000+
  'E_Division by zero'#000+
  'E_Invalid floating point operation'#000+
  'E_Type mismatch'#000+
  'E_Integer expression expected'#000+
  'E_Ordinal expression expected'#000+
  'E_Type identifier expected'#000+
  'E_Variable identifier expected'#000,+
  'E_pointer type expected'#000+
  'E_class type expected'#000+
  'E_Variable or type indentifier expected'#000+
  'E_Can'#039't evaluate constant expression'#000+
  'E_Set elements are not compatible'#000+
  'E_Operation not implemented for sets $1'#000+
  'W_Automatic type conversion from floating t','ype to COMP which is an i'+
  'nteger type'#000+
  'W_Using / will give a floating point result'#000+
  'H_use DIV instead to get an integer result'#000+
  'E_string types doesn'#039't match, because of $V+ mode'#000+
  'E_succ or pred on enums with assignments not possible'#000+
  'E_Can'#039't read',' or write variables of this type'#000+
  'E_Type conflict between set elements'#000+
  'E_Integer or real expression expected'#000+
  'E_Identifier not found $1'#000+
  'F_Internal Error in SymTableStack()'#000+
  'E_Duplicate identifier $1'#000+
  'E_Unknown identifier $1'#000+
  'E_Forward declaratio','n not solved $1'#000+
  'F_Identifier type already defined as type'#000+
  'E_Error in type definition'#000+
  'E_Type identifier not defined'#000+
  'E_Forward type not resolved $1'#000+
  'E_Only static variables can be used in static methods or outside metho'+
  'ds'#000+
  'E_Invalid call to tva','rsym.mangledname()'#000+
  'F_record or class type expected'#000+
  'E_Instances of classes or objects with an abtsract method are not allo'+
  'wed'#000+
  'E_Label not defined $1'#000+
  'E_Illegal label declaration'#000+
  'E_GOTO und LABEL are not supported (use command line switch -Sg)',#000+
  'E_Label not found'#000+
  'E_identifier isn'#039't a label'#000+
  'E_label already defined'#000+
  'E_illegal type declaration of set elements'#000+
  'E_Forward class definition not resolved $1'#000+
  'H_Parameter not used $1'#000+
  'N_Local variable not used $1'#000+
  'E_Set type expected'#000+
  'W_Function ','result does not seem to be set'#000+
  'E_Unknown record field identifier $1'#000+
  'W_Local variable $1 does not seem to be initialized'#000+
  'E_identifier idents no member $1'#000+
  'B_Found declaration: $1'#000+
  'E_BREAK not allowed'#000+
  'E_CONTINUE not allowed'#000+
  'E_Expression too com','plicated - FPU stack overflow'#000+
  'E_Illegal expression'#000+
  'E_Invalid integer'#000+
  'E_Illegal qualifier'#000+
  'E_High range limit < low range limit'#000+
  'E_Illegal counter variable'#000+
  'E_Can'#039't determine which overloaded function to call'#000+
  'E_Parameter list size exceeds 65535',' bytes'#000+
  'E_Illegal type conversion'#000+
  'E_File types must be var parameters'#000+
  'E_The use of a far pointer isn'#039't allowed there'#000+
  'E_illegal call by reference parameters'#000+
  'E_EXPORT declared functions can'#039't be called'#000+
  'W_Possible illegal call of constructor or',' destructor (doesn'#039't ma'+
  'tch to this context)'#000+
  'N_Inefficient code'#000+
  'W_unreachable code'#000+
  'E_procedure call with stackframe ESP/SP'#000+
  'E_Abstract methods can'#039't be called directly'#000+
  'F_Internal Error in getfloatreg(), allocation failure'#000+
  'F_Unknown float type',#000+
  'F_SecondVecn() base defined twice'#000+
  'F_Extended cg68k not supported'#000+
  'F_32-bit unsigned not supported in MC68000 mode'#000+
  'F_Internal Error in secondinline()'#000+
  'D_Register $1 weight $2 $3'#000+
  'E_Stack limit excedeed in local routine'#000+
  'D_Stack frame is omited'#000,+
  'E_Unable to inline object methods'#000+
  'E_Unable to inline procvar calls'#000+
  'E_No code for inline procedure stored'#000+
  'E_Element zero of an ansi/wide- or longstring can'#039't be accessed, u'+
  'se (set)length instead'#000+
  'F_Divide by zero in asm evaluator'#000+
  'F_Evaluator ','stack overflow'#000+
  'F_Evaluator stack underflow'#000+
  'F_Invalid numeric format in asm evaluator'#000+
  'F_Invalid Operator in asm evaluator'#000+
  'F_Unknown error in asm evaluator'#000+
  'W_Invalid numeric value'#000+
  'E_escape sequence ignored: $1'#000+
  'E_Asm syntax error - Prefix not ','found'#000+
  'E_Asm syntax error - Trying to add more than one prefix'#000+
  'E_Asm syntax error - Opcode not found'#000+
  'E_Invalid symbol reference'#000+
  'W_Calling an overload function in an asm'#000+
  'E_Constant value out of bounds'#000+
  'E_Non-label pattern contains @'#000+
  'E_Invalid ','Operand: $1'#000+
  'W_Override operator not supported'#000+
  'E_Error in binary constant: $1'#000+
  'E_Error in octal constant: $1'#000+
  'E_Error in hexadecimal constant: $1'#000+
  'E_Error in integer constant: $1'#000+
  'E_Invalid labeled opcode'#000+
  'F_Internal error in Findtype()'#000+
  'E_Invalid',' size for MOVSX/MOVZX'#000+
  'E_16-bit base in 32-bit segment'#000+
  'E_16-bit index in 32-bit segment'#000+
  'E_Invalid Opcode'#000+
  'E_Constant reference not allowed'#000+
  'W_Fwait can cause emulation problems with emu387'#000+
  'E_Invalid combination of opcode and operands'#000+
  'W_Opcode ','$1 not in table, operands not checked'#000+
  'F_Internal Error in ConcatOpcode()'#000+
  'E_Invalid size in reference'#000+
  'E_Invalid middle sized operand'#000+
  'E_Invalid three operand opcode'#000+
  'E_Assembler syntax error'#000+
  'E_Invalid operand type'#000+
  'E_Segment overrides not suppo','rted'#000+
  'E_Invalid constant symbol $1'#000+
  'F_Internal Errror converting binary'#000+
  'F_Internal Errror converting hexadecimal'#000+
  'F_Internal Errror converting octal'#000+
  'E_Invalid constant expression'#000+
  'E_Unknown identifier: $1'#000+
  'E_Trying to define an index register mo','re than once'#000+
  'E_Invalid field specifier'#000+
  'F_Internal Error in BuildScaling()'#000+
  'E_Invalid scaling factor'#000+
  'E_Invalid scaling value'#000+
  'E_Scaling value only allowed with index'#000+
  'E_Invalid assembler syntax. No ref with brackets)'#000+
  'E_Expressions of the form [','sreg:reg...] are currently not supported'#000+
  'E_Trying to define a segment register twice'#000+
  'E_Trying to define a base register twice'#000+
  'E_Trying to use a negative index register'#000+
  'E_Asm syntax error - error in reference'#000+
  'E_Local symbols not allowed as r','eferences'#000+
  'E_Invalid operand in bracket expression'#000+
  'E_Invalid symbol name: $1'#000+
  'E_Invalid Reference syntax'#000+
  'E_Invalid string as opcode operand: $1'#000+
  'W_@CODE and @DATA not supported'#000+
  'E_Null label references are not allowed'#000+
  'W_Calling of an overloaded',' function in direct assembler'#000+
  'E_Cannot use SELF outside a method'#000+
  'E_Asm syntax error - Should start with bracket'#000+
  'E_Asm syntax error - register: $1'#000+
  'E_SEG and OFFSET not supported'#000+
  'E_Asm syntax error - in opcode operand'#000+
  'E_Invalid String express','ion'#000+
  'E_Constant expression out of bounds'#000+
  'F_Internal Error in BuildConstant()'#000+
  'W_A repeat prefix and a segment override on <= i386 may result in erro'+
  'rs if an interrupt occurs'#000+
  'E_Invalid or missing opcode'#000+
  'E_Invalid combination of prefix and opco','de: $1'#000+
  'E_Invalid combination of override and opcode: $1'#000+
  'E_Too many operands on line'#000+
  'E_Duplicate local symbol: $1'#000+
  'E_Unknown label identifer: $1'#000+
  'E_Assemble node syntax error'#000+
  'E_Undefined local symbol: $1'#000+
  'D_Starting intel styled assembler parsi','ng...'#000+
  'D_Finished intel styled assembler parsing...'#000+
  'E_Not a directive or local symbol: $1'#000+
  'E_/ at beginning of line not allowed'#000+
  'E_NOR not supported'#000+
  'E_Invalid floating point register name'#000+
  'W_Modulo not supported'#000+
  'E_Invalid floating point constan','t: $1'#000+
  'E_Size suffix and destination register do not match'#000+
  'E_Internal error in ConcatLabeledInstr()'#000+
  'W_Floating point binary representation ignored'#000+
  'W_Floating point hexadecimal representation ignored'#000+
  'W_Floating point octal representation igno','red'#000+
  'E_Invalid real constant expression'#000+
  'E_Parenthesis are not allowed'#000+
  'E_Invalid Reference'#000+
  'E_Cannot use __SELF outside a method'#000+
  'E_Cannot use __OLDEBP outside a nested procedure'#000+
  'W_Identifier $1 supposed external'#000+
  'E_Invalid segment override expr','ession'#000+
  'E_Strings not allowed as constants'#000+
  'D_Starting AT&T styled assembler parsing...'#000+
  'D_Finished AT&T styled assembler parsing...'#000+
  'E_Switching sections is not allowed in an assembler block'#000+
  'E_Invalid global definition'#000+
  'E_Line separator expecte','d'#000+
  'W_globl not supported'#000+
  'W_align not supported'#000+
  'W_lcomm not supported'#000+
  'W_comm not supported'#000+
  'E_Invalid local common definition'#000+
  'E_Invalid global common definition'#000+
  'E_local symbol: $1 not found inside asm statement'#000+
  'E_assembler code not returned to',' text'#000+
  'F_internal error in BuildReference()'#000+
  'E_invalid opcode size'#000+
  'W_NEAR ignored'#000+
  'W_FAR ignored'#000+
  'D_Creating inline asm lookup tables'#000+
  'E_Using a defined name as a local label'#000+
  'F_internal error in HandleExtend()'#000+
  'E_Invalid character: <'#000+
  'E_Invalid ch','aracter: >'#000+
  'E_Unsupported opcode'#000+
  'E_Increment and Decrement mode not allowed together'#000+
  'E_Invalid Register list in movem/fmovem'#000+
  'E_Invalid Register list for opcode'#000+
  'E_68020+ mode required to assemble'#000+
  'D_Starting Motorola styled assembler parsing..','.'#000+
  'D_Finished Motorola styled assembler parsing...'#000+
  'W_XDEF not supported'#000+
  'W_Functions with void return value can'#039't return any value in asm c'+
  'ode'#000+
  'E_Invalid suffix for intel assembler'#000+
  'E_Extended not supported in this mode'#000+
  'E_Comp not supported in ','this mode'#000+
  'W_You need GNU as version >= 2.81 to compile this MMX code'#000+
  'F_Too many assembler files'#000+
  'F_Selected assembler output not supported'#000+
  'E_Unsupported symbol type for operand'#000+
  'W_Source operating system redefined'#000+
  'I_Assembling (pipe) $1'#000+
  'E_Can',#039't create assember file $1'#000+
  'W_Assembler $1 not found, switching to external assembling'#000+
  'T_Using assembler: $1'#000+
  'W_Error while assembling exitcode $1'#000+
  'W_Can'#039't call the assembler, error $1 switching to external assembl'+
  'ing'#000+
  'I_Assembling $1'#000+
  'W_Linker ','$1 not found, switching to external linking'#000+
  'T_Using linker: $1'#000+
  'W_Object $1 not found, Linking may fail !'#000+
  'W_Library $1 not found, Linking may fail !'#000+
  'W_Error while linking'#000+
  'W_Can'#039't call the linker, switching to external linking'#000+
  'I_Linking $1'#000+
  'W_','binder not found, switching to external binding'#000+
  'W_ar not found, switching to external ar'#000+
  'E_Dynamic Libraries not supported'#000+
  'I_Closing script $1'#000+
  'T_Unitsearch: $1'#000+
  'T_PPU Loading $1'#000+
  'U_PPU Name: $1'#000+
  'U_PPU Flags: $1'#000+
  'U_PPU Crc: $1'#000+
  'U_PPU Time: $1'#000+
  'U_P','PU File too short'#000+
  'U_PPU Invalid Header (no PPU at the begin)'#000+
  'U_PPU Invalid Version $1'#000+
  'U_PPU is compiled for an other processor'#000+
  'U_PPU is compiled for an other target'#000+
  'U_PPU Source: $1'#000+
  'U_Writing $1'#000+
  'F_Can'#039't Write PPU-File'#000+
  'F_reading PPU-File'#000+
  'F_u','nexpected end of PPU-File'#000+
  'F_Invalid PPU-File entry: $1'#000+
  'F_PPU Dbx count problem'#000+
  'E_Illegal unit name: $1'#000+
  'F_Too much units'#000+
  'F_Circular unit reference between $1 and $2'#000+
  'F_Can'#039't compile unit $1, no sources available'#000+
  'W_Compiling the system unit re','quires the -Us switch'#000+
  'F_There were $1 errors compiling module, stopping'#000+
  'U_Load from $1 ($2) unit $3'#000+
  'U_Recompiling $1, checksum changed for $2'#000+
  'U_Recompiling unit, static lib is older than ppufile'#000+
  'U_Recompiling unit, shared lib is older than ','ppufile'#000+
  'U_Recompiling unit, obj and asm are older than ppufile'#000+
  'U_Recompiling unit, obj is older than asm'#000+
  'U_Parsing interface of $1'#000+
  'U_Parsing implementation of $1'#000+
  'U_Second load for unit $1'#000+
  'U_PPU Check file $1 time $2'#000+
  '$1 [options] <inputfile>',' [options]'#000+
  'W_Only one source file supported'#000+
  'W_DEF file can be created only for OS/2'#000+
  'E_nested response files are not supported'#000+
  'F_No source file name in command line'#000+
  'E_Illegal parameter: $1'#000+
  'H_-? writes help pages'#000+
  'F_Too many config files neste','d'#000+
  'F_Unable to open file $1'#000+
  'N_Reading further options from $1'#000+
  'W_Target is already set to: $1'#000+
  'W_Shared libs not supported on DOS platform, reverting to static'#000+
  'F_too many IF(N)DEFs'#000+
  'F_too many ENDIFs'#000+
  'F_open conditional at the end of the file'#000+
  'W_','Debug information generation is not supported by this executable'#000+
  'H_Try recompiling with -dGDB'#000+
  'Free Pascal Compiler version $FPCVER [$FPCDATE] for $FPCTARGET'#000+
  'Copyright (c) 1993-98 by Florian Klaempfl'#000+
  'Free Pascal Compiler version $FPCVER'#000+
  #000+
  'Com','piler Date  : $FPCDATE'#000+
  'Compiler Target: $FPCTARGET'#000+
  #000+
  'This program comes under the GNU General Public Licence'#000+
  'For more information read COPYING.FPC'#000+
  #000+
  'Report bugs,suggestions etc to:'#000+
  '                fpc-devel@mail.tolna.hungary.net'#000+
  '**0*_+ switc','h option on, - off'#000+
  '**1a_the compiler doesn'#039't delete the generated assembler file'#000+
  '**2al_list sourcecode lines in assembler file'#000+
  '*t1b_use EMS'#000+
  '**1B_build all modules'#000+
  '**1C_code generation options'#000+
  '3*2CD_create dynamic library'#000+
  '**2Ch<n>_<n> bytes ','heap (between 1023 and 67107840)'#000+
  '**2Ci_IO-checking'#000+
  '**2Cn_omit linking stage'#000+
  '**2Co_check overflow of integer operations'#000+
  '**2Cr_range checking'#000+
  '**2Cs<n>_set stack size to <n>'#000+
  '**2Ct_stack checking'#000+
  '3*2CS_create static library'#000+
  '3*2Cx_use smartlinki','ng'#000+
  '**1d<x>_defines the symbol <x>'#000+
  '*O1D_generate a DEF file'#000+
  '*O2Dd<x>_set description to <x>'#000+
  '*O2Dw_PM application'#000+
  '**1e<x>_set path to executable'#000+
  '**1E_same as -Cn'#000+
  '**1F_set file names and paths'#000+
  '**2FD<x>_sets the directory where to search for co','mpiler utilities'#000+
  '**2Fe<x>_redirect error output to <x>'#000+
  '**2FE<x>_set exe/unit output path to <x>'#000+
  '*L2Fg<x>_same as -Fl'#000+
  '**2Fi<x>_adds <x> to include path'#000+
  '**2Fl<x>_adds <x> to library path'#000+
  '*L2FL<x>_uses <x> as dynamic linker'#000+
  '**2Fo<x>_adds <x> t','o object path'#000+
  '**2Fr<x>_load error message file <x>'#000+
  '**2Fu<x>_adds <x> to unit path'#000+
  '**2FU<x>_set unit output path to <x>, overrides -FE'#000+
  '*g1g_generate debugger information'#000+
  '*g2gg_use gsym'#000+
  '*g2gd_use dbx'#000+
  '**1i_information'#000+
  '**1I<x>_adds <x> to inclu','de path'#000+
  '**1k<x>_Pass <x> to the linker'#000+
  '**1l_write logo'#000+
  '**1n_don'#039't read the default config file'#000+
  '**1o<x>_change the name of the executable produced to <x>'#000+
  '**1pg_generate profile code for gprof'#000+
  '*L1P_use pipes instead of creating temporary asse','mbler files'#000+
  '**1S_syntax options'#000+
  '**2S2_switch some Delphi 2 extensions on'#000+
  '**2Sc_supports operators like C (*=,+=,/= and -=)'#000+
  '**2Sd_tries to be Delphi compatible'#000+
  '**2Se_compiler stops after the first error'#000+
  '**2Sg_allow LABEL and GOTO'#000+
  '**2Si_suppo','rt C++ stlyed INLINE'#000+
  '**2Sm_support macros like C (global)'#000+
  '**2So_tries to be TP/BP 7.0 compatible'#000+
  '**2Sp_tries to be gpc compatible'#000+
  '**2Ss_constructor name must be init (destructor must be done)'#000+
  '**2St_allow static keyword in objects'#000+
  '**2Sv_allo','w variable directives (cvar,external,public,export)'#000+
  '**1s_don'#039't call assembler and linker (only with -a)'#000+
  '**1u<x>_undefines the symbol <x>'#000+
  '**1U_unit options'#000+
  '**2Un_don'#039't check the unit name'#000+
  '**2Up<x>_same as -Fu<x>'#000+
  '**2Us_compile a system unit'#000+
  '*','*1v<x>_Be verbose. <x> is a combination of the following letters :'#000+
  '**2*_e : Show errors (default)       d : Show debug info'#000+
  '**2*_w : Show warnings               u : Show unit info'#000+
  '**2*_n : Show notes                  t : Show tried/used fil','es'#000+
  '**2*_h : Show hints                  m : Show defined macros'#000+
  '**2*_i : Show general info           p : Show compiled procedures'#000+
  '**2*_l : Show linenumbers            c : Show conditionals'#000+
  '**2*_a : Show everything             0 : Show nothi','ng (except errors'+
  ')'#000+
  '**2*_b : Show all procedure          r : Rhide/GCC compatibility mode'#000+
  '**2*_    declarations if an error'#000+
  '**2*_    occurs'#000+
  '**1X_executable options'#000+
  '*L2Xc_link with the c library'#000+
  '**2XD_link with dynamic libraries (defines FPC_','LINK_DYNAMIC)'#000+
  '**2Xs_strip all symbols from executable'#000+
  '**2XS_link with static libraries (defines FPC_LINK_STATIC)'#000+
  '**0*_Processor specific options:'#000+
  '3*1A<x>_output format'#000+
  '3*2Ao_coff file using GNU AS'#000+
  '3*2Anasmcoff_coff file using Nasm'#000+
  '3*2Anasme','lf_elf32 (linux) file using Nasm'#000+
  '3*2Anasmobj_obj file using Nasm'#000+
  '3*2Amasm_obj using Masm (Mircosoft)'#000+
  '3*2Atasm_obj using Tasm (Borland)'#000+
  '3*1R<x>_assembler reading style'#000+
  '3*2Ratt_read AT&T style assembler'#000+
  '3*2Rintel_read Intel style assembler'#000+
  '3*','2Rdirect_copy assembler text directly to assembler file'#000+
  '3*1O<x>_optimizations'#000+
  '3*2Og_generate smaller code'#000+
  '3*2OG_generate faster code (default)'#000+
  '3*2Or_keep certain variables in registers (still BUGGY!!!)'#000+
  '3*2Ou_enable uncertain optimizations (','see docs)'#000+
  '3*2O1_level 1 optimizations (quick optimizations)'#000+
  '3*2O2_level 2 optimizations (-O1 + slower optimizations)'#000+
  '3*2O3_level 3 optimizations (same as -O2u)'#000+
  '3*2Op_target processor'#000+
  '3*3Op1_set target processor to 386/486'#000+
  '3*3Op2_set target ','processor to Pentium/PentiumMMX (tm)'#000+
  '3*3Op3_set target processor to PPro/PII/c6x86/K6 (tm)'#000+
  '3*1T<x>_Target operating system'#000+
  '3*2TGO32V1_version 1 of DJ Delorie DOS extender'#000+
  '3*2TGO32V2_version 2 of DJ Delorie DOS extender'#000+
  '3*2TLINUX_Linux'#000+
  '3*2TO','S2_OS/2 2.x'#000+
  '3*2TWin32_Windows 32 Bit'#000+
  '6*1A<x>_output format'#000+
  '6*2Ao_Unix o-file using GNU AS'#000+
  '6*2Agas_GNU Motorola assembler'#000+
  '6*2Amit_MIT Syntax (old GAS)'#000+
  '6*2Amot_Standard Motorola assembler'#000+
  '6*1O_optimizations'#000+
  '6*2Oa_turn on the optimizer'#000+
  '6*2Og_g','enerate smaller code'#000+
  '6*2OG_generate faster code (default)'#000+
  '6*2Ox_optimize maximum (still BUGGY!!!)'#000+
  '6*2O2_set target processor to a MC68020+'#000+
  '6*1R<x>_assembler reading style'#000+
  '6*2RMOT_read motorola style assembler'#000+
  '6*1T<x>_Target operating system',#000+
  '6*2TAMIGA_Commodore Amiga'#000+
  '6*2TATARI_Atari ST/STe/TT'#000+
  '6*2TMACOS_Macintosh m68k'#000+
  '6*2TLINUX_Linux-68k'#000+
  '**1*_'#000+
  '**1?_shows this help'#000+
  '**1h_shows this help without waiting'#000
);
