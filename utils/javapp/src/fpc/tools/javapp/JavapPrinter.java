/*
addIdentifier * Copyright (c) 2002, 2005, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
/*
 * Portions Copyright (c) 2011 Jonas Maebe
 */


package fpc.tools.javapp;

import java.util.*;
import java.io.*;

import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;
import org.jgrapht.traverse.TopologicalOrderIterator;

import static fpc.tools.javapp.RuntimeConstants.*;

/**
 * Program to print information about class files
 *
 * @author  Sucheta Dambalkar
 */
public class JavapPrinter {
    JavapEnvironment env;
    PascalClassData cls;
//    byte[] code;
    String lP= "";
    PrintWriter out;
    String prefix;
    boolean doCollectDependencies;
    boolean printOnlySkel;
    
    private ArrayList<JavapPrinter> innerClassPrinters;

    public JavapPrinter(InputStream cname, PrintWriter out, JavapEnvironment env, String prefix, PascalClassData outerClass, boolean doCollectDependencies, boolean printOnlySkel){
        this.out = out;
        this.cls = new PascalClassData(cname,outerClass,env,doCollectDependencies);
        this.env = env;
        this.prefix = prefix;
        this.doCollectDependencies = doCollectDependencies;
        this.printOnlySkel = printOnlySkel;
        innerClassPrinters = new ArrayList<JavapPrinter>();
        collectInnerClasses();
    }

    /**
     *  Entry point to print class file information.
     */
    public void print(){
    	printclassHeader();
    	if (!printOnlySkel) {
    		printfields();
    		printMethods();
    	}
        printend();
    }

    /**
     * Print a description of the class (not members).
     */
    public void printclassHeader(){
    	String vis = cls.getVisibilitySectionName();
		String shortname;
    	if (!cls.isInnerClass())
    		shortname = cls.getShortPascalClassName();
    	else {
    		shortname = cls.getShortClassName();
    	}
    	if (!cls.isInnerClass()) {
    		if (vis != null) {
    			// inner class in separate visibility section
    			// (except in case of interface, those cannot have visibility
    			//  sections)
    			if (!PascalClassData.currentUnit.parentIsKnownInterface(cls.getClassName())) {
    				out.println(prefix.substring(4)+vis);
    			}
    			out.println(prefix.substring(2)+"type");
    		}
    	}
    	// the actual class/interface
    	out.print(prefix);
    	if(cls.isInterface())   {
    		// The only useful access modifier of an interface is
    		// public; interfaces are always marked as abstract and
    		// cannot be final.
    		out.print(shortname + " = interface ");
    	}
    	else if(cls.isClass()) {
    		out.print(shortname+" = class ");
    		String []accflags = cls.getModifiers();
    		printAccess(accflags);
    	}

    	out.print("external ");
    	String pkgname = cls.getClassPackageName();
    	if (pkgname != null)
    		out.print("'"+pkgname+"' ");
    	out.print("name '"+cls.getExternalShortClassName()+"' ");

    	if (!printOnlySkel) {
    		// FPC doesn't like it when you say that an interface's superclass is
    		// java.lang.Object, since that's a class (although at the JVM level
    		// it's true)
    		boolean printedOpeningBracket = false;
    		String superClass = cls.getSuperClassName();
    		if((superClass != null) &&
    				(cls.isClass() ||
    						!PascalClassData.getShortPascalClassName(superClass).equals("JLObject"))){
    			printedOpeningBracket = true;
    			String fullPascalSuperClass = PascalClassData.getFullPascalClassName(superClass);
    			String reducedPascalSuperClass = fullPascalSuperClass;
    			if (!PascalClassData.currentUnit.isExternalInnerClass(superClass) &&
    					((PascalClassData)cls).outerClass != null) {
    				reducedPascalSuperClass = fullPascalSuperClass.replace(((PascalClassData)cls).outerClass.getShortPascalClassName()+".","");
    			}
    			if (reducedPascalSuperClass.equals(fullPascalSuperClass)) {
    				out.print("(" + PascalClassData.getShortPascalClassName(superClass));
    			} else {
    				out.print("(" + reducedPascalSuperClass);
    			}

    		}

    		String []interfacelist =  cls.getPascalSuperInterfaces();
    		if(interfacelist.length > 0){
    			// assume all classes that implement interfaces inherit from
    			// a class (correct, since java.lang.Object does not implement
    			// any interfaces
    			if (!printedOpeningBracket) {
    				out.print("(");
    				printedOpeningBracket=true;
    				out.print(interfacelist[0]);
    			}
    			else
    				out.print(", "+interfacelist[0]);
    			for(int j = 1; j < interfacelist.length; j++){
    				out.print(", "+interfacelist[j]);
    			}
    		}
    		if (printedOpeningBracket)
    			out.print(")");
        }
        /* inner classes */
        printClassAttributes();
    }

    /**
     * Print verbose output.
     */
    public void printverbosecls(){
        out.println(prefix+"  minor version: "+cls.getMinor_version());
        out.println(prefix+"  major version: "+cls.getMajor_version());
        out.println(prefix+"  Constant pool:");
        printcp();
        env.showallAttr = true;
    }

    /**
     * Print class attribute information.
     */
    public void printClassAttributes(){
        out.println();
        AttrData[] clsattrs = cls.getAttributes();
        for(int i = 0; i < clsattrs.length; i++){
            String clsattrname = clsattrs[i].getAttrName();
            if(clsattrname.equals("InnerClasses")){
                printInnerClasses();
            }
        }
    }

    /**
     * Print the fields
     */
    public void printfields(){
        FieldData[] fields = cls.getFields();
        String prevVis = "";
        String prevMod = "";
        prefix=prefix+"    ";
        for(int f = 0; f < fields.length; f++){
        	PascalFieldData field = (PascalFieldData)fields[f];
        	if (field.isSynthetic())
        		continue;
            String[] accflags = field.getAccess();
            if(checkAccess(accflags)){
            	String newVis = field.getVisibilitySectionName();
            	String newMod = field.getModifiers();
            	/* new visibility section? (Java interfaces can have
            	 * public const sections)
            	 * Also rewrite in case of a changed modifier (var,
            	 * const, ...), because in Delphi mode that resets
            	 * the visibility too
            	 **/
            	if (cls.isClass() &&
            			(!newVis.equals(prevVis) ||
            					!newMod.equals(prevMod))) {
            		out.println(prefix.substring(4)+newVis);
            		prevVis=newVis;
            		/* actually "var", but that will introduce gaps */
            		prevMod="";
            	}
            	/* new modifiers section? (sealed, var, class var, const) */
            	if (!newMod.equals(prevMod)) {
            		out.println(prefix.substring(2)+newMod);
            		prevMod=newMod;
            	}
            	// the field name
            	String fieldName = field.getName();
            	out.print(prefix);
            	// allowed, but discouraged and mainly for auto-generated fields
            	// not yet possible in Pascal, hide for now
            	if (fieldName.contains("$"))
            		out.print("// ");
            	if (!field.isFormalConst()) {
            		out.print(fieldName+": "+field.getType());
            	} else {
            		out.print(fieldName);
                	printConstantValue(field);
            	}

                // print field attribute information.
                printFieldAttributes(field);
            	if (!field.isFormalConst()) {
            		out.print("; external name '"+fieldName.substring(env.prefix_field.length())+"'");
            	}
                out.println(";");
            }
        }
        prefix=prefix.substring(4);
    }


    /* print field attribute information. */
    public void printFieldAttributes(FieldData field){
    	if (field.isDeprecated())
    		out.print(" deprecated");
    }

    /**
     * Print the methods
     */
    public void printMethods(){
        MethodData[] methods = cls.getMethods();
        String prevVis = "";
        prefix=prefix+"  ";
        for(int m = 0; m < methods.length; m++){
        	PascalMethodData method = (PascalMethodData)methods[m];
        	if (method.isSynthetic())
        		continue;
            String[] accflags = method.getAccess();
            if(checkAccess(accflags)){
            	String newVis = method.getVisibilitySectionName();
            	/* new visibility section? (interfaces don't have visibility
            	 * sections, and Java interface methods are also all public)
            	 **/
            	if (cls.isClass() &&
            			!newVis.equals(prevVis)) {
            		out.println(prefix.substring(2)+newVis);
            		prevVis=newVis;
            	}
                printMethodSignature(method);
            }
        }
        prefix=prefix.substring(2);
    }

    protected void PrintSignatureVariants(PascalMethodData method, StringBuilder sigStart, StringBuilder sigEnd, boolean useConstOpenArray, boolean forceSingleVarVersion){
        java.util.Set<PascalTypeSignature.ParaFlags> paraFlags;
        
        paraFlags = java.util.EnumSet.noneOf(PascalTypeSignature.ParaFlags.class);
        String dynArrParas = method.getParameters(paraFlags);

        paraFlags.add(PascalTypeSignature.ParaFlags.OpenArrays);
        if (useConstOpenArray)
            paraFlags.add(PascalTypeSignature.ParaFlags.OpenConstArrays);
        String openArrParas = method.getParameters(paraFlags);

        String regularVarParas = "";
        if (env.addVarOverloads &&
                (!useConstOpenArray ||
                 forceSingleVarVersion)) {
            paraFlags = java.util.EnumSet.noneOf(PascalTypeSignature.ParaFlags.class);
            paraFlags.add(PascalTypeSignature.ParaFlags.SingleVar);
            regularVarParas = method.getParameters(paraFlags);
        }
        
        out.print(sigStart+dynArrParas+sigEnd);
        printExceptions(method);
        out.println();
        if (!dynArrParas.equals(openArrParas)) {
            out.print(sigStart+openArrParas+sigEnd);
            printExceptions(method);
            out.println();
        }
        if ((regularVarParas != "") &&
                !dynArrParas.equals(regularVarParas)) {
            out.print(sigStart+regularVarParas+sigEnd);
            printExceptions(method);
            out.println();
        }
    }
    
    /**
     * Print method signature.
     */
    public void printMethodSignature(PascalMethodData method){
    	StringBuilder sigStart = new StringBuilder();
    	StringBuilder sigEnd;
    	sigStart.append(prefix);
    	String pascalName = method.getName();
    	boolean varargs = (method.access & ACC_VARARGS) != 0;
        if(pascalName.equals("<init>")){
        	sigStart.append("constructor create");
        	sigEnd = new StringBuilder();
            // to fix compilation in Delphi mode
        	sigEnd.append("; overload;");
        	PrintSignatureVariants(method,sigStart,sigEnd,true,true);
        }else if(pascalName.equals("<clinit>")){
        	sigStart.append("class constructor classcreate");
        }else{
        	String rettype = method.getReturnType();
        	java.util.Set<PascalTypeSignature.ParaFlags> paraFlags;
        	if (method.isStatic())
        		sigStart.append("class ");
        	if (rettype.equals(""))
        		sigStart.append("procedure ");
        	else
        		sigStart.append("function ");
        	sigStart.append(pascalName);
        	sigEnd = new StringBuilder();
            if (!rettype.equals(""))
            	sigEnd.append(": "+rettype);
        	if (method.isStatic())
        		sigEnd.append("; static");
        	String externalName = method.getExternalName();
        	if (externalName != null)
        		sigEnd.append("; external name '"+externalName+"'");
            // to fix compilation in Delphi mode
        	sigEnd.append("; overload;");
            // all interface methods are marked as "abstract", and cannot be final
            if (!cls.isInterface())
            	sigEnd.append(method.getModifiers());
            
            PrintSignatureVariants(method,sigStart,sigEnd,varargs,false);
        }
    }

    /**
     * print method attribute information.
     */
    public void printMethodAttributes(MethodData method){
        Vector methodattrs = method.getAttributes();
//        Vector codeattrs =  method.getCodeAttributes();
        for(int k = 0; k < methodattrs.size(); k++){
            String methodattrname = ((AttrData)methodattrs.elementAt(k)).getAttrName();
            if(methodattrname.equals("Code")){
/*            	
                printcodeSequence(method);
                printExceptionTable(method);
                for(int c = 0; c < codeattrs.size(); c++){
                    String codeattrname = ((AttrData)codeattrs.elementAt(c)).getAttrName();
                    if(codeattrname.equals("LineNumberTable")){
                        printLineNumTable(method);
                    }else if(codeattrname.equals("LocalVariableTable")){
                        printLocVarTable(method);
                    }else if(codeattrname.equals("StackMapTable")) {
                        // Java SE JSR 202 stack map tables
                        printStackMapTable(method);
                    }else if(codeattrname.equals("StackMap")) {
                        // Java ME CLDC stack maps
                        printStackMap(method);
                    } else {
                        printAttrData((AttrData)codeattrs.elementAt(c));
                    }
                }
*/                    
            }else if(methodattrname.equals("Exceptions")){
                out.println(prefix+"  Exceptions: ");
                printExceptions(method);
            }else if (methodattrname.equals("Deprecated")){
                out.println(prefix+"  Deprecated: "+ method.isDeprecated());
            }else if (methodattrname.equals("Synthetic")){
                out.println(prefix+"  Synthetic: "+ method.isSynthetic());
            }else {
                printAttrData((AttrData)methodattrs.elementAt(k));
            }
        }
        out.println();
    }

    /**
     * Print exceptions.
     */
    public void printExceptions(MethodData method){
        int []exc_index_table = method.get_exc_index_table();
        if (exc_index_table != null) {
            out.print("  // throws ");
            int k;
            int l = exc_index_table.length;

            for (k=0; k<l; k++) {
                out.print(javaclassname(cls.getClassName(exc_index_table[k])));
                if (k<l-1) out.print(", ");
            }
        }
    }

    /**
     * Print code sequence.
     */
    public void  printcodeSequence(MethodData method){
/*
    	code = method.getCode();
        if(code != null){
            out.println(prefix+"  Code:");
            if(env.showVerbose){
                printVerboseHeader(method);
            }

            for (int pc=0; pc < code.length; ) {
                out.print(prefix+"   "+pc+":\t");
                pc=pc+printInstr(pc);
                out.println();
            }
        }
*/  
    }

    /**
     * Print instructions.
     */
//    public int printInstr(int pc){
//        int opcode = getUbyte(pc);
//        int opcode2;
//        String mnem;
//        switch (opcode) {
//        case opc_nonpriv:
//        case opc_priv:
//            opcode2 = getUbyte(pc+1);
//            mnem=Tables.opcName((opcode<<8)+opcode2);
//            if (mnem==null)
//                // assume all (even nonexistent) priv and nonpriv instructions
//                // are 2 bytes long
//                mnem=Tables.opcName(opcode)+" "+opcode2;
//            out.print(mnem);
//            return 2;
//        case opc_wide: {
//            opcode2 = getUbyte(pc+1);
//            mnem=Tables.opcName((opcode<<8)+opcode2);
//            if (mnem==null) {
//                // nonexistent opcode - but we have to print something
//                out.print("bytecode "+opcode);
//                return 1;
//            }
//            out.print(mnem+" "+getUShort(pc+2));
//            if (opcode2==opc_iinc) {
//                out.print(", "+getShort(pc+4));
//                return 6;
//            }
//            return 4;
//        }
//        }
//        mnem=Tables.opcName(opcode);
//        if (mnem==null) {
//            // nonexistent opcode - but we have to print something
//            out.print("bytecode "+opcode);
//            return 1;
//        }
//        if (opcode>opc_jsr_w) {
//            // pseudo opcodes should be printed as bytecodes
//            out.print("bytecode "+opcode);
//            return 1;
//        }
//        out.print(Tables.opcName(opcode));
//        switch (opcode) {
//        case opc_aload: case opc_astore:
//        case opc_fload: case opc_fstore:
//        case opc_iload: case opc_istore:
//        case opc_lload: case opc_lstore:
//        case opc_dload: case opc_dstore:
//        case opc_ret:
//            out.print("\t"+getUbyte(pc+1));
//            return  2;
//        case opc_iinc:
//            out.print("\t"+getUbyte(pc+1)+", "+getbyte(pc+2));
//            return  3;
//        case opc_tableswitch:{
//            int tb=align(pc+1);
//            int default_skip = getInt(tb); /* default skip pamount */
//            int low = getInt(tb+4);
//            int high = getInt(tb+8);
//            int count = high - low;
//            out.print("{ //"+low+" to "+high);
//            for (int i = 0; i <= count; i++)
//                out.print( "\n\t\t" + (i+low) + ": "+lP+(pc+getInt(tb+12+4*i))+";");
//            out.print("\n\t\tdefault: "+lP+(default_skip + pc) + " }");
//            return tb-pc+16+count*4;
//        }
//
//        case opc_lookupswitch:{
//            int tb=align(pc+1);
//            int default_skip = getInt(tb);
//            int npairs = getInt(tb+4);
//            out.print("{ //"+npairs);
//            for (int i = 1; i <= npairs; i++)
//                out.print("\n\t\t"+getInt(tb+i*8)
//                                 +": "+lP+(pc+getInt(tb+4+i*8))+";"
//                                 );
//            out.print("\n\t\tdefault: "+lP+(default_skip + pc) + " }");
//            return tb-pc+(npairs+1)*8;
//        }
//        case opc_newarray:
//            int type=getUbyte(pc+1);
//            switch (type) {
//            case T_BOOLEAN:out.print(" boolean");break;
//            case T_BYTE:   out.print(" byte");   break;
//            case T_CHAR:   out.print(" char");   break;
//            case T_SHORT:  out.print(" short");  break;
//            case T_INT:    out.print(" int");    break;
//            case T_LONG:   out.print(" long");   break;
//            case T_FLOAT:  out.print(" float");  break;
//            case T_DOUBLE: out.print(" double"); break;
//            case T_CLASS:  out.print(" class"); break;
//            default:       out.print(" BOGUS TYPE:"+type);
//            }
//            return 2;
//
//        case opc_anewarray: {
//            int index =  getUShort(pc+1);
//            out.print("\t#"+index+"; //");
//            PrintConstant(index);
//            return 3;
//        }
//
//        case opc_sipush:
//            out.print("\t"+getShort(pc+1));
//            return 3;
//
//        case opc_bipush:
//            out.print("\t"+getbyte(pc+1));
//            return 2;
//
//        case opc_ldc: {
//            int index = getUbyte(pc+1);
//            out.print("\t#"+index+"; //");
//            PrintConstant(index);
//            return 2;
//        }
//
//        case opc_ldc_w: case opc_ldc2_w:
//        case opc_instanceof: case opc_checkcast:
//        case opc_new:
//        case opc_putstatic: case opc_getstatic:
//        case opc_putfield: case opc_getfield:
//        case opc_invokevirtual:
//        case opc_invokespecial:
//        case opc_invokestatic: {
//            int index = getUShort(pc+1);
//            out.print("\t#"+index+"; //");
//            PrintConstant(index);
//            return 3;
//        }
//
//        case opc_invokeinterface: {
//            int index = getUShort(pc+1), nargs=getUbyte(pc+3);
//            out.print("\t#"+index+",  "+nargs+"; //");
//            PrintConstant(index);
//            return 5;
//        }
//
//        case opc_multianewarray: {
//            int index = getUShort(pc+1), dimensions=getUbyte(pc+3);
//            out.print("\t#"+index+",  "+dimensions+"; //");
//            PrintConstant(index);
//            return 4;
//        }
//        case opc_jsr: case opc_goto:
//        case opc_ifeq: case opc_ifge: case opc_ifgt:
//        case opc_ifle: case opc_iflt: case opc_ifne:
//        case opc_if_icmpeq: case opc_if_icmpne: case opc_if_icmpge:
//        case opc_if_icmpgt: case opc_if_icmple: case opc_if_icmplt:
//        case opc_if_acmpeq: case opc_if_acmpne:
//        case opc_ifnull: case opc_ifnonnull:
//            out.print("\t"+lP+(pc + getShort(pc+1)) );
//            return 3;
//
//        case opc_jsr_w:
//        case opc_goto_w:
//            out.print("\t"+lP+(pc + getInt(pc+1)));
//            return 5;
//
//        default:
//            return 1;
//        }
//    }
    /**
     * Print code attribute details.
     */
    public void printVerboseHeader(MethodData method) {
/*
    	int argCount = method.getArgumentlength();
        if (!method.isStatic())
            ++argCount;  // for 'this'

        out.println("   Stack=" + method.getMaxStack()
                           + ", Locals=" + method.getMaxLocals()
                           + ", Args_size=" + argCount);
*/
    }


    /**
     * Print the exception table for this method code
     */
    void printExceptionTable(MethodData method){//throws IOException
/*
    	Vector exception_table = method.getexception_table();
        if (exception_table.size() > 0) {
            out.println(prefix+"  Exception table:");
            out.println(prefix+"   from   to  target type");
            for (int idx = 0; idx < exception_table.size(); ++idx) {
                TrapData handler = (TrapData)exception_table.elementAt(idx);
                out.print(prefix);
                printFixedWidthInt(handler.start_pc, 6);
                printFixedWidthInt(handler.end_pc, 6);
                printFixedWidthInt(handler.handler_pc, 6);
                out.print("   ");
                int catch_cpx = handler.catch_cpx;
                if (catch_cpx == 0) {
                    out.println("any");
                }else {
                    out.print("Class ");
                    out.println(cls.getClassName(catch_cpx));
                    out.println("");
                }
            }
        }
 */
    }

    /**
     * Print LineNumberTable attribute information.
     */
    public void printLineNumTable(MethodData method) {
/*    	
        int numlines = method.getnumlines();
        Vector lin_num_tb = method.getlin_num_tb();
        if( lin_num_tb.size() > 0){
            out.println(prefix+"  LineNumberTable: ");
            for (int i=0; i<numlines; i++) {
                LineNumData linnumtb_entry=(LineNumData)lin_num_tb.elementAt(i);
                out.println(prefix+"   line " + linnumtb_entry.line_number + ": "
                               + linnumtb_entry.start_pc);
            }
        }
        out.println();
 */
    }

    /**
     * Print LocalVariableTable attribute information.
     */
    public void printLocVarTable(MethodData method){
/*    	
        int siz = method.getloc_var_tbsize();
        if(siz > 0){
            out.println(prefix+"  LocalVariableTable: ");
            out.print(prefix+"   ");
            out.println("Start  Length  Slot  Name   Signature");
        }
        Vector loc_var_tb = method.getloc_var_tb();

        for (int i=0; i<siz; i++) {
            LocVarData entry=(LocVarData)loc_var_tb.elementAt(i);

            out.println(prefix+"   "+entry.start_pc+"      "+entry.length+"      "+
                               entry.slot+"    "+cls.StringValue(entry.name_cpx)  +
                               "       "+cls.StringValue(entry.sig_cpx));
        }
        out.println();
*/
    }

    /**
     * Print StackMap attribute information.
     */
    public void printStackMap(MethodData method) {
/*    	
        StackMapData[] stack_map_tb = method.getStackMap();
        int number_of_entries = stack_map_tb.length;
        if (number_of_entries > 0) {
            out.println(prefix+"  StackMap: number_of_entries = " + number_of_entries);

            for (StackMapData frame : stack_map_tb) {
                frame.print(this);
            }
        }
       out.println();
*/       
    }

    /**
     * Print StackMapTable attribute information.
     */
    public void printStackMapTable(MethodData method) {
/*    	
        StackMapTableData[] stack_map_tb = method.getStackMapTable();
        int number_of_entries = stack_map_tb.length;
        if (number_of_entries > 0) {
            out.println(prefix+"  StackMapTable: number_of_entries = " + number_of_entries);

            for (StackMapTableData frame : stack_map_tb) {
                frame.print(this);
            }
        }
        out.println();
*/        
    }

    void printMap(String name, int[] map) {
        out.print(name);
        for (int i=0; i<map.length; i++) {
            int fulltype = map[i];
            int type = fulltype & 0xFF;
            int argument = fulltype >> 8;
            switch (type) {
                case ITEM_Object:
                    out.print(" ");
                    PrintConstant(argument);
                    break;
                case ITEM_NewObject:
                    out.print(" " + Tables.mapTypeName(type));
                    out.print(" " + argument);
                    break;
                default:
                    out.print(" " + Tables.mapTypeName(type));
            }
            out.print( (i==(map.length-1)? ' ' : ','));
        }
        out.println("]");
    }

    /**
     * Print ConstantValue attribute information.
     */
    public void printConstantValue(FieldData field){
        int cpx = (field.getConstantValueIndex());
        if (cpx==0)
        	return;
        byte tag=0;
        try {
            tag=cls.getTag(cpx);

        } catch (IndexOutOfBoundsException e) {
            return;
        }
        switch (tag) {
        case CONSTANT_METHOD:
        case CONSTANT_INTERFACEMETHOD:
        case CONSTANT_FIELD:
        case CONSTANT_NAMEANDTYPE:
        	// don't print their value in the header, since they're not
        	// constant expressions anyway. We treat them as external data.
        	return;
        }
        out.print(" = "+ cls.StringValue(cpx));
    }

    
    /**
     * Collect InnerClasses
     */
    public void collectInnerClasses(){//throws ioexception
        InnerClassData[] innerClasses = cls.getInnerClasses();
        if(innerClasses != null){
            if(innerClasses.length > 0){
            	JavapPrinter innerPrinter;
        		String curClassName = cls.getClassName();
            	for(int i = 0 ; i < innerClasses.length; i++){
            		String[] accflags = innerClasses[i].getAccess();
            		PascalInnerClassData inner = (PascalInnerClassData)innerClasses[i];
            		String innerClassName = cls.StringValue(inner.inner_class_info_index);
            		// * inner class names that do not begin with this class' name are
            		//   unrelated to this class (they're nested somewhere else)
            		// * inner class names that start with 0-9 are anonymous
            		if (innerClassName.startsWith(curClassName+"$") &&
            				!((innerClassName.charAt(curClassName.length()+1) >= '0') &&
            						(innerClassName.charAt(curClassName.length()+1) <= '9'))) {
            			boolean accessOk = checkAccess(accflags);
            			boolean isStaticInner = inner.isStatic();
            			innerPrinter = new JavapPrinter(env.getFileInputStream(javaclassname(innerClassName)), out, env, prefix+"    ", cls,
            					doCollectDependencies && accessOk && isStaticInner, printOnlySkel || !accessOk || !isStaticInner);
            			innerClassPrinters.add(innerPrinter);
            		}
            	}
            }
        }
    }
    
    /**
     * Print InnerClass attribute information.
     */
	private final int VIS_PRIVATE = 0;
	private final int VIS_PACKAGE = 1;
	private final int VIS_PROTECTED = 2;
	private final int VIS_PUBLIC = 3;

	private boolean checkInnerVisibility(int access, int visOk) {
		switch (visOk) {
		case VIS_PRIVATE:
			return ((access & ACC_PRIVATE) != 0);
		case VIS_PACKAGE:
			return ((access & (ACC_PUBLIC|ACC_PROTECTED|ACC_PRIVATE)) == 0);
		case VIS_PROTECTED:
			return ((access & ACC_PROTECTED) != 0);
		case VIS_PUBLIC:
			return ((access & ACC_PUBLIC) != 0);
		default:
			return false;
		}
	}
	
	private String visibilitySectionName(int vis) {
		switch (vis) {
		case VIS_PRIVATE:
			return "strict private";
		case VIS_PACKAGE:
			// should be "private", but then won't work for classes of the
			// same package declared in different units, which happens
			// at least for the classes in the system unit...
			return "public";
		case VIS_PROTECTED:
			return "protected";
		case VIS_PUBLIC:
			return "public";
		default:
			return "";
		}
	}
    
    public void printInnerClasses(){//throws ioexception
    	
    	if (innerClassPrinters.size() > 1)
    		orderInnerClasses();

		if (innerClassPrinters.size() > 0) {
    		for (int protpub = VIS_PACKAGE; protpub <= VIS_PUBLIC; protpub++) {
    			// no vibility sections in interfaces
    			boolean first = true;
    			for (int i = 0; i < innerClassPrinters.size(); i++) {
    				JavapPrinter innerPrinter = innerClassPrinters.get(i);
    				if (checkInnerVisibility(innerPrinter.cls.access,protpub)) {
    					String shortInnerName = PascalClassData.getShortClassName(env,innerPrinter.cls.getClassName()); 
        				String shortInnerSafeName = ClassIdentifierInfo.AddIdentifierNameForClass(cls.getClassName(),shortInnerName);
    					if (first) {
    						if (!cls.isInterface()) {
    							out.print(prefix);
    							out.println(visibilitySectionName(protpub));
    						}
    			    		out.println(innerPrinter.prefix.substring(2)+"type");
    						first = false;
    					}
    					out.print(innerPrinter.prefix+shortInnerSafeName+" = ");
    					if (innerPrinter.cls.isClass())
    						out.println("class;");
    					else
    						out.println("interface;");
    					PascalUnit.printArrayTypes(out, innerPrinter.prefix, shortInnerName, shortInnerSafeName);
    				}
    			}
    	    	for (int i = 0; i < innerClassPrinters.size(); i++) {
    	    		JavapPrinter innerPrinter = innerClassPrinters.get(i);
    				if (checkInnerVisibility(innerPrinter.cls.access,protpub)) {
    					innerPrinter.print();
    				}
    	    	}
    		}
    	}
    }

	/**
	 * Orders the inner classes according to their interdependencies
	 */
	private void orderInnerClasses() {
		boolean haveDependencies = false;
		for (int i = 0; i < innerClassPrinters.size(); i++) {
			if (!innerClassPrinters.get(i).cls.getDependencies().isEmpty()) {
				haveDependencies = true;
				break;
			}
		}
		if (haveDependencies) {
			SimpleDirectedGraph<String,DefaultEdge> classDependencies = new SimpleDirectedGraph<String, DefaultEdge>(DefaultEdge.class);
			for (int i = 0; i < innerClassPrinters.size(); i++) {
				JavapPrinter innerPrinter = innerClassPrinters.get(i);
				String currentClass = innerPrinter.cls.getClassName();
				if (!classDependencies.containsVertex(currentClass))
					classDependencies.addVertex(currentClass);

				HashSet<String> dependencies = innerPrinter.cls.getDependencies();
				Iterator<String> depStepper = dependencies.iterator();
				while (depStepper.hasNext()) {
					String dep = depStepper.next();
					if (!classDependencies.containsVertex(dep))
						classDependencies.addVertex(dep);
					classDependencies.addEdge(dep, currentClass);
				}
			}
			TopologicalOrderIterator<String,DefaultEdge> printerStepper = new TopologicalOrderIterator<String,DefaultEdge>(classDependencies);

			ArrayList<JavapPrinter> orderedInnerClassPrinters = new ArrayList<JavapPrinter>(innerClassPrinters.size());
			while (printerStepper.hasNext()) {
				String currentName = printerStepper.next();
				for (int i = 0; i < innerClassPrinters.size(); i++) {
					if (innerClassPrinters.get(i).cls.getClassName().equals(currentName)) {
						orderedInnerClassPrinters.add(innerClassPrinters.get(i));
						break;
					}
				}
			}
			innerClassPrinters = orderedInnerClassPrinters;
		}
	}

    /**
     * Print constant pool information.
     */
    public void printcp(){
        int cpx = 1 ;

        while (cpx < cls.getCpoolCount()) {
            out.print("const #"+cpx+" = ");
            cpx+=PrintlnConstantEntry(cpx);
        }
        out.println();
    }

    /**
     * Print constant pool entry information.
     */
    public int PrintlnConstantEntry(int cpx) {
        int size=1;
        byte tag=0;
        try {
            tag=cls.getTag(cpx);
        } catch (IndexOutOfBoundsException e) {
            out.println("  <Incorrect CP index>");
            return 1;
        }
        out.print(cls.StringTag(cpx)+"\t");
        Object x=cls.getCpoolEntryobj(cpx);
        if (x==null) {
            switch (tag) {
            case CONSTANT_LONG:
            case CONSTANT_DOUBLE:
                size=2;
            }
            out.println("null;");
            return size;
        }
        String str=cls.StringValue(cpx);

        switch (tag) {
        case CONSTANT_CLASS:
        case CONSTANT_STRING:
            out.println("#"+(((CPX)x).cpx)+";\t//  "+str);
            break;
        case CONSTANT_FIELD:
        case CONSTANT_METHOD:
        case CONSTANT_INTERFACEMETHOD:
            out.println("#"+((CPX2)x).cpx1+".#"+((CPX2)x).cpx2+";\t//  "+str);
            break;
        case CONSTANT_NAMEANDTYPE:
            out.println("#"+((CPX2)x).cpx1+":#"+((CPX2)x).cpx2+";//  "+str);
            break;
        case CONSTANT_LONG:
        case CONSTANT_DOUBLE:
            size=2;
        default:
            out.println(str+";");
        }
        return size;
    }

    public boolean checkAccess(String accflags[]){
    	return TypeSignature.checkAccess(accflags, this.env);
    }

    /**
     * Prints access of class, field or method.
     */
    public void printAccess(String []accflags){
    	
    	for(int j = 0; j < accflags.length; j++){
            out.print(accflags[j]+" ");
        }
    }

    /**
     * Print an integer so that it takes 'length' characters in
     * the output.  Temporary until formatting code is stable.
     */
    public void printFixedWidthInt(long x, int length) {
        CharArrayWriter baStream = new CharArrayWriter();
        PrintWriter pStream = new PrintWriter(baStream);
        pStream.print(x);
        String str = baStream.toString();
        for (int cnt = length - str.length(); cnt > 0; --cnt)
            out.print(' ');
        out.print(str);
    }
/*
    protected int getbyte (int pc) {
        return code[pc];
    }

    protected int getUbyte (int pc) {
        return code[pc]&0xFF;
    }

    int getShort (int pc) {
        return (code[pc]<<8) | (code[pc+1]&0xFF);
    }

    int getUShort (int pc) {
        return ((code[pc]<<8) | (code[pc+1]&0xFF))&0xFFFF;
    }

    protected int getInt (int pc) {
        return (getShort(pc)<<16) | (getShort(pc+2)&0xFFFF);
    }
*/
    /**
     * Print constant value at that index.
     */
    void PrintConstant(int cpx) {
        if (cpx==0) {
            out.print("#0");
            return;
        }
        byte tag=0;
        try {
            tag=cls.getTag(cpx);

        } catch (IndexOutOfBoundsException e) {
            out.print("#"+cpx);
            return;
        }
        switch (tag) {
        case CONSTANT_METHOD:
        case CONSTANT_INTERFACEMETHOD:
        case CONSTANT_FIELD: {
            // CPX2 x=(CPX2)(cpool[cpx]);
            CPX2 x = (CPX2)(cls.getCpoolEntry(cpx));
            if (x.cpx1 == cls.getthis_cpx()) {
                // don't print class part for local references
                cpx=x.cpx2;
            }
        }
        }
        out.print(cls.TagString(tag)+" "+ cls.StringValue(cpx));
    }

    protected static int  align (int n) {
        return (n+3) & ~3 ;
    }

    public void printend(){
        out.println(prefix+"end;");
        out.println();
/*        
        if (cls.isInnerClass()) {
        	String shortName = PascalClassData.getShortClassName(cls.getClassName());
        	if (PascalKeywords.isPascalKeyword(shortName))
        		shortName = "&" + shortName;
        	String pascalShortName = PascalClassData.getShortPascalClassName(cls.getClassName());
        	out.println(prefix+pascalShortName+" = "+shortName+";");
        }
        out.println();
*/        
    }

    public String javaclassname(String name){
        return name.replace('/','.');
    }
    
    public String javaparentclassname(String name) {
    	int index = name.lastIndexOf('$');
    	if (index == -1)
    		return "";
    	else
    		return name.substring(0,index);
    }
    
    public int javaclassnestinglevel(String name) {
    	int count, i;
    	count = 0;
    	for (i=1; i < name.length(); i++)
    		if (name.charAt(i) == '$')
    			count++;
    	return count;
    }
    
    /**
     * Print attribute data in hex.
     */
    public void printAttrData(AttrData attr){
        byte []data = attr.getData();
        int i = 0;
        int j = 0;
        out.print("  "+attr.getAttrName()+": ");
        out.println("length = " + cls.toHex(attr.datalen));

        out.print("   ");


        while (i < data.length){
            String databytestring = cls.toHex(data[i]);
            if(databytestring.equals("0x")) out.print("00");
            else if(databytestring.substring(2).length() == 1){
                out.print("0"+databytestring.substring(2));
            } else{
                out.print(databytestring.substring(2));
            }

             j++;
            if(j == 16) {
                out.println();
                out.print("   ");
                j = 0;
            }
            else out.print(" ");
            i++;
        }
        out.println();
    }
}
