package fpc.tools.javapp;

import fpc.tools.javapp.MethodData;
import java.util.*;
import java.io.*;

import static fpc.tools.javapp.RuntimeConstants.*;

public class PascalMethodData extends MethodData {
	
	private String cachedName;
	
    public PascalMethodData(ClassData cls) {
		super(cls);
	}

	public String getVisibilitySectionName(){
        if ((access & ACC_PUBLIC)   !=0) return "public";
        if ((access & ACC_PRIVATE)   !=0) return "strict private";
        if ((access & ACC_PROTECTED)   !=0) return "strict protected";
        /* package visibility = visible in this unit */
        return "private";
    }

    /**
     * Return modifiers of the method that matter to Pascal import.
     */
    public String getModifiers(){
        if ((access & ACC_FINAL)    !=0) return " final;";
        if ((access & ACC_ABSTRACT) !=0) return " abstract;";
        return "";
    }

    /**
     * Return java return type signature of method.
     */
    public String getReturnType(){

        String rttype = (new PascalTypeSignature(getInternalSig(), cls)).getReturnType();
        return rttype;
    }

    /**
     * Return java return type signature of method.
     */
    public String getRawBaseReturnType(){

        String rttype = (new TypeSignature(getInternalSig())).getReturnType();
        int arrPos = rttype.indexOf('[');
        if (arrPos != -1)
        	rttype = rttype.substring(0,arrPos);
        return rttype;
    }

    /**
     * Return java type parameter signature.
     */
    public String getParameters(){
        String ptype = (new PascalTypeSignature(getInternalSig(),cls)).getParameters();

        return ptype;
    }
    

    public String getName(){
    	if (cachedName == null) {
    		String realName = super.getName();
    		cachedName = ClassIdentifierInfo.AddIdentifierNameForClass(cls.getClassName(),realName);
    		// this will require compiler support for remapping field names
    		// (we also need it for Objective-C and C++ anyway)
    		if ((cachedName.charAt(0) != '&') &&
    				!cachedName.equals(realName)) {
    			System.out.println("  Duplicate identifier conflict in "+cls.getClassName()+" for method '"+realName+"', disabled");
    		}
    				
    	}
    	return cachedName;
    }
}
