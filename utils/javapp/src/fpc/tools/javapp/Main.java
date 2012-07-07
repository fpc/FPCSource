/*
 * Copyright (c) 2002, 2003, Oracle and/or its affiliates. All rights reserved.
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
import java.nio.charset.Charset;

import org.jgrapht.alg.CycleDetector;
import org.jgrapht.graph.*;
import org.jgrapht.traverse.*;

/**
 * Entry point for javap, class file disassembler.
 *
 * @author  Sucheta Dambalkar (Adopted code from old javap)
 */
public class Main{

	private ArrayList<String> pkgList = new ArrayList<String>();
	JavapEnvironment env = new JavapEnvironment();
	private static boolean errorOccurred = false;
	private static final String progname = "javapp";


	public Main(){
	}

	public static void main(String argv[]) {
		entry(argv);
		if (errorOccurred) {
			System.exit(1);
		}
	}


	/**
	 * Entry point for tool if you don't want System.exit() called.
	 */
	public static void entry(String argv[]) {
		Main jpmain = new Main();
		jpmain.perform(argv);
	}

	/**
	 * Process the arguments and perform the desired action
	 */
	private void perform(String argv[]) {
		if (parseArguments(argv)) {
			displayResults();

		}
	}

	private void error(String msg) {
		errorOccurred = true;
		System.err.println(msg);
		System.err.flush();
	}

	/**
	 * Print usage information
	 */
	private void usage() {
		java.io.PrintStream out = System.out;
		out.println("Usage: " + progname + " <options> [<packages>] [<individual_classes>]");
		out.println("  Suffix package names with '.'");
		out.println();
		out.println("where options include:");
		out.println("   -a <class_or_pkgename>    Create empty skeleton versions of these classes");
		out.println("   -classpath <pathlist>     Specify where to find user class files");
		out.println("   -help                     Print this usage message");
		out.println("   -J<flag>                  Pass <flag> directly to the runtime system");
		out.println("   -i                        Generate include files instead of complete unit");
		out.println("   -l                        Print line number and local variable tables");
		out.println("   -o <output_base_name>     Base name of output unit (default: java");
		out.println("   -public                   Print only public classes and members");
		out.println("   -protected                Print protected/public classes and members");
		out.println("   -private                  Show all classes and members");
		out.println("   -prefix_constants <x>     Prefix constant names with <x> (default: <nothing>)");
		out.println("   -prefix_fields <x>        Prefix field names with <x> (default: f)");
		out.println("   -prefix_innerclasses <x>  Prefix inner class names with <x> (default: Inner)");
		out.println("   -x <class_or_pkgename>    Treat this class/package as defined in another unit (suffix package names with '.'");
		out.println("   -s                        Print internal type signatures");
		out.println("   -bootclasspath <pathlist> Override location of class files loaded");
		out.println("                             by the bootstrap class loader");
		out.println("   -varparas                Add overloads that translate non-varargs array parameters into single-element 'var' parameters");
		out.println("   -verbose                  Print stack size, number of locals and args for methods");
		out.println("                             If verifying, print reasons for failure");
		out.println();
	}

	/**
	 * Parse the command line arguments.
	 * Set flags, construct the class list and create environment.
	 */
	private boolean parseArguments(String argv[]) {
		for (int i = 0 ; i < argv.length ; i++) {
			String arg = argv[i];
			if (arg.startsWith("-")) {
				if (arg.equals("-l")) {
					env.showLineAndLocal = true;
				} else if (arg.equals("-private") || arg.equals("-p")) {
					env.showAccess = env.PRIVATE;
				} else if (arg.equals("-package")) {
					env.showAccess = env.PACKAGE;
				} else if (arg.equals("-protected")) {
					env.showAccess = env.PROTECTED;
				} else if (arg.equals("-public")) {
					env.showAccess = env.PUBLIC;
				} else if (arg.equals("-verbose"))  {
					env.showVerbose = true;
				} else if (arg.equals("-v")) {
					env.showVerbose = true;
				} else if (arg.equals("-i"))  {
					env.generateInclude = true;
				} else if (arg.equals("-h")) {
					error("-h is no longer available - use the 'javah' program");
					return false;
				} else if (arg.equals("-verify")) {
					error("-verify is no longer available - use 'java -verify'");
					return false;
				} else if (arg.equals("-verify-verbose")) {
					error("-verify is no longer available - use 'java -verify'");
					return false;
				} else if (arg.equals("-help")) {
					usage();
					return false;
				} else if (arg.equals("-classpath")) {
					if ((i + 1) < argv.length) {
						env.classPathString = argv[++i];
					} else {
						error("-classpath requires argument");
						usage();
						return false;
					}
				} else if (arg.equals("-bootclasspath")) {
					if ((i + 1) < argv.length) {
						env.bootClassPathString = argv[++i];
					} else {
						error("-bootclasspath requires argument");
						usage();
						return false;
					}
				} else if (arg.equals("-extdirs")) {
					if ((i + 1) < argv.length) {
						env.extDirsString = argv[++i];
					} else {
						error("-extdirs requires argument");
						usage();
						return false;
					}
				} else if (arg.equals("-o")) {
					if ((i + 1) < argv.length) {
						env.outputName = argv[++i];
					} else {
						error("-o requires argument");
						usage();
						return false;
					}
				} else if (arg.equals("-x")) {
					if ((i + 1) < argv.length) {
						env.excludePrefixes.add(argv[++i].replace('.','/'));
					} else {
						error("-x requires argument");
						usage();
						return false;
					}
				} else if (arg.equals("-a")) {
					if ((i + 1) < argv.length) {
						env.skelPrefixes.add(argv[++i].replace('.','/'));
					} else {
						error("-a requires argument");
						usage();
						return false;
					}
				} else if (arg.equals("-all")) {
					env.showallAttr = true;
				} else if (arg.equals("-prefix_constants")) {
					if ((i + 1) < argv.length) {
						env.prefix_constant = argv[++i];
					} else {
						error("-prefix_constants requires argument");
						usage();
						return false;
					}
				} else if (arg.equals("-prefix_fields")) {
					if ((i + 1) < argv.length) {
						env.prefix_field = argv[++i];
					} else {
						error("-prefix_fields requires argument");
						usage();
						return false;
					}
				} else if (arg.equals("-prefix_innerclasses")) {
					if ((i + 1) < argv.length) {
						env.prefix_innerclass = argv[++i];
					} else {
						error("-prefix_innerclasses requires argument");
						usage();
						return false;
					}
				} else if (arg.equals("-varparas")) {
				    env.addVarOverloads = true;
				} else {
					error("invalid flag: " + arg);
					usage();
					return false;
				}
			} else {
				pkgList.add(arg);
				env.nothingToDo = false;
			}
		}
		if (env.nothingToDo) {
			System.out.println("No classes were specified on the command line.  Try -help.");
			errorOccurred = true;
			return false;
		}
		return true;
	}

	private PrintWriter createFile(String fname) {
		PrintWriter res = null;
		try {
			res = new PrintWriter(new OutputStreamWriter(new PrintStream(new File(fname))));
		} catch (FileNotFoundException e) {
			System.out.println("Unable to create file "+fname+", aborting...");
			e.printStackTrace();
			System.exit(1);
		}
		return res;
	}

	/**
	 * Display results
	 */
	private void displayResults() {

		if (pkgList.isEmpty())
			return;

		// collect all class names in the environment (format: /package/name/classname)
		SortedSet<String> classes = env.getClassesList();
		// sort package lists that should/should not be printed
		// to optimize checking; combine exclude and skeleton prefixes in one list
		Collections.sort(pkgList);
		ArrayList<String> dontPrintPrefixes = new ArrayList<String>(env.excludePrefixes.size()+env.skelPrefixes.size());
		dontPrintPrefixes.addAll(env.excludePrefixes);
		dontPrintPrefixes.addAll(env.skelPrefixes);
		Collections.sort(dontPrintPrefixes);
		// create the unit
		PrintWriter includeFile;
		PrintWriter mainUnitFile;
		PascalUnit thisUnit;
		String includeName, mainUnitName;

		if (env.generateInclude) {
			mainUnitName = env.outputName+"h.inc";
		} else {
			mainUnitName = env.outputName+".pas";			
		}
		includeName = env.outputName+".inc";

		includeFile = createFile(includeName);
		mainUnitFile = createFile(mainUnitName);
		thisUnit = new PascalUnit(mainUnitFile, env, pkgList, includeName);
		PascalClassData.currentUnit = thisUnit;

		// create unique short names for all classes we may need
		System.out.println("Determining short Pascal class names...");
		for (Iterator<String> iter = classes.iterator(); iter.hasNext(); ) {
			thisUnit.registerClassName(iter.next());
		}

		// first read all requested classes and build dependency graph
		Iterator<String> classStepper = classes.iterator();
		Iterator<String> argStepper = pkgList.iterator();
		Iterator<String> skipPkgsStepper = dontPrintPrefixes.iterator();
		HashSet<String> classesToPrintList = new HashSet<String>();
		SimpleDirectedGraph<String,DefaultEdge> classDependencies = new SimpleDirectedGraph<String, DefaultEdge>(DefaultEdge.class);

		try {
			String currentExcludePkg; 
			String currentPrefix;
			if (skipPkgsStepper.hasNext())
				currentExcludePkg = skipPkgsStepper.next();
			else
				currentExcludePkg = ".......";
			if (argStepper.hasNext()) {
				currentPrefix = argStepper.next();
				if (!currentPrefix.equals(".")) {
					currentPrefix = currentPrefix.replace('.', '/');
				} else {
					currentPrefix = "./";
				}
			} else
				currentPrefix = ".......";
			System.out.println("Indexing classes under "+currentPrefix+"...");
			do {
				String currentClass = classStepper.next();
				// if the current class name is > the current package name, skip packages
				// until we have a package to which the current class belongs, or the
				// next in line
				while (argStepper.hasNext() &&
						!PascalUnit.classOrPackageInPrefix(currentClass,currentPrefix) &&
						(currentClass.compareTo(currentPrefix) > 0)) {
					String currentArg = argStepper.next();
					if (!currentArg.equals(".")) {
						currentArg = currentArg.replace('.', '/');
						currentPrefix = currentArg;
					} else {
						currentArg = "./";
						currentPrefix = currentArg;
					}
					System.out.println("Indexing classes under "+currentPrefix+"...");
					currentPrefix = currentArg;
				}
				boolean doPrintClass = false;
				// should check whether the class is explicitly excluded from being printed
				if (PascalUnit.classOrPackageInPrefix(currentClass,currentPrefix)) {
					while (skipPkgsStepper.hasNext() &&
							!PascalUnit.classOrPackageInPrefix(currentClass,currentExcludePkg) &&
							(currentClass.compareTo(currentExcludePkg) > 0)) {
						currentExcludePkg = skipPkgsStepper.next();
					}
					if (!PascalUnit.classOrPackageInPrefix(currentClass,currentExcludePkg)) {
						doPrintClass = true;
					}
				}
				// always construct the class to record the identifiers
				// (so we can properly rename identifiers in subclasses),
				// but only collect dependency information if we actually
				// have to print the class
				InputStream classin = env.getFileInputStream(currentClass);
				JavapPrinter printer = new JavapPrinter(classin, includeFile, env, "  ",null,doPrintClass,true);
				if (doPrintClass) {
					if (!classDependencies.containsVertex(currentClass))
						classDependencies.addVertex(currentClass);

					HashSet<String> dependencies = printer.cls.getDependencies();
					Iterator<String> depStepper = dependencies.iterator();
					while (depStepper.hasNext()) {
						String dep = depStepper.next();
						thisUnit.registerUsedClass(dep);
						if (!classDependencies.containsVertex(dep))
							classDependencies.addVertex(dep);
						/*								
								if (currentClass.equals("java/awt/Window"))
									System.out.println("  java/awt/Window depends on "+dep);
								if (dep.equals("java/awt/Window"))
									System.out.println("dep = java/awt/Window for "+currentClass);
						 */									
						classDependencies.addEdge(dep, currentClass);
					}

					classesToPrintList.add(currentClass);

					//    							JavapClassPrinter.PrintClass(env,includeFile,currentClass,"  ");
				}
			} while (classStepper.hasNext());
			// no longer needed
			classes = null;
			pkgList = null;
			System.out.println("Printing classes...");
			//    		Iterator<String> printerStepper = classesToPrintList.iterator();
			TopologicalOrderIterator<String,DefaultEdge> printerStepper = new TopologicalOrderIterator<String,DefaultEdge>(classDependencies); 
			while (printerStepper.hasNext()) {
				String currentClass = printerStepper.next();
				// also contains external classes
				if (!classesToPrintList.remove(currentClass))
					continue;
				try {
					InputStream classin = env.getFileInputStream(currentClass);
					JavapPrinter printer = new JavapPrinter(classin, includeFile, env, "  ",null, true,false);
					printer.print();

					//					JavapClassPrinter.PrintClass(env,includeFile,currentClass,"  ");
				} catch (IllegalArgumentException exc) {
					error(exc.getMessage());
					exc.printStackTrace();
					System.out.println("Error while processing class "+currentClass+", aborting...");
					includeFile.close();
					System.exit(1);
				}
			}

			// the iterator
			if (!classesToPrintList.isEmpty()) {
				Iterator<String> leftOvers = classesToPrintList.iterator();
				System.out.println("Classes part of dependency cycles, or related to these classes:");
				CycleDetector<String, DefaultEdge> cycles = new CycleDetector<String, DefaultEdge>(classDependencies);
				while (leftOvers.hasNext()) {
					String currentClass = leftOvers.next();
					System.out.println("  "+currentClass+" is part of cycle "+cycles.findCyclesContainingVertex(currentClass));

					try {
						InputStream classin = env.getFileInputStream(currentClass);
						JavapPrinter printer = new JavapPrinter(classin, includeFile, env, "  ",null,true,false);
						printer.print();

						//					JavapClassPrinter.PrintClass(env,includeFile,currentClass,"  ");
					} catch (IllegalArgumentException exc) {
						error(exc.getMessage());
						exc.printStackTrace();
						System.out.println("Error while processing class "+currentClass+", aborting...");
						includeFile.close();
						System.exit(1);
					}
				}
			}

		} finally {
			includeFile.close();
			thisUnit.printUnit();
			mainUnitFile.close();
		}
		/*        Class[] classes = ClassListBuilder.getClassesInPackage("fpc.tools.javapp");  
        for (Class c : classes) {  
            System.out.println("Found: " + c.getCanonicalName());  
        }
		 */  
		System.out.println("Done!");
	}

}
