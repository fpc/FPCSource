package fpc.tools.javapp;

import java.io.File;  
import java.io.IOException;
import java.net.URL;  
import java.util.ArrayList;  
import java.util.Enumeration;
import java.util.List;  
  
public class ClassListBuilder {  
  
    public static Class[] getClassesInPackage(String pckgname) {  
        File directory = getPackageDirectory(pckgname);  
        if (!directory.exists()) {  
            throw new IllegalArgumentException("Could not get directory resource for package " + pckgname + ".");  
        }  
  
        return getClassesInPackage(pckgname, directory);  
    }  
  
    private static Class[] getClassesInPackage(String pckgname, File directory) {  
        List<Class> classes = new ArrayList<Class>();  
        for (String filename : directory.list()) {  
            if (filename.endsWith(".class")) {  
                String classname = buildClassname(pckgname, filename);  
                try {  
                    classes.add(Class.forName(classname));  
                } catch (ClassNotFoundException e) {  
                    System.err.println("Error creating class " + classname);  
                }  
            }  
        }  
        return classes.toArray(new Class[classes.size()]);  
    }  
  
    private static String buildClassname(String pckgname, String filename) {  
        return pckgname + '.' + filename.replace(".class", "");  
    }  
  
    private static File getPackageDirectory(String pckgname) {  
        ClassLoader cld = Thread.currentThread().getContextClassLoader();  
        if (cld == null) {  
            throw new IllegalStateException("Can't get class loader.");  
        }
        Enumeration<URL> resources;
        try {
        	resources = cld.getResources(pckgname.replace('.', '/'));
        } catch (IOException e) {
        	throw new IllegalStateException("can't get resourcs.");
        }
        System.out.println("Found any elements: "+resources.hasMoreElements());
        while (resources.hasMoreElements()) {
            System.out.println(resources.nextElement().getPath());
        }
        
        URL resource = cld.getResource(pckgname.replace('.', '/'));  
        if (resource == null) {  
            throw new RuntimeException("Package " + pckgname + " not found on classpath.");  
        }  
  
        return new File(resource.getFile());  
    }  
  /*
    public static void main(String[] args) {  
        Class[] classes = getClassesInPackage("com.mycomp");  
        for (Class c : classes) {  
            System.out.println("Found: " + c.getCanonicalName());  
        }  
    }  
*/  
}  