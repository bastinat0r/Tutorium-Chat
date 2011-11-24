import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.util.HashMap;

public class SharedSecrets {
    private static HashMap<String,String> db = new HashMap<String, String>();

    public static synchronized void addSecret(String username, String secret)
	throws Exception {
	if(!db.containsKey(username))
	    db.put(username, secret);
	else
	    throw new Exception();
    }

    public static synchronized String getSecret(String username) {
	return db.get(username);
    }

    public static void save(String path) {
	try {
	    ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(path));
	    out.writeObject(db);
	}
	catch (Exception e) { // TODO: more precise error handling
	    System.err.println("error: " + e);
	}
    }

    public static void load(String path) {
	try {
	    ObjectInputStream in = new ObjectInputStream(new FileInputStream(path));
	    db = (HashMap<String,String>)in.readObject();
	}
	catch (Exception e) {
	    System.err.println("error:" + e);
	}
    }
}
