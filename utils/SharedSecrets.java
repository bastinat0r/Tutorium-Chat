import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.util.concurrent.ConcurrentHashMap;

public class SharedSecrets {
    private ConcurrentHashMap<String,String> db;

    public SharedSecrets() {
	db = new ConcurrentHashMap<String,String>();
    }
    
    public void addSecret(String username, String secret)
	throws Exception {
	if(!db.containsKey(username))
	    db.put(username, secret);
	else
	    throw new Exception();
    }

    public String getSecret(String username) {
	return db.get(username);
    }

    public void save(String path) {
	try {
	    ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(path));
	    out.writeObject(db);
	}
	catch (Exception e) { // TODO: more precise error handling
	    System.err.println("error: in save() " + e);
	}
    }

    public void load(String path) {
	try {
	    ObjectInputStream in = new ObjectInputStream(new FileInputStream(path));
	    db = (ConcurrentHashMap<String,String>)in.readObject();
	}
	catch (Exception e) {
	    System.err.println("error: in load()" + e);
	}
    }
}
