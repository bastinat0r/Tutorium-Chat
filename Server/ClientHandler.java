import java.net.Socket;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;

// This is just a dummy yet. You can't compile it because some code is still
// missing. Look at the TODOs to see which parts these are.

public class ClientHandler extends Thread {
    private Socket socket;
    private ObjectOutputStream out;
    private ObjectInputStream in;
    private boolean loggedIn = false;

    public ClientHandler(Socket socket) {
	this.socket = socket;
	try {
	    out = new ObjectOutputStream(socket.getOutputStream());
	    in = new ObjectInputStream(socket.getInputStream());
	}
	catch(IOException e) {
	    System.err.println("error: " + e);
	}
    }

    public void process(AuthMessage m) throws Exception {
	AuthMessage random = new AuthMessage(m.getUsername(), 1024);
	out.writeObject(m);
	out.flush();

	// TODO: implement a shared secret database
	random.genAuthCode(SharedSecrets.getSharedSecret(m.getUsername));
	if(!random.isCorrectAuthCode((AuthMessage)in.readObject()))
	    throw new Exception(); // TODO: throw a more appropriate exception
	else
	    loggedIn = true;
    }

    public void process(TextMessage m) {
    }

    public void process(RegisterMessage m) {
    }
    
    public void run() {
	try {
	    while(true)
		// Does polymorphism work here? Does the object know what it
		// *really* is?
		process((Message)in.readObject());
	}
	catch(Exception e) { // TODO: more precise error handling
	    System.err.println("error: " + e);
	    socket.close();
	}
    }
}
