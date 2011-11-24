import java.net.Socket;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;

import java.io.IOException;
import java.io.EOFException;
import java.io.StreamCorruptedException;

public class ClientHandler extends Thread {
    private Socket socket;
    private ObjectOutputStream out;
    private ObjectInputStream in;
    private boolean loggedIn = false;
    private SharedSecrets db;

    private boolean run = true; // used to indicate that the thread should stop

    public ClientHandler(Socket socket, SharedSecrets db) {
	this.socket = socket;
	this.db = db;
	try {
	    out = new ObjectOutputStream(socket.getOutputStream());
	    in = new ObjectInputStream(socket.getInputStream());
	}
	catch(StreamCorruptedException e) {
	    run = false;
	    System.err.println("error in ClientHandler(): user has no valid client (" + socket.getInetAddress() + ")");
	    
	    try {
		socket.getOutputStream().write("error: You are not using a valid client.\r\n".getBytes());
		socket.getOutputStream().flush();
	    }
	    catch(Exception writeError) {
		System.err.println("error in ClientHandler(): Could not even send error message to " + socket.getInetAddress());
	    }
	}
	catch(EOFException e) {
	    run = false;
	    System.err.println("error in ClientHandler(): Client closed the connection (" + socket.getInetAddress() + ")");
	}
	catch(Exception e) {
	    run = false;
	    System.err.println("unknown error in ClientHandler(): " + e);
	}
    }


    public void process(AuthMessage m) throws Exception {
	AuthMessage random = new AuthMessage(m.getUsername(), 1024);
	out.writeObject(m);
	out.flush();

	random.genAuthCode(db.getSecret(m.getUsername()));
	if(!random.isCorrectAuthCode((AuthMessage)in.readObject()))
	    throw new Exception(); // TODO: throw a more appropriate exception
	else
	    loggedIn = true;
    }

    /*
      public void process(TextMessage m) {
    }

    public void process(RegisterMessage m) {
    }
    */
    
    public void run() {
	try {
	    while(run) {
		Message inp = (Message)in.readObject();
		if(inp instanceof AuthMessage) {
		    process((AuthMessage)inp);
		}
	    }
	    socket.close();
	}
	catch(Exception e) { // TODO: more precise error handling
	    System.err.println("error in run(): " + e);
	}
    }
}
