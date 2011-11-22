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

    public void run() {
	try {
	    // TODO: check if user is already registered
	    String username = (Message)in.readObject().getUsername();

	    AuthMessage m = new AuthMessage(username, 1024);
	    out.writeObject(m);
	    // TODO: implement a shared secret database
	    m.getAuthCode(getSharedSecret(username));

	    if(m.isCorrectAuthCode((AuthMessage)in.readObject())) {
		out.writeObject(new String("Welcome to this server!"));
	    }
	}
	catch(Exception e) { // TODO: more precise error handling
	    System.err.println("error: " + e);
	    socket.close();
	}
    }
}
