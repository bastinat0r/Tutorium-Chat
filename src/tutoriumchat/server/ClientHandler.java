package tutoriumchat.server;

import java.net.Socket;
import java.io.ObjectInputStream;
import java.io.IOException;

import tutoriumchat.utils.*;

public class ClientHandler extends Thread {
    private Socket socket;
    private ObjectInputStream inp;
    private SharedSecrets db;
    private Server server;
    private boolean authorized = false;
    private boolean run = true; // used to determine whether thread should stop

    public ClientHandler(Socket socket, SharedSecrets db, Server server) {
	this.socket = socket;
	this.db = db;
	this.server = server;
	try {
	    this.inp = new ObjectInputStream(socket.getInputStream());
	}
	catch(Exception e) {
	    System.err.println("error in ClientHandler():" + e);
	    try {
		socket.close();
	    }
	    catch(IOException ioe) {
		System.err.println("error: Could not even close socket (" +
				   socket.getInetAddress() + "): " + ioe);
	    }
	}
    }

    public void process(AuthMessage m) {
	System.out.println("client: " + m);
    }

    public void run() {
	try {
	    while(run) {
		Message m = (Message)inp.readObject();
	
		if(m instanceof AuthMessage) {
		    process((AuthMessage)m);
		}
	    }
	}
	catch(Exception e) {
	    System.err.println("error in run(): " + e);
	    run = false;
	}
	finally {
	    try {
		socket.close();
	    }
	    catch(IOException e) {
		System.err.println("error: Could not even close socket (" +
				   socket.getInetAddress() + "): " + e);
	    }
	}
    }
}
