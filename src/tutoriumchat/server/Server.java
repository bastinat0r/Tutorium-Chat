package tutoriumchat.server;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ConcurrentHashMap;

import tutoriumchat.utils.*;

/*
  ATTENTION: We have a very stupid bug: The server blocks. I don't know the
  reason since the server seems to wait for the first client to input something
  although different client are handled in different threads.

  Feel free to find out what's wrong...
*/

public class Server {
    private ServerSocket socket;
    private ConcurrentHashMap<Socket, ObjectOutputStream> connectionmap;
    private SharedSecrets db;

    public Server(int port) {
	try {
	    socket = new ServerSocket(port);
	} catch (IOException e) {
	    System.err.println("error in ChatServer(): " + e);
	}
	connectionmap = new ConcurrentHashMap<Socket, ObjectOutputStream>();
	listen();
    }

    private void listen() {
	while (true) {
	    try {
		// TODO: We should keep track of clients so we can send
		// messages to them
		Socket newconnection = socket.accept();
		// We give Reference for own Server in Thread, then they can use
		// functions of our object.
		// For Example sendToAll or removeSelf. That I would implement.
		ObjectOutputStream newstream
		    = new ObjectOutputStream(newconnection.getOutputStream());
		connectionmap.put(newconnection, newstream);
		new ClientHandler(newconnection, db, this).start();
	    } catch (IOException e) {
		System.err.println("error in void listen(): " + e);
	    }
	}
    }
    // public void message(Object ){
    // }

}
