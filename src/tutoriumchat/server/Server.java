package tutoriumchat.server;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.*;

import tutoriumchat.utils.*;

public class Server{
	
	private ServerSocket socket;
    private Hashtable connectionmap;
	private SharedSecrets db;

	public Server(int port) {
		try {
			socket = new ServerSocket(port);
		} catch (IOException e) {
			System.err.println("error in ChatServer(): " + e);
		}
		connectionmap = new Hashtable<Socket, ObjectOutputStream>();
		listen();
	}

	public void listen() {
		while (true) {
			try {
				// TODO: We should keep track of clients so we can send
				// messages to them
				Socket newconnection = socket.accept();
				new ClientHandler(newconnection, this, db);
				ObjectOutputStream newstream = new ObjectOutputStream(socket.getOutputStream());
				connectionmap.put(newconnection, newstream);
			} catch (IOException e) {
				System.err.println("error in void listen(): " + e);
			}
		}
	
	public void message()
	}
}
