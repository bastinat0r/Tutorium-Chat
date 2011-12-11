package tutoriumchat.server;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ConcurrentHashMap;

import tutoriumchat.utils.SharedSecrets;

/*
 * 
 Socket newconnection = socket.accept(); 
 blocks until we get new connection./
 Server hanged in this operation ALL time. It is not a bug, or what you mean?

 ATTENTION: We have a very stupid bug: The server blocks. I don't know the
 reason since the server seems to wait for the first client to input something
 although different client are handled in different threads.

 Feel free to find out what's wrong...
 */

public class Server {
    private ServerSocket socket;
    private ConcurrentHashMap<Socket, DataOutputStream> connectionmap;
    private SharedSecrets db;

    public SharedSecrets getDb() {
        return db;
    }

    public Server(int port) {
        System.out.println("Starting server on port:" + port);
        try {
            socket = new ServerSocket(port);
        } catch (IOException e) {
            System.err.println("error in ChatServer(): " + e);
        }
        connectionmap = new ConcurrentHashMap<Socket, DataOutputStream>();
        System.out.println("Started...");
        listen();
    }

    public void listen() {
        while (true) {
            try {
                new Thread(new ClientHandler(socket.accept(), this)).start();
            } catch (IOException e) {
                System.err.println("error in void listen(): " + e);
            }
        }
    }

    // We add OutputStream to Server
    // You don't need Lock here, that's what you use
    public void authorized(Socket socket, DataOutputStream newstream) {
        connectionmap.put(socket, newstream);
    }

    public void sendMessage(DataOutputStream output, String message) {
        try {
            for (DataOutputStream value : connectionmap.values())
                if (output != value)
                    value.writeChars(message);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void removeClient(Socket socket) {
        connectionmap.remove(socket);
        try {
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
