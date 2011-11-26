package tutoriumchat.server;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

import tutoriumchat.utils.SharedSecrets;

public class Server {

    private ServerSocket socket;
    private Map<Socket, ObjectOutputStream> connectionmap;
    ReentrantLock lock;
    @SuppressWarnings("unused")
    private SharedSecrets db;

    public Server(int port) {
        System.out.println("Starting server on port:" + port);
        try {
            socket = new ServerSocket(port);
        } catch (IOException e) {
            System.err.println("error in ChatServer(): " + e);
        }
        connectionmap = new HashMap<Socket, ObjectOutputStream>();
        System.out.println("Started...");
        listen();
    }

    public void listen() {
        while (true) {
            try {
                Socket newconnection = socket.accept();
                Thread newtread = new Thread(new ClientHandler(newconnection,
                        this));
                newtread.start();
            } catch (IOException e) {
                System.err.println("error in void listen(): " + e);
            }
        }
    }

    // We add OutputStream to Server
    public void authorized(Socket socket, ObjectOutputStream newstream) {
        // ObjectOutputStream newstream;
        // newstream = new ObjectOutputStream(
        // socket.getOutputStream());
        lock.lock();
        connectionmap.put(socket, newstream);
        lock.unlock();
    }

    public void message(Object object) {
        try {
            lock.lock();
            for (ObjectOutputStream value : connectionmap.values()) {
                value.writeObject(object);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            lock.unlock();
        }
    }

    public void removeSelf(Socket socket) {
        lock.lock();
        connectionmap.remove(socket);
        try {
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            lock.unlock();
        }

    }
}