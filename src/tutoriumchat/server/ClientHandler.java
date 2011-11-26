package tutoriumchat.server;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

import tutoriumchat.utils.AuthMessage;
import tutoriumchat.utils.Message;
import tutoriumchat.utils.SharedSecrets;

/*
 This part still has a big and unsolved problem: We can not read and write
 at the same time. We should start at least one new thread for reading or
 writing.
 */

public class ClientHandler implements Runnable {
    private Server server;
    private Socket socket;
    private ObjectOutputStream out;
    private ObjectInputStream in;
    @SuppressWarnings("unused")
    private boolean loggedIn = false;
    private SharedSecrets db;

    public ClientHandler(Socket socket, Server server) {
        this.server = server;
        this.socket = socket;
    }

    public void process(AuthMessage m) throws Exception {
        AuthMessage random = new AuthMessage(m.getUsername(), 1024);
        out.writeObject(m);
        out.flush();

        random.genAuthCode(db.getSecret(m.getUsername()));
        if (!random.isCorrectAuthCode((AuthMessage) in.readObject()))
            throw new Exception(); // TODO: throw a more appropriate exception
        else
            loggedIn = true;
    }

    /*
     * public void process(TextMessage m) { }
     * 
     * public void process(RegisterMessage m) { }
     */

    public void run() {

        try {
            this.in = new ObjectInputStream(socket.getInputStream());
            while (true) {
                Message inp = (Message) in.readObject();
                if (inp instanceof AuthMessage) {
                    // process((AuthMessage) inp);
                }
            }
        } catch (IOException e) { // TODO: (may be) more precise error handling
            e.printStackTrace();
        } catch (ClassNotFoundException e) { // TODO: (may be) more precise
                                             // error handling
            e.printStackTrace();
        } finally {
            try {
                in.close();
            } catch (Exception e) { // Ignore all exceptions by closing.
            } finally {
                try {
                    out.close();
                } catch (Exception e) {
                } finally {
                    server.removeSelf(socket);
                }
            }
        }
    }
}