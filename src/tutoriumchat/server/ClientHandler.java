package tutoriumchat.server;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

import tutoriumchat.server.utils.AuthorisationException;
import tutoriumchat.utils.AuthMessage;
import tutoriumchat.utils.Message;
import tutoriumchat.utils.TextMessage;

/*
 This part still has a big and unsolved problem: We can not read and write
 at the same time. We should start at least one new thread for reading or
 writing.
 */

public class ClientHandler implements Runnable {
    private Server server;
    private Socket socket;
    private ObjectOutputStream outstream;
    private ObjectInputStream instream;
    private boolean authorized = false;
    private boolean run = true; // used to determine whether thread should stop

    public ClientHandler(Socket socket, Server server) {
        this.server = server;
        this.socket = socket;
    }

    public boolean process(AuthMessage message) throws IOException,
            ClassNotFoundException, AuthorisationException {
        AuthMessage random = new AuthMessage(message.getUsername(), 1024);
        outstream.writeObject(message);
        outstream.flush();
        random.genAuthCode(server.getDb().getSecret(message.getUsername()));
        if (!random.isCorrectAuthCode((AuthMessage) instream.readObject()))
            throw new AuthorisationException("Some Error"); // TODO: throw a
                                                            // more appropriate
                                                            // exception
        else
            return true;
    }

    public void process(TextMessage message) {
        server.sendMessage(message);
    }

    /*
     * public void process(RegisterMessage m) { }
     */

    // Let us use a new and fancy feature from Java 7 here: try-with-resources
    public void run() {
        try (this.instream = new ObjectInputStream(socket.getInputStream());
	     this.outstream = new ObjectOutputStream(socket.getOutputStream()))
		{
		    waitAuthorisation();
		    server.authorized(socket, outstream); // We add our
                                                  // OutputStreamSocket
		    messageLoop();
		}
	catch (Exception e) {
	    e.printStackTrace();
	}
	finally {
	    server.removeClient(socket);
	}
    }

    public void waitAuthorisation() throws IOException, ClassNotFoundException,
            AuthorisationException {
        while ((authorized == false) && run) {
            Message inp = (Message) instream.readObject();
            if (inp instanceof AuthMessage) {
                authorized = process((AuthMessage) inp);

            }
        }
    }

    public void messageLoop() throws IOException, ClassNotFoundException {
        while (run) {
            Message inp = (Message) instream.readObject();
            if (inp instanceof TextMessage) {
                process((TextMessage) inp);
            }
        }
    }
}