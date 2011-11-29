package tutoriumchat.server;

import java.io.IOException;
import java.io.StreamCorruptedException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

import tutoriumchat.server.utils.AuthorisationException;
import tutoriumchat.utils.AuthMessage;
import tutoriumchat.utils.Message;
import tutoriumchat.utils.TextMessage;

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

    /*
      Awesome, 20 years after Common Lisp has a standardized with-open-file
      macro (and of course you could have written this yourself since the
      beginning of the Lisp era), the Java guys finally added a (less
      powerful) version of it in Java 7! It's time to use it! Please don't
      reinvent the wheel for backward compatibility, simply upgrade to Java 7!
      -- Rosario
    */
    public void run() {
        try (ObjectInputStream inp =
	     new ObjectInputStream(socket.getInputStream());
	     ObjectOutputStream out =
	     new ObjectOutputStream(socket.getOutputStream()))
		{
		    instream = inp;
		    outstream = out;
		    waitAuthorisation();
		    server.authorized(socket, outstream);
		    messageLoop();
		}
	catch(Exception e) {
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
