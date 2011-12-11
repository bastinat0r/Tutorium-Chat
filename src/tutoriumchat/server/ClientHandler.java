package tutoriumchat.server;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;

public class ClientHandler implements Runnable {
    private Server server;
    private Socket socket;
    private DataOutputStream outstream;
    private DataInputStream instream;
    private boolean authorized = false;
    private boolean run = true; // used to determine whether thread should stop

    public ClientHandler(Socket socket, Server server) {
        this.server = server;
        this.socket = socket;
    }

    /*
     * public void process(RegisterMessage m) { }
     */

    /*
     * Awesome, 20 years after Common Lisp has a standardized with-open-file
     * macro (and of course you could have written this yourself since the
     * beginning of the Lisp era), the Java guys finally added a (less powerful)
     * version of it in Java 7! It's time to use it! Please don't reinvent the
     * wheel for backward compatibility, simply upgrade to Java 7! -- Rosario
     */
    public void run() {

        try {
            this.instream = new DataInputStream(socket.getInputStream());
            new DataInputStream(socket.getInputStream());
            this.outstream = new DataOutputStream(socket.getOutputStream());
            waitAuthorisation();
            server.authorized(socket, outstream); // We add our
                                                  // OutputStreamSocket
            messageLoop();
        } catch (IOException e) { // TODO: (may be) more precise error handling
            e.printStackTrace();
        } finally {
            try {
                instream.close();
            } catch (Exception e) { // Ignore all exceptions by closing.
            } finally {
                try {
                    outstream.close();
                } catch (Exception e) {
                } finally {
                    server.removeClient(socket);
                }
            }
        }

    }

    public void waitAuthorisation() throws IOException {
        while ((authorized == false) && run) {
            String inp = instream.readUTF();
            authorized = authorization(inp);
        }
    }

    private boolean authorization(String inp) {
        return true;
    }

    public void messageLoop() throws IOException {
        while (run) {
            String inp = instream.readUTF();
            processMessage(inp);
        }
    }

    private void processMessage(String inp) {
        server.sendMessage(inp);
    }
}
