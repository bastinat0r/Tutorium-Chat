import java.net.ServerSocket;
import java.io.IOException;

public class ChatServer {
    private ServerSocket socket;
    private SharedSecrets db;
    
    public ChatServer(int port) {
	try {
	    socket = new ServerSocket(port);
	}
	catch(IOException e) {
	    System.err.println("error in ChatServer(): " + e);
	}
    }

    public void listen() {
	while(true) {
	    try {
		// TODO: We should keep track of clients so we can send
		// messages to them
		new ClientHandler(socket.accept(), db).start();
	    }
	    catch(IOException e) {
		System.err.println("error in void listen(): " + e);
	    }
	}
    }
}
