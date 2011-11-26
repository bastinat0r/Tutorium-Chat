package tutoriumchat.server;

public class ServerStart {
	public class ChatServerTest {
	    public static void main(String[] args) {
		Server s = new Server(8080);
		s.listen();
	    }
	}
}
