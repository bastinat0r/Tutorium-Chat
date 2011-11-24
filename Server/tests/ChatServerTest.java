public class ChatServerTest {
    public static void main(String[] args) {
	ChatServer s = new ChatServer(8080);
	s.listen();
    }
}
