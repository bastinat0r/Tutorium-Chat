package tutoriumchat.utils;

public class AuthMessageTest {
	public static void testEquality(AuthMessage serverMsg,
			AuthMessage clientMsg, Password sharedSecret) {
		serverMsg.genAuthCode(sharedSecret);
		clientMsg.genAuthCode(sharedSecret);

		System.out.println("server: " + serverMsg.getText());
		System.out.println("client: " + clientMsg.getText());

		if (serverMsg.isCorrectAuthCode(clientMsg))
			System.out.println("server: Client authorized.");
		else {
			System.out.println("server: This should not happen...");
		}
	}

	public static void main(String[] args) {
		Password sharedSecret = new Password("secretPasssword",
				Password.genSalt(1024));
		sharedSecret.genSaltedHash();

		AuthMessage serverMsg = new AuthMessage("foobar", 1024);
		AuthMessage clientMsg = new AuthMessage(serverMsg);

		testEquality(serverMsg, clientMsg, sharedSecret);

		// let's try a second round

		serverMsg.genNewRandomSalt(1024);
		clientMsg = new AuthMessage(serverMsg);

		testEquality(serverMsg, clientMsg, sharedSecret);
	}
}
