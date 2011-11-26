/*
  The basic idea is that we need to avoid two things:
   
   - The server should not know whether two users have the same password.
   - Even if an unencrypted connection is used, the password should not be
   sent plain text and thus be readable to the world.
   
   The solution is based on a shared secret between the client and the server.
   Based on this secret, the server generates and sends a random salt
   everytime the client tries to connect. This new salt and the shared
   secret can be used to create a new hashed 'password' which the client sends
   back to the server which validates this.
   
   The very first time both, the server and the client, get to know each other,
   the shared secret is exchanged. The user creates this secret based on
   his/her passphrase and a random salt which is only know by him-/herself.
   The server gets the hashed and salted 'password' and stores it.

   As you can see, security fades away if the very first connection is
   insecure.
*/

public class AuthMessage extends Message {
    private byte[] randomSalt;

    public AuthMessage(String username, byte[] randomSalt) {
	super(username, "");
	this.randomSalt = randomSalt;
    }

    public AuthMessage(String username, int saltLen) {
	this(username, Password.genSalt(saltLen));
    }

    public AuthMessage(AuthMessage m) {
	this(m.getUsername(), m.getRandomSalt());
    }

    public byte[] getRandomSalt() {
	return randomSalt;
    }

    public void genNewRandomSalt(int saltLen) {
	randomSalt = Password.genSalt(saltLen);
    }

    public void genAuthCode(String sharedSecret) {
	Password authCode = new Password(sharedSecret, randomSalt);
	authCode.genSaltedHash();
	text = authCode.toString();
    }

    public void genAuthCode(Password sharedSecret) {
	genAuthCode(sharedSecret.toString());
    }

    public boolean isCorrectAuthCode(AuthMessage m) {
	return text.equals(m.getText());
    }
}
