import java.security.SecureRandom;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

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

public class Password {
    private byte[] password;
    private byte[] salt;
    private boolean hashed = false;
    
    public Password(byte[] password) {
	this.password = password;
    }

    public Password(String password) {
	this(password.getBytes());
    }
    
    public Password(String password, byte[] salt) {
	this(password);
	this.salt = salt;
    }

    public byte[] getPassword() {
	return password;
    }

    public void setPassword(String password) {
	this.password = password.getBytes();
    }

    public byte[] getSalt() {
	return salt;
    }
    
    public void setSalt(byte[] salt) {
	this.salt = salt;
    }

    public static byte[] genSalt(int length) {
	SecureRandom random = new SecureRandom();
	byte[] saltedBytes = new byte[length];
	random.nextBytes(saltedBytes);

	return saltedBytes;
    }

    public void salt() {
	byte[] saltedPw = new byte[password.length + salt.length];

	System.arraycopy(password, 0, saltedPw, 0, password.length);
	System.arraycopy(salt, 0, saltedPw, password.length, salt.length);

	password = saltedPw;
    }

    public void hash() {
	try {
	    MessageDigest md = MessageDigest.getInstance("SHA-256");
	    password = md.digest(password);
	    hashed = true;
	}
	catch(NoSuchAlgorithmException e) {
	    // SHA-256 is a required digest, so this exception should never be
	    // thrown anyway.
	}
    }

    public boolean isHashed() {
	return hashed;
    }

    private String toHexString() {
	// That's messy! I don't like Perl or PHP very much but even there
	// this can be expressed in *one* line...
	
	StringBuffer result = new StringBuffer();
	for(byte b: password) {
	    String hex = Integer.toHexString(0xFF & b);

	    // To get a fixed length every element of this hex string actually
	    // consists of 2 characters
	    if(hex.length() == 1) {
		result.append('0');
	    }
	    
	    result.append(hex);
	}
	
	return result.toString();
    }
    
    public void genSaltedHash() {
	if(salt != null)
	    salt();
	hash();
    }

    // just for user-friendliness we do 2 steps in one
    public void genSaltedHash(byte[] salt) {
	setSalt(salt);
	genSaltedHash();
    }

    // returns either the plain text password or the hashed value in
    // hexadecimal form
    public String toString() { 
	if(hashed)
	    return toHexString();
	else
	    return new String(password);
    }
}
