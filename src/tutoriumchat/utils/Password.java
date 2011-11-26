package tutoriumchat.utils;

import java.security.SecureRandom;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

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
	if(salt != null) {
	    byte[] saltedPw = new byte[password.length + salt.length];

	    System.arraycopy(password, 0, saltedPw, 0, password.length);
	    System.arraycopy(salt, 0, saltedPw, password.length, salt.length);

	    password = saltedPw;
	}
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
