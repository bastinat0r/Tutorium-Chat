package tutoriumchat.utils;

import static org.junit.Assert.*;

import org.junit.Test;

public class PasswordTest {

	@Test
	public void test() {
		fail("Not yet implemented");
	}

	public static void main(String[] args) {
		try {
			SharedSecrets s = new SharedSecrets();
			s.addSecret("foo", "bar");
			s.addSecret("23", "42");
			s.save("db.txt");

			SharedSecrets t = new SharedSecrets();
			t.load("db.txt");
			System.out.println(t.getSecret("foo"));
			System.out.println(t.getSecret("23"));
		} catch (Exception e) {
			System.err.println("error: " + e);
		}
	}
}
