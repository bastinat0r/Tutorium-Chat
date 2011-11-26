package tutoriumchat.utils;

import java.io.Serializable;
import java.util.Date;

/**
 *
 * @author sebastian
 */
public abstract class Message implements Serializable {

	Date timeStamp;
	String text;
	String username;
	
	public Message(Message m)
	{
		timeStamp = m.getTimeStamp();
		text = m.getText();
		username = m.getUsername();
	}
	
	public Message(String username, String text)
	{
		timeStamp = new Date();
		this.text = text;
		this.username = username;
	}
	
	public String getText() {
		return text;
	}

	public Date getTimeStamp() {
		return timeStamp;
	}

	public String getUsername() {
		return username;
	}

	public String toString()
	{
		return timeStamp.toString() + " " + username + " : " + text;	
	}
	
}
