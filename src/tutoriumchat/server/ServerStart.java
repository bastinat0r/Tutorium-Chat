package tutoriumchat.server;

public class ServerStart {
	public static void main(String[] args) {
	    
		if(args.length > 0){
			int port = Integer.parseInt( args[0] );
			new Server(port);
			} else new Server(8080);
		}
	}