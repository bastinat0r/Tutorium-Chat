package tutoriumchat.client;
import java.util.Scanner;
import java.net.*;

public class Client {
    
    // class instance variables
    private String name = "User";

    // needed for using input strings in command switch
    public enum Command {

        EXIT("/exit"),
        LEAVE("/leave"),
        QUIT("/quit"),
        NEW_NICK("/nick"),
        RENAME("/name"),
        DEFAULT("");

        private String text;

        Command(String text) {
            this.text = text;
        }

        // convert String to enum
        public static Command fromString(String text) {
            for (Command b : Command.values()) {
                if (text.equalsIgnoreCase(b.text))
                    return b;
            }
            return DEFAULT;
        }
    }

    // ChatClient methods

    public static void main (String [] args) throws Exception
    {
        (new Client()).run();
    }

    public void run () throws Exception {
        localPrint("Your messages:"); // TODO: remove
        String input = "";

        while(true) {
            input = getInput();

            // parse command or send input as message
            if ( input.indexOf('/') == 0 ) {
                parseCommand(input);
            } else {
                send(this.name + ": " + input);
            }
        }
    }

    // send text to server
    // TODO: use POST or non-http send
    // TODO: send username separate from text
    private void send( String text ) throws Exception {
        text = URLEncoder.encode( text, "UTF-8" );
        URL u = new URL( "http://bastinat0r.de/chat/chat.php?c=" + text );
        u.openStream();
    }

    private void chatExit( int errorCode ) {

        System.exit(errorCode);
    }

    // get line from terminal
    // TODO: accept control characters / replace with GUI-method
    private String getInput () {
        Scanner scan = new Scanner(System.in);
        try {
            return scan.nextLine().trim();
        } catch( Exception e ) {return "";}
    }

    private void localPrint( String text ) {
        System.out.println(text); // TODO: replace with GUI-print
    }

    private void parseCommand( String command ) throws Exception {

        // split command from parameters
        int splitIndex = command.indexOf(' ');
        String parameters;
        if (splitIndex > 0) {
            parameters = command.substring( splitIndex + 1, command.length()).trim();
            command = command.substring(0, splitIndex);
        } else {
            parameters = "";
        }
  
        // parse and execute command
        switch ( Command.fromString(command) ) {
            case EXIT:
            case LEAVE:
            case QUIT:
                chatExit(0);
                break;
            case NEW_NICK:
            case RENAME:
                if( parameters.isEmpty() )
                    localPrint("Cannot change name without name parameter!");
                else
                    changeName(parameters);
                break;
            default:
                localPrint("Unknown command value '" + command + "'!");
        }
    }

    // COMMANDS:
    
    private void changeName( String name ) throws Exception {
        send(this.name + " changed name to " + name);
        this.name = name;
    }
}
