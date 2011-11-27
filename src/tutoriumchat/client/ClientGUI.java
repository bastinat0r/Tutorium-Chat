package tutoriumchat.client;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;

public class ClientGUI extends JFrame implements ActionListener {

    protected static final String textFieldString = "JTextField";
    protected static final String passwordFieldString = "JPasswordField";
    protected static final String buttonString = "JButton";
    private static final long serialVersionUID = 1L;
    JLabel debugLabel;
    JTextField textField;
    JPasswordField passwordField;
    Container panel;

    public ClientGUI() {
    }

    public void addComponentsToPane() {
        JLabel labelLogin = new JLabel("Login Form:    ");
        labelLogin.setBounds(5, 5, 150, 20);
        panel.add(labelLogin);

        debugLabel = new JLabel();
        debugLabel.setBounds(5, 65, 150, 20);
        panel.add(debugLabel);

        textField = new JTextField();
        textField.setActionCommand(textFieldString);
        textField.addActionListener(this);
        textField.setBounds(5, 25, 150, 20);
        panel.add(textField);

        passwordField = new JPasswordField();
        passwordField.setActionCommand(passwordFieldString);
        passwordField.addActionListener(this);
        passwordField.setBounds(5, 45, 150, 20);
        panel.add(passwordField);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        String prefix = "You typed \"";
        if (textFieldString.equals(e.getActionCommand())) {
            JTextField source = (JTextField) e.getSource();
            debugLabel.setText(prefix + source.getText() + "\"");
        } else if (passwordFieldString.equals(e.getActionCommand())) {
            JPasswordField source = (JPasswordField) e.getSource();
            debugLabel
                    .setText(prefix + new String(source.getPassword()) + "\"");
        } else if (buttonString.equals(e.getActionCommand())) {
        }
    }

    public void createGUI() {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        panel = new JPanel();
        getContentPane().add(panel);
        panel.setLayout(null);
        addComponentsToPane();
        setTitle("Java Tutorium Chat");
        setSize(250, 600);
        setLocationRelativeTo(null);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setVisible(true);
    }

    public static void main(String[] args) {
        new ClientGUI().createGUI();
    }
}
