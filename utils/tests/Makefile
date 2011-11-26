all:
	javac -classpath '.:../' *.java

# dirty misuse of this Makefile... :)
run:	
	java -classpath '.:../' PasswordTest
	java -classpath '.:../' AuthMessageTest
	java -classpath '.:../' SharedSecretsTest
clean:
	rm -f *.class ../*.class
	rm -f db.txt
