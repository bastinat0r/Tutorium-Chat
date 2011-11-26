all: clean build

build:
	javac tutoriumchat/server/ServerStart.java -Xlint:unchecked
clean:
	rm -f tutoriumchat/utils/*.class
	rm -f tutoriumchat/server/*.class
server:
	java tutoriumchat.server.ServerStart
