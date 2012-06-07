package com.tigertext.lucene;

import java.io.IOException;
import java.util.logging.Logger;

import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.stdlib.OtpGenServer;

public class LuceneNode {
	private static final Logger jlog = Logger.getLogger(LuceneNode.class.getName());

	public static void main(String[] args) {
		String nodeName = args.length >= 1 ? args[0] : "lucene@localhost";
		jlog.info("Hi, I'm the Java Lucene Node\nNode: " + nodeName);
		OtpNode node;
		try {
			node = args.length >= 2 ? new OtpNode(nodeName, args[1])
					: new OtpNode(nodeName);
			OtpGenServer server = new LuceneServer(node);
			while (true) {
				try {
					server.start();
					jlog.info("Node terminating since the server terminated normally");
					System.exit(0);
				} catch (Exception e) {
					jlog.severe("Server crashed: " + e);
					e.printStackTrace();
					jlog.info("Restarting the server");
				}
			}
		} catch (IOException e1) {
			jlog.severe("Couldn't create node: " + e1);
			e1.printStackTrace();
			System.exit(1);
		}
	}
}