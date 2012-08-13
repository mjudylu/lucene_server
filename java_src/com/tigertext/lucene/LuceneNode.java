package com.tigertext.lucene;

import java.io.IOException;
import java.util.logging.Logger;

import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.stdlib.OtpGenServer;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 * Main node for the Lucene Server
 */
public class LuceneNode {
	private static final Logger jlog = Logger.getLogger(LuceneNode.class.getName());

	/**
	 * Start up function
	 * @param args Command line arguments: [name] [cookie]
	 */
	public static void main(String[] args) {
		String nodeName = args.length >= 1 ? args[0] : "lucene@localhost";
		OtpNode node;
		try {
			node = args.length >= 2 ? new OtpNode(nodeName, args[1])
					: new OtpNode(nodeName);
			OtpGenServer server = new LuceneServer(node);
			jlog.info("Java Lucene Node Started at: " + nodeName);
			System.out.println("READY");
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