package com.tigertext.lucene;

import java.io.IOException;
import java.util.logging.Logger;

import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.stdlib.OtpGenServer;

/**
 * Main node for the Lucene Server
 * 
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 */
public class LuceneNode {
	private static final Logger	jlog	= Logger.getLogger(LuceneNode.class
												.getName());

	private static int			ALLOWED_THREADS;

	/**
	 * This Erlang node
	 */
	public static OtpNode		NODE;

	/**
	 * Peer node name
	 */
	public static String		PEER;

	/**
	 * Start up function
	 * 
	 * @param args
	 *            Command line arguments: [name] [cookie]
	 */
	public static void main(String[] args) {
		String peerName = args.length >= 1 ? args[0]
				: "lucene_server@localhost";
		String nodeName = args.length >= 2 ? args[1]
				: "lucene_server_java@localhost";
		try {
			NODE = args.length >= 3 ? new OtpNode(nodeName, args[2])
					: new OtpNode(nodeName);
			PEER = peerName;
			ALLOWED_THREADS = args.length >= 4 ? Integer.parseInt(args[3]) : 25;
			final OtpGenServer server = new LuceneServer(NODE, ALLOWED_THREADS);
			jlog.info("Java Lucene Node Started at: " + nodeName
					+ "\nConnected to: " + PEER);
			server.start();
			System.out.println("READY");
		} catch (IOException e1) {
			jlog.severe("Couldn't create node: " + e1);
			e1.printStackTrace();
			System.exit(1);
		}
	}
}