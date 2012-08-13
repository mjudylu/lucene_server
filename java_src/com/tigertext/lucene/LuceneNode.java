package com.tigertext.lucene;

import java.io.IOException;
import java.util.logging.Logger;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.stdlib.OtpGenServer;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net> Main node for the Lucene
 *         Server
 */
public class LuceneNode {
	private static final Logger	jlog	= Logger.getLogger(LuceneNode.class
												.getName());

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
		String nodeName = args.length >= 1 ? args[0]
				: "lucene_server_java@localhost";
		try {
			NODE = args.length >= 2 ? new OtpNode(nodeName, args[1])
					: new OtpNode(nodeName);
			PEER = peerName;
			final OtpGenServer server = new LuceneServer(NODE);
			final OtpGenServer rpc = new RpcServer(NODE);
			jlog.info("Java Lucene Node Started at: " + nodeName);
			forever(server);
			forever(rpc);
			System.out.println("READY");
		} catch (IOException e1) {
			jlog.severe("Couldn't create node: " + e1);
			e1.printStackTrace();
			System.exit(1);
		}
	}

	protected static void forever(final OtpGenServer server) {
		new Thread() {
			@Override
			public void run() {
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
			}
		}.start();
	}
}