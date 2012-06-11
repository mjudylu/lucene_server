package com.ericsson.otp.stdlib;

import java.util.logging.Logger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public abstract class OtpGenServer {
	private static final Logger jlog = Logger.getLogger(OtpGenServer.class.getName());
	private OtpMbox mbox;

	public OtpGenServer(OtpNode host, String name) {
		mbox = host.createMbox(name);
	}

	protected final OtpErlangPid getSelf() {
		return mbox.self();
	}

	public void start() throws OtpErlangException {
		boolean running = true;
		while (running) {
			try {
				jlog.fine("Receiving Requests at " + mbox.getName());
				OtpErlangObject o = mbox.receive();
				jlog.finer("Received Request: " + o);
				if (o instanceof OtpErlangTuple) {
					OtpErlangTuple msg = (OtpErlangTuple) o;
					OtpErlangAtom kind = (OtpErlangAtom) msg.elementAt(0);
					if (kind.atomValue().equals("$gen_call")
							&& ((OtpErlangTuple) o).arity() == 3) {
						OtpErlangTuple from = (OtpErlangTuple) msg.elementAt(1);
						OtpErlangObject cmd = msg.elementAt(2);
						OtpErlangObject reply = handleCall(cmd, from);
						reply(from, reply);
					} else if (kind.atomValue().equals("$gen_cast")
							&& ((OtpErlangTuple) o).arity() == 2) {
						OtpErlangObject cmd = msg.elementAt(1);
						handleCast(cmd);
					} else {
						handleInfo(o);
					}
				} else {
					handleInfo(o);
				}
			} catch (OtpErlangExit oee) {
				OtpErlangObject reason = oee.reason();
				jlog.warning("Linked process exited. Reason: " + reason);
				try {
					handleExit(oee);
				} catch (OtpStopException ose) {
					jlog.fine("Server stopping normally");
					running = false;
				}
			} catch (OtpContinueException ose) {
				running = true;
			} catch (OtpStopException ose) {
				jlog.fine("Server stopping normally");
				running = false;
			}
		}
		jlog.fine("...leaving");
	}

	protected void handleExit(OtpErlangExit oee) throws OtpErlangExit, OtpStopException {
		throw oee;
	}

	public void reply(OtpErlangTuple from, OtpErlangObject reply) {
		OtpErlangPid to = (OtpErlangPid) from.elementAt(0);
		OtpErlangRef tag = (OtpErlangRef) from.elementAt(1);
		OtpErlangTuple tuple = new OtpErlangTuple((new OtpErlangObject[] { tag,
				reply }));
		mbox.send(to, tuple);
	}

	protected abstract OtpErlangObject handleCall(OtpErlangObject cmd,
			OtpErlangTuple from) throws OtpStopException, OtpContinueException, OtpErlangException;

	protected abstract void handleCast(OtpErlangObject cmd) throws OtpStopException, OtpErlangException;

	protected abstract void handleInfo(OtpErlangObject cmd) throws OtpStopException;
}
