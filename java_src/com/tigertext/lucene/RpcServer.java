package com.tigertext.lucene;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.stdlib.OtpContinueException;
import com.ericsson.otp.stdlib.OtpGenServer;
import com.ericsson.otp.stdlib.OtpStopException;

public class RpcServer extends OtpGenServer {

	public RpcServer(OtpNode host) {
		super(host, "rpc_server");
	}

	@Override
	protected OtpErlangObject handleCall(OtpErlangObject cmd,
			OtpErlangTuple from) throws OtpStopException, OtpContinueException,
			OtpErlangException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected void handleCast(OtpErlangObject cmd) throws OtpStopException,
			OtpErlangException {
		// TODO Auto-generated method stub

	}

	@Override
	protected void handleInfo(OtpErlangObject cmd) throws OtpStopException {
		// TODO Auto-generated method stub

	}

}
