package org.kframework.krun.ioserver.commands;

import org.kframework.krun.ioserver.resources.FileResource;
import org.kframework.krun.ioserver.resources.ResourceSystem;

import java.net.Socket;
import java.util.logging.Logger;

public class CommandSeek extends Command {


	private long ID;
	private int position;

	public CommandSeek(String[] args, Socket socket, Logger logger) { //, Long maudeId) {
		super(args, socket, logger); //, maudeId);
		
		try {
			ID = Long.parseLong(args[1]);
			position = Integer.parseInt(args[2]);
		} catch (NumberFormatException nfe) {
			fail("seek operation aborted: " + nfe.getLocalizedMessage());
		}
	}

	public void run() {
		// retrieve file struct
		FileResource resource = (FileResource)ResourceSystem.getResource(ID);
		
		try {
			resource.seek(position);
			succeed(new String[] { "success" });
		} catch (Exception e) {
			fail("seek: cannot peek from resource " + ID + " at position " + position);
		}

	}

}
