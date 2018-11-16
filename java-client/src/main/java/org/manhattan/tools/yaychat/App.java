package org.manhattan.tools.yaychat;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.extern.slf4j.Slf4j;
import lombok.val;
import org.apache.commons.lang3.StringUtils;
import org.manhattan.tools.yaychat.dto.ChatDetail;
import org.manhattan.tools.yaychat.dto.Login;
import org.manhattan.tools.yaychat.dto.Registration;
import org.manhattan.tools.yaychat.dto.Response;
import org.manhattan.tools.yaychat.utils.Utils;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.Map;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import static org.manhattan.tools.yaychat.utils.Utils.*;


@Slf4j
public class App {

	private long loggedInMobile;
	private Lock lock = new ReentrantLock();
	private Socket socket = null;
	private InputStream in;
	private OutputStream os;
	private LinkedBlockingDeque<ChatDetail> queue = new LinkedBlockingDeque<>();

	private void init() throws IOException {

		String host = "127.0.0.1";
		int port = 9000;
    System.out.println("Host: "+ host + ":" + port);
		try {
			socket = new Socket(host, port);
			log.info("Successfully connected to yaychat server");
			in = socket.getInputStream();
			os = socket.getOutputStream();
		} catch (IOException e) {
			log.error("Error connecting yayChat Server", e);
			throw e;
		}

	}

	public static void main(String[] args) throws IOException {
		log.info("Starting up yaychat client");
		App app = new App();
		app.init();
		//send registration request
		app.register();
		//send login request
		app.login();
		//start chat acceptor
		Thread accept = new Thread(app::accept);
		accept.start();
		//show incoming chat
		Thread t = new Thread(app::show);
		t.setDaemon(true);
		t.start();
		//send chat
		app.chat();
		try {
			accept.join();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}


	}

	private void login() {
		System.out.println("YayChat>> Login Details >>");

		Login login = new Login();
		login.setMobile(prompt("Mobile", 10));
		login.setPassword(prompt("Password"));

		if (!send(login, os)) {
			System.err.println("Failed to login. Will retry");
			login();
		}
		try {
			Response response = read(in, Response.class);
			if (response != null && StringUtils.equals(response.getSuccess(), "true")) {
				loggedInMobile = Long.valueOf(((Map) response.getData()).get("mobile").toString());
				return;
			}
			System.err.format("Login failed. Reason: %s%n", response != null ? response.getMessage() : "");
			login();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void register() {
		System.out.println("YayChat>> Registration Details >>");
		String answer = prompt("Do you like to register (Y/N)");
		if (!StringUtils.equals("Y", StringUtils.upperCase(answer)) && !StringUtils.equals("YES", StringUtils.upperCase(answer))) {
			System.err.format("Seems registered, please login now");
			login();
		}

		Registration data = new Registration();
		data.setFname(prompt("FName"));
		data.setLname(prompt("LName"));
		data.setEmail(prompt("Email"));
		data.setMobile(prompt("Mobile", 10));
		data.setPassword(prompt("Password"));

		Response response = sendRegistration(data);

		if (response == null || StringUtils.equals(response.getSuccess(), "false")) {
			System.err.format("Registration failed. Reason: %s%n", response != null ? response.getMessage() : "");
			register();
		}
		else {
			System.out.println(response.getMessage());
			System.out.println("Registration is successful");
		}


	}

	private void chat() {
		System.out.println("YayChat>> Chat Details >>");

		lock.lock();
		ChatDetail chat = new ChatDetail();
		chat.setMessage(prompt("Message"));
		chat.setTo(prompt("Recipient", 10));
		chat.setFrom(loggedInMobile);

		log.debug("Sending chat {}", chat);
		lock.unlock();

		while (!send(chat, os)) {
			System.err.println("Failed to send. Will retry now");
			send(chat, os);
		}
		try {
			TimeUnit.SECONDS.sleep(1);
		} catch (InterruptedException e) {
		}
		chat();
	}

	private void accept() {
		while (!Thread.currentThread().isInterrupted()) {
			try {
				JsonNode c = Utils.read(in, JsonNode.class);
				log.debug("Received message {}", c);
				JsonNode type;
				if (c != null && ((type = c.get("type")) != null && StringUtils.equals(type.asText(), "chat"))) {
					ChatDetail chat = Utils.fromJsonNode(c.get("data"), ChatDetail.class);
					log.debug("Parsed message {}", chat);
					if (chat != null)
						queue.offer(chat);
				}
			} catch (EOFException e) {
				log.warn("Unable to accept new message", e);
			}
			catch (IOException e) {
				log.error("{} is terminating", Thread.currentThread().getName());
				break;
			}
		}
	}

	private void show() {
		while (!Thread.currentThread().isInterrupted()) {
			try {
				ChatDetail c = queue.take();
				lock.lock();
				showChat(c);
			} catch (InterruptedException e) {
				log.error("{} is terminating", Thread.currentThread().getName());
				break;
			} finally {
				lock.unlock();
			}

		}
	}


}
