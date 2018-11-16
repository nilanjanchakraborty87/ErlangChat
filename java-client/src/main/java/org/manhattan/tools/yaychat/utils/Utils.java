package org.manhattan.tools.yaychat.utils;


import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;

import org.apache.commons.lang3.StringUtils;
import org.manhattan.tools.yaychat.dto.ChatDetail;
import org.manhattan.tools.yaychat.dto.Record;
import org.manhattan.tools.yaychat.dto.Registration;
import org.manhattan.tools.yaychat.dto.Request;
import org.manhattan.tools.yaychat.dto.Response;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.mashape.unirest.http.HttpResponse;
import com.mashape.unirest.http.Unirest;
import com.mashape.unirest.http.exceptions.UnirestException;


public interface Utils {

    Scanner scanner = new Scanner(System.in);
    ObjectMapper mapper = new ObjectMapper();
    Logger log = LoggerFactory.getLogger(Utils.class);

    static String prompt(String message){
        System.out.print("YayChat>> " + message + ": ");
        String response = scanner.nextLine();
        return StringUtils.isEmpty(response) ? prompt(message) : response;
    }

    static long prompt(String message, int radix){
        System.out.print("YayChat>> " + message + ": ");
        long answer = scanner.nextLong(radix);
        scanner.nextLine();
        return answer == 0 ? prompt(message, radix) : answer;
    }

    static String toJson(final Object e){
        String reqJson = null;
        try{
            reqJson = mapper.writeValueAsString(e);
        }
        catch(JsonProcessingException jpe){
            log.error("Error converting into json. Cause: ", jpe);
        }
        return reqJson;
    }

    static <T> T fromJson(final String source, Class<T> clazz){

        if(StringUtils.isBlank(source))
            return null;

        T obj = null;
        try{
            obj = mapper.readValue(source, clazz);
        }
        catch(IOException ie){
            log.error("Error converting from json. Cause: ", ie);
        }
        return obj;
    }

    static <T> T fromJson(final JsonNode source, Class<T> clazz){

        T obj = null;
        try{
            obj = mapper.treeToValue(source, clazz);
        }
        catch(IOException ie){
            log.error("Error converting from json. Cause: ", ie);
        }
        return obj;
    }

    static <T extends Record> boolean send(T data, OutputStream os){
        Request jsonReq = new Request();
        jsonReq.setType(data.getRecordType());
        jsonReq.setData(data);

        String reqJson = toJson(jsonReq);
        log.debug("Json request {}", reqJson);

        DataOutputStream dout = new DataOutputStream(os);
        try {
            dout.writeInt(reqJson.length());
            dout.write(reqJson.getBytes(StandardCharsets.UTF_8), 0, reqJson.length());
            return true;
        } catch (IOException e) {
            log.error("Error sending the request to server", e);
        }
        return false;
    }

    static <T extends Record> Response sendRegistration(Registration data){
        String reqJson = toJson(data);
        log.debug("Json request: {}", reqJson);



		try {
			HttpResponse<com.mashape.unirest.http.JsonNode> jsonResponse = Unirest.post("http://localhost:8080/yaychat/user/register")
			                                            .header("Content-Type", "application/json")
			                                            .body(reqJson)
			                                            .asJson();
			String responseJSONString = jsonResponse.getBody().toString();
			System.out.println("Response =>" + responseJSONString);
			return fromJson(responseJSONString, Response.class);

		} catch (UnirestException e) {
		}

        return null;


    }

    static <T> T read(InputStream in, Class<T> clazz) throws IOException{
        DataInputStream dis = new DataInputStream(in);
        int packetLength = dis.readInt();
        byte[] buf = new byte[packetLength];
        dis.readFully(buf, 0, packetLength);
        String jsonResponse = new String(buf, StandardCharsets.UTF_8);
        log.debug("Json response received {}", jsonResponse);
        return fromJson(jsonResponse, clazz);
     }

    static <T> T fromJsonNode(JsonNode node, Class<T> clazz) {
        if(node == null)
            return null;
        try {
            return mapper.treeToValue(node, clazz);
        } catch (JsonProcessingException e) {
            return null;
        }
    }

    static void showChat(ChatDetail c){
        System.out.println("Received message ============================================");
        System.out.println("Message: " + c.getMessage());
        System.out.println("Sender: " + c.getFrom());
        System.out.println("=============================================================");
    }


}
