package org.manhattan.tools.yaychat.utils;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.val;
import org.apache.commons.lang3.StringUtils;
import org.manhattan.tools.yaychat.dto.ChatDetail;
import org.manhattan.tools.yaychat.dto.Request;
import org.manhattan.tools.yaychat.dto.Record;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.DataInputStream;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;


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
        Request jsonReq = Request.builder().type(data.getRecordType()).data(data).build();
        val reqJson = toJson(jsonReq);
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

    static <T> T read(InputStream in, Class<T> clazz) throws IOException{
        DataInputStream dis = new DataInputStream(in);
        int packetLength = dis.readInt();
        byte[] buf = new byte[packetLength];
        dis.readFully(buf, 0, packetLength);
        val jsonResponse = new String(buf, StandardCharsets.UTF_8);
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
